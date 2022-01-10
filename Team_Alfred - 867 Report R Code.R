#load the libraries
library(dplyr) 
library(ggplot2) 
library(readxl) 
library(fpp)

#load the data
sales_train <- read.csv("/Users/jacquelinemak/MMA /MMA 867 Predictive Modelling/Team Project/competitive-data-science-predict-future-sales/sales_train.csv")

#explore the data
head(sales_train)
str(sales_train)
summary(sales_train)
dim(sales_train)

#Number of missing data for each variable 
lapply(sales_train, function(x)
  sum(is.na(x))) #no missing data

#convert data variable to date format
sales_train <- sales_train %>%
  mutate(date = gsub("[.]", "/", date))%>%
  mutate(date = as.Date.character(date, format="%d/%m/%Y"))

# convert number variable into factor variable  
sales_train <- sales_train %>%
  mutate(shop_id=factor(shop_id))%>%
  mutate(item_id=factor(item_id))%>%
  mutate(month=factor(month(date)))%>%
  mutate(year=factor(year(date)))

#Visualizations
#Viz 1. Top 10 - Highest Selling Shops
popular_shops <-sales_train%>% 
  group_by(shop_id) %>% 
  summarize(total_count = sum(item_cnt_day)) %>% 
  ungroup() %>% 
  arrange(desc(total_count))

head(popular_shops,10)
options(scipen=999)

head(popular_shops,10) %>% 
  ggplot(aes(x = reorder(as.factor(shop_id), total_count), y = total_count,fill=as.factor(shop_id))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Quantity Sold', x = 'Shops', title = 'Top 10 - Highest Selling Shops') +
  coord_flip()

# Viz 2. Top 10 Highest Selling Items 
popular_items <-sales_train%>% group_by(item_id) %>% summarize(Icount = sum(item_cnt_day)) %>% ungroup() %>% arrange(desc(Icount))
head(popular_items,10)

head(popular_items,10) %>% ggplot(aes(x = reorder(as.factor(item_id), Icount), y = Icount,fill=as.factor(item_id))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none")+
  labs(y = 'Total sales', x = 'Item ID', title = 'Highest Selling Items') +
  coord_flip()

#Viz 3. Most Profiting Shops
pop_items_per_shop <- sales_train %>%
  group_by(shop_id, item_id) %>%
  summarise(Revenue = sum(item_cnt_day*item_price)) %>%
  filter(Revenue == max(Revenue)) %>%
  arrange(desc(Revenue)) %>%
  ungroup()
top_10_revenue <- head(pop_items_per_shop,10)

ggplot(data=top_10_revenue, aes(x = reorder(as.factor(shop_id), Revenue), y = Revenue, fill = as.factor(item_id))) + 
  geom_bar(stat = "identity", fill = 'lightblue') + 
  coord_flip() +
  theme(legend.position = "none")+
  labs(title= "Most profiting Item per Shop", x= "Shop ID", y = "Revenue", fill = "Item ID")

#-------------------------------
#Summarize products sold by days 
sales_train_daily <- sales_train %>%
  group_by(date) %>%
  summarise(item_cnt_daily = sum(item_cnt_day))

#assign new variable to item count daily
sales_train_daily_temp <- sales_train_daily$item_cnt_daily

#change to time series object
sales_train_daily1 <- ts(sales_train_daily_temp, frequency=365, start=c(2013, 1))

#Check for variance
plot.ts(sales_train_daily1, xlab="Year", ylab="Items sold daily")

#Check seasonality
library(fpp)
Acf(diff(sales_train_daily1,1),lag.max =25) 
#big spike at lag 7, 14, 21; the pattern is weeks, multiple of 7

# We now remove the seasonality using seasonal differencing
sales_train_daily1.deSeasonality <- diff(sales_train_daily1,7)
plot.ts(sales_train_daily1.deSeasonality, xlab="Date", ylab="Items sold daily after removing trend and seasonality") #theres no weekly pattern
Acf(sales_train_daily1.deSeasonality,lag.max =25) 

#-------------Automatic ARIMA Modeling -------------------
# To begin, we use an automated algorithm to find a good model. However, there is no guarantee that it is the best model. So we treat it as a starting point. 
model.auto.daily.sales <- auto.arima(sales_train_daily1.deSeasonality, stepwise=FALSE, seasonal= FALSE) 
model.auto.daily.sales

# It suggests a ARIMA(2,0,3) model with 0 mean
checkresiduals(model.auto.daily.sales)  # Check the quality of fit. Residuals should: 
# (1) not have any significant autocorrelation
# (2) follow normal distribution
# (3) have stable variance over time

#we can now fit the model, autoarima suggested 2, 0, 3
# We can use the auto selected model to make forecasting; the arima function can build the model
fit.daily.sales1 <- Arima(sales_train_daily1.deSeasonality, order=c(2,0,3))
fit.daily.sales1

####################################################
#improve the model but manually choosing the p, d, q
####################################################
fit.daily.sales2 <- Arima(sales_train_daily1.deSeasonality, order=c(3,0,3)) #slightly better
fit.daily.sales2

fit.daily.sales3 <- Arima(sales_train_daily1.deSeasonality, order=c(1,0,3)) #worse than autoarima
fit.daily.sales3

fit.daily.sales4 <- Arima(sales_train_daily1, order=c(20,0,8),seasonal=list(order=c(0,1,0),period=7)) #model with the lowest AIC/BIC/AICc
fit.daily.sales4

#check residuals for final model
checkresiduals(fit.daily.sales4) #looks good 

fc1.SALES<-forecast(fit.daily.sales4,30) #Sales Forecast - number of products sold per day in november 2015

#forecast 
fc1.SALES$mean
autoplot(fc1.SALES)

#####################################################
#Sales Forecast - Number of products sold by shop_id
#####################################################

#check the number of shops
unique(sales_train$shop_id) #60 shops

#split the train dataset based on the threshold for item priced at $1000

#Item priced at <= $1000 and grouped by shop_id and date
sales_train1 <- sales_train %>%
  filter(sales_train$item_price <= 1000) %>%
  group_by(date, shop_id) %>%
  summarise(count = sum(item_cnt_day))

#Item priced at > $1000 and grouped by shop_id and date
sales_train2 <- sales_train %>%
  filter(sales_train$item_price > 1000) %>%
  group_by(date, shop_id) %>%
  summarise(count = sum(item_cnt_day))

#------------------------------------- to be removed 
#sales data by shop (to be removed)
sales_by_shop <- data.frame()
#sales_by_shop <- sales_train1 %>%
#  ungroup(sales_train1) %>%
#  group_by(date) %>% 
#  filter(sales_train1$shop_id == 2)

#for(i in 0:59){
#  sales_by_shop[i+1] <- sales_train %>%
#    filter(sales_train$shop_id == i)
#}
#------------------------------------- 

#Split the dataset by shop_id 
split.data<- split(sales_train1, sales_train1$shop_id)

#For testing purposes - not in final model 
#using order 20,0,8  as this is model with the lowest AIC/BIC/AICc for Shop ID 1

split.data1 <- split.data[1]
fit.daily.sales.byshop.self <- Arima(split.data1$'0'$count, order = c(20,0,8))
fit.daily.sales.byshop.self
fc <- forecast(fit.daily.sales.byshop.self, 30)
fc$mean
autoplot(fc)
#create empty datasets
#fit.daily.sales.byshop.self
#fit.daily.sales.byshop.auto


######################################################################
#For loop to forecast sales for items priced at or below $1000 by shop_id
######################################################################

forecasted_item_count_by_shop <- list()
sum_of_forecasted_item_by_shop <- matrix(nrow = 60, ncol = 2) 

for(i in 1:60){
  num_temp <- i-1
  data_temp <-split.data[i]
  ts.data_temp <- ts(data_temp[[as.character(num_temp)]]$count, frequency=365, start=c(2013, 1))
  fit_temp <- auto.arima(ts.data_temp, stepwise=FALSE, seasonal= FALSE, max.p = 8, max.q = 8, max.order = 8, trace = TRUE)
  best_aic <- fit_temp$aic
  best_model <- fit_temp
  print("Forecast for shop")
  print(as.character(num_temp))
  print(fit_temp$arma)
  print(best_model$arma)
  print(fit_temp$aic)
  forecasted_item_count_by_shop[[i]] <- forecast(fit_temp, 30)#our best model
  print(forecasted_item_count_by_shop[[i]])
  sum_of_forecasted_item_by_shop[i,1] <- num_temp
  sum_of_forecasted_item_by_shop[i,2] <- sum(forecasted_item_count_by_shop[[i]]$mean)
}

#Top 10 for items price at or below $1000 by shop_id
top_10_shops <- as.data.frame(sum_of_forecasted_item_by_shop) 

desc_order_top_10 <- top_10_shops %>%
  arrange(desc(V2))

top_ten_shops1 <- head(desc_order_top_10,10)

top_ten_shops1 %>% 
  ggplot(aes(x = reorder(as.factor(V1), V2), y = V2,fill=as.factor(V1))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Quantity Sold', x = 'Shops', title = 'Top 10 - Highest Selling Shops for items priced at or below $1000') +
  coord_flip()

#Bottom 10 for items price at or below $1000 by shop_id
bottom_10_shops <- as.data.frame(sum_of_forecasted_item_by_shop) 

ascen_order_bottom_10 <- bottom_10_shops %>%
  arrange(V2)

bottom_ten_shops1 <- head(ascen_order_bottom_10,10)

bottom_ten_shops1 %>% 
  ggplot(aes(x = reorder(as.factor(V1), V2), y = V2,fill=as.factor(V1))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Quantity Sold', x = 'Shops', title = 'Bottom 10 - Lowest Selling Shops for items priced at or below $1000') +
  coord_flip()
  
######################################################################
#For loop to forecast sales for items priced above $1000 by shop_id
######################################################################

split.data1<- split(sales_train2, sales_train2$shop_id)
forecasted_item_count_by_shop1 <- list()
sum_of_forecasted_item_by_shop1 <- matrix(nrow = 60, ncol = 2)

for(i in 1:60){
  num_temp <- i-1
  data_temp <-split.data1[i]
  ts.data_temp <- ts(data_temp[[as.character(num_temp)]]$count, frequency=365, start=c(2013, 1))
  fit_temp <- auto.arima(ts.data_temp, stepwise=FALSE, seasonal= FALSE,max.p = 8, max.q = 8, max.order = 8, trace = TRUE)
  print("Forecast for shop")
  print(as.character(num_temp))
  print(fit_temp)
  forecasted_item_count_by_shop1[[i]] <- forecast(fit_temp, 30)
  print(forecasted_item_count_by_shop1[[i]])
  sum_of_forecasted_item_by_shop1[i,1] <- num_temp
  sum_of_forecasted_item_by_shop1[i,2] <- sum(forecasted_item_count_by_shop1[[i]]$mean)
}

bar_graph1 <- 
  ggplot(as.data.frame(sum_of_forecasted_item_by_shop1), aes(x=V1, y=V2))+ 
  geom_bar(stat = "identity", colour = "black", fill= "lightpink") +
  labs(x="Shop ID", y = "Quantity Sold") +
  ggtitle("Forecasted count for items priced above $1000") +
  theme_minimal() 
bar_graph1

#Top 10 for items price above $1000 by shop_id
top_10_shops_b <- as.data.frame(sum_of_forecasted_item_by_shop1) 

desc_order_top_10_b <- top_10_shops_b %>%
  arrange(desc(V2))

top_ten_shops2 <- head(desc_order_top_10_b,10)

top_ten_shops2 %>% 
  ggplot(aes(x = reorder(as.factor(V1), V2), y = V2,fill=as.factor(V1))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Quantity Sold', x = 'Shops', title = 'Top 10 - Highest Selling Shops for items priced above $1000') +
  coord_flip()

#Bottom 10 for items priced above $1000 by shop_id
bottom_10_shops_b <- as.data.frame(sum_of_forecasted_item_by_shop1) 

ascen_order_bottom_10_b <- bottom_10_shops_b %>%
  arrange(V2)

bottom_ten_shops2 <- head(ascen_order_bottom_10_b,10)

bottom_ten_shops2 %>% 
  ggplot(aes(x = reorder(as.factor(V1), V2), y = V2,fill=as.factor(V1))) +
  geom_bar(stat = 'identity') + 
  theme(legend.position = "none") +
  labs(y = 'Quantity Sold', x = 'Shops', title = 'Bottom 10 - Lowest Selling Shops for items priced above $1000') +
  coord_flip()












