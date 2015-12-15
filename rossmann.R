# !/bin/usr/Rscript
library(lubridate)
library(caret)
library(randomForest)
library(dplyr)

train <- read.csv('/home/dheeraj/kaggle/Rossmann/train.csv', stringsAsFactors = F)
test <- read.csv('/home/dheeraj/kaggle/Rossmann/test.csv', stringsAsFactors = F)
sample_sub <- read.csv('/home/dheeraj/kaggle/Rossmann/sample_submission.csv', stringsAsFactors = F)
store <- read.csv('/home/dheeraj/kaggle/Rossmann/store.csv', stringsAsFactors = F)
sum(test$Id %in% store$Store)
sum(test$Id %in% train$Id)
train_store_merged <- merge(x= train, y= store, by='Store', all.x = T)
test_store_merged <- merge(x= test, y= store, by= 'Store', all.x= T)
summary(train_store_merged)
summary(test_store_merged)
#since date variable is character, converting it into date class
train_store_merged$Date <- as.Date(train_store_merged$Date)
test_store_merged$Date <- as.Date(test_store_merged$Date)
#converting character variables into factors
character_vars <- lapply(train_store_merged, class)
character_vars <- names(train_store_merged)[lapply(train_store_merged, class) == 'character']
for(i in character_vars){
  train_store_merged[[i]] <- as.factor(train_store_merged[[i]])
  test_store_merged[[i]] <- as.factor(test_store_merged[[i]])
  }
#extracting day, month and year from date
train_store_merged$day <- day(train_store_merged$Date)
train_store_merged$month <- month(train_store_merged$Date) 
train_store_merged$year <- year(train_store_merged$Date)
train_store_merged$dayname <- weekdays(train_store_merged$Date, abbreviate = T)
test_store_merged$day <- day(test_store_merged$Date)
test_store_merged$month <- month(test_store_merged$Date) 
test_store_merged$year <- year(test_store_merged$Date)
test_store_merged$dayname <- weekdays(test_store_merged$Date, abbreviate = T)
#checking for 0 variance 
# nearZeroVar(train_store_merged, saveMetrics = T)
# #checking for linear combined variables
# findLinearCombos(train_store_merged)
#getting ns'a count for each column
na_count <- data.frame(count = apply(train_store_merged, 2, function(x) sum(is.na(x))))
na_count$na_percent = (na_count$count / nrow(train_store_merged))*100
train_store_merged1 <- train_store_merged
train_store_merged1[is.na(train_store_merged1)] <- 0
test_store_merged1 <- test_store_merged
test_store_merged1[is.na(test_store_merged1)] <- 0
#removing columns with high %na values
train_store_merged1 <- subset(train_store_merged, select = -c(Promo2SinceWeek, Promo2SinceYear, CompetitionOpenSinceMonth,
                                                             CompetitionOpenSinceYear, CompetitionDistance))
test_store_merged1 <- subset(test_store_merged, select = -c(Promo2SinceWeek, Promo2SinceYear, CompetitionOpenSinceMonth,
                                                              CompetitionOpenSinceYear, CompetitionDistance))
#removing date
train_store_merged1 <- subset(train_store_merged1, select = -c(Date))
train_store_merged1$PromoInterval <- as.character(train_store_merged1$PromoInterval)
train_store_merged1$PromoInterval[train_store_merged1$PromoInterval == ""] <- 0
train_store_merged1$PromoInterval <- as.factor(train_store_merged1$PromoInterval)
str(train_store_merged1)
test_store_merged1 <- subset(test_store_merged1, select = -c(Date))
test_store_merged1$PromoInterval <- as.character(test_store_merged1$PromoInterval)
test_store_merged1$PromoInterval[test_store_merged1$PromoInterval == ""] <- 0
test_store_merged1$PromoInterval <- as.factor(test_store_merged1$PromoInterval)
str(test_store_merged1)
vars <- c('DayOfWeek', 'Promo', 'SchoolHoliday', 'Promo2', 'month', 'year', 'day', 'dayname')
for(i in vars){
  train_store_merged1[[i]] <- as.factor(train_store_merged1[[i]])
  test_store_merged1[[i]] <- as.factor(test_store_merged1[[i]])
}
#model data
train_store_merged1 <- subset(train_store_merged1, train_store_merged1$Open == 1)
train_store_merged1 <- subset(train_store_merged1, train_store_merged1$Customers != 0)
test_store_merged1 <- subset(test_store_merged1, test_store_merged1$Open == 1)

smp_size <- floor(0.70 *nrow(train_store_merged1))
set.seed(123)
train_index <- sample(seq_len(nrow(train_store_merged1)), size = smp_size)
train_rm <- train_store_merged1[train_index, ]
test_rm <- train_store_merged1[-train_index, ]
features <- setdiff(names(train_store_merged1), c('Customers', 'Sales', 'Open', 'dayname', 'day'))
rm(list = setdiff(ls(), c('train_rm', 'test_rm', 'test_store_merged1', 'features')))
model <- randomForest(train_rm[,features], 
                      log(train_rm$Sales+1),
                      
                      ntree=20,
                      sampsize=100000,
                      do.trace=TRUE)
model <- randomForest(train_rm[,features], 
                      train_rm$Sales,
                      mtry=5,
                      ntree=20,
                      sampsize=100000,
                      do.trace=TRUE)
save(model, file= '/home/dheeraj/kaggle/Rossmann/model.Rda')
test_rm <- subset(test_rm, test_rm$Customers > 5)
test_rm$predict <- exp(predict(model, test_rm)) -1
test_rm$error <- ((test_rm$Sales-test_rm$predict)/test_rm$Sales)^2
#make same levels for train and test
for(i in test_store_merged1[lapply(test_store_merged1, function(x) class(x)) == 'factor']){
  levels(test_store_merged1[[i]]) <- levels(train_rm[[i]])
}
test_store_merged1$predicted_sales <- exp(predict(model, test_store_merged1))-1
test_store_merged2 <- subset(test_store_merged1, select = c(Id, predicted_sales))
final <- merge(x= test, y= test_store_merged2, by= 'Id', all.x= T)
final <- subset(final, select = c(Id, predicted_sales))
final[is.na(final)] <- 0
