#################################################Cab Rentals Project########################################
x = c("ggplot2", "corrgram", "DMwR", "caret",  "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", 'sampling', 'DataCombine', "VIM")
library(ggplot2)
install.packages(x)
library(corrgram)
library(dplyr)
setwd("C:/Users/Mahesh/Desktop/Final Project")
train_cab<- read.csv("train_cab.csv", header=T, na.strings = c(" ", "" ,"N.A"))
 ####sum(is.na(train_cab)) = 1393 - Total null values in train_cab
#str(train_cab)

####Conversion of fare_amount from factor to character to numeric as data in fare_amount got altered when direct conversion took place####
train_cab$fare_amount <- as.character(train_cab$fare_amount)          
train_cab$fare_amount <- as.numeric(train_cab$fare_amount) 

#Data Cleaning - It is imperative that before performing any sort of Missing Value Analysis or Outlier Analysis,we first explore the data
#and find out peculiarities about it.

#The process in this project is divided into  several parts : Detecting invalid passenger_count i.e less than 1 or greater than 6 as the 
#underlying assumption is that more than 6 passengers can't enter the car


#1.Removing less than 1  passenger_count and the one's whose value exceeds 6 
View(train_cab[which(train_cab$passenger_count < 1 ),]) - #Total 58 rows with passenger_count as 0 which needs removal
for( i in 1:nrow(train_cab)){                               
  if (!is.na(train_cab$passenger_count[i])){
    if(train_cab$passenger_count[i] < 1 ){
      train_cab <- train_cab[-i,]
  }
  }
}
#nrow(train_cab) = #16009
#Removing rows with passenger_count value exceeding 6 

View(train_cab[which(train_cab$passenger_count > 6 ),]) - # Total 20 rows with passenger_count exceeding 6 which needs removal 
for( i in 1:nrow(train_cab)){
  if (!is.na(train_cab$passenger_count[i])){
    if(train_cab$passenger_count[i] > 6 ){
      train_cab <- train_cab[-i,]
    }
  }
}
#nrow(train_cab) = 15989  
#So, in total 78 rows have been removed due to invalid passenger_count

#Removing invalid latitude and longitude values
#Valid Range for latitude is between -90 and 90 while for longitude is -180 and 180
#range(train_cab$pickup_longitude) - -74.43823  40.76613 
#range(train_cab$pickup_latitude) -  -74.00689  401.08333
#range(train_cab$dropoff_longitude)) - -74.42933  40.80244
#range(train_cab$dropoff_latitude)-  -74.00638  41.36614

nrow(train_cab[which(train_cab$pickup_latitude> 90 ),]) # Result is 1 i.e. that is 1 variable needs to be removed

for( i in 1 :nrow(train_cab)){
  if (!is.na(train_cab$pickup_latitude[i])){
    if((train_cab$pickup_latitude)[i] > 90 ){   #Valid values for latitude is between -90 and 90 
      train_cab <- train_cab[-i,]
    }
  }
} 

#nrow(train_cab) - 15988 rows
#1 row removed

#Before exploring data further , we can also derive Distance from Haversine Formula and check for some invalid values
#For more info : https://andrew.hedges.name/experiments/haversine/

cnames <- c("pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")
for(i in cnames){
  print(i)
  train_cab[,i] <- train_cab[,i] * (pi/180)               #Converting degrees to radians  
}



for(i in 1:nrow(train_cab)){
  dlon <- (train_cab$dropoff_longitude)[i] - (train_cab$pickup_longitude)[i]                         
  dlat <- (train_cab$dropoff_latitude)[i]  - (train_cab$pickup_latitude)[i]                           
  a <- ( (sin(dlat/2))^2 + ( cos(train_cab$pickup_latitude[i]) *                 
                               cos(train_cab$dropoff_latitude[i]) * (sin(dlon/2))^2 ) )
  
  b <- 2 * atan2(sqrt(a) , sqrt(1-a))
  train_cab$Distance[i] = (6373 * b)                                  # 6373 kilometers = Radius of the earth 
}

nrow(train_cab[which(train_cab$Distance == 0),]) # Total rows which distance == 100 equals 456 which needs removal
#View(train_cab[which(train_cab$Distance == 0),])

print(paste("Before removal of 0 valued Distance column , the number of rows were",nrow(train_cab))) #15988 rows
train_cab = train_cab[-which(train_cab$Distance == 0),]
print(paste("After removal of 0 valued Distance column , the number of rows were",nrow(train_cab))) #15532 rows

#Now onto the removal of latitude and longitude valued 0 as it belongs to the equator.
print(paste("Pickup longitude equals to 0=",nrow(train_cab[which(train_cab$pickup_longitude == 0),])))
print(paste("Pickup latitude equals to 0=",nrow(train_cab[which(train_cab$pickup_longitude == 0),])))
print(paste("Dropoff latitude  equals to 0=",nrow(train_cab[which(train_cab$dropoff_latitude == 0),])))
print(paste("Dropoff longitude equals to 0=",nrow(train_cab[which(train_cab$dropoff_longitude == 0),])))

#train_cab_2 <- train_cab
#train_cab <- train_cab_2
print(paste("Before removal of zero valued latitudes and longitudes , the number of rows present were :" ,nrow(train_cab)))

#All the values 
for( i in 1:nrow(train_cab)){
  if(train_cab$pickup_longitude[i] == 0){
    train_cab <- train_cab[-i,]
  }
}
#All the rows with pickup_longitude and pickup_latitude valued 0 are removed as all of them were paired together

#Now onto the dropoff_latitude and dropoff_longitude
print(paste("Dropoff latitude  equals to 0=",nrow(train_cab[which(train_cab$dropoff_latitude == 0),])))
print(paste("Dropoff longitude equals to 0=",nrow(train_cab[which(train_cab$dropoff_longitude == 0),])))

for( i in 1:nrow(train_cab)){
  if(train_cab$dropoff_latitude[i] == 0){
    train_cab <- train_cab[-i,]
}
} # 9 rows gets removed due to invalid dropoff_latitude

for( i in 1:nrow(train_cab)){
  if(train_cab$dropoff_longitude[i] == 0){
    train_cab <- train_cab[-i,]
  }
}

#train_cab_2 <- train_cab

#Thus total 2 values were removed due to the presence of 0 valued pickup_longitudes while 9 got removed along with pickup_latitudes
print(paste("After removal of zero valued latitudes and longitudes , the number of rows present were :" ,nrow(train_cab)))



#2.Removing fare_amount whose value is less than 1 and very large amount- Removes total 6 variables 

ggplot(train_cab , aes(Distance , fare_amount)) + geom_point(size=3)
#From the above scatterplot we can observe some peculiar observations : 
#2 Observations where the distance exceeds 1000 km yet the fare remains very bare minimum.
#Upon further inspection , it is revealed that the passenger_count is 1
#Thus ,it's better to remove these observations as its meaningless 
View(train_cab[which(train_cab$fare_amount >1000),])
train_cab  <- train_cab[-which(train_cab$fare_amount > 1000 ),] # No of rows falls down from 15511 to 15509

#The other is the one with very high fare_amount yet minimal distance

View(train_cab[which(train_cab$Distance > 1000 ),])
train_cab = train_cab[-which(train_cab$Distance > 1000),] # Number of rows falls down to 15507

#Now removal of fare_amount which are less than 1
#There's 4 of them - which needs removal 
train_cab_2 <- train_cab

View(train_cab[which(train_cab$fare_amount < 1),])
train_cab <- train_cab[-which(train_cab$fare_amount < 1),] # 5 rows gets removed

#The total number of rows eliminated with data cleaning is 565.

#####Extracting information from pickup_datetime###
datetime <- select(train_cab , pickup_datetime)
train_cab <- select(train_cab, -pickup_datetime)
datetime$pickup_date <- as.Date(datetime$pickup_datetime)
datetime$month <- as.factor(format(datetime$pickup_date , "%m"))
datetime$Year <- as.factor(format(datetime$pickup_date , "%y"))
datetime$day <- as.numeric(format(datetime$pickup_date , "%d"))
datetime$pickup_datetime <- as.character(datetime$pickup_datetime) #Extracting hours by converting factor datetime into character 
datetime$hour <- substr(datetime$pickup_datetime,12,13)           # and then using the substring function    
train_cab <- select(train_cab , fare_amount,passenger_count,Distance,month,Year,day,hour)
#train_cab_2<- train_cab
 


#####################Missing Value Analysis###########################################

missing_val <- sapply(train_cab,function(x){sum(is.na(x))})
missing_val <- as.data.frame(missing_val)
View(missing_val)
missing_val$Columns <- rownames(missing_val) 
#Indexing of missing_val 
for(i in 1:nrow(missing_val)){
    row.names(missing_val)[i] <- i     
}
for( i in 1:nrow(missing_val)){
  missing_val$missing_val[i] <- (missing_val$missing_val[i]/nrow(train_cab)) * 100
}

#Converting factorial to numerical to facilitate KNN imputation
train_cab$Year <- as.numeric(train_cab$Year)
train_cab$month <- as.numeric(train_cab$Year)
train_cab$hour<- as.numeric(train_cab$hour)


df <- train_cab #creates a temporary variable
train_cab$fare_amount[1] # 4.5
train_cab$fare_amount[1] = NA
train_cab$fare_amount[is.na(train_cab$fare_amount)] <- mean(train_cab$fare_amount,na.rm = TRUE)  #train_cab$fare_amount[1,] = 11.36888
train_cab <-df
train_cab$fare_amount[1] = NA
train_cab$fare_amount[is.na(train_cab$fare_amount)] <- median(train_cab$fare_amount,na.rm = TRUE) #train_cab$fare_amount[1,] = 8.5
train_cab <- df
train_cab$fare_amount[1] =NA
train_cab = knnImputation(train_cab , k = 3) # train_cab$fare_amount[1,] = 7.54,k=5 =8.49,k=5 = 8.98
### Missing value gets imputed with KnnImputation as the predicted value via KNN is the closes to the actual value 

train_cab <-df
train_cab = knnImputation(train_cab , k = 3)
sum(is.na(train_cab))
train_cab$passenger_count <- ceiling(train_cab$passenger_count)
#train_cab_2 <- train_cab

str(train_cab)
table(train_cab$passenger_count)
train_cab$passenger_count <- as.factor(train_cab$passenger_count)
train_cab$pickup_datetime = datetime$pickup_datetime
train_cab$pickup_date = datetime$pickup_date

train_cab$Month = datetime$month
train_cab$Year = datetime$Year
train_cab$hour = datetime$hour
train_cab$day = datetime$day

######Outlier Analysis##########
ggplot(train_cab , aes(passenger_count,fare_amount)) + geom_boxplot()
#Lets get a rough idea of the number of outliers present in the dataframe
nrow(train_cab[which(train_cab$fare_amount > 20),])
#mean(train_cab$fare_amount) - 11.37
#1624 outliers are present in the data frame
#It is imperative that we get rid of these outliers as they can cause unnecessary deflection in our final prediction model

val = train_cab$fare_amount[train_cab$fare_amount %in% boxplot.stats(train_cab$fare_amount)$out]
train_cab$fare_amount[train_cab$fare_amount %in% val] = NA
train_cab = select(train_cab,fare_amount,passenger_count,Distance,Month,Year,day,hour)
train_cab$hour = as.numeric(train_cab$hour)
train_cab = knnImputation(train_cab , k = 3)

str(train_cab)
train_cab$passenger_count <- as.numeric(train_cab$passenger_count)
train_cab$Month <- as.numeric(train_cab$Month)
train_cab$Year <- as.numeric(train_cab$Year)
train_cab$day <- as.numeric(train_cab$day)

#####Corelation Analysis#########

library(corrgram)
corrgram(train_cab,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)
train_cab$passenger_count <- as.factor(train_cab$passenger_count)
train_cab$Month <- as.factor(train_cab$Month)
train_cab$Year <- as.factor(train_cab$Year)

#library(dplyr)

print(chisq.test(table(train_cab$fare_amount , train_cab$passenger_count)))
print(chisq.test(table(train_cab$fare_amount , train_cab$Month)))
print(chisq.test(table(train_cab$fare_amount , train_cab$Year)))


#The p value is slightly greater than 0.05.Even though in general cases , we might reject that 
#value, we'll still keep the value 


#rmExcept(train_cab) #Clean the environment
####Building a linear regression model########
library(caTools) # sample.split is in this library
set.seed(101)
sample <- sample.split(train_cab$fare_amount, SplitRatio = 0.8)
train  <- subset(train_cab,sample==TRUE)
test <- subset(train_cab , sample == FALSE)
model <- lm(fare_amount ~ ., train_cab )
summary(model)

prediction_LR = predict(model,test[,2:7])
mape(test[,1] , prediction_LR)
#Error Rate via MAPE is 36.36%

############Building a Decision Tree Regression Model#####################
library(rpart)
library(MASS)
#Building a Model 
fit = rpart(fare_amount ~ ., data = train , method = "anova")
#Prediction for new test cases 
prediction_FT = predict(fit,test[-1])
prediction_FT

#Calculating accuracy of the Decision Tree Model######
mape = function(y ,ywhat){
  mean(abs((y-ywhat)/y))
}
plot(fit, uniform=TRUE, main="Main Title")
text(fit, use.n=TRUE, all=TRUE)


mape(test[,1],prediction_FT) #The accuracy of the Decision Tree Model comes out to be 1 - 0.21 =79%

#####Building a random forest regression model####
install.packages("randomForest")
library(randomForest)
modelRf <- randomForest(train_cab$fare_amount ~., data = train_cab)
prediction_RF <- predict(modelRf,test[-1])
mape(test[,1],prediction_RF)
