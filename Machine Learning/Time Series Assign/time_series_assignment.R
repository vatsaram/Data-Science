#-------------------------------------------------------------------------#
#                             Group Case Study                            #
#-------------------------------------------------------------------------#

#----Set the Working Directory-------#
getwd()                                              #Check the Working Directory
setwd("C:/Users/shris/Documents")                    #Set the working directory


#-----Load the Dataset----------#
store_data <- read.csv("Global Superstore.csv", header = TRUE, sep = ",")       #Load the Data Set 


#----Summarize the DataSet------#
head(store_data , 3)                      #Check first 3 rows
summary(store_data)                       #Summary of the Data set
str(store_data)                           #Structure of the Data set


#-----Load necessary packages---------#
install.packages('GGally')                        #Installing all necessary pacages
install.packages('MASS')
install.packages('ggplot2')
install.packages('car')
install.packages('caTools')
install.packages('caret')
install.packages('forecast')

library(GGally)                                   #Loading the packages into R
library(MASS)
library(ggplot2)
library(car)
library(caTools)
library(caret)
library(forecast)
library(dplyr)


#------Check for NA values-------#
sapply(store_data, function(x) sum(is.na(x)))     #41296 Na values present in Postal Code

#------Data Cleaning & Preparation-----#
store_data <- distinct(store_data)

store_data$Market <- as.factor(store_data$Market)  #Factorise the Market Segment geographically
summary(store_data$Market)

store_data$Segment <- as.factor(store_data$Segment) #Factorise the Customer Segment
summary(store_data$Segment)
        
africa_data <- store_data[which(store_data$Market == "Africa"),]       #Create Subsets of Market segment
apac_data <- store_data[which(store_data$Market == "APAC"),]
canada_data <- store_data[which(store_data$Market == "Canada"),]
emea_data <- store_data[which(store_data$Market == "EMEA"),]
eu_data <- store_data[which(store_data$Market == "EU"),]
latam_data <- store_data[which(store_data$Market == "LATAM"),]
us_data <- store_data[which(store_data$Market == "US"),]


africa_date <- as.factor(substr(africa_data$Order.Date,1,2))       #Changing the date format into months
africa_month <- as.factor(substr(africa_data$Order.Date,4,5))      #substr() is used to extract particular element(s) from a string

apac_date <- as.factor(substr(apac_data$Order.Date,1,2))
apac_month <- as.factor(substr(apac_data$Order.Date,4,5))

canada_date <- as.factor(substr(canada_data$Order.Date,1,2))
canada_month <- as.factor(substr(canada_data$Order.Date,4,5))

emea_date <- as.factor(substr(emea_data$Order.Date,1,2))
emea_month <- as.factor(substr(emea_data$Order.Date,4,5))

eu_date <- as.factor(substr(eu_data$Order.Date,1,2))
eu_month <- as.factor(substr(eu_data$Order.Date,4,5))

latam_date <- as.factor(substr(latam_data$Order.Date,1,2))
latam_month <- as.factor(substr(latam_data$Order.Date,4,5))

us_date <- as.factor(substr(us_data$Order.Date,1,2))
us_month <- as.factor(substr(us_data$Order.Date,4,5))

store_data$Order.Date <- as.Date(store_data$Order.Date,"%d-%m-%Y")
store_data <- store_data[with(store_data,order(Order.Date)),]  #order the data as per order date

store_data$Order.monyear <- format(store_data$Order.Date,"%m-%Y")       #create a time series of month
store_data$Order.month <- format(store_data$Order.Date,"%m")
store_data$Order.year <- format(store_data$Order.Date,"%Y")
as.factor(store_data$Order.monyear)

months <- as.data.frame(levels(as.factor(store_data$Order.monyear)))   # create a Data Frame of the order month
colnames(months) <- c("Months")
months$year <- substr(months$Months,4,7)
months$time <- substr(months$Months,1,2)
months <- months[with(months,order(year,time)),]
months$time.series <- seq(1:48)
months <- subset(months,select = c(Months,time.series))

store_data_ts <- merge.data.frame(store_data,months,by.x = "Order.monyear",by.y = "Months",all.x = T)    #merge the Time Series variable into the main data frame



#-------Exploratory Data Analysis-------------#
ggplot(store_data_ts,aes(x=Sales)) + geom_histogram(col="blue", fill="red") + ggtitle("Sales") + xlab("Sales") + ylab("Freq") 
boxplot(store_data_ts$Sales)                                                                 #Presence of Outlier in Sales
iqr <- as.double(quantile(store_data_ts$Sales,.75) - quantile(store_data_ts$Sales,.25))         
a <- as.integer(quantile(store_data_ts$Sales,0.75) + (iqr*1.5))                              # Q3 + (IQR*1.5)
b <- which(store_data_ts$Sales %in% boxplot.stats(store_data_ts$Sales)$out)
store_data_ts$Sales[b] <- a

boxplot(store_data_ts$Quantity)                                                                #Outliers present in Sales
iqr1 <- as.integer(quantile(store_data_ts$Quantity,.75) - quantile(store_data_ts$Quantity,.25))
c <- as.integer(quantile(store_data_ts$Quantity,0.75) + (iqr1*1.5))                             #Q3 + (IQR*1.5)
d <- which(store_data_ts$Quantity %in% boxplot.stats(store_data_ts$Quantity)$out)
store_data_ts$Quantity[d] <- c

boxplot(store_data_ts$Discount)                                                                   #Otliers present in Discount
iqr2 <- as.double(quantile(store_data_ts$Discount,.75) - quantile(store_data_ts$Discount,.25))
e <- as.double(quantile(store_data_ts$Discount,0.75) + (iqr2*1.5))                             #Q3 + (IQR*1.5)
f <- which(store_data_ts$Discount %in% boxplot.stats(store_data_ts$Discount)$out)
Global_Superstore_TS$Discount[f] <- e

boxplot(store_data_ts$Profit)                                                                 #Outliers present in profit
iqr3 <- as.double(quantile(store_data_ts$Profit,.75) - quantile(store_data_ts$Profit,.25))
g <- as.double(quantile(store_data_ts$Profit,0.75) + (iqr3*1.5))                       #Q3 + (IQR*1.5)
h <- which(store_data_ts$Profit %in% boxplot.stats(store_data_ts$Profit)$out)
store_data_ts$Profit[h] <- g

boxplot(store_data_ts$Shipping.Cost)                                                                  #Outliers present in Shipping Cost
iqr4 <- as.double(quantile(store_data_ts$Shipping.Cost,.75) - quantile(store_data_ts$Shipping.Cost,.25))
i <- as.double(quantile(store_data_ts$Shipping.Cost,0.75) + (iqr4*1.5))                         #Q3 + (IQR*1.5)
j <- which(store_data_ts$Shipping.Cost %in% boxplot.stats(store_data_ts$Shipping.Cost)$out)
store_data_ts$Shipping.Cost[j] <- i

