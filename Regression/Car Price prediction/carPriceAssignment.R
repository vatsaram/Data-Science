# Importing the Car Price Assignment

carprice <- read.csv("CarPrice_Assignment.csv")
str(carprice)

library(stringr)

# Retaining the brand and discarding the car model since 
# there are too many models which does not help in analysis
Dummy <- data.frame(cbind(str_split_fixed(carprice$CarName, " ", 2)))

carprice$CarName <- as.factor(tolower(Dummy$X1))


# cleaning up car brand data
levels(carprice$CarName)
carprice$CarName <- str_replace_all(carprice$CarName, "maxda", "mazda")
carprice$CarName <- str_replace_all(carprice$CarName, "toyouta", "toyota")
carprice$CarName <- str_replace_all(carprice$CarName, "vw", "volkswagen")
carprice$CarName <- str_replace_all(carprice$CarName, "vokswagen", "volkswagen")
carprice$CarName <- str_replace_all(carprice$CarName, "porcshce", "porsche")
carprice$CarName <- as.factor(carprice$CarName)




# Changing the 2- factor columns. Since its two factors, I am considering other option as
# not the first option. Example: Gas Fuel type or NO Gas fuel type

levels(carprice$fueltype)<-c(0,1) 
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]
colnames(carprice)[4] <- "Gasfueltype"


levels(carprice$aspiration)<-c(0,1) 
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
colnames(carprice)[5] <- "Turboaspiration"


levels(carprice$doornumber)<-c(0,1) 
carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]
colnames(carprice)[6] <- "Twodoornumber"


levels(carprice$enginelocation)<-c(0,1) 
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]
colnames(carprice)[9] <- "Rearenginelocation"

str(carprice)

# Update for type of body
Dummy <- data.frame(model.matrix( ~carbody, data = carprice))
carprice <- cbind(carprice[,-7], Dummy[,-1])

# Update for drive wheel
Dummy <- data.frame(model.matrix( ~drivewheel, data = carprice))
carprice <- cbind(carprice[,-7], Dummy[,-1])


# Update for Engine type
Dummy <- data.frame(model.matrix( ~enginetype, data = carprice))
carprice <- cbind(carprice[,-13], Dummy[,-1])

# Update for cylinder number
Dummy <- data.frame(model.matrix( ~cylindernumber, data = carprice))
carprice <- cbind(carprice[,-13], Dummy[,-1])

# Update for fuelsystem
Dummy <- data.frame(model.matrix( ~fuelsystem, data = carprice))
carprice <- cbind(carprice[,-14], Dummy[,-1])


# Converting symboling as factor variable
carprice$symboling <- as.factor(carprice$symboling)
Dummy <- data.frame(model.matrix( ~symboling, data = carprice))
carprice <- cbind(carprice[,-2], Dummy[,-1])

#carprice$CarName <- NULL

# Converting Carname as factor variable
Dummy <- data.frame(model.matrix( ~CarName, data = carprice))
carprice <- cbind(carprice[,-2], Dummy[,-1])

Dummy <- NULL

# Checking for outliers
quantile(carprice$wheelbase, probs = seq(0, 1, 0.01))
quantile(carprice$carlength, probs = seq(0, 1, 0.01))
quantile(carprice$carwidth, probs = seq(0, 1, 0.01))
quantile(carprice$carheight, probs = seq(0, 1, 0.01))

# cleaning outlier for curbweight 
quantile(carprice$curbweight, probs = seq(0, 1, 0.01))
carprice$curbweight[which(carprice$curbweight>3768.40)] <- 3768.40
carprice$curbweight[which(carprice$curbweight<1874)] <- 1874

# cleaning outlier for enginesize
quantile(carprice$enginesize, probs = seq(0, 1, 0.01))
carprice$enginesize[which(carprice$enginesize>209.00)] <- 209.00

quantile(carprice$boreratio, probs = seq(0, 1, 0.01))
quantile(carprice$stroke, probs = seq(0, 1, 0.01))

# cleaning outlier for compression ratio
quantile(carprice$compressionratio, probs = seq(0, 1, 0.01))
carprice$compressionratio[which(carprice$compressionratio>21.00)] <- 21.00

# cleaning outlier for horsepower
quantile(carprice$horsepower, probs = seq(0, 1, 0.01))
carprice$horsepower[which(carprice$horsepower>207.00)] <- 207.00


quantile(carprice$peakrpm, probs = seq(0, 1, 0.01))

# cleaning outlier for citympg
quantile(carprice$citympg, probs = seq(0, 1, 0.01))
carprice$citympg[which(carprice$citympg>44.72)] <- 44.72


quantile(carprice$highwaympg, probs = seq(0, 1, 0.01))

# cleaning carprice for the outliers
quantile(carprice$price, probs = seq(0, 1, 0.01))
carprice$citympg[which(carprice$price>35490.72)] <- 35490.72



# Coming up with the model. Considering the dataset has limited set of values,
# I am using entire data set for arriving at the model.

# Loading library for StepAIC
library(car)
library(MASS)

train<- carprice

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
stepAIC(model_1)
summary(model_1)


# dropping columns which have NA and ID column since it does not add any value for model building
cols_to_drop <- c("cylindernumbertwo", "fuelsystemidi", "CarNamepeugeot", "CarNamesubaru", "car_ID")

train <- train[, ! names(train) %in% cols_to_drop, drop = F]

# Running the model on updated data
model_2 <-lm(price~.,data=train)
summary(model_2)
vif(model_2)

# Dropping columns which have high p-value >0.1

cols_to_drop_1 <- c("Twodoornumber", "enginesize", "boreratio","stroke", "compressionratio", 
                    "horsepower", "highwaympg", "carbodyhardtop", "carbodysedan", "carbodywagon",
                    "drivewheelfwd", "drivewheelrwd", "enginetypedohcv", "enginetypeohc", 
                    "enginetyperotor", "cylindernumberthree", "cylindernumbertwelve", "fuelsystem4bbl",
                    "fuelsystemmfi", "fuelsystemmpfi", "fuelsystemspdi", "fuelsystemspfi", "symboling.1",
                    "symboling0", "symboling1", "symboling2", "symboling3", "CarNameaudi", "CarNamebuick",
                    "CarNamehonda", "CarNamejaguar", "CarNamemazda", "CarNameporsche", "CarNamerenault",
                    "CarNamesaab", "CarNamevolkswagen", "CarNamevolvo")

train <- train[, ! names(train) %in% cols_to_drop_1, drop = F]

# Running updated model
model_3 <-lm(price~.,data=train)
summary(model_3)
vif(model_3)


# Removing the above columns and runing the updated model/ vif
cols_to_drop_2 <- c("Gasfueltype", "peakrpm", "enginetypeohcv")

train <- train[, ! names(train) %in% cols_to_drop_2, drop = F]
model_4 <-lm(price~.,data=train)
summary(model_4)
vif(model_4)



# Removing the columns related to car model as many models have been
# already knocked down due to high p-value
cols_to_drop_3 <- c("CarNamechevrolet", "CarNamedodge", "CarNameisuzu", "CarNamemercury",
                    "CarNamemitsubishi", "CarNamenissan", "CarNameplymouth", "CarNametoyota")

train <- train[, ! names(train) %in% cols_to_drop_3, drop = F]
model_5 <-lm(price~.,data=train)
summary(model_5)
vif(model_5)


# Removing the columns which have high p-value and vif
cols_to_drop_4 <- c("wheelbase", "carheight", "fuelsystem2bbl")

train <- train[, ! names(train) %in% cols_to_drop_4, drop = F]
model_6 <-lm(price~.,data=train)
summary(model_6)
vif(model_6)


# Stopping the model generation as VIF values are significant for the P-value which are high
# and the p-values are highly significant for the VIF values which are high


# Running the prediction on data set by creating column test price
Predict_1 <- predict(model_6,carprice[,-19])
carprice$test_price <- Predict_1
cor(carprice$price,carprice$test_price)^2

# The R square value and Adjusted R square value are nearly same and 
# there is high degree of correlation between both the price and test price (97%)
