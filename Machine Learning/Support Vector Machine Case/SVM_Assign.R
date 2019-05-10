

#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(gridExtra)


#Loading Data

mnist_total_set_read <- read_csv("mnist_train.csv", col_names = FALSE)
mnist_tst_set_read <- read_csv("mnist_test.csv", col_names = FALSE)

# to avoid reading the csv multiple times as part of coding, above variable has been used
mnist_total_set <- mnist_total_set_read
mnist_tst_set <- mnist_tst_set_read

#Understanding Dimensions

dim(mnist_total_set)
dim(mnist_tst_set)

#Structure of the dataset

str(mnist_total_set)
str(mnist_tst_set)

#checking missing value

sum(is.na(mnist_total_set))
sum(is.na(mnist_tst_set))

# setting the values as numeric
mnist_total_set <- data.frame(lapply(mnist_total_set, as.numeric))
mnist_tst_set <- data.frame(lapply(mnist_tst_set, as.numeric))


#Making our target colunmn to factor

colnames(mnist_total_set)[1] <- "Numbers"
colnames(mnist_tst_set)[1] <- "Numbers"


mnist_total_set$Numbers<-factor(mnist_total_set$Numbers)
mnist_tst_set$Numbers<-factor(mnist_tst_set$Numbers)


# Split the data into train and test set

set.seed(100)
indices_training = sample(1:nrow(mnist_total_set), 0.15*nrow(mnist_total_set))
mninst_train_data = mnist_total_set[indices_training, ]

# scaling the data since pixel values varies between 0 to 255

mninst_train_data[,-1] <- mninst_train_data[,-1]/255
mnist_tst_set[,-1] <- mnist_tst_set[,-1]/255

#Constructing Model

#Using Linear Kernel
Linear_model <- ksvm(Numbers~ ., data = mninst_train_data, kernel = "vanilladot", scale = FALSE)
Linear_evaln<- predict(Linear_model, mnist_tst_set)

#confusion matrix - Linear Kernel
confusionMatrix(Linear_evaln,mnist_tst_set$Numbers)


#Using RBF Kernel
RBF_Model <- ksvm(Numbers~ ., data = mninst_train_data, kernel = "rbfdot")
RBF_Evualn<- predict(RBF_Model, mnist_tst_set)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_Evualn,mnist_tst_set$Numbers)


############   Hyperparameter tuning and Cross Validation #####################

# Using Number of folds in Cross validation as 4 
# sampling has 9000 odd entries (plenty to train on)....

trainControl <- trainControl(method="cv", number=4)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

set.seed(150)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


# running train function for the Algorithm svmRadial with metric as Accuracy.

fit_svm <- train(Numbers~., data=mninst_train_data, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit_svm)

#Resampling results across tuning parameters:

#sigma  C    Accuracy   Kappa
#0.025  0.1  0.1071112  0    
#0.025  0.5  0.1071112  0    
#0.025  1.0  0.1071112  0    
#0.025  2.0  0.1071112  0    
#0.050  0.1  0.1071112  0    
#0.050  0.5  0.1071112  0    
#0.050  1.0  0.1071112  0    
#0.050  2.0  0.1071112  0    


# running cross validation with 5
trainControl <- trainControl(method="cv", number=5)


# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.

metric <- "Accuracy"

set.seed(150)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )


# running train function for the Algorithm svmRadial with metric as Accuracy.

fit_svm <- train(Numbers~., data=mninst_train_data, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit_svm)

plot(fit_svm)
