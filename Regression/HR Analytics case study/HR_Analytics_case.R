# Importing data

in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE)
employee_survey_data <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE)
manager_survey_data <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE)
general_data <- read.csv("general_data.csv", stringsAsFactors = FALSE)


# Finding the difference in employee ID 
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID) 
setdiff(manager_survey_data$EmployeeID,general_data$EmployeeID) 


# setting column names for In time and out time data set
colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"


# The NA entries match so any one of dataframe can be used for computation of holidays.
# merging the employee info

employee_master <- merge(general_data, employee_survey_data, by= "EmployeeID", all = FALSE)
employee_master <- merge(employee_master, manager_survey_data, by= "EmployeeID", all = FALSE)



# Finding the difference in employee ID for in time and out time
setdiff(in_time$EmployeeID,general_data$EmployeeID)
setdiff(out_time$EmployeeID,general_data$EmployeeID)


# Checking if NAs which we observe in In time is same as out time
library(dplyr)
all_equal(sapply(in_time, is.na), sapply(out_time, is.na))


# Removing columns which have only NA

in_time <- in_time[,colSums(is.na(in_time))<nrow(in_time)]
out_time <- out_time[,colSums(is.na(out_time))<nrow(out_time)]


#. next step is to calculate the number of holidays spent by employee
employee_master$holidays <- apply(in_time, 1, function(x) sum(is.na(x)))


# estimate number of hours spent by the employee at office 

in_time <- data.frame(apply(in_time, 2, function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M")))
out_time <- data.frame(apply(out_time, 2, function(x) as.POSIXlt(x, format = "%Y-%m-%d %H:%M")))

diff_time <- data.frame(sapply(out_time - in_time,as.numeric))[-1]

# calculate the total hours spent and save it  as part of the employee master record

employee_master$totalhours <- apply(diff_time, 1, function(x) sum(x, na.rm = TRUE))


# Removing unwanted columns
length(employee_master$EmployeeCount==1)
length(employee_master$StandardHours==8)
length(employee_master$Over18=='Y')


# Since these columns have the same information, we can delete these rows
employee_master <- employee_master[, !(names(employee_master) %in% 
                                         c("EmployeeCount", "StandardHours", "Over18"))] 


sapply(employee_master[,c(13:17)], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no significant outlier


sapply(employee_master[,c(18:21)], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier


# Understanding NA values 
sapply(employee_master, function(x) sum(is.na(x)))
#NumCompaniesWorked: 19, TotalWorkingYears: 9, EnvironmentSatisfaction: 25, 
# JobSatisfaction: 20, WorkLifeBalance: 38

# Given that we cannot remove these variables and there is no relation between NA values
# and the other variables, NA will be replaced by the mode.
mode_val <- function(x){
  uniq <- unique(x)
  uniq[which.max(tabulate(match(x, uniq)))]
}

employee_master <- data.frame(lapply(employee_master, function(x) replace(x, is.na(x), mode_val(x))))



# Salary information to be binned for bar chart.
employee_master$MonthlyIncome_bin <-  with(employee_master, 
                                          ifelse(employee_master$MonthlyIncome > 150000,"> 150000", 
                                          ifelse(employee_master$MonthlyIncome > 100000,"> 100000", 
                                          ifelse(employee_master$MonthlyIncome > 75000,"> 750000", 
                                          ifelse(employee_master$MonthlyIncome > 40000,"> 400000","< 400000")))))


sapply(employee_master[,c(22:25)], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier


sapply(employee_master[,c(26:28)], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier


# Calculate the overall attrition percentage: ~ 16%
(sum(employee_master$Attrition=='Yes')/ length(employee_master$Attrition))*100


# Barcharts for categorical features with stacked employee information
library(ggplot2)
library(cowplot)

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")



plot_grid(ggplot(employee_master, aes(x=Age,fill=Attrition))+ geom_bar(position = "fill"), 
          ggplot(employee_master, aes(x=BusinessTravel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=Department,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=DistanceFromHome,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=Education,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=EducationField,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")   

# Youngsters (early 20s) and HR personnel seem to have higher attrition


plot_grid(ggplot(employee_master, aes(x=Gender,fill=Attrition))+ geom_bar(position = "fill"), 
          ggplot(employee_master, aes(x=JobLevel,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=JobRole,fill=Attrition)) + geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=MaritalStatus,fill=Attrition)) + geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=MonthlyIncome_bin,fill=Attrition)) + geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=NumCompaniesWorked,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")   

# People who are single and who have worked in more companies tend to have higher attrition

plot_grid(ggplot(employee_master, aes(x=PercentSalaryHike,fill=Attrition)) + geom_bar(position = "fill"), 
          ggplot(employee_master, aes(x=StockOptionLevel,fill=Attrition)) + geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=TotalWorkingYears,fill=Attrition)) + geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=TrainingTimesLastYear,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=YearsAtCompany,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=YearsSinceLastPromotion,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")   

# Employees who have total working years of 1 year or employeed who have 1 year at company
# have the highest attrition rate
# Group of employees who have worked for 40 years in the same company is probably outlier 
# as they may  be retiring. Attrition observed in 1-2 years and 22~27 years of working in company


plot_grid(ggplot(employee_master, aes(x=YearsWithCurrManager,fill=Attrition))+ geom_bar(position = "fill"), 
          ggplot(employee_master, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=JobInvolvement,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          ggplot(employee_master, aes(x=PerformanceRating,fill=Attrition))+ geom_bar(position = "fill")+bar_theme1,
          align = "h")   


#Attrition is higher for employees who are low on motivation factors like job involvement
#work life balance and performance. Also Attrition is higher for employees who have had manager change


ggplot(employee_master, aes(x=holidays,fill=Attrition))+ geom_bar(position = "fill") 


# Years with current manager being zero, Lower environment and worklife balance point to possible cause

# For columns having two levels changing the columns to 1 or 0
employee_master$Attrition<- ifelse(employee_master$Attrition=="Yes",1,0)
employee_master$Gender <- ifelse(employee_master$Gender=="Male",1,0)

# Discarding the employees who have worked for 40 years as they're probably retiring
employee_master <- employee_master[!(employee_master$TotalWorkingYears==40),]


# creating a dataframe of categorical data and covert to factors
employee_fact<- data.frame(sapply(employee_master[,c(4,5,8,11,12)], factor))
str(employee_fact)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(employee_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_fact))[,-1]))


# Final dataset after removing categorical variables. 
# Removed binned monthly income variable. salary would be scaled along with other numeric variables
employee_master<- cbind(employee_master[,-c(4,5,8,11,12,29)],dummies) 
employee_fact <- NULL
dummies <- NULL


# scaling the parameters Age, DistanceFromHome, MonthlyIncome, PercentSalaryHike, 
#TotalWorkingYears, holidays and Total working hours

employee_master[,c(2,4,8,10,12,14,22,23)] <- lapply(employee_master[,c(2,4,8,10,12,14,22, 23)], scale)


########################################################################
# splitting the data between train and test
set.seed(100)

sel_index = sample(1:nrow(employee_master), 0.7*nrow(employee_master))

train = employee_master[sel_index,]

test_empl_att = employee_master[-sel_index,]

########################################################################
# Logistic Regression: 

#Initial model
library("car")

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

# Removing unwanted variables through Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)


# Removing the parameters out of StepAIC and re-running the model
model_3 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_3) 
vif(model_3)
#formula = Attrition ~ tenure + MonthlyCharges +  
#  PhoneService + Contract.xOne.year + Contract.xTwo.year + 
#  PaperlessBilling + PaymentMethod.xElectronic.check + SeniorCitizen + 
#  MultipleLines.xYes + InternetService.xFiber.optic + InternetService.xNo + 
#  OnlineBackup.xYes + DeviceProtection.xYes + StreamingTV.xYes + 
#  StreamingMovies.xYes, family = "binomial", data = train)

# Removing Department.xResearch...Development since its has VIF >3 and higher p

train <- train[, !(names(train) %in% c("Department.xResearch...Development"))]

# Removing the parameters out of StepAIC and re-running the model
model_4 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_4) 
vif(model_4)



# Removing BusinessTravel.xTravel_Rarely since its has VIF >3 and p-value 0.01251 *

train <- train[, !(names(train) %in% c("BusinessTravel.xTravel_Rarely"))]

# Removing the parameters out of StepAIC and re-running the model
model_5 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_5) 
vif(model_5)


# Removing Education since it has highest p-value: 0.15353


train <- train[, !(names(train) %in% c("Education"))]

# Removing the parameters out of StepAIC and re-running the model
model_6 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_6) 
vif(model_6)


# Removing JobInvolvement since it has highest p-value: 0.13478


train <- train[, !(names(train) %in% c("JobInvolvement"))]

# Removing the parameters out of StepAIC and re-running the model
model_7 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_7) 
vif(model_7)

 
#Removing holidays since it has highest p-value: 0.06532 


train <- train[, !(names(train) %in% c("holidays"))]

# Removing the parameters out of StepAIC and re-running the model
model_8 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_8) 
vif(model_8)



#Removing NumCompaniesWorked since it has highest p-value: 0.06669 


train <- train[, !(names(train) %in% c("NumCompaniesWorked"))]

# Removing the parameters out of StepAIC and re-running the model
model_9 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_9) 
vif(model_9)


#Removing TotalWorkingYears since it has highest p-value: 0.06538 and high VIF 


train <- train[, !(names(train) %in% c("TotalWorkingYears"))]

# Removing the parameters out of StepAIC and re-running the model
model_10 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_10) 
vif(model_10)


#Removing JobLevel since it has high p-value: 0.08710

train <- train[, !(names(train) %in% c("JobLevel"))]

# Removing the parameters out of StepAIC and re-running the model
model_11 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_11) 
vif(model_11)



#Removing JobRole.xSales.Executive since it has high p-value: 0.06148

train <- train[, !(names(train) %in% c("JobRole.xSales.Executive"))]

# Removing the parameters out of StepAIC and re-running the model
model_12 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_12) 
vif(model_12)


#Removing EducationField.xOther since it has high p-value: 0.014927

train <- train[, !(names(train) %in% c("EducationField.xOther"))]

# Removing the parameters out of StepAIC and re-running the model
model_13 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_13) 
vif(model_13)



#Removing Department.xSales since it has highest p-value: 0.014927

train <- train[, !(names(train) %in% c("Department.xSales"))]

# Removing the parameters out of StepAIC and re-running the model
model_14 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_14) 
vif(model_14)


#Removing JobRole.xResearch.Director since it has highest p-value: 0.009822

train <- train[, !(names(train) %in% c("JobRole.xResearch.Director"))]

# Removing the parameters out of StepAIC and re-running the model
model_15 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_15) 
vif(model_15)


#Removing EducationField.xTechnical.Degree since it has highest p-value: 0.004905

train <- train[, !(names(train) %in% c("EducationField.xTechnical.Degree"))]

# Removing the parameters out of StepAIC and re-running the model
model_16 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_16) 
vif(model_16)

# Since all P-values and vif are significant, considering model_16 as final

final_att_model <- model_16



### Model Evaluation#######

test_empl_att$predicted = predict(final_att_model, type = "response", 
                    newdata = test[,-1])


# Result of final test

View(test_empl_att)


# Assuming more than 40% as the probability cutoff 

test_pred_att <- factor(ifelse(test$predicted >= 0.40, "Yes", "No"))
test_actual_att <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_pred_att,test_actual_att)

library(e1071)
library(caret)

test_conf <- confusionMatrix(test_pred_att, test_actual_att, positive = "Yes")
test_conf
#######################################################################

#------------------------------------------------------------------------------------------
