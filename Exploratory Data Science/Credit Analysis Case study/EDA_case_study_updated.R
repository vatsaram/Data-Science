loan_data <- read.csv("loan.csv", stringsAsFactors = FALSE)
str(loan_data)

# clean off all unwanted rows

loan_data<- loan_data[,-c(50:111)]

library(ggplot2)
library(stringr)

# Remove the % / months character to help convert it to numeric for analysis
loan_data$int_rate <- as.numeric(str_replace_all(loan_data$int_rate, '%', ''))
loan_data$revol_util <- as.numeric(str_replace_all(loan_data$revol_util, '%', ''))


# Check NA values in the above

sum(is.na(loan_data$funded_amnt)== TRUE)
sum(is.na(loan_data$funded_amnt_inv)== TRUE)
sum(is.na(loan_data$annual_inc)==TRUE)
sum(is.na(loan_data$dti)==TRUE)
sum(is.na(loan_data$int_rate)== TRUE)
sum(is.na(loan_data$term)==TRUE)
sum(is.na(loan_data$revol_util)==TRUE)

# Setting to zero for those rows whose funded amount investor has very low values
loan_data$funded_amnt_inv[which(loan_data$funded_amnt_inv < 1)] <- 0

# Setting the revol_util to zero for NA values
loan_data$revol_util[which(is.na(loan_data$revol_util))] <- 0


# Univariate analysis of key parameters to understand population

# Total number of loans disbursed
length(loan_data$id)

round((sum(loan_data$loan_status=="Fully Paid")/ length(loan_data$id)) *100)
# Percentage of population who pay up is 84%


round((sum(loan_data$loan_status=="Charged Off")/ length(loan_data$id)) *100)
# Percentage of population who default is 14%

ggplot(loan_data, aes(x = loan_status, fill = "blue")) + geom_bar() +
  labs(title = "Loan status for the given dataset")

sum(loan_data$funded_amnt > mean(loan_data$funded_amnt))
# 16284 loans higher than mean funded amount


sum(loan_data$dti > mean(loan_data$dti))
# 20040 loans which are having DTI more than mean 


sum(loan_data$int_rate > mean(loan_data$int_rate))
# 18478 loans which are having interest rate more than mean


sum(loan_data$annual_inc < mean(loan_data$annual_inc))
# 24706 loans which are provided to people who have salary less than mean salary


sum(loan_data$term == " 36 months")
# 29096 loans which are at 36 months 


sum(loan_data$term == " 60 months")
# Only 10621 loans which are of 60 months so more short term loans than long term.

# Given that 14% of the population are defaulting, 83% are paying up 
# which doesnt include current, focus would be to compare how the defaulters 
# fare with respect to people who pay up and then segregate for further analysis.


# Bivariate analysis to compare the 2 sets of population

# Comparing funded amount for both sections

summary(loan_data$funded_amnt[which(loan_data$loan_status=="Fully Paid")])
summary(loan_data$funded_amnt[which(loan_data$loan_status=="Charged Off")])

# Binning funded amount as per the overall summary statistics
loan_data$funded_amnt_bin <-  with(loan_data, 
 ifelse(loan_data$funded_amnt > quantile(loan_data$funded_amnt, 0.75),
"Top 25% funding", ifelse(loan_data$funded_amnt > quantile(loan_data$funded_amnt, 0.5),
"Top 50% funding", ifelse(loan_data$funded_amnt > quantile(loan_data$funded_amnt, 0.25),
"25~50% funding", "Lower 25% funding"))))


ggplot(loan_data, aes(x= funded_amnt, fill = loan_status)) + 
  geom_histogram(bins = 7) + 
  labs(title = "Funded amount distribution wrt Loan Status")

# In general, Fully paid loan amount tend to be lower than defaulted ones 


# Comparing annual income for both sections

summary(loan_data$annual_inc[which(loan_data$loan_status=="Fully Paid")])
summary(loan_data$annual_inc[which(loan_data$loan_status=="Charged Off")])


# In general, loans to people who have higher salary tend to be Fully paid 


# Comparing Debt to income ratio for both sections

summary(loan_data$dti[which(loan_data$loan_status=="Fully Paid")])
summary(loan_data$dti[which(loan_data$loan_status=="Charged Off")])

ggplot(loan_data, aes(x= dti, fill = loan_status)) + 
  geom_histogram(bins = 7) + 
  labs(title = "Debt to Income ratio distribution wrt Loan Status")

loan_data$dti_bin <-  with(loan_data, 
ifelse(loan_data$dti > quantile(loan_data$dti, 0.75), "Top 25% of credit debt", 
ifelse(loan_data$funded_amnt > quantile(loan_data$funded_amnt, 0.5), "Top 50% of credit debt", 
ifelse(loan_data$funded_amnt > quantile(loan_data$funded_amnt, 0.25), "25~50% of credit debt", "Lower 25% debt"))))


# In general, loans which have DTI ratio lesser tend to be Full paid

# Comparing interest rate for both sections

summary(loan_data$int_rate[which(loan_data$loan_status=="Fully Paid")])
summary(loan_data$int_rate[which(loan_data$loan_status=="Charged Off")])

ggplot(loan_data, aes(x= int_rate, fill = loan_status)) + 
  geom_histogram(bins = 7) +
  labs(title = "Interest rate impact wrt Loan Status")


# Lower interest rates translates to better payment of loans

# Understanding coorelation between loan_amount , funded amount and investor funded
cor(loan_data$loan_amnt, loan_data$funded_amnt)
cor(loan_data$funded_amnt, loan_data$funded_amnt_inv)

# There is 98% correlation between loan applied vs funded amount
# and 96% correlation between funded amount and funded by investor which means 
# that as the loan amount increases there is significant investor funded loans.
# Picking funded_amount for any analyis since this the amount funded by investor

# Understand how terms affect the loan status

round(sum(loan_data$term == " 36 months" & loan_data$loan_status=="Charged Off") / 
  sum(loan_data$term == " 36 months") *100)

round(sum(loan_data$term == " 60 months" & loan_data$loan_status=="Charged Off") / 
        sum(loan_data$term == " 60 months") *100)

round(sum(loan_data$term == " 60 months" & loan_data$loan_status=="Charged Off") / 
        sum(loan_data$loan_status=="Charged Off") *100)


# 23% of the loans which are of long term are getting defaulted as opposed to only
# 11% of short terms loans and account for 43% of the overall defaulted loans

# Performing subset of the charged off loans for further anallysis

loans_charged_off <- subset(loan_data, loan_status == "Charged Off")


# plot of loan term 

ggplot(loans_charged_off, aes(x = term)) + geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + 
  facet_wrap(~funded_amnt_bin) + 
  labs(title = "Loan term for the Defaulted loans")


# Understand the impact of house ownership for default

ggplot(loans_charged_off, aes(x = home_ownership)) + geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + facet_wrap(~dti_bin) + 
  labs(title = "House ownership distribution for Defaulted loans")

# Mortgage/ rent has a very high impact on default than rest.

# The impact on purpose of the loan for default

ggplot(loans_charged_off, aes(x = purpose)) + geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + facet_wrap(~dti_bin) +
  labs(title = "Loan Purpose and its impact for Defaulted loans") 

# Debt consolidation seems to have higher effect 


ggplot(loan_data, aes(x = purpose)) + geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + facet_wrap(~dti_bin) +
  labs(title = "Distribution of loans wrt Purpose")

# Given that bulk of loans issued is for debt consolidation, this might be following 
# population trend and cannot be accounted for.
# There is a write off for small business and other types


# Impact on verification status on default

ggplot(loans_charged_off, aes(x = verification_status)) + geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + facet_wrap(~funded_amnt_bin) +
  labs(title = "Verification status and its impact for Defaulted loans")

# Loans which has verification status as Source_Verified has lower default rate.


# Impact of grade on default

ggplot(loans_charged_off, aes(x = grade)) + geom_bar(position = "dodge") +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + facet_wrap(~funded_amnt_bin) +
  labs(title = "Loan Grade and its impact for Defaulted loans")

# B & C grades seem to have higher rate to default. Comparing with population

ggplot(loan_data, aes(x = grade)) + geom_bar() +
  geom_text(stat = "count", aes(label = ..count.., vjust = -0.1)) + 
  labs(title = "Loan Grade distribution across the loans disbursed")



# B grade loans are disbursed more than rest of the loans
# A Grade loans seem to be safe bet
# Loans which need to be taken care:
# 1. customers looking for small business
# 2. who are in rented / mortgage
# 3. Loan Grade which are B/C
# 4. And loans which are long term for 60 months

