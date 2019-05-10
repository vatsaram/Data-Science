# import data

uber_data <- read.csv("Uber Request Data.csv", stringsAsFactors = F)
str(uber_data)

library(stringr)

# Clean timestamp

uber_data$Request.timestamp <- str_replace_all(uber_data$Request.timestamp, "-", "/")
uber_data$Drop.timestamp <- str_replace_all(uber_data$Drop.timestamp, "-", "/")

# Find request weekday
uber_data$reqdate <- weekdays(strptime(uber_data$Request.timestamp, "%d/%m/%Y"))


# Extract request hour
# Split the request time stamp colum and store in a data frame
uber_temp <- data.frame(cbind(str_split_fixed(uber_data$Request.timestamp, " ", 2)))

# Removing unwanted columns and extracting hour
uber_temp <- uber_temp[-1]
uber_temp$X2 <- as.numeric(format(strptime(uber_temp$X2,"%H:%M"), "%H"))

# Binning the hour to morning, mid-day and evening
uber_data$reqslot <- with(uber_temp,  ifelse(X2 >= 0 & X2<=9, "morning",
                           ifelse(X2>9 & X2<=16, "mid-day", "evening")))


library(ggplot2)

# Plot of the request slot vs status for Airport and City
ggplot(uber_data, aes(x=reqslot, fill = Status)) + 
  geom_bar() + facet_wrap(~Pickup.point) +
  labs(title ="Plot of the request slot vs status for Airport and City")

# Plot the request date vs status for Airport and City
ggplot(uber_data, aes(x=reqdate, fill = Status)) + 
  geom_bar(position = "dodge") + facet_wrap(~Pickup.point) +
  labs(title = "Plot the request date vs status for Airport and City")

# Plot the request date vs request time slot for Airport and City
ggplot(uber_data, aes(x=reqdate, fill = reqslot)) + 
  geom_bar(position = "dodge") + facet_wrap(~Pickup.point) +
  labs(title = "Plot the request date vs request time slot for Airport and City")
