#Adding relevant libraries
library(dplyr)
library(compare)

# ------ TODO -- CREATE THE FOLLOWING OBJECTS ----

# filter() mtcars to include cars having 8 cylinders (cyl) and hp > 190
# Store the filtered df as cars_1
View(mtcars)
cars_1 <- filter(mtcars, cyl == 8, hp> 190)


# arrange() cars_1 in descending order of hp
# Store the df as cars_2
cars_2 <- arrange(cars_1, desc(hp))


# group_by the mtcars df by cyl levels
# Store the grouped df as cyl_groups
cyl_groups <- group_by(mtcars, cyl)


# summarise() the cyl_groups created above by average hp
# Store the summarised df as cyl_hp
cyl_hp <- summarise(cyl_groups, mean(hp, na.rm = TRUE))



popularity <- read.csv("popularity.csv")
data <- popularity
newdata <- gather(data, day, my_val, weekday_is_monday:weekday_is_sunday)
newdata <- newdata[!(newdata$my_val == 0),]
newdata <- newdata[, -56]


newdata_weekday <- filter(newdata, !(day=="weekday_is_saturday"), !(day=="weekday_is_sunday"))
newdata_weekend <- filter(newdata, is_weekend == 1)
avg_shares_weekday <- mean(newdata_weekday$shares)
avg_shares_weekend <- mean(newdata_weekend$shares)
weekday_shares <- summarise(group_by(newdata_weekday, day), mean(shares, na.rm = TRUE))


newdata_channel <- gather(newdata, channel, my_channel, data_channel_is_lifestyle:data_channel_is_world)
newdata_channel <- newdata_channel[!(newdata_channel$my_channel==0), ]
newdata_channel <- newdata_channel[, -51]   
channel_shares <- summarise(group_by(newdata_channel, channel), mean(shares, na.rm = TRUE))


install.packages("stringr")
library(stringr)
library(swirl)                            
install_course()
