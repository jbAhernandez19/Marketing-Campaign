# Load packages you might need
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)

# Set working directory
setwd("/Users/jbhernandez/Desktop/CareDash")

# Email marketing campaign data
email <- read.csv("Take_Home_Test_Database.csv", stringsAsFactors = F, na.strings = "")


# -------------------------------------------------------------------------------------------------
# VIEWING, CLEANING, TIDYING DATA

str(email)
View(head(email, 10))

email$send_time <- as_datetime(email$send_time)
email$open_time <- as_datetime(email$open_time)

# Mean number of days between sending and opening email (86,400 sec in a day)
meanDays <- mean(interval(email$send_time, email$open_time), na.rm = T) / 86400
# Note: Difference between send_date and open_date is one day or less

# Separate datetime into two separate columns and change the class of time
# Create new columns for the dates first that way you don't lose the original send_time
emailDT <- email
emailDT$send_date <- as.Date(emailDT$send_time)
emailDT$open_date <- as.Date(emailDT$open_time)
emailDT$send_time <- format(strptime(emailDT$send_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
emailDT$open_time <- format(strptime(emailDT$open_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")

# In order to just keep the time in the original send_time and open_time variables,
# you would convert them to class POSIXlt first and then reformat it to keep only the time.

emailReordered <- emailDT[c(7, 1, 8, 2, 3, 4, 5, 6)]
str(emailReordered)


# -------------------------------------------------------------------------------------------------
# CREATING TABLES & BASIC INFO ABOUT DATA

# When were the emails sent/opened
table(year(emailReordered$send_date))
table(month(emailReordered$send_date))
table(day(emailReordered$send_date))
table(wday(emailReordered$send_date, label = T, week_start = getOption("lubridate.week.start", 1)))
# Emails were sent over the span of 7 (consecutive) days
# March 17th, 2014 - March 23rd, 2014

table(year(emailReordered$open_date))
table(month(emailReordered$open_date))
table(day(emailReordered$open_date))
prop.table(table(day(emailReordered$open_date))) * 100
prop.table(table(wday(emailReordered$open_date, label = T, week_start = getOption("lubridate.week.start", 1)))) * 100
# Emails sent were opened over the span of 9 (consecutive) days
# March 17th, 2014 - March 25th, 2014

# Dataframes that contains information about when emails where sent and when they were opened
dfEmailSent <- data.frame(table(day(email$send_date)),
                      prop.table(table(wday(emailReordered$send_date, label = T, 
                                            week_start = getOption("lubridate.week.start", 1)))) * 100, 
                      row.names = c("Mon", "Tue", "Wed", "Thurs", "Fri", "Sat", "Sun"))

dfEmailSent <- dfEmailSent %>%
  rename(Day = Var1, Count = Freq, Prop = Freq.1) %>%
  select(-Var1.1)

dfEmailOpened <- data.frame(table(day(email$open_date)),
                          prop.table(table(day(emailReordered$open_date))) * 100,
                          row.names = c("Mon_1", "Tues_1", "Wed", "Thurs", "Fri", "Sat", "Sun", "Mon_2", "Tues_2"))

dfEmailOpened <-  dfEmailOpened %>%
  rename(Day = Var1, Count = Freq, Prop = Freq.1) %>%
  select(-Var1.1)

# Basic stats about email data
totalSent <- sum(emailReordered$sends)
totalOpened <- sum(emailReordered$opens)
proportionOpened <- round((totalOpened / totalSent) * 100, 2)

meanSent <- mean(emailReordered$sends)
meanOpened <- mean(emailReordered$opens)

totalClicks <- sum(emailReordered$clicks)
totalUnsubs <- sum(emailReordered$unsubs)

# Percentage rates
deliveryRate <- (totalSent / sum(emailReordered$sends)) * 100
openRate <- (totalOpened / totalSent) * 100
clickRate <- (totalClicks / totalSent) * 100
# Of all the emails sent, only 25.3% of them were opened.
# About 7% were clicked on and less than one percent of the clicks were to unsub.

dfRate <- data.frame(deliveryRate, openRate, clickRate) %>%

# Emails that were opened
emailsOpened <- filter(emailReordered, !is.na(open_time))

byDate <- group_by(emailsOpened, send_date, open_date)
sentOpen <- summarise(byDate, count = n())

# Order sentOpen by count from greatest to least
sentOpen[order(sentOpen$count, decreasing = T), ] %>%
  print


# -------------------------------------------------------------------------------------------------
# VISUALS

# Plotting number of emails sent, opened, clicked on, and unsubs
barplot(c(totalSent, totalOpened, totalClicks, totalUnsubs), names.arg = c("Sent", "Opened",
                                                                           "Clicked", "Unsub"),
        main = "Email Marketing Campaign Data", col = c("green", "blue", "red", "purple"))

# Delivery, Open, and Click rate
p <- ggplot(dfRate)
p + geom_bar(aes(x = reorder("Delivery", deliveryRate) , y = deliveryRate), stat = "identity", fill = "blue") + 
  geom_bar(aes(x = reorder("Open", openRate), y = openRate), stat = "identity", fill  = "red") + 
  geom_bar(aes(x = reorder("Click", clickRate), y = clickRate), stat = "identity", fill = "purple") + xlab("Category") + 
  ylab("Percentage Rate") + coord_flip()







