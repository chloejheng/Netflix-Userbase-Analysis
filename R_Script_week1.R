#Part I – create document
# Pei-Yu Jheng, 11/06/23, ALY6010 Week 1

#the environment reset code 
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

#Load any standard packages might need
library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(pacman)

#Part II – Import the file into R using read.csv( ) function
data <- read.csv("C:\\Users\\user\\Desktop\\2023 Fall\\ALY6010\\Module 1\\Project 1\\Netflix_userbase.csv")
class(data) #The class() function is used to show the object's category
glimpse(data) 

#Part III – Prepare data.frame for analysis
library("janitor")
data <- clean_names(data) #turn variables name to lower case
glimpse(data) #this function is used to see every column in a data frame.

#Remove any rows that contain NAs
df <- data %>% drop_na()

#Correcting data types
str(df)
#The data types are all correct, so no need to change

#Removing columns
df = subset(df, select = -c(user_id))

#Reorganizing the data
#The lubridate package contains helpful functions to convert dates represented as
#strings to dates represented as dates. Convert the join_date and last_payment_date columns to a
#type date using the ymd function.
library(lubridate)
colnames(data)
df$join_date <- ymd(dmy(df$join_date))
df$last_payment_date <- ymd(dmy(df$last_payment_date))


#Determine descriptive statistics for interesting variables
#max, min, mean, median, mode. standard deviation, variance, and range.
summary(df) #returns the minimum, maximum, mean, median, and 1st and 3rd quartiles for a numerical vector.

unique_characters <- unique(df$subscription_type)
character_vector <- paste(unique_characters, collapse = ", ")
cat(character_vector)

by(df, df$subscription_type, summary) #print summary by group

range(df$age)
range(df$monthly_revenue)

country_character <- unique(df$country)
country_vector <- paste(country_character, collapse = ", ")
cat(country_vector)
by(df, df$country, summary) #print summary by group

gender_character <- unique(df$gender)
gender_vector <- paste(gender_character, collapse = ", ")
cat(gender_vector)

device_character <- unique(df$device)
device_vector <- paste(device_character, collapse = ", ")
cat(device_vector)

plan_character <- unique(df$plan_duration)
plan_vector <- paste(plan_character, collapse = ", ")
cat(plan_vector)

last_payment_character <- unique(df$last_payment_date)
last_payment_vector <- paste(last_payment_character, collapse = ", ")
cat(last_payment_vector)
#means the customers are all currently active members in Netflix

#Part IIII – Prepare and export

#Produce visualizations from the raw data
#gender distribution
sum(df$gender == 'Male')
sum(df$gender == 'Female')
library(ggplot2) 

ggplot(df, aes(x = age, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution Comparison", x = "Age", y = "Number of Subscriptions", fill = "Gender") +
  scale_fill_manual(values = c("Male" = "aquamarine4", "Female" = "aquamarine3")) +
  theme_minimal()

#Question 1 about Revenue
#Which country has the highest revenue and which subscription type contributes the most in each country?

#(1) Sum the total revenue earned from each person: 
#how many months they joined and times the months and the monthly revenue
df$enrolled_months <- as.numeric(difftime(df$last_payment_date, df$join_date, units = "days") / 30.44)
df$enrolled_months <- floor(df$enrolled_months)

df$revenue_earned <- df$monthly_revenue * df$enrolled_months

#(2) prepare for creating chart
stacking_order <- c("Basic", "Standard", "Premium")

#(3) Stacked Bar Chart to show the subscription type for each country
ggplot(df, aes(x = country, y = revenue_earned, fill = subscription_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Revenue by Country Stacked by Subscription", x = "Country", y = "Total Revenue", fill = "Subscription Type") +
  theme_minimal() +
  coord_flip()+
  theme(plot.title = element_text(hjust = 0.5))

#Question 2 about Marketing performance
#How was the marketing performance reflected on new subscriptions during the year 2021 to the year 2023 by comparing?
#We can evaluate the marketing performance by counting the new subscriptions to Netflix.
library(ggthemes)
library(ggeasy)

#new subscriptions present by year and evaluated marketing performance by subscribing plan types
ggplot(df,aes(x=join_date,y=subscription_type,col=subscription_type))+geom_point()+ theme_tufte()+
  labs(title = "New Subscribers by Year & Subscription Type", x = 'Join Year', y ='Subscription Type', col = 'Subscription Type')

#new subscriptions present by year and evaluated marketing performance by country
ggplot(df, aes(x = join_date, y = country)) +
  geom_point() +
  labs(title = "New Subscribers by Year & Country ", x = "Join Year", y = "Country") +
  theme_minimal() 

#Question 3 about Customers behavior and preferences
#Which device are they using the most?
#Does age impact the type of device used?

sum_device <- df %>% group_by(device) %>% tally()
#the total number of each device didn't have very difference

colnames(sum_device)[2] ="subsrcibers_amount"

# find the largest number of devices
max_quantity <- max(sum_device$subsrcibers_amount)

# use subset function to filter the largest number of devices
most_common_device <- subset(sum_device, subsribers_amount == max_quantity)$device

# print result
cat("The largest number of devices are:", most_common_device, ", The quantity is:", max_quantity, "\n")

#create new column for scatter plot
df$join_year <- year(df$join_date)

#Scatter plot
ggplot(df,aes(x=device,y=age,col=join_year))+geom_point()+ theme_tufte()+
  labs(title = "Scatter Plot of Device vs. Age", x = 'Device', y ='Age', col = 'Join Year') +
  theme(plot.title = element_text(hjust = 0.5))

data_summary <- aggregate(country ~ age + device, df, length)

#Bubble Chart
ggplot(data_summary, aes(x = age, y = device, size = country, color = device)) +
  geom_point(alpha = 0.7) +  #Set bubble transparency
  scale_size_continuous(range = c(3, 15)) + #Set bubble size range
  labs(title = "Relationship Between Device Type & Age", x = "Age", y = "Device", color = 'Device', size = "Number of Subscribers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.2))

#create frequency table of age equal or older than 40
# Filter out customers over 40 years old
data_40plus <- df[df$age >= 40, ]

# Calculate the frequency of use of each device
frequency_table <- table(data_40plus$device)

# Calculate total rows
total <- sum(frequency_table)

# Calculate relative frequency
relative_frequency <- (frequency_table / total) * 100

#Create the final frequency table
final_table <- data.frame(
  Device = names(frequency_table),
  Frequency = as.vector(frequency_table),
  Relative_Frequency = as.vector(relative_frequency)
)

#Add total row
total_row <- data.frame(Device = "Total", Frequency = total, Relative_Frequency = 100)
final_table <- rbind(final_table, total_row)

print(final_table)
final_table$Relative_Frequency <- floor(final_table$Relative_Frequency * 10) / 10
print(final_table)
final_table$Relative_Frequency <- paste(final_table$Relative_Frequency, "%", sep = "")
print(final_table)

#create frequency table of age less than 40
# Filter out customers under 40 years old
data_40below <- df[df$age < 40, ]

# Calculate the frequency of use of each device
frequency_table1 <- table(data_40below$device)

# Calculate total rows
total1 <- sum(frequency_table1)

# Calculate relative frequency
relative_frequency1 <- (frequency_table1 / total1) * 100

#Create the final frequency table
younger_table <- data.frame(
  Device = names(frequency_table1),
  Frequency = as.vector(frequency_table1),
  Relative_Frequency = as.vector(relative_frequency1)
)

#Add total row
total_row1 <- data.frame(Device = "Total", Frequency = total1, Relative_Frequency = 100)
younger_table <- rbind(younger_table, total_row1)

print(younger_table)
younger_table$Relative_Frequency <- floor(younger_table$Relative_Frequency * 10) / 10
print(younger_table)
younger_table$Relative_Frequency <- paste(younger_table$Relative_Frequency, "%", sep = "")
print(younger_table)

#people older than 40 is the potential market for Smart TV, there are only 23.4% utilization
#Smart TV is a device least used for subscribers equal to or older than 40
#Laptop and Smart TV are two devices different between young and middle-aged people.
#Netflix can make effort on Laptop platform it is using by the largest customers and especially for middle-age people.
#To improve the platform can save them from stop subscribe

