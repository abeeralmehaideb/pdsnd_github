library("tidyverse")
library("lubridate")
library("stringr")
library("dplyr")
library("readr")
library("ggplot2")

getwd()
setwd("/Users/abeeralmuhaidib/Desktop/udacityR")

ny <- read.csv("newyork.csv")
wash <- read.csv("washington.csv")
chi <- read.csv("chicago.csv")



data_all <- list.files(pattern = "*.csv", full.names = T) %>%
  lapply(read.csv) %>% bind_rows()
  
  

# Question 1: What is the most common users ages on Chicago and New york cities have?

qplot(x = City, y = Birth.Year, data = data_all)
ggplot(aes(x = City, y = Birth.Year), data = data_all) + geom_point()

# It seems that on Chicago they are between 1945 - 2000 years and on New york the are between 1940 - 2000 years.


# Question 2: What are the counts of each user type?
data_all %>% 
  filter(User.Type == "Customer" | User.Type == "Subscriber") %>%
  drop_na(User.Type) %>%
  ggplot(aes(User.Type), fill = User.Type) + 
  geom_bar(position = "dodge",
           alpha = 0.5 ) +
  labs(title = "Total User type", x = "User type", y = "Number")
  
# It seems that the most user types in all cities are subscribers.
 

# Question 3: What are the counts of each gender (only available for NYC and Chicago)?
total = sort(table(data_all$Gender))
print(total)

round((total / length(data_all$Gender) * 100), digits = 2)

data_all %>% 
  filter(Gender == "Female" | Gender == "Male") %>%
  drop_na(Gender) %>%
ggplot(aes(Gender), fill = City) +
  geom_bar(position = 'dodge', colour="black") +
  ggtitle('counts of each gender on NYC and Chicago') +
  scale_x_discrete(labels = c('Female', 'Male')) +
  labs(y = 'Total Number', x = 'Gender') 

# It seems that the most riders are males.


