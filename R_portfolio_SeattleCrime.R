# Author: Scott Erickson
# Author Date: 01/01/2020
# R portfolio

rm(list = ls())

# load packages

library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)

# Project Goals: 
# Broad Questions
# Seattle has undergone an unprecedented period of growth over the past 10 years
# How has this growth affected crime in the city? 

# Question 1: 

# Has Seattle become more dangerous over time? 

# Approach
# Descriptive statistics of overall crime rates plotted over overall plot of population
# Determine whether or not the crime rate (crimes/population) has changed in Seattle over the life of the dataset


# Method 1: Simple F test of trend over the years of the dataset for the equation: 
# Y(crime rate) = A(constant) + B(crime rate)*X(Years)
# Method 2: Plot of trend of crime rates in Seattle
# Y-axis - Crime Rate
# X-axis - Years


# Question 2:
# Evaluate when and where crimes in Seattle crimes occurr

# Approach: 

# Import city of Seattle crime data from <<XXXX>>

# Dataset 1 is from the Seattle City Dataset containing all crimes that were reported until 01/2019
crime <- read.csv("Seattle_crime.csv")

# Dataset 2 is a list of City of Seattle population reports from US census
# Google city of seattle population, king count 
#https://www.google.com/publicdata/explore?ds=kf7tgg1uo9ude_&met_y=population&idim=sub_county:5363000:0667000&hl=en&dl=en#!ctype=l&strail=false&bcs=d&nselm=h&met_y=population&scale_y=lin&ind_y=false&rdim=sub_county&idim=sub_county:5363000&ifdim=sub_county:county:53033&tstart=1167638400000&tend=1483257600000&hl=en_US&dl=en&ind=false


sea.pop <- data.frame(yr = seq(2008,2017, by = 1),
                       pop = c(603404,	618420, #2008
                               610333,	622532,	#2010
                               635974,	654176, #2012
                               669641,	685447,	#2014
                               707255, 724745) # 2016
                       )

# dataset properties
# Appears to be highly curated
# Protects privacy by localizing crime reports and occurrences to beat locations

#### Data cleaning ###########

#Format dates and then create a date-time variable 
crime <- crime %>% mutate(
  #standardize time variable to 4 digits
  occur.time = str_pad(crime$Occurred.Time, width = 4, pad = "0"),
  report.time = str_pad(crime$Reported.Time, width = 4, pad = "0"),
  # Create two date-time object for occurence and report
  occur.dt = mdy_hm(paste(Occurred.Date,
                          occur.time,
                          sep = " ")),
  report.dt = mdy_hm(paste(Reported.Date,
                           report.time, 
                           sep = " ")),
  report.yr = year(report.dt),
  occur.yr = year(occur.dt),
  # Determine difference in hours between occurence and report
  occ.to.rep = difftime(report.dt, occur.dt, units = "hours")
)


# Are we missing any values in our newly created variable
length(crime[is.na(crime$occ.to.rep),])

######## Start of dataset
# Lets look at whether data was flowing into the dataset at approximately equal rates each year
summary(as.factor(crime$occur.yr))
# Looks as though our dataset begins collection in 2008 and has only partial collection into 2019

# Given the large spike in the number of cases in 2008 I would hazard that reporting for this dataset began in volume then
# Earlier values were likely either trial data for this dataset which are likely not representative of our actual field of interest

# To incorporate a washout period I will look for where the data begins to be incorporated in large volumes
# looks like 2008 was when data collection began in earnest
# lets take a brief rough look @ the months to see if data collection really began on the 1st of the year
# There is a high chance of this happening as 2008 has approximately the same number of datapoints collected as 2009
count(crime %>% filter(occur.dt < as.POSIXct("2008-02-01")))
count(crime %>% filter(occur.dt < as.POSIXct("2008-03-01") & occur.dt >= as.POSIXct("2008-02-01")))

# January and February have approximately the same number of reported crimes.
# 3501 v 3501

# We will say collection begain in earnest in 2008 and set start date there
# We have census data between 2008 and 2017
# These two criteria will inform our data censoring

c <- crime %>% filter(occur.dt >= as.POSIXct("2008-01-01") & occur.dt < as.POSIXct("2018-01-01"))

nrow(crime) - nrow(c) # we extracted 67795 crimes via censoring

###### Missingness Check #########
# lets look for missingness in our dataset
nrow(c) - nrow(complete(c))
# neat, there is no missingness in this dataset

##### Other oddities ########
# number of cases where crime was reported before it occurred
length(crime[crime$occ.to.rep < 0,]) 
# 18/523591 

# Possibly dubious, but plausible and as such we will leave them in the system

#### Analysis ############

# Question 1:

# Now that the data is censored to our appropriate ranges lets make our rates
c.yr <- c %>% group_by(occur.yr) %>% summarise(crime.count = n())
# merging our population rates into our dataset
cr <- merge(c.yr, sea.pop, by.x = "occur.yr", by.y = "yr")
cr$crime.rate = cr$crime.count/cr$pop

# Lets examine a linear model
mod1 <- lm(crime.rate ~ occur.yr, data = cr)
summary(mod1)
anova(mod1)

f <- predict(mod1, se.fit = T)

pred.mod <- data.frame(
  yr = cr$occur.yr, 
  fit = f$fit, 
  up = f$fit + 1.96*f$se.fit,
  low = f$fit - 1.96 *f$se.fit)

# lets plot this out
ggplot(cr, aes(x = occur.yr, y = crime.rate))+ 
  geom_point()+
  geom_line(data = pred.mod, aes(x = yr, y = fit), lty = 2)+
  geom_ribbon(data = pred.mod, aes(x = yr, y = fit, ymin = low, ymax = up), alpha = 0.3)+
  labs(x = "Year", 
       y = "Crimes Rate (# of Crimes occured / Seattle Population)")+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(xlim = c(2008, 2018), ylim = c(.01, .1), expand = 0.01)+
  theme_classic()

# Summary, in jargon: We fail to reject the null hypothesis that B() != 0
# Summary: There is no definitive evidence that the overall crime rate in Seattle has changed meaningfully
# If anything the line indicates that overall the crime rate has fallen slightly from 2008 to now

# Subquery, has the type of crime changed meaningfully over this time perio
# Subquery: What are the different categories of crime reported in this dataset
levels(c$Crime.Subcategory)
# There are 31 different crime in this dataset

# Lets briefly look at the categories
ggplot(c, aes(Crime.Subcategory))+
  geom_histogram(stat = "count")+
  coord_flip()

# We can likely group these into a few more meaningful categories
miss.str <- levels(c$Crime.Subcategory)[1]
theft.str <- levels(c$Crime.Subcategory)[26:29]
robbery.str <- levels(c$Crime.Subcategory)[22:24]
burglary.str <- levels(c$Crime.Subcategory)[5:8]

c$crime.cat <- as.character(c$Crime.Subcategory)
c$crime.cat[c$crime.cat %in% miss.str] <- "MISSING CATEGORY" 
c$crime.cat[c$crime.cat %in% theft.str] <- "THEFT"
c$crime.cat[c$crime.cat %in% robbery.str] <- "ROBBERY"
c$crime.cat[c$crime.cat %in% burglary.str] <- "BURGLARY"


# Re-examine the plot
ggplot(c, aes(crime.cat))+
  geom_histogram(stat = "count")+
  coord_flip()

# Lets make this into something that looksnice
crime.sum <- setDT(data.frame(summary(factor(c$crime.cat))), 
                   keep.rownames = T)

colnames(crime.sum) <- c("cat", "count")
crime.sum <- crime.sum[complete.cases(crime.sum)]
crime.sum$count.thous = crime.sum$count/1000

ggplot(crime.sum, aes(reorder(cat, count.thous), count.thous))+
  geom_bar(stat = "identity")+
  ggtitle("Number of crimes reported by category in Seattle between 05/2000 and 05/2019")+
  ylab("Number of Crimes (Thousands)")+
  xlab("Crime Category")+
  coord_flip(expand = F)+
  theme_classic()

# Lets collapse these categories even more

property.str <- c("BURGLARY",
                   "MOTOR VEHICLE THEFT",
                   "THEFT",
                  "CAR PROWL")
violent.str <- c("AGGRAVATED ASSAULT",
                 "AGGRAVATED ASSAULT-DV",
                 "ARSON",
                 "HOMICIDE",
                 "WEAPON",
                 "RAPE",
                 "ROBBERY")
behavioral.str <- c("DISORDERLY CONDUCT",
                    "GAMBLE",
                    "LOITERING",
                    "TRESPASS",
                    "MISSING CATEGORY")
drug.str <- c("DUI",
              "LIQUOR LAW VIOLATION",
              "NARCOTIC")
sexual.str <- c("FAMILY OFFENSE-NONVIOLENT",
                "PORNOGRAPHY",
                "PROSTITUTION",
                "SEX OFFENSE-OTHER")

c$large.cat[c$crime.cat %in% property.str] <- "PROPERTY" 
c$large.cat[c$crime.cat %in% violent.str] <- "VIOLENT"
c$large.cat[c$crime.cat %in% behavioral.str] <- "BEHAVIORAL"
c$large.cat[c$crime.cat %in% drug.str] <- "DRUG OR ALCOHOL"
c$large.cat[c$crime.cat %in% sexual.str] <- "SEXUAL"

summary(as.factor(c$large.cat))


# Conclusions: 
# Seattle's four most common types of crime reported over this period of time are property crimes
# Not exactly surprising, this is the case for most cities, but lets see look at some longitudinal trends

ggplot(c, aes(x = occur.yr, color = crime.cat, fill = crime.cat))+ 
  geom_bar(position = "stack")+
  coord_cartesian(expand = F)+
  theme_classic()+
  facet_wrap(~large.cat)

# Total number of crimes is on the rise
c.cat.yr = c %>% group_by(occur.yr, large.cat) %>% summarise(count = n())
c.cat.yr.m = merge(c.cat.yr, sea.pop, by.x = "occur.yr", 
                   by.y = "yr", all.x = T)


c.cat.yr.m$crime.rate = c.cat.yr.m$count/c.cat.yr.m$pop
c.cat.yr.m = c.cat.yr.m[complete.cases(c.cat.yr.m),]

mod2 = lm(crime.rate ~ occur.yr + large.cat, data = c.cat.yr.m)

f2 <- predict(mod2, se.fit = T)

pred.mod2 <- data.frame(
  yr = c.cat.yr.m$occur.yr, 
  cat = c.cat.yr.m$large.cat,
  rate = c.cat.yr.m$crime.rate,
  fit = f2$fit, 
  up = f2$fit + 1.96*f2$se.fit,
  low = f2$fit - 1.96 *f2$se.fit)

ggplot(pred.mod2, aes(x = yr, y = rate, color = cat))+
  geom_point()+
  geom_smooth(lty = 3, se = F)+
 # geom_line(data = pred.mod2, aes(y = fit), lty = 2)+
  labs(x = "Year", 
       y = "Crimes Rate (# of Crimes occured / Seattle Population)")+
  scale_y_continuous(labels = scales::percent)+
  coord_cartesian(xlim = c(2008, 2018), ylim = c(0, .065), expand = F)+
  theme_classic()

write.csv(c, "Seattle_crime.csv")
write.csv(c.cat.yr.m, "sea_crimerate.csv")
