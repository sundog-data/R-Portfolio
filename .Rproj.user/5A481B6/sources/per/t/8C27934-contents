# Author: Scott Erickson
# Author Date: 01/01/2020
# R portfolio

rm(list = ls())

# load packages

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(data.table)

# Goal: Clean and modify data to allow for easier data analysis
# Broader goal, exploratory analysis
# Initial hypothesis: Areas that have lower SES in Seattle will have greater occurence to report times 

# Dataset 1 is from the Seattle City Dataset containing all crimes that were reported until 01/2019
crime <- read.csv("Crime_Data.csv")

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
# Curated and fairly clean
# Protects privacy by localizing crime reports and occurrences to beat locations

#### Data cleaning ###########

#Format dates and then create a date-time variable 
c_m <- crime %>% mutate(
  #standardize time variable to 4 digits
  occur.time = str_pad(crime$Occurred.Time, width = 4, pad = "0"),
  report.time = str_pad(crime$Reported.Time, width = 4, pad = "0"),
# Create two date-time object for occurence and report (we do this for R, tableu can actually handle this easily)
  occur.dt = mdy_hm(paste(Occurred.Date,
                          occur.time,
                          sep = " ")),
  report.dt = mdy_hm(paste(Reported.Date,
                           report.time, 
                           sep = " ")),
  # Determine difference in hours between occurence and report
  occ.to.rep = round(difftime(report.dt, occur.dt, units = "hours"),2)
)

# Dataset states data begins in earnest in 2008, but what are the realistic ranges
range(c_m$occur.dt[complete.cases(c_m)])
# events between 1908 and 05/2019 

# The subcategories have 31 levels, lets see if we can generate some broader categories
summary(c_m$Crime.Subcategory)

# Lets draw our categories from Uniform Crime Reporting Statistics
# https://www.ucrdatatool.gov/offenses.cfm

# Separate into violent crime + Nonviolent crime

violent.string <- c("ASSAULT", "ARSON", "HOMICIDE", "RAPE", "ROBBERY", "WEAPON")
nonviolent.string <- c("BURGLARY","CAR", "DISORDERLY",  "DUI", "GAMBLE",
                       "LOITERING", "LIQUOR", "NARCOTIC", "NONVIOLENT",
                       "PORNOGRAPHY", "PROSTITUTION","SEX", "THEFT", "TRESPASS")

# Do a string search for violent and non-violent key words
# side benefit of identifying errors in my code

c_m = c_m %>% mutate(violence.b = ifelse(grepl(paste(violent.string, collapse = "|"),
                                            as.character(Crime.Subcategory)),
                                   "Violent", 
                                   ifelse(grepl(paste(nonviolent.string, collapse = "|"),
                                                as.character(Crime.Subcategory)),
                                          "Nonviolent",
                                          "What")))

summary(as.factor(c_m$violence.b))
# looks as though there are some groups that were not assigned a subcategory for some reason 
# I will remove these values as SPD did not classify them into larger subgroups

c_m[c_m$violence.b == "What",] <- NA

summary(as.factor(c_m$violence.b))

summary(c_m$Neighborhood)

n <- as.character(unique(c_m$Neighborhood))
n

tract2neighborhood <- read_excel("SeattleCensusBlocksandNeighborhoodCorrelationFile.xlsx")
t2n <- tract2neighborhood

t <- unique(t2n$CRA_NAME)

n.s <- sort(n)

df = data.frame(n = n.s, t = c(t, rep("", 6)))

write.csv(df, "neighborhood.csv")

# Now I had to align the census tract data to the SPD neighborhoods which do not perfectly align.
# Neighborhoods with suspect matching are indicated by the suspect column
# Some neighborhoods were likely combinations of areas

n_merge <- read.csv("neighborhood_modified.csv")
t2n$CRA_NAME

t.key <- t2n[,c("TRACT_10", "CRA_NAME")]

# Now we will take 
