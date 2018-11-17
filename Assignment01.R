setwd("~/Dropbox/R Programming Course Materials/Week 1/datasets")

## WHO dataset
WHO <- read.csv("WHO.csv")

# Country with the lowest literacy
WHO$Country[which.min(WHO$LiteracyRate)]

# Richest country in Europe based on GNI
WHO.Europe <- subset(WHO, Region == "Europe")
WHO.Europe$Country[which.max(WHO.Europe$GNI)]

# Mean Life expectancy of countries in Africa
WHO.Africa <- subset(WHO, Region == "Africa")
mean(WHO.Africa$LifeExpectancy)

# Number of countries with population greater than 10,000
sum(WHO$Population > 10000)

# Top 5 countries in the Americas with the highest child mortality
top5 <- order(WHO.Americas$ChildMortality, decreasing = TRUE)[1:5]
WHO.Americas$Country[top5]

## NBA dataset (Historical NBA Performance.xlsx)
# The year Bulls has the highest winning percentage  
library(readxl)
NBA = read_excel("Historical NBA Performance.xlsx")
NBA.Bulls = subset(NBA, Team == "Bulls")
NBA.Bulls$Year[which.max(NBA.Bulls$`Winning Percentage`)]

# Teams with an even win-loss record in a year 
NBA.EvenWinLoss = subset(NBA, NBA$`Winning Percentage`==0.5)
NBA.EvenWinLoss

## Seasons_Stats.csv  
# Player with the highest 3-pt attempt rate in a season.   
# Player with the highest free throw rate in a season. 

# What year/season does Lebron James scored the highest?
Seasons_Stats = read.csv("Seasons_Stats.csv")
Lebron = subset(Seasons_Stats, Player=="LeBron James")
Lebron$Year[which.max(Lebron$PTS)]

# What year/season does Michael Jordan scored the highest?
Jordan = subset(Seasons_Stats, Player =="Michael Jordan*")
Jordan$Year[which.max(Jordan$PTS)]

# Player efficiency rating of Kobe Bryant in the year where his MP is the lowest? 
Kobe = subset(Seasons_Stats, Player == "Kobe Bryant")
Kobe$PER[which.min(Kobe$MP)]

## National Universities Rankings.csv   
univ = read.csv("National Universities Rankings.csv")

# University with the most number of undergrads    
# Average Tuition in the Top 10 University 
