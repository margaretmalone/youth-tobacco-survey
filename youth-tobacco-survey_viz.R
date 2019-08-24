library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(readxl)
youth.tobacco.survey <- read.csv("~/Documents/R/opinion_polling/csv-1.csv")
yts.hs <- youth.tobacco.survey %>% filter(Education == "High School")
yts.hs <- yts.hs %>% filter(Gender == "Overall")

cigarette.frequent <- yts.hs %>% filter(MeasureDesc == "Smoking Status") %>% filter(Response == "Frequent")
smokeless.frequent <- yts.hs %>% filter(MeasureDesc == "User Status") %>% filter(Response == "Frequent")
frequent.use <- rbind(cigarette.frequent, smokeless.frequent)
frequent.use.nc <- frequent.use %>% filter(LocationAbbr == "NC")
ggplot(frequent.use.nc, 
       aes(factor(YEAR), Data_Value, 
           group=TopicDesc,
           shape=TopicDesc,
           color=TopicDesc)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete("Year") +
  scale_y_continuous("Percentage") + 
  theme_minimal()

smokeless.ever <- yts.hs %>% filter(MeasureDesc == "User Status") %>% filter(Response == "Ever")
smokeless.use <- rbind(smokeless.frequent, smokeless.ever)
smokeless.use.nc <- smokeless.use %>% filter(LocationAbbr == "NC")
ggplot(smokeless.use.nc, 
       aes(factor(YEAR), Data_Value, 
           group=Response,
           shape=Response,
           color=Response)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete("Year") +
  scale_y_continuous("Percentage") + 
  theme_minimal()

cig99 <- cigarette.frequent %>% filter(YEAR == "1999")
mean(cig99$Data_Value)
cig02 <- cigarette.frequent %>% filter(YEAR == "2002")
mean(cig02$Data_Value)
cig04 <- cigarette.frequent %>% filter(YEAR == "2004")
mean(cig04$Data_Value)
cig05 <- cigarette.frequent %>% filter(YEAR == "2005")
mean(cig05$Data_Value)
cig09 <- cigarette.frequent %>% filter(YEAR == "2009")
mean(cig09$Data_Value)
cig11 <- cigarette.frequent %>% filter(YEAR == "2011")
mean(cig11$Data_Value)
cig13 <- cigarette.frequent %>% filter(YEAR == "2013")
mean(cig13$Data_Value)
cig15 <- cigarette.frequent %>% filter(YEAR == "2015")
mean(cig15$Data_Value)

smo99 <- smokeless.frequent %>% filter(YEAR == "1999")
mean(smo99$Data_Value)
smo02 <- smokeless.frequent %>% filter(YEAR == "2002")
mean(smo02$Data_Value)
smo04 <- smokeless.frequent %>% filter(YEAR == "2004")
mean(smo04$Data_Value)
smo05 <- smokeless.frequent %>% filter(YEAR == "2005")
mean(smo05$Data_Value)
smo09 <- smokeless.frequent %>% filter(YEAR == "2009")
mean(smo09$Data_Value)
smo11 <- smokeless.frequent %>% filter(YEAR == "2011")
mean(smo11$Data_Value)
smo13 <- smokeless.frequent %>% filter(YEAR == "2013")
mean(smo13$Data_Value)
smo15 <- smokeless.frequent %>% filter(YEAR == "2015")
mean(smo15$Data_Value)

us_averages <- read_excel("averages.xlsx")
ggplot(us_averages, 
       aes(factor(YEAR), Average, 
           group=Location,
           shape=Location,
           color=Location)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete("Year") +
  scale_y_continuous("Percentage") + 
  facet_grid(.~Type)
  theme_minimal()
  
us.smokeless <- us_averages %>% filter(Type == "Smokeless Tobacco")
ggplot(us.smokeless, 
       aes(factor(YEAR), Average, 
           group=Location,
           shape=Location,
           color=Location)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete("Year") +
  scale_y_continuous("Percentage") + 
  theme_minimal()

us.cig <- us_averages %>% filter(Type == "Cigarette")
ggplot(us.cig, 
       aes(factor(YEAR), Average, 
           group=Location,
           shape=Location,
           color=Location)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete("Year") +
  scale_y_continuous("Percentage") + 
  theme_minimal()

smoe99 <- smokeless.ever %>% filter(YEAR == "1999")
mean(smoe99$Data_Value)
smoe02 <- smokeless.ever %>% filter(YEAR == "2002")
mean(smoe02$Data_Value)
smoe04 <- smokeless.ever %>% filter(YEAR == "2004")
mean(smoe04$Data_Value)
smoe05 <- smokeless.ever %>% filter(YEAR == "2005")
mean(smoe05$Data_Value)
smoe09 <- smokeless.ever %>% filter(YEAR == "2009")
mean(smoe09$Data_Value)
smoe11 <- smokeless.ever %>% filter(YEAR == "2011")
mean(smoe11$Data_Value)
smoe13 <- smokeless.ever %>% filter(YEAR == "2013")
mean(smoe13$Data_Value)
smoe15 <- smokeless.ever %>% filter(YEAR == "2015")
mean(smoe15$Data_Value)

ever_averages <- read_excel("ever-averages.xlsx")
ggplot(ever_averages, 
       aes(factor(YEAR), Average, 
           group=Use,
           shape=Use,
           color=Use)) + 
  geom_line() + 
  geom_point() +
  scale_x_discrete("Year") +
  scale_y_continuous("Percentage") + 
  facet_grid(.~Location) +
  theme_minimal()

quit <- yts.hs %>% filter(MeasureDesc == "Percent of Current Smokers Who Want to Quit")
attempt.quit <- yts.hs %>% filter(MeasureDesc == "Quit Attempt in Past Year Among Current Cigarette Smokers")




