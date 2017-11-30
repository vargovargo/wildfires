rm(list=ls())

library(tidyverse)
library(stringr)

one <- read.csv("~/Wildfires/test/data (4).csv", header=T, colClasses="character")
two <- read.csv("~/Wildfires/test/data (5).csv", header=T, colClasses="character")
three <- read.csv("~/Wildfires/test/data (6).csv", header=T, colClasses="character")

temp <- str_split(all$name, "_")
all <- bind_rows(one, two, three) %>%
  mutate(variable = matrix(unlist(temp), ncol=6, byrow=TRUE)[1], 
         model = matrix(unlist(temp), ncol=6, byrow=TRUE)[2],
         RCP = matrix(unlist(temp), ncol=6, byrow=TRUE)[3],
         PopProj = matrix(unlist(temp), ncol=6, byrow=TRUE)[4],
         year = matrix(unlist(temp), ncol=6, byrow=TRUE)[6], 
         value = as.numeric(value))
unique(all$R)
str_extract_all(all$name, "_")

matrix(unlist(temp), ncol=3, byrow=TRUE)


all %>% 
  filter(year == 1954) %>% 
  ggplot(aes(x=x, y=y, color=value)) + geom_point() + facet_grid(RCP ~ PopProj)



rm(list=ls())
library(jsonlite)
library(tidyverse)


CalAdapt <- as.data.frame(fromJSON("http://api.cal-adapt.org/api/series/fire_HadGEM2-ES_rcp45_H_mu/rasters/?ref=%2Fapi%2Fcounties%2F1%2F"))


  ACSpop <- ACSpop[-1,]
  