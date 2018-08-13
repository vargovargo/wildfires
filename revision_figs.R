rm(list = ls())

library(tidyverse)
library(data.table)
library(ggthemes)
setwd("~/Wildfires/revision/")



#####################################
######### results tables
#####################################

resultsTables <- read.csv("tidyResults.csv", header=T, stringsAsFactors = F) %>%
  mutate(class = factor(ifelse(Disease %in% c("Respiratory Index", 
                                              "Asthma",
                                              "Acute bronchitis",
                                              "COPD",
                                              "Pneumonia",
                                              "Upper Respiratory Infection",
                                              "Respiratory symptoms",
                                              "Bronchitis (not specified)"),"Respiratory",
                               ifelse(Disease %in% c("Cardiovascular Index",
                                                     "Ischemic heart disease",
                                                     "Dysrythmia",
                                                     "Chronic heart failure",
                                                     "Stroke",
                                                     "Diseases of the Peripheral Circulation"),"Cardiovascular","Overall")),
                        levels = c("Overall","Respiratory","Cardiovascular")))

resultsTables %>% 
  filter(period == "days1_5" & age !="all" &
           Disease %in% c("Respiratory Index", "Asthma", "Acute bronchitis", "Pneumonia", "Upper Respiratory Infection", "Respiratory symptoms", "COPD", "Bronchitis (not specified)")) %>%
  mutate(RR = as.numeric(as.character(RR)), 
         Disease = factor(Disease, levels= c("Asthma",
                                             "Upper Respiratory Infection",
                                             "Respiratory symptoms",
                                             "Acute bronchitis",
                                             "Bronchitis (not specified)",
                                             "Pneumonia",
                                             "COPD",
                                             "Respiratory Index",
                                             "Cardiovascular Index",
                                             "Ischemic heart disease",
                                             "Dysrythmia",
                                             "Chronic heart failure",
                                             "Stroke",
                                             "Diseases of the Peripheral Circulation",
                                             "Total episodes of care")),
         age = factor(age, levels= c( "Age 0-1","Age 2-4","Age 0-4","Age 5-17","Age 18-64", "all")),
         visit=factor(visit,  levels = c("Emergency Presentations" , "Inpatient Hospitalizations", "Outpatient Visits")),
         period = factor(period, levels= c("days1_5","days6_10","days11_15")), 
         sig = ifelse(U95 < 1 | L95 > 1, "Significant","Not Signficant")) %>%
  ggplot() + 
  geom_point(aes(x=Disease, y=RR,  color=visit, shape=visit, alpha=sig), size=3,position = position_dodge(width = 0.7)) + 
  geom_linerange(aes(x=Disease, ymax=as.numeric(U95), ymin=as.numeric(L95), color=visit, alpha=sig), position = position_dodge(width = 0.7)) + 
  geom_hline(yintercept = 1, linetype="dashed") + 
  scale_alpha_discrete(range = c(0.3, 1)) + 
  coord_cartesian(ylim=c(0, 3.5)) + facet_grid(.~age)  +
  theme_calc() # export as pdf 16"x3"

resultsTables %>% 
  filter(age =="all") %>%
  mutate(RR = as.numeric(as.character(RR)), 
         Disease = factor(Disease, levels= c("Asthma",
                                             "Upper Respiratory Infection",
                                             "Respiratory symptoms",
                                             "Acute bronchitis",
                                             "Bronchitis (not specified)",
                                             "Pneumonia",
                                             "COPD",
                                             "Respiratory Index", 
                                             "Cardiovascular Index",
                                             "Ischemic heart disease",
                                             "Dysrythmia",
                                             "Chronic heart failure",
                                             "Stroke",
                                             "Diseases of the Peripheral Circulation",
                                             "Total episodes of care" )),
         visit=factor(visit,  levels = c("Emergency Presentations" , "Inpatient Hospitalizations", "Outpatient Visits")),
         period = factor(period, levels= c("days1_5","days6_10","days11_15")), 
         sig = ifelse(U95 < 1 | L95 > 1, "Significant","Not Signficant")) %>%
  # filter(Disease %in% c("Respiratory Index", "Cardiovascular Index", "Total episodes of care" )) %>%
  filter(Disease %in% c("Asthma",
                        "Upper Respiratory Infection",
                        "Respiratory symptoms",
                        "Acute bronchitis",
                        "Bronchitis (not specified)",
                        "Pneumonia",
                        "COPD")) %>%

  ggplot() + 
  geom_point(aes(x=period, y=RR,  color=visit, shape=visit, alpha=sig), size=2,position = position_dodge(width = 0.7)) + 
  geom_linerange(aes(x=period, ymax=as.numeric(U95), ymin=as.numeric(L95), color=visit, alpha=sig), position = position_dodge(width = 0.7)) + 
  geom_hline(yintercept = 1, linetype="dashed") + 
  scale_alpha_discrete(range = c(0.3, 1)) + 
  coord_cartesian(ylim=c(0.5, 2.5)) + facet_grid(.~ Disease) +
  theme_calc() # export as pdf 16"x3"


+ coord_flip() 



#####################################
######### zip code tables
#####################################

# install.packages("scales")
library("scales")

zipPM <- read.csv("~/Wildfires/data/zipcodePM.csv", header=T, stringsAsFactors = F)%>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

read.csv("~/Wildfires/data/zipcodePM.csv", header=T, stringsAsFactors = F)  %>%
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  select(zipcode, date, pm25) %>%
  spread(key = date,value = pm25) %>%
  write.csv(zipPM, "./data/zipPMspread.csv", row.names = F)

avgPM <- zipPM %>%
  group_by(date)%>%
  summarise(average= mean(pm25)) %>%
  filter(date >= as.Date("2007-10-11") & date < as.Date("2007-11-11"))


zipPM %>%  
  filter(date >= as.Date("2007-10-11") & date < as.Date("2007-11-11")) %>%
  ggplot() + 
  geom_point(aes(x=date, y=pm25,group=zipcode),color="gray90", alpha=0.25) + 
  geom_line(data = avgPM, aes(x=date, y=average),  color="blue", size=1.4, alpha=0.4)+
  geom_boxplot(aes(x=date, y=pm25,group=date),color="gray20", alpha=0.1) +
  geom_hline(yintercept = 35, linetype="dashed", color="red", alpha=0.5, size=0.5) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme(axis.text.x = element_text(angle=45))


# write zip code averages
zipPM %>%  
  filter(date >= as.Date("2007-10-11") & date < as.Date("2007-11-11")) %>%
  mutate(day1_5 = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-26"), pm25,"no"),
         day6_10 = ifelse(date >= as.Date("2007-10-27") & date <= as.Date("2007-10-31"), pm25,"no"),
         day11_15 = ifelse(date >= as.Date("2007-11-01") & date <= as.Date("2007-11-05"), pm25,"no")) %>%
  gather(day1_5, day6_10, day11_15, key = period, value = pm25_period) %>%
  filter(pm25_period != "no") %>%
  group_by(period) %>%
  summarise(average = mean(as.numeric(pm25_period), na.rm=T), 
            SD = sd(as.numeric(pm25_period),na.rm = T),
            max = max(as.numeric(pm25_period), na.rm = T),
            min = min(as.numeric(pm25_period), na.rm=T),
            obs = length(as.numeric(pm25)),
            per5th = quantile(as.numeric(pm25), probs=0.05),            
            per25th = quantile(as.numeric(pm25), probs=0.25),
            per50th = quantile(as.numeric(pm25), probs=0.5),
            per75th = quantile(as.numeric(pm25), probs=0.75),
            per95th = quantile(as.numeric(pm25), probs=0.95)) %>%
  write.csv("./revision/county_pm_stats.csv", row.names = F)


# write daily stats
zipPM %>%  
  filter(date >= as.Date("2007-10-11") & date < as.Date("2007-11-11")) %>%
  group_by(date) %>%
  summarise(average = mean(as.numeric(pm25), na.rm=T), 
            SD = sd(as.numeric(pm25),na.rm = T),
            max = max(as.numeric(pm25), na.rm = T),
            min = min(as.numeric(pm25), na.rm=T),
            obs = length(as.numeric(pm25)),
            per5th = quantile(as.numeric(pm25), probs=0.05),            
            per25th = quantile(as.numeric(pm25), probs=0.25),
            per50th = quantile(as.numeric(pm25), probs=0.5),
            per75th = quantile(as.numeric(pm25), probs=0.75),
            per85th = quantile(as.numeric(pm25), probs=0.85),
            per95th = quantile(as.numeric(pm25), probs=0.95)) %>%
  write.csv("./revision/daily_pm_stats.csv", row.names = F)

# box plot
zipPM %>%  
  filter(date >= as.Date("2007-10-11") & date < as.Date("2007-11-11")) %>%
  ggplot() + geom_boxplot(aes(x=date, y=pm25,group=date),color="gray50", alpha=0.25) 
  geom_hline(yintercept = 35, linetype="dashed", color="red", alpha=0.5, size=0.5) +
  scale_x_date(labels = date_format("%m/%d")) +
  theme(axis.text.x = element_text(angle=45))
  
# write zip code averages to map
  zipPM %>%  
    filter(date >= as.Date("2007-10-11") & date < as.Date("2007-11-11")) %>%
    mutate(day1_5 = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-26"), pm25,"no"),
           day6_10 = ifelse(date >= as.Date("2007-10-27") & date <= as.Date("2007-10-31"), pm25,"no"),
           day11_15 = ifelse(date >= as.Date("2007-11-01") & date <= as.Date("2007-11-05"), pm25,"no")) %>%
    gather(day1_5, day6_10, day11_15, key = period, value = pm25_period) %>%
    filter(pm25_period != "no") %>%
    group_by(zipcode, period) %>%
    summarise(average = mean(as.numeric(pm25_period), na.rm = T)) %>%
    spread(key = period, value = average)%>%
    write.csv("~/Wildfires/data/zip_code_pm_avg_4GIS_revision.csv", row.names = F)
  
  # write zip code averages to map
  zipPM %>%  
    filter(date >= as.Date("2007-10-22") & date < as.Date("2007-10-27")) %>%
    select(zipcode, date, pm25) %>%
    spread(key = date, value = pm25) %>%
    rename(day1 = "2007-10-22",
           day2 = "2007-10-23",
           day3 = "2007-10-24",
           day4 = "2007-10-25",
           day5 = "2007-10-26") %>%
    write.csv("~/Wildfires/data/zip_code_pm_avg_4GIS_5day.csv", row.names = F)
