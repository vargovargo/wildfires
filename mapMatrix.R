rm(list = ls())

library(ncdf4)
library(tidyverse)
library(data.table)
setwd("~/Wildfires/")
# retrieve a list of nc files in my data folder:

#flist <- list.files(path = "data/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")

flist <- c(
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp45/CNRM-CM5_45_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp45/CNRM-CM5_45_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp45/CNRM-CM5_45_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp85/CNRM-CM5_85_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp85/CNRM-CM5_85_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp85/CNRM-CM5_85_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp45/CanESM2_45_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp45/CanESM2_45_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp45/CanESM2_45_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp85/CanESM2_85_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp85/CanESM2_85_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp85/CanESM2_85_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp45/HadGEM2-ES_45_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp45/HadGEM2-ES_45_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp45/HadGEM2-ES_45_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp85/HadGEM2-ES_85_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp85/HadGEM2-ES_85_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp85/HadGEM2-ES_85_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp45/MIROC5_45_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp45/MIROC5_45_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp45/MIROC5_45_AA.all.bau.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp85/MIROC5_85_AA.all.H.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp85/MIROC5_85_AA.all.L.mu.nc",
  "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp85/MIROC5_85_AA.all.bau.mu.nc"
)


#####################################
# Define our function to process wildfire netcdf
#####################################
dname = "hectares"

process_nc <- function(files){
    # iterate through the nc
    for (i in 1:length(files)){
     
      # download.file(url = flist[1], destfile = "~/tempWF.nc")
      ncdfURL <- paste0(flist[i])
      dest <-  paste0("~/tempWFnetcdf.nc") 
      download.file(url=ncdfURL,destfile=dest, mode = "wb") 
      
      ncin <- nc_open(dest)
      
      # store values from variables and atributes
      attributes(ncin$dim)$names
      nc_lat <- ncvar_get(ncin, "lat")
      n_lat <- dim(nc_lat)
      nc_lon <- ncvar_get(ncin, "lon")
      n_lon <- dim(nc_lon)
      nc_year <- ncvar_get(ncin, "time")+1953
      n_year <- dim(nc_year)
      
      # get hectares
      tmp_array <- ncvar_get(ncin,dname)
      dlname <- ncatt_get(ncin,dname,"long_name")
      dunits <- ncatt_get(ncin,dname,"units")
      fillvalue <- ncatt_get(ncin,dname,"_FillValue")
      dim(tmp_array)
      
      # get global attributes
      # title <- ncatt_get(ncin,0,"title")
      # institution <- ncatt_get(ncin,0,"institution")
      # datasource <- ncatt_get(ncin,0,"source")
      # references <- ncatt_get(ncin,0,"references")
      # history <- ncatt_get(ncin,0,"history")
      # Conventions <- ncatt_get(ncin,0,"Conventions")
      
      nc_close(ncin)
      
      # replace netCDF fill values with NA's
      tmp_array[tmp_array==fillvalue$value] <- NA
      
      # check number of missing data values
      # length(na.omit(as.vector(tmp_array[,,1])))
      # dimnames(tmp_array) <-list(lon=nc_lon, lat=nc_lat, year=nc_year)
      
      bar <- data.frame(expand.grid(lon = nc_lon, lat = nc_lat)) %>%
        cbind(matrix(as.vector(tmp_array), nrow = n_lon*n_lat, ncol = n_year)) %>% 
        mutate(model=unlist(str_split(string = files[i], pattern = "/"))[7],
               scenario = unlist(str_split(string = files[i], pattern = "_"))[2],
               population = unlist(str_split(string = files[i], pattern = "_"))[3]) %>% 
        select(model, scenario, population, lon, lat, 3:150)
        
      names(bar) <- c("model","scenario","population","lon","lat",1954:2100) 
    
      
      # set the name of my new variable and bind the new data to it
      if(exists("wildfire")){
        wildfire <- bind_rows(wildfire, bar)
      }else{
        wildfire <- bar
      }
      # tidy up, not sure if necessary really, but neater
      rm(nc_year, nc_lat, nc_lon, ncin, bar)
    }
    
    return(wildfire)
  }


keycols <- c("lon", "lat")

all_fire_dt <- as.data.table(process_nc(flist)) %>% setkeyv(keycols)

#expand.grid(lon = unique(all_fire$lon), lat = unique(all_fire$lat)) %>% write.csv(.,"./data/latlon.csv", row.names = F)

# bring in file that links LOCA points to counties
locationData <- as.data.table(read.csv("https://raw.githubusercontent.com/vargovargo/wildfires/master/LOCAcounties.csv", header=T)) %>% 
  select(lon, lat, County, ClimateRegion, stcoFIPS) %>% setkeyv(keycols) 




#####################################
######### wildfire maps and  tables
#####################################

threshold <- 40.4686 # 100 acres

# create map data San Diego 
SanDiegoMaps <- all_fire_dt[locationData] %>% na.omit() %>%
  filter(County %in% c("San Diego") & scenario == "85" & population == "AA.all.bau.mu.nc") %>%
  gather(6:152, key = "year", value = "hectares") %>% 
  mutate(year = as.integer(as.character(year))) %>% 
  mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                      ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                             ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                               levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs")),
         period = factor(ifelse(year %in% c(2000:2019),"current",
                                ifelse(year %in% c(2040:2059),"future", "other")),
                                levels = c("current","future","other")),
         geography = "San Diego",
         freq = ifelse(hectares > threshold, 1, 0))  %>% 
  filter(period != "other") %>%
  group_by(lat, lon, period, geography) %>% 
  summarise(total_hectares = sum(hectares, na.rm=T),
            mean_hectares = mean(hectares, na.rm=T),
            probability = as.numeric(sum(freq)/80))%>%
  as.data.table() %>% setkeyv(c("lat", "lon"))

  
SanDiegoMaps %>%
  ggplot(aes(x=lon, y=lat, color=probability)) + geom_point(size=3, shape=15) + facet_grid(.~ period)  + scale_color_distiller(palette = "Spectral")

SanDiegoMaps %>%
  ggplot(aes(x=lon, y=lat, color=mean_hectares)) + geom_point(size=3, shape=15) + facet_grid(.~ period) + 
  scale_color_distiller(palette = "RdGy")



SanDiegoMaps %>%
  filter(period == "current")  %>%
write.csv("./data/spatial/SDmaps_current.csv", row.names = F)

SanDiegoMaps %>%
  filter(period == "future")  %>%
write.csv("./data/spatial/SDmaps_future.csv", row.names = F)

# create map data San Diego 
CAMaps <- all_fire_dt[locationData] %>% na.omit() %>%
  filter(scenario == "85" & population == "AA.all.bau.mu.nc") %>%
  gather(6:152, key = "year", value = "hectares") %>% 
  mutate(year = as.integer(as.character(year))) %>% 
  mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                      ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                             ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                               levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs")),
         period = factor(ifelse(year %in% c(2000:2019),"current",
                                ifelse(year %in% c(2040:2059),"future", "other")),
                         levels = c("current","future","other")),
         geography = "State", 
         freq = ifelse(hectares > threshold, 1, 0))  %>% 
  filter(period != "other") %>%
  group_by(lat, lon, period, geography) %>% 
  summarise(total_hectares = sum(hectares, na.rm=T),
            mean_hectares = mean(hectares, na.rm=T),
            probability = sum(freq)/80)%>%
  as.data.table() %>% setkeyv(c("lat", "lon"))




CAMaps %>%
  ggplot(aes(x=lon, y=lat, color=probability)) + geom_point(size=1, shape=15) + facet_grid(.~ period) + 
  scale_color_distiller(palette = "Spectral")

CAMaps %>%
  ggplot(aes(x=lon, y=lat, color=mean_hectares)) + geom_point(size=1, shape=15) + facet_grid(.~ period) + 
  scale_color_distiller(palette = "RdGy")


CAMaps %>%
  filter(period == "current") %>%
write.csv("./data/spatial/CAmaps_current.csv", row.names = F)

CAMaps %>%
  filter(period == "future")  %>%
write.csv("./data/spatial/CAmaps_future.csv", row.names = F)



matrix <- bind_rows(SanDiegoMaps, CAMaps)

matrix %>%
  ggplot(aes(x=lon, y=lat, color=probability)) + geom_point(size=1, shape=15) + facet_grid(geography~ period) + 
  scale_color_distiller(palette = "Spectral")

matrix %>%
  ggplot(aes(x=lon, y=lat, color=mean_hectares)) + geom_point(size=1, shape=15) + facet_grid(geography~ period) + 
  scale_color_distiller(palette = "RdGy")


#####################################
######### population data
#####################################

POP2000 <- as.data.table(read.csv("./data/Intercensal_2000-2010_DBInput_csv.txt", header=T, stringsAsFactors = F)) %>%
  mutate(ageCat = ifelse(Age <=5, "under5",ifelse(Age >=65,"over65","other"))) %>%
  filter(Year != "4/1/2000 0:00:00" & Year != "4/1/2010 0:00:00" & CountyName != "California") %>% 
  mutate(year = as.numeric(substring(sapply(strsplit(as.character(Year), "/"), "[[", 3),1,4))) %>%
  mutate(county_name = ifelse(CountyName =="ContraCosta", "Contra Costa",
                              ifelse(CountyName =="DelNorte", "Del Norte",
                                     ifelse(CountyName =="ElDorado", "El Dorado",
                                            ifelse(CountyName =="LosAngeles", "Los Angeles",
                                                   ifelse(CountyName =="SanBenito", "San Benito",
                                                          ifelse(CountyName =="SanBernardino", "San Bernardino",
                                                                 ifelse(CountyName =="SanDiego", "San Diego",
                                                                        ifelse(CountyName =="SanFrancisco", "San Francisco",
                                                                               ifelse(CountyName =="SanJoaquin", "San Joaquin",
                                                                                      ifelse(CountyName =="SanLuisObispo", "San Luis Obispo",
                                                                                             ifelse(CountyName =="SanMateo", "San Mateo",
                                                                                                    ifelse(CountyName =="SantaBarbara", "Santa Barbara",
                                                                                                           ifelse(CountyName =="SantaClara", "Santa Clara",
                                                                                                                  ifelse(CountyName =="SantaCruz", "Santa Cruz",CountyName))))))))))))))) %>%
  group_by(ageCat, year, county_name) %>%
  summarise(people = sum(Population, na.rm=T)) %>% 
  left_join(read.csv("~/CA_stcoFIPS_key.csv", header=T, stringsAsFactors = FALSE)) %>%
  select(ageCat, year, stcoFIPS, county_name, people)



# bring in Population Projection Data
POPproj <- as.data.table(read.csv("./data/P3_Complete.csv", header=T)) %>%
  mutate(ageCat = ifelse(agerc <=5, "under5",ifelse(agerc >=65,"over65","other")),
         stcoFIPS = as.numeric(as.character(fips))) %>%
  full_join(read.csv("~/CA_stcoFIPS_key.csv", header=T)) %>%
  group_by(ageCat, year, stcoFIPS, county_name) %>%
  summarise(people = sum(perwt, na.rm=T)) %>%
  bind_rows(POP2000) %>% 
  mutate(period = factor(ifelse(year %in% c(2000:2019),"current",
                                ifelse(year %in% c(2040:2059),"future", "other")),
                         levels = c("current","future","other"))) %>%
  filter(period != "other") %>%
  group_by(ageCat, stcoFIPS, county_name, period) %>%
  summarise(people = mean(people, na.rm=T)) %>%
  as.data.table() %>% setkeyv("county_name")

POPproj %>% spread(key = ageCat, value = people) %>%
  mutate(totalPop = (other + over65 + under5)) %>% 
  mutate(pctOld = over65/(totalPop),
         pctYoung = under5/(totalPop)) %>% write.csv(., "./data/StatePOPprojectionsPCT.csv", row.names = F)


POPproj %>%
  ggplot(aes(x=factor(county_name), y=people, color=decade)) + geom_point() + facet_grid(.~ ageCat, scales = "free_x")+ coord_flip()

POPproj %>%
  filter(county_name =="San Diego")

POPproj %>%
  group_by(ageCat, decade) %>%
  summarise(statePOP = sum(people))



#####################################
######### results tables
#####################################

resultsTables <- read.csv("https://raw.githubusercontent.com/vargovargo/wildfires/master/AgeSpecificResults.csv", header=T, stringsAsFactors = F) %>%
  mutate(class = factor(ifelse(Disease %in% c("Respiratory (all)", 
                                              "Asthma",
                                              "Acute Bronchitis",
                                              "COPD",
                                              "Pneumonia",
                                              "Upper Respiratory Infection",
                                              "Respiratory Symptoms",
                                              "Bronchitis (not specified)"),"Respiratory",
                               ifelse(Disease %in% c("Cardiovascular (all)",
                                                     "Ischemic Heart Disease",
                                                     "Dysrythmia",
                                                     "Chronic Heart Failure",
                                                     "Stroke",
                                                     "Diseases of the Peripheral Circulation"),"Cardiovascular","Overall")),
                        levels = c("Overall","Respiratory","Cardiovascular")))

resultsTables %>% 
  filter(lag == "5-day" & age !="all" &
           Disease %in% c("Respiratory (all)", "Asthma", "Acute Bronchitis", "Pneumonia", "Upper Respiratory Infection", "Respiratory Symptoms", "COPD", "Bronchitis (not specified)")) %>%
  mutate(RR = as.numeric(as.character(RR)), 
         Disease = factor(Disease, levels= c("Asthma",
                                             "Upper Respiratory Infection",
                                             "Respiratory Symptoms",
                                             "Acute Bronchitis",
                                             "Bronchitis (not specified)",
                                             "Pneumonia",
                                             "COPD",
                                             "Respiratory (all)",
                                             "Cardiovascular (all)",
                                             "Ischemic Heart Disease",
                                             "Dysrythmia",
                                             "Chronic Heart Failure",
                                             "Stroke",
                                             "Diseases of the Peripheral Circulation",
                                             "Total Encounters")),
         visit=factor(visit, levels = c("ED" , "Inpatient Hospitalizations", "Doctor's Office and Clinic Visits")),
         age = factor(age, levels= c("Age 0-4","Age 5-17","Age 18+", "all")),
         lag = factor(lag, levels= c("5-day","8-day","14-day")), 
         sig = ifelse(U95 < 1 | L95 > 1, "Significant","Not Signficant"))%>%
  filter(age %in% c("Age 0-4","Age 5-17","Age 18+")) %>%
  ggplot() + 
  geom_point(aes(x=Disease, y=RR,  color=visit, shape=visit, alpha=sig), size=3,position = position_dodge(width = 0.7)) + 
  geom_linerange(aes(x=Disease, ymax=as.numeric(U95), ymin=as.numeric(L95), color=visit, alpha=sig), position = position_dodge(width = 0.7)) + 
  geom_hline(yintercept = 1, linetype="dashed") + 
  coord_cartesian(ylim=c(0, 3)) + facet_grid(.~age) 

resultsTables %>% 
  filter(age =="all") %>%
  mutate(RR = as.numeric(as.character(RR)), 
         Disease = factor(Disease, levels= c("Respiratory (all)", 
                                             "Asthma",
                                             "Acute Bronchitis",
                                             "COPD",
                                             "Pneumonia",
                                             "Upper Respiratory Infection",
                                             "Respiratory Symptoms",
                                             "Bronchitis (not specified)",
                                             "Cardiovascular (all)",
                                             "Ischemic Heart Disease",
                                             "Dysrythmia",
                                             "Chronic Heart Failure",
                                             "Stroke",
                                             "Diseases of the Peripheral Circulation",
                                             "Total Encounters")),
         visit=factor(visit, levels = c("ED" , "Inpatient Hospitalizations", "Doctor's Office and Clinic Visits")),
         age = factor(age, levels= c("Age 0-4","Age 5-17","Age 18+","all")),
         lag = factor(lag, levels= c("5-day","8-day","14-day")), 
         sig = ifelse(U95 < 1 | L95 > 1, "Significant","Not Signficant"))%>%
  filter(Disease %in% c("Respiratory (all)", "Cardiovascular (all)", "Total Encounters")) %>%
  ggplot() + 
  geom_point(aes(x=lag, y=RR,  color=visit, shape=visit, alpha=sig), size=2,position = position_dodge(width = 0.7)) + 
  geom_linerange(aes(x=lag, ymax=as.numeric(U95), ymin=as.numeric(L95), color=visit, alpha=sig), position = position_dodge(width = 0.7)) + 
  geom_hline(yintercept = 1, linetype="dashed") + 
  coord_cartesian(ylim=c(0.5, 2)) + facet_grid(.~ Disease)


+ coord_flip() 



#####################################
######### zip code tables
#####################################

install.packages("scales")
library("scales")

zipPM <- read.csv("./data/zipcodePM.csv", header=T, stringsAsFactors = F)%>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

read.csv("./data/zipcodePM.csv", header=T, stringsAsFactors = F)  %>%
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
  mutate(fiveDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-26"), pm25,""),
         eightDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-29"), pm25,""),
         fourteenDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-11-04"), pm25,"")) %>%
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
  mutate(fiveDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-26"), pm25,"no"),
         eightDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-29"), pm25,"no"),
         fourteenDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-11-04"), pm25,"no")) %>%
  gather(fiveDay, eightDay, fourteenDay, key = lag, value = pm25_period) %>%
  filter(pm25_period != "no") %>%
  group_by(lag) %>%
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
  write.csv("./data/county_pm_stats.csv", row.names = F)


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
  write.csv("./data/daily_pm_stats.csv", row.names = F)

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
    mutate(fiveDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-26"), pm25,"no"),
           eightDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-10-29"), pm25,"no"),
           fourteenDay = ifelse(date >= as.Date("2007-10-22") & date <= as.Date("2007-11-04"), pm25,"no")) %>%
    gather(fiveDay, eightDay, fourteenDay, key = lag, value = pm25_period) %>%
    filter(pm25_period != "no") %>%
    group_by(zipcode, lag) %>%
    summarise(average = mean(as.numeric(pm25_period), na.rm = T)) %>%
    spread(key = lag, value = average)%>%
    write.csv("./data/zip_code_pm_avg_4GIS_2.csv", row.names = F)
