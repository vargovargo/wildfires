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
# Define our function
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
            mean_hectares = mean(hectares, na.rm=T)/80,
            probability = as.numeric(sum(freq)/80))%>%
  as.data.table() %>% setkeyv(c("lat", "lon"))


SanDiegoMaps %>%
  ggplot(aes(x=lon, y=lat, color=probability)) + geom_point(size=3, shape=15) + facet_grid(.~ period)


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
            mean_hectares = sum(hectares, na.rm=T)/80,
            probability = sum(freq)/80)%>%
  as.data.table() %>% setkeyv(c("lat", "lon"))


CAMaps %>%
  ggplot(aes(x=lon, y=lat, color=probability)) + geom_point(size=1, shape=15) + facet_grid(.~ period)

CAMaps %>%
  filter(period == "current") %>%
write.csv("./data/spatial/CAmaps_current.csv", row.names = F)

CAMaps %>%
  filter(period == "future")  %>%
write.csv("./data/spatial/CAmaps_future.csv", row.names = F)




















# bring in Population Projection Data
POPproj <- as.data.table(read.csv("./data/P3_Complete.csv", header=T)) %>%
  mutate(ageCat = ifelse(agerc <=5, "under5",ifelse(agerc >=65,"over65","other"))) %>%
  filter(ageCat != "other") %>%
    mutate(gender = ifelse(sex == 1,"Female","Male"),
           race = factor(ifelse(race7 == 1, "White, Non-Hispanic", 
                         ifelse(race7 == 2, "Black, Non-Hispanic",
                                ifelse(race7 == 3, "American Indian or Alaska Native, Non-Hispanic",
                                       ifelse(race7 == 4,"Asian, Non-Hispanic", 
                                              ifelse(race7 == 5, "Native Hawaiian or Pacific Islander, Non-Hispanic",
                                                     ifelse(race7 == 6,"Multiracial (two or more of above races), Non-Hispanic","Hispanic (any race)")))))), 
           levels = c("White, Non-Hispanic", 
                      "Black, Non-Hispanic", 
                      "American Indian or Alaska Native, Non-Hispanic", 
                      "Asian, Non-Hispanic", 
                      "Native Hawaiian or Pacific Islander, Non-Hispanic",
                      "Multiracial (two or more of above races), Non-Hispanic",
                      "Hispanic (any race)")), 
           stcoFIPS = as.numeric(as.character(fips)),
           decade = factor(ifelse(year %in% c(2010:2019),"2010s",
                                  ifelse(year %in% c(2020:2029), "2020s",
                                         ifelse(year %in% c(2030:2039),"2030s",
                                                ifelse(year %in% c(2040:2049), "2040s","2050s")))),
                           levels = c("2010s","2020s","2030s","2040s","2050s"))) %>%
  group_by(ageCat, year, stcoFIPS, decade) %>%
    summarise(people = sum(perwt, na.rm=T)) %>%
  group_by(ageCat, stcoFIPS, decade) %>%
  summarise(people = mean(people, na.rm=T)) %>%
  as.data.table() %>% setkeyv("stcoFIPS")



write.csv(POPproj, "./data/StatePOPprojections.csv", row.names = F)


POPproj %>%
  ggplot(aes(x=stcoFIPS, y=people, fill=decade)) + geom_bar(stat="identity",position="dodge") + facet_grid(.~ ageCat)+ coord_flip()

