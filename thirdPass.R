library(ncdf4)
library(tidyverse)

setwd("~/Wildfires/")
# retrieve a list of nc files in my data folder:


flist <- list.files(path = "data/", pattern = "^.*\\.(nc|NC|Nc|Nc)$")

#flist <- c(
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp45/CNRM-CM5_45_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp45/CNRM-CM5_45_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp45/CNRM-CM5_45_AA.all.bau.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp85/CNRM-CM5_85_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp85/CNRM-CM5_85_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CNRM-CM5/rcp85/CNRM-CM5_85_AA.all.bau.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp45/CanESM2_45_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp45/CanESM2_45_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp45/CanESM2_45_AA.all.bau.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp85/CanESM2_85_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp85/CanESM2_85_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/CanESM2/rcp85/CanESM2_85_AA.all.bau.mu.nc",  
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp45/HadGEM2-ES_45_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp45/HadGEM2-ES_45_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp45/HadGEM2-ES_45_AA.all.bau.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp85/HadGEM2-ES_85_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp85/HadGEM2-ES_85_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/HadGEM2-ES/rcp85/HadGEM2-ES_85_AA.all.bau.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC5/rcp45/MIROC52_45_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC52/rcp45/MIROC52_45_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC52/rcp45/MIROC52_45_AA.all.bau.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC52/rcp85/MIROC52_85_AA.all.H.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC52/rcp85/MIROC52_85_AA.all.L.mu.nc",
#   "http://albers.cnr.berkeley.edu/data/ucmerced/wildfire/MIROC52/rcp85/MIROC52_85_AA.all.bau.mu.nc"
# )


dname = "hectares"

  # Define our function
process_nc <- function(files){
    # iterate through the nc
    for (i in 1:length(files)){
     
      # download.file(url = flist[1], destfile = "~/tempWF.nc")
      
      # open a conneciton to the ith nc file
      ncin <- nc_open(paste0("data/", flist[i]))
      
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
        mutate(model=unlist(str_split(string = files[i], pattern = "_"))[1],
               scenario = unlist(str_split(string = files[i], pattern = "_"))[2],
               population = unlist(str_split(string = files[i], pattern = "_"))[3]
               ) %>% 
        select(model, scenario, population, lon, lat, 3:150)
        
      names(bar) <- c("model","scenario","population","lon","lat",1954:2100) 
      
      bar <- bar %>%  
        
      
      # set the name of my new variable and bind the new data to it
      if (exists("wildfire")){
        wildfire <- bind_rows(wildfire, bar)
      }else{
        wildfire <- bar
      }
      # tidy up, not sure if necesarry really, but neater
      rm(nc_wildfire, nc_lat, nc_lon, ncin, bar)
    }
    
    return(wildfire)
  }


all_fire <- process_nc(flist)

write.csv(all_fire,"./data/all_WF.csv", row.names=F)

expand.grid(lon = unique(all_fire$lon), lat = unique(all_fire$lat)) %>% write.csv(.,"./data/latlon.csv", row.names = F)


locationData <- read.csv("./data/latlon_ClimateRegions.csv", header=T) %>% 
  select(lon, lat, County, AIR_Name)

names(locationData) <- c("lon","lat","County","ClimateRegion")

CAonly <- inner_join(all_fire, locationData) %>% na.omit() %>%
  filter(County != "") %>%
  gather(6:152, key = "year", value = "hectares") %>% 
  mutate(year = as.integer(as.character(year))) %>% 
           mutate(period = factor(ifelse(year %in% c(1961:1990),"baseline (1961-1990)", 
                                         ifelse(year %in% c(2000:2020), "early (2000-2020)",
                                                ifelse(year %in% c(2040:2060),"mid-century (2040-2060)",
                                                       ifelse(year %in% c(2080:2100), "late-century (2080-2100)", "between")))),
                                         levels = c("baseline (1961-1990)","early (2000-2020)","mid-century (2040-2060)","late-century (2080-2100)","between"))) %>%
  filter(period != "between") %>%
  mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                      ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                             ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                               levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs")),
         
         RCP = factor(ifelse(scenario == "45", "RCP4.5 (emissions peak 2040, stabiliazation by 2100)","emissions continue to rise throughout the 21st century"),
                               levels = c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","emissions continue to rise throughout the 21st century")), 
                      
         PopulationGrowth = factor(ifelse(population == "AA.all.bau.mu.nc","Central Projection",
                                           ifelse(population == "AA.all.L.mu.nc","Low Projection","High Projection")),
                                    levels = c("Low Projection", "Central Projection","High Projection")))
         
                 
                  
CAonly %>% group_by(climateModel, ClimateRegion, period, RCP, PopulationGrowth, lon, lat) %>% 
  summarise(mean_hectares = mean(hectares, na.rm=T)) %>% 
  filter(PopulationGrowth == "Central Projection") %>%
  ggplot(aes(x=lon, y=lat, color=mean_hectares)) + geom_point() + facet_grid(climateModel + RCP ~ period)


CAonly %>% group_by(climateModel, ClimateRegion, period, RCP, PopulationGrowth, ClimateRegion) %>% 
  summarise(mean_hectares = mean(hectares, na.rm=T)) %>% 
  ggplot(aes(x=period, fill=RCP, y=mean_hectares)) + geom_bar(stat="identity", position="dodge") + facet_grid(. ~ climateModel)

CAonly %>% ggplot(aes(x=period, y=hectares, color=climateModel)) + geom_boxplot() + facet_wrap(~ ClimateRegion)
