rm(list = ls())

library(ncdf4)
library(tidyverse)
library(data.table)

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


#####################################
# Define our function
#####################################
dname = "hectares"

process_nc <- function(files){
    # iterate through the nc
    for (i in 1:length(files)){
     
      # download.file(url = flist[1], destfile = "~/tempWF.nc")
      
      # open a conneciton to the ith nc file
      ncin <- nc_open(paste0("data/", files[i]))
      
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
               population = unlist(str_split(string = files[i], pattern = "_"))[3]) %>% 
        select(model, scenario, population, lon, lat, 3:150)
        
      names(bar) <- c("model","scenario","population","lon","lat",1954:2100) 
    
      
      # set the name of my new variable and bind the new data to it
      if(exists("wildfire")){
        wildfire <- bind_rows(wildfire, bar)
      }else{
        wildfire <- bar
      }
      # tidy up, not sure if necesarry really, but neater
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

# process combined WildFire Data 
CAannual <- all_fire_dt[locationData] %>% na.omit() %>%
  filter(ClimateRegion == "South Coast") %>%
  gather(6:152, key = "year", value = "hectares") %>% 
  mutate(year = as.integer(as.character(year))) %>% 
  mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                      ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                             ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                               levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs")),
         
         RCP = factor(ifelse(scenario == "45", "RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)"),
                      levels = c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")), 
         
         PopulationGrowth = factor(ifelse(population == "AA.all.bau.mu.nc","Central Projection",
                                          ifelse(population == "AA.all.L.mu.nc","Low Projection","High Projection")),
                                   levels = c("Low Projection", "Central Projection","High Projection")))  %>% 
  group_by(climateModel,stcoFIPS, County, ClimateRegion, year, RCP, PopulationGrowth) %>% 
  summarise(total_hectares = sum(hectares, na.rm=T)) %>%
  as.data.table() %>% setkeyv(c("year","stcoFIPS"))



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
           stcoFIPS = as.numeric(as.character(fips))) %>%
  group_by(ageCat, year, stcoFIPS) %>%
    summarise(people = sum(perwt, na.rm=T)) %>%
  as.data.table() %>% setkeyv(c("year","stcoFIPS"))
  

fullTab <- merge(CAannual, POPproj, allow.cartesian = T) %>% 
  mutate(decade = factor(ifelse(year %in% c(2010:2019),"2010s",
                                ifelse(year %in% c(2020:2029), "2020s",
                                        ifelse(year %in% c(2030:2039),"2030s",
                                               ifelse(year %in% c(2040:2049), "2040s","2050s")))),
         levels = c("2010s","2020s","2030s","2040s","2050s"))) %>%
  group_by(County, climateModel, ClimateRegion, RCP, race, gender, ageCat, PopulationGrowth, decade) %>%
  summarise(Ha = mean(total_hectares, na.rm=T),
            population = mean(people, na.rm=T)) 


fullTab %>%
  ggplot(aes(x=Ha, y=population, color=decade)) + geom_point() + facet_grid(ageCat ~ race, scales = "free_y")

 
###########################
# Statewide plot
###########################
CAstate <- all_fire_dt[locationData] %>% na.omit() %>%
  filter(County != "") %>%
  gather(6:152, key = "year", value = "hectares") %>% 
  mutate(year = as.integer(as.character(year)))  %>%
  mutate(climateModel = factor(ifelse(model == "CanESM2","CanESM2 (average)",
                                      ifelse(model == "CNRM-CM5","CNRM-CM5 (Cool/Wet)",
                                             ifelse(model == "HadGEM2-ES","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs"))),
                               levels = c("CanESM2 (average)", "CNRM-CM5 (Cool/Wet)","HadGEM2-ES (Warm/Dry)","MIROC5 (Complement/Covers a range of outputs")),
         
         RCP = factor(ifelse(scenario == "45", "RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)"),
                      levels = c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")), 
         
         PopulationGrowth = factor(ifelse(population == "AA.all.bau.mu.nc","Central Projection",
                                          ifelse(population == "AA.all.L.mu.nc","Low Projection","High Projection")),
                                   levels = c("Low Projection", "Central Projection","High Projection")))  %>%
  group_by(year, climateModel, RCP, PopulationGrowth)%>%
  summarise(Ha = sum(hectares, na.rm=T)) %>% as.data.table() %>% setkeyv(c("year","climateModel","RCP","PopulationGrowth"))

CAstate %>%
  filter(PopulationGrowth == "Central Projection") %>%
  ggplot(aes(x=year, y=Ha, linetype=RCP)) + geom_line() +facet_grid(climateModel ~. )


  
# random plots
CAperiods <- all_fire_dt[locationData] %>% na.omit() %>%
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
           
           RCP = factor(ifelse(scenario == "45", "RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)"),
                        levels = c("RCP4.5 (emissions peak 2040, stabiliazation by 2100)","RCP8.5 (emissions continue to rise throughout the 21st century)")), 
           
           PopulationGrowth = factor(ifelse(population == "AA.all.bau.mu.nc","Central Projection",
                                            ifelse(population == "AA.all.L.mu.nc","Low Projection","High Projection")),
                                     levels = c("Low Projection", "Central Projection","High Projection")))
  
midcentury <- CAperiods %>% 
  filter(PopulationGrowth == "Central Projection") %>%
  group_by(climateModel,County, stcoFIPS, ClimateRegion, period, RCP, PopulationGrowth) %>% 
    summarise(mean_hectares = mean(hectares, na.rm=T))%>%
    spread(key = period, value = mean_hectares) %>%
    gather(`early (2000-2020)`,`mid-century (2040-2060)`,`late-century (2080-2100)`, key = futurePeriod, value = futureHa) %>%
    mutate(pct_change = 100*(futureHa -`baseline (1961-1990)`)/`baseline (1961-1990)`, 
           futurePeriod = factor(futurePeriod,
                           levels = c("early (2000-2020)","mid-century (2040-2060)","late-century (2080-2100)")))

# box plots of change by period
midcentury %>%
  ggplot(aes(x=ClimateRegion, y=pct_change, fill=futurePeriod)) + geom_boxplot() + facet_grid(climateModel ~ RCP) 


POPchange <- POPproj %>% spread(key = year, value = people) %>%
  mutate(pop_pct_change = 100*(`2060`-`2010`)/`2010`) %>% 
  select(ageCat, stcoFIPS, `2010`, `2060`, pop_pct_change)

midcentury %>% full_join(POPchange) %>% 
  filter(PopulationGrowth =="Central Projection", futurePeriod =="mid-century (2040-2060)" ) %>% 
 ggplot(aes(x=pct_change, y=pop_pct_change, color=RCP))+ geom_point() + facet_grid( ageCat~ climateModel )
# write this to csv to create tableau viz
# https://public.tableau.com/views/Wildfires_w_Popchange/Dashboard1?:embed=y&:display_count=yes&publish=yes

# plots of the entire state                 
CAperiods %>% group_by(climateModel, ClimateRegion, period, RCP, PopulationGrowth, lon, lat) %>% 
  summarise(mean_hectares = mean(hectares, na.rm=T)) %>% 
  filter(PopulationGrowth == "Central Projection") %>%
  ggplot(aes(x=lon, y=lat, color=mean_hectares)) + geom_point() + facet_grid(climateModel + RCP ~ period)




