library(ncdf4)
library(tidyverse)

setwd("~/Wildfires/")
# retrieve a list of nc files in my data folder:
flist <- list.files(path = "./data", pattern = "^.*\\.(nc|NC|Nc|Nc)$")

# Open a connection to the first file in our list
nc <- nc_open(paste0("data/", flist[1]))


# Save the print(nc) dump to a text file (same name as the nc file with a txt extension)
# {
#   sink(paste0("data/", flist[1], ".txt"))
#   print(nc)
#   sink()
# }


# Get a list of the NetCDF's R attributes:
attributes(nc)$names

print(paste("The file has",nc$nvars,"variables,",nc$ndims,"dimensions and",nc$natts,"NetCDF attributes"))

# Get a list of the nc variable names.
attributes(nc$var)$names

ncatt_get(nc, attributes(nc$var)$names[1])

# Retrieve a matrix of the chlorophyll data using the ncvar_get function:
hectares_mean <- ncvar_get(nc, attributes(nc$var)$names[1])
dim(hectares_mean)


# Retrieve the latitude and longitude values.
attributes(nc$dim)$names

nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[1])
nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[2])
nc_time <- ncvar_get( nc, attributes(nc$dim)$names[3])

print(paste(dim(nc_lat), "latitudes and", dim(nc_lon), "longitudes"))


# a quick look at a (pseudo) random section of this data
hectares_mean[35:37, 115:137,1:4]
 

# Change the dimension names of our matrix to "lon" and "lat", 
# and the row and column names to the latitude and longitude values.
dimnames(hectares_mean) <- list(lon=nc_lon, lat=nc_lat)
hectares_mean[35:37, 115:137, 1:4]




# Retrieve the global attributes
nc_atts <- ncatt_get(nc, 0)

# List all the attributes (commented to save space).
# names(nc_atts)

# Retrieve the start and end date-times
date_time_start <- as.POSIXct(nc_atts$start_time, format = "%Y%m%dT%H%M%SZ", tz = "UTC")
date_time_end <- as.POSIXct(nc_atts$end_time, format = "%Y%m%dT%H%M%SZ", tz = "UTC")

nc_close(nc)











