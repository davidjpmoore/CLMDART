#Purpose: To download MODIS MCD43A product and calculate albedo  
#Author: Dave Moore
#Date 3/18/2015
# install.packages("MODISTools")
require(MODISTools)
# Sitename/FLUXNET ID: Harvard Forest / US-Ha1
# Country: USA
# State/Province: Massachusetts
# Sponsor: DOE/NICCR
# Latitude (+N/-S): 42.5378
# Longitude (+E/-W): -72.1715
# Elevation: 340m

#set up lat and long for Harvard Forest - it should be possible to extent this to several other sites
lat =42.5378
long = -72.1715

#set up the subset request
modis.subset =data.frame(lat,long)
modis.subset = data.frame(lat = modis.subset[ ,1], long = modis.subset[ ,2])
#start date of request
modis.subset$start.date <- rep(2002, nrow(modis.subset))
#end date of request
modis.subset$end.date <- rep(2014, nrow(modis.subset))

#ask ORNL what products are available
GetProducts()

#ask ORNL for the bands available for MCD43A1
AvailBandsMCD43A1 = GetBands(Product = "MCD43A1")

AvailBandsMCD43A2 = GetBands(Product = "MCD43A2")
# AvailBandsMCD43A2
# [1] "BRDF_Albedo_Quality"      "BRDF_Albedo_Ancillary"    "BRDF_Albedo_Band_Quality"
# [4] "Snow_BRDF_Albedo" 

#ask ORNL for the dates available for MCD43A1
AvailDatesMCD43A1 =GetDates(Product = "MCD43A1", Lat = modis.subset$lat[1], Long = modis.subset$long[1])

#Coefficients for The Polynomial hk (theta) = g0k + g1ktheta2 + g2ktheta3
#See table 1 of  http://modis.gsfc.nasa.gov/data/atbd/atbd_mod09.pdf
whiteskyInt =   data.frame(1.0, 0.189184, -1.377622)
colnames(whiteskyInt) = c("Isotropic", "RossThick", "LiSparseR")

#data will be directly downloaded to current working directory
#Here I'm requesting the 3 "kernel weights" for each spectral channel - there are 7 channels - and the bands are listed in the first 21 elements of AvailBandsMCD43A1
#this took several minutes from a home internet connection.
MODISSubsets(LoadDat = modis.subset, Products = "MCD43A1",
             Bands = c(paste(AvailBandsMCD43A1[1]),
                       paste(AvailBandsMCD43A1[2]),
                       paste(AvailBandsMCD43A1[3]),
             paste(AvailBandsMCD43A1[4]),
             paste(AvailBandsMCD43A1[5]),
             paste(AvailBandsMCD43A1[6]),
             paste(AvailBandsMCD43A1[7]),
             paste(AvailBandsMCD43A1[8]),
             paste(AvailBandsMCD43A1[9]),
             paste(AvailBandsMCD43A1[10]),
             paste(AvailBandsMCD43A1[11]),
             paste(AvailBandsMCD43A1[12]),
             paste(AvailBandsMCD43A1[13]),
             paste(AvailBandsMCD43A1[14]),
             paste(AvailBandsMCD43A1[15]),
             paste(AvailBandsMCD43A1[16]),
             paste(AvailBandsMCD43A1[17]),
             paste(AvailBandsMCD43A1[18]),
             paste(AvailBandsMCD43A1[19]),
             paste(AvailBandsMCD43A1[20]),
             paste(AvailBandsMCD43A1[21])),
             Size = c(1,1))
# 

             
             
#open data from .asc file
subset.string <- read.csv(list.files(pattern = ".asc")[1],
                          header = FALSE, as.is = TRUE)

#can I use dplyr to re-order the data 
library(dplyr)
library(tidyr)

yearIND = as.numeric(substr(AvailDatesMCD43A1,2,5))
DOYIND = as.numeric(substr(AvailDatesMCD43A1,6,8))
YearDOYIND = as.numeric(substr(AvailDatesMCD43A1,2,8))


Var14 = as.numeric(unlist(subset.string[14]))
summary(Var14)
plot(Var14[Var14<5000])
# 
# save (subset.string, file='MCD43A1_subset.rdata') 

HarvAlbedo <- as.data.frame(lapply(subset.string, function(x){replace(x, x ==32767,NA)}))
Year= as.numeric(substr(HarvAlbedo$V8, 2,5))
DOY = as.numeric (substr(HarvAlbedo$V8, 6,8))


= tapply(FEFACOmonthly$CO21, FEFACOmonthly$DayIndex, min, na.rm = TRUE)


MODISSummaries()

MODISSummaries(LoadDat = modis.subset, Product = "MCD43A1", Bands = paste(AvailBandsMCD43A1[14]),
               ValidRange = c(-2000,10000), NoDataFill = -3000, ScaleFactor = 0.0001,
               StartDate = TRUE, QualityScreen = FALSE
               )


Junk1= read.table("MODIS_Summary_MCD43A1_2015-04-07_h15-m27-s33.csv",header = T,sep = ",")



