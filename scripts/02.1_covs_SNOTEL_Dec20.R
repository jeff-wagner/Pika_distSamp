# Collared pika abundance surveys in Alaska.
# Data collected July - Sep 2018 and July - Aug 2019
# Author: J. Wagner, P. Schuette
# Last updated: 16 Nov 2020

###############################################################-#

###           Step 2.1: SNOTEL Temperature Data               ###

###############################################################-#

# This script locates the nearest weather station to each survey site, imports daily temperature data from
# SNOTEL, and calculates the mean summer and winter temperature at each site.

# Read in initial script - This script sets the working directory, loads all of our required libraries, and
# defines the path to our data.
source("scripts/initscript.r")

# Part 1: Load in AK SNOTEL & pika survey site info  --------------------------------------------------------
WS_info <- snotel_info()
WS_info <- WS_info %>%
  filter(state == "AK")

# Load in pika site info
pika_sites <- read.csv("./data/pika_site_occupation.csv")

# Convert to spatial objects
WS_info <- st_as_sf(WS_info, coords = c("longitude", "latitude"))
st_crs(WS_info) <- 4326 #Set coordinate system
st_crs(WS_info)

pika_sites <- st_as_sf(pika_sites, coords = c("longitude", "latitude"))
st_crs(pika_sites) <- 4326

# # Map the pika sites and weather stations
mapviewGetOption("basemaps")
mapviewOptions(basemaps = "CartoDB.Positron", fgb = FALSE)
mapview(pika_sites, col.regions = "blue") + mapview(WS_info, col.regions = "green")

# #Find nearest weather station for each pika site 
pika_sites <- pika_sites %>%
              group_by(Site) %>%
              mutate(WS_info.id = st_nearest_feature(geometry, WS_info),
                    dist.WS = as.numeric(st_distance(geometry, WS_info[WS_info.id,])),
                    WS.ID = WS_info$site_id[match(WS_info.id, row.names(WS_info))],
                    WS.name = WS_info$site_name[match(WS_info.id, row.names(WS_info))])

# Part 2: Download SNOTEL weather data  ------------------------------------------------------------------
WS.ids <- unique(pika_sites$WS.ID)

# There is a problem with Independence Mine (ID 1091), so we will remove it for now
WS.ids <- WS.ids[-3]

snotel.download <- snotel_download(site_id = WS.ids, path = paste(getwd(), "./data/SNOTEL"), internal = TRUE)
summary(snotel.download)

# Manual download from https://wcc.sc.egov.usda.gov/ with Independence Mine included
snotel <- read_excel("./data/snotel_data.xlsx", na = 'NA')
summary(snotel)

# Part 3: Clean up the data   -----------------------------------------------------------------------------
# We are primarily interest in the mean seasonal temperature (summer and winter). We can calculate this using
# daily temperature averages across the period of record for each weather station.
hist(snotel$tempAvg_degC)

# Truncate to exclude unrealistic values (e.g. errors in the data)
snotel.tr <- snotel %>% 
  filter(tempAvg_degC < 38 & tempAvg_degC > -60) # These are more realistic values across our study area
hist(snotel.tr$tempAvg_degC)
summary(snotel.tr$tempAvg_degC) # Looks good

# Select columns of interest for mean seasonal temperature calculation
mean.temp <- snotel.tr %>% 
  select(-State, -Network_Code, -End_Date, -tempMin_degC, -tempMax_degC)

# Part 4: Subset temperature data by season   --------------------------------------------------------------
# We want to calculate mean temperature across seasons for as many years of data that are available. To do this,
# we will need to diregard year in the calculations for mean summer temperature (Jun-Aug) and mean winter
# temperature (Dec-Mar).

# Separate the date into individual columns
mean.temp$month <- months(mean.temp$Date, abbreviate = TRUE)
mean.temp$day <- day(mean.temp$Date)
mean.temp$year <- year(mean.temp$Date)

# Create subsets for summer and winter temperature values
summer.temp <- mean.temp %>% 
  filter(month %in% c('Jun', 'Jul', "Aug"))

winter.temp <- mean.temp %>% 
  filter(month %in% c('Dec', 'Jan', 'Feb', 'Mar'))

# Part 5: Calculate mean seasonal temperature by site   -----------------------------------------------------

summer.mean <- tapply(summer.temp$tempAvg_degC, summer.temp$Station_Id, mean)
summer.mean

winter.mean <- tapply(winter.temp$tempAvg_degC, winter.temp$Station_Id, mean)
winter.mean

# Convert these to dataframes for easy joining
summer.mean <- melt(summer.mean)
colnames(summer.mean) <- c("WS.ID", "summerTemp")

winter.mean <- melt(winter.mean)
colnames(winter.mean) <- c("WS.ID", "winterTemp")

# Add this into our pika_sites dataframe
snotel.cov <- left_join(pika_sites, summer.mean, by = "WS.ID")
snotel.cov <- left_join(snotel.cov, winter.mean, by = "WS.ID")

# # Part 6: Calculate growing degree days (GDD) by site   -----------------------------------------------------
# # Growing degree days are calculated as (Daily Max Temp - Daily Min Temp)/2 - Base Temp
# # We will use a base of 50ºF (10ºC), as is standard practice for Alaska
# 
# # First, check and truncate min and max temperature values
# hist(snotel.tr$tempMin_degC)
# 
# snotel.minMax <- snotel.tr %>% 
#   filter(tempMin_degC < 38 & tempMin_degC > -60)
# 
# summary(snotel.minMax$tempMin_degC)
# hist(snotel.minMax$tempMin_degC)
# 
# 
# hist(snotel.tr$tempMax_degC)
# 
# snotel.minMax <- snotel.minMax %>% 
#   filter(tempMax_degC < 38 & tempMax_degC > -60)
# 
# summary(snotel.minMax$tempMax_degC)
# hist(snotel.minMax$tempMax_degC)
# 
# # Now calculate growing degree days with the pollen package
# library(pollen)
# gdd(tmax = snotel.minMax$tempMax_degC, tmin = snotel.minMax$tempMin_degC, tbase = 10, type = "B")
# 
# snotel.minMax$gdd <- NA
# 
# for(i in 1:length(snotel.minMax$Date)){
#   gdd <- ((snotel.minMax$tempMax_degC[i] - snotel.minMax$tempMin_degC[i])/2) - 10
#   snotel.minMax$gdd[i] <- gdd
# }
# 
# # Make negative values 0s
# snotel.minMax$gdd <- ifelse(snotel.minMax$gdd < 0, 0, snotel.minMax$gdd)
# 
# summary(snotel.minMax$gdd)

# Part 7: Final Cleanup   -----------------------------------------------------------------------------------

#Remove unnecessary object from environment
rm(mean.temp, pika_sites, snotel, snotel.tr, summer.mean, summer.temp, winter.mean,
   winter.temp, WS_info, path)

#rm(snotel.download, snotel.minMax, gdd, i, WS.ids)
