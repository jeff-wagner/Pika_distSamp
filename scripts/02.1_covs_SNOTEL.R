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
mapviewOptions(basemaps = "CartoDB.Positron")
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

# snotel.download <- snotel_download(site_id = WS.ids, path = paste(getwd(), "./data/SNOTEL"), internal = TRUE)
# summary(snotel.download)

# Manual download from https://wcc.sc.egov.usda.gov/ with Independence Mine included
snotel <- read_excel("./data/snotel_data.xlsx", na = 'NA')
summary(snotel)

# Part 3: Clean up the data   -----------------------------------------------------------------------------
# We are primarily interest in the mean seasonal temperature (summer and winter) from the year preceding survey. We can calculate this using
# daily temperature averages for each weather station.
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

# Create subsets for summer and winter temperature values and years of interest. Create separate objects for the different survey years.
summer.temp.2017 <- mean.temp %>% 
  filter(month %in% c('Jun', 'Jul', "Aug")) %>% 
  filter(year %in% 2017) %>% 
  mutate(survey.year = 2018)

summer.temp.2018 <- mean.temp %>% 
  filter(month %in% c('Jun', 'Jul', "Aug")) %>% 
  filter(year %in% 2018) %>% 
  mutate(survey.year = 2019)

winter.temp.2017 <- mean.temp %>% 
  filter(
    Date > "2017-12-01" & Date < "2018-04-01") %>% 
  mutate(survey.year = 2018)

winter.temp.2018 <- mean.temp %>% 
  filter(
      Date > "2018-12-01" & Date < "2019-04-01") %>% 
  mutate(survey.year = 2019)




# Part 5: Calculate mean seasonal temperature by site   -----------------------------------------------------

summer.mean.2017 <- tapply(summer.temp.2017$tempAvg_degC, summer.temp.2017$Station_Id, mean)
summer.mean.2017

summer.mean.2018 <- tapply(summer.temp.2018$tempAvg_degC, summer.temp.2018$Station_Id, mean)
summer.mean.2018

winter.mean.2017 <- tapply(winter.temp.2017$tempAvg_degC, winter.temp.2017$Station_Id, mean)
winter.mean.2017

winter.mean.2018 <- tapply(winter.temp.2018$tempAvg_degC, winter.temp.2018$Station_Id, mean)
winter.mean.2018

# Convert these to dataframes for easy joining
summer.mean.2017 <- melt(summer.mean.2017)
colnames(summer.mean.2017) <- c("WS.ID", "summerTemp2017")

summer.mean.2018 <- melt(summer.mean.2018)
colnames(summer.mean.2018) <- c("WS.ID", "summerTemp2018")


winter.mean.2017 <- melt(winter.mean.2017)
colnames(winter.mean.2017) <- c("WS.ID", "winterTemp2017")


winter.mean.2018 <- melt(winter.mean.2018)
colnames(winter.mean.2018) <- c("WS.ID", "winterTemp2018")

# Add this into our pika_sites dataframe
snotel.cov <- left_join(pika_sites, summer.mean.2017, by = "WS.ID")
snotel.cov <- left_join(snotel.cov, summer.mean.2018, by = "WS.ID")
snotel.cov <- left_join(snotel.cov, winter.mean.2017, by = "WS.ID")
snotel.cov <- left_join(snotel.cov, winter.mean.2018, by = "WS.ID")

# Only keep the data that pertains to the year preceding survey at each site
snotel.cov$summerTemp <- ifelse(snotel.cov$Year == '2018', snotel.cov$summerTemp2017, NA)
snotel.cov$summerTemp <- ifelse(snotel.cov$Year == '2019', snotel.cov$summerTemp2018, snotel.cov$summerTemp)

snotel.cov$winterTemp <- ifelse(snotel.cov$Year == '2018', snotel.cov$winterTemp2017, NA)
snotel.cov$winterTemp <- ifelse(snotel.cov$Year == '2019', snotel.cov$winterTemp2018, snotel.cov$winterTemp)

snotel.cov <- snotel.cov %>% 
  select(-summerTemp2017, -summerTemp2018, -winterTemp2017, -winterTemp2018)


# # Part 6: Calculate number of days over a threshold by site   -----------------------------------------------------
# # We will use a base of (25ºC), as is the case for American Pika
hotdays2017 <- snotel.tr %>% 
  filter(Date > "2017-01-01" & Date < "2018-01-01") %>%
  filter(tempMax_degC >= 25)

hotdays2018 <- snotel.tr %>% 
  filter(Date > "2018-01-01" & Date < "2019-01-01") %>%
  filter(tempMax_degC >= 25)

# Summarize by site
hotdays2017 <- tapply(hotdays2017$tempAvg_degC, hotdays2017$Station_Id, length)
hotdays2017

hotdays2018 <- tapply(hotdays2018$tempAvg_degC, hotdays2018$Station_Id, length)
hotdays2018

# Add in zeros for missing sites and append to snotel.covs
hotdays2017 <- melt(hotdays2017)
colnames(hotdays2017) <- c("WS.ID", "hotdays2017")

hotdays2018 <- melt(hotdays2018)
colnames(hotdays2018) <- c("WS.ID", "hotdays2018")

WS.ids <- data.frame(WS.ID = unique(pika_sites$WS.ID))

hotdays2017 <- full_join(hotdays2017, WS.ids, by = "WS.ID")
hotdays <- left_join(hotdays2017, hotdays2018, by = "WS.ID")
hotdays[is.na(hotdays)] <- 0

snotel.cov <- left_join(snotel.cov, hotdays, by = "WS.ID")

snotel.cov$TTD <- ifelse(snotel.cov$Year == '2018', snotel.cov$hotdays2017, NA)
snotel.cov$TTD <- ifelse(snotel.cov$Year == '2019', snotel.cov$hotdays2018, snotel.cov$TTD)

snotel.cov <- snotel.cov %>% 
  select(-hotdays2017, -hotdays2018)

# Part 7: Final Cleanup   -----------------------------------------------------------------------------------

#Remove unnecessary object from environment
rm(mean.temp, pika_sites, snotel, snotel.tr, summer.mean, summer.temp, winter.mean,
   winter.temp, WS_info, path, summer.mean.2017, summer.mean.2018, summer.temp.2017, summer.temp.2018, winter.mean.2017,
   winter.mean.2018, winter.temp.2017, winter.temp.2018, hotdays, hotdays2017, hotdays2018)

#rm(snotel.download, snotel.minMax, gdd, i, WS.ids)
