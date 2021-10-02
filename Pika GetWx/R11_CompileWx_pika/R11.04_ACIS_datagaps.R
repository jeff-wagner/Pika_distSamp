# This script identifies and downloads data for seasons with weather data gaps.

# Data needs:
# 
# Temperature: 
#   - Summer 2018: Replacement for ACIS:83041 - Denali Visitors Center
#   - Winter 2017: Replacement for ACIS:79408 - Glennallen 64N
#   - Winter 2018: Replacement for ACIS:83041 - Denali Visitors Center
# 
# Precipitation:
#   - Winter 2017: Replacement for ACIS:79398 - Denali 27 N
#                  Replacement for ACIS:79408 - Glenallen 64N

# Projections strings
WGS84.prjinfo <- "+proj=longlat +datum=WGS84"

# working projection string for Alaska
projNAD83 <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


# Install Required Packages
# Automattically install required packages if necessary
rqdPkgs <- c('rnoaa','rgeos','rgdal','maptools', 'leaflet','stringdist','mapview','sf','sp','dplyr', 'readr', 'stringr' )     
a <- which( !rqdPkgs %in% installed.packages()[,1])
if ( length( a ) > 0 ){
  install.packages( rqdPkgs[ a ] )
}   

# Load required packages  
sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )

# set working directory  
setwd( "C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/Pika GetWx/R10_CompileWx" )


# Load base database (ACIS only) with weather data
load( file = "_output/WxDbase(R10.01).rda" )

ACISmeta <- meta@data


# Temperature Gaps --------------------------------------------------------

## Summer 2018 ##

# Select variables and filter for dates of interest
wx <- wx %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-pcpn) %>% 
  filter(date >= "2018-06-01" & date <= "2018-08-31")

# Recode missing values "M" as NAs
m.replace <- function(x) x = ifelse(x=="M", NA, x)
wx <- wx %>% 
  mutate_at(vars(maxt, avgt), m.replace) %>% 
  mutate(maxt = as.numeric(maxt), avgt = as.numeric(avgt))

uids <- unique(wx$uid)

df <- data.frame(uid = uids, result = NA)

i <- 1
for(i in 1:length(uids)){
  a <- subset(wx, uid == uids[i])
  days <- length(unique(a$date))
  nas <- sum(is.na(a$avgt))
  if(nas > days/2){
    df[i, "result"] <- "toss"
  }else{
    df[i, "result"] <- "keep"
  } 
}

wx <- left_join(wx, df, by = "uid")
wx <- wx %>% 
  filter(result == "keep")

summary(wx)

# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Filter out USGS streamflow stations
NRCSmeta <- NRCSmeta %>% 
  filter(ntwk %in% c("SNTL", "SCAN", "SNTLT", "COOP"))

# Get rid of ACIS sids column
ACISmeta <- ACISmeta %>% 
  dplyr::select(-sids)

# Load in pika site info
pika_sites <- read.csv("_data/pika_sites.csv")

# Convert to spatial objects
NRCSmeta <- st_as_sf(NRCSmeta, coords = c("longitude", "latitude"))
st_crs(NRCSmeta) <- 4326

pika_sites <- st_as_sf(pika_sites, coords = c("longitude", "Latitude"))
st_crs(pika_sites) <- 4326

ACISmeta <- st_as_sf(ACISmeta, coords = c("long", "lat"))
st_crs(ACISmeta) <- 4326

# # Map the pika sites and weather stations
mapviewOptions(basemaps = "Esri.WorldGrayCanvas", native.crs = TRUE)
mapview(pika_sites, col.regions = "blue") + mapview(NRCSmeta, col.regions = "green") + mapview(ACISmeta, col.regions = "red")

# Create a common list of weather stations
NRCSmeta <- NRCSmeta %>% 
  dplyr::select(ntwk, site_name, site_id, geometry)
NRCSmeta <- rename(NRCSmeta, source = ntwk)

# Filter for only stations we want to keep
uids <- unique(wx$uid)
ACISmeta <- ACISmeta %>% 
  dplyr::select(source, name, uid, geometry) %>% 
  filter(uid %in% uids)
ACISmeta <- rename(ACISmeta, site_name = name, site_id = uid)

WSmeta <- rbind(NRCSmeta, ACISmeta)

# RUNNING LIST OF STATIONS TO FILTER OUT BASED ON DATA AVAILABILITY --------
# ACIS:83041 - Denali Visitors Center

WSmeta <- WSmeta %>%
  filter(!site_id %in% c("83041"))

# Find the nearest weather station for each pika site
pika_sites <- pika_sites %>%
  group_by(Site) %>%
  mutate(WSmeta.id = st_nearest_feature(geometry, WSmeta),
         dist.WS = as.numeric(st_distance(geometry, WSmeta[WSmeta.id,])),
         WS.ID = WSmeta$site_id[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.name = WSmeta$site_name[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.source = WSmeta$source[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.geometry = WSmeta$geometry[match(WSmeta.id, 1:nrow(WSmeta))])

# Looking for site P619
# Next nearest sites with data is ACIS:83140 - Wigand Alaska; save this in a new df
summer2018_temp.sites <- as.data.frame(pika_sites[pika_sites$WS.ID %in% c("83140"),]) %>% 
  filter(Year == 2019)
summer2018_temp.sites$season <- "summer"
summer2018_temp.sites$year <- 2018
summer2018_temp.sites$type <- "temp"

## Winter 2017 ##
# Load base database (ACIS only) with weather data
load( file = "_output/WxDbase(R10.01).rda" )

ACISmeta <- meta@data

# Select variables and filter for dates of interest
wx <- wx %>%
  mutate(date = as.Date(date)) %>%
  dplyr::select(-pcpn) %>%
  filter(date >= "2017-12-01" & date <= "2018-03-31")

# Recode missing values "M" as NAs
m.replace <- function(x) x = ifelse(x=="M", NA, x)
wx <- wx %>%
  mutate_at(vars(maxt, avgt), m.replace) %>%
  mutate(maxt = as.numeric(maxt), avgt = as.numeric(avgt))

uids <- unique(wx$uid)

df <- data.frame(uid = uids, result = NA)

i <- 1
for(i in 1:length(uids)){
  a <- subset(wx, uid == uids[i])
  days <- length(unique(a$date))
  nas <- sum(is.na(a$avgt))
  if(nas > days/2){
    df[i, "result"] <- "toss"
  }else{
    df[i, "result"] <- "keep"
  }
}

wx <- left_join(wx, df, by = "uid")
wx <- wx %>%
  filter(result == "keep")

summary(wx)

# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Filter out USGS streamflow stations
NRCSmeta <- NRCSmeta %>%
  filter(ntwk %in% c("SNTL", "SCAN", "SNTLT", "COOP"))

# Get rid of ACIS sids column
ACISmeta <- ACISmeta %>%
  dplyr::select(-sids)

# Load in pika site info
pika_sites <- read.csv("_data/pika_sites.csv")

# Convert to spatial objects
NRCSmeta <- st_as_sf(NRCSmeta, coords = c("longitude", "latitude"))
st_crs(NRCSmeta) <- 4326

pika_sites <- st_as_sf(pika_sites, coords = c("longitude", "Latitude"))
st_crs(pika_sites) <- 4326

ACISmeta <- st_as_sf(ACISmeta, coords = c("long", "lat"))
st_crs(ACISmeta) <- 4326

# Create a common list of weather stations
NRCSmeta <- NRCSmeta %>%
  dplyr::select(ntwk, site_name, site_id, geometry)
NRCSmeta <- rename(NRCSmeta, source = ntwk)

# Filter for only stations we want to keep
uids <- unique(wx$uid)
ACISmeta <- ACISmeta %>%
  dplyr::select(source, name, uid, geometry) %>%
  filter(uid %in% uids)
ACISmeta <- rename(ACISmeta, site_name = name, site_id = uid)

WSmeta <- rbind(NRCSmeta, ACISmeta)

# RUNNING LIST OF STATIONS TO FILTER OUT BASED ON DATA AVAILABILITY --------
# ACIS:79408 - Glennallen 64N

WSmeta <- WSmeta %>%
  filter(!site_id %in% c("79408"))

# Find the nearest weather station for each pika site
pika_sites <- pika_sites %>%
  group_by(Site) %>%
  mutate(WSmeta.id = st_nearest_feature(geometry, WSmeta),
         dist.WS = as.numeric(st_distance(geometry, WSmeta[WSmeta.id,])),
         WS.ID = WSmeta$site_id[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.name = WSmeta$site_name[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.source = WSmeta$source[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.geometry = WSmeta$geometry[match(WSmeta.id, 1:nrow(WSmeta))])

# Looking for sites P100 & P618
# Next nearest sites with data are SNTL:1268 - Fielding Lake (P100) and
# ACIS:65770 - Paxson Alaska (P618); save this in a new df
winter2017_temp.sites <- as.data.frame(pika_sites[pika_sites$WS.ID == "65770" | pika_sites$Site == "P100",]) %>% 
  filter(Year == 2018)
winter2017_temp.sites$season <- "winter"
winter2017_temp.sites$year <- 2017
winter2017_temp.sites$type <- "temp"

## Winter 2018 ##
# Load base database (ACIS only) with weather data
load( file = "_output/WxDbase(R10.01).rda" )

ACISmeta <- meta@data

# Select variables and filter for dates of interest
wx <- wx %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-pcpn) %>% 
  filter(date >= "2018-12-01" & date <= "2019-03-31")

# Recode missing values "M" as NAs
m.replace <- function(x) x = ifelse(x=="M", NA, x)
wx <- wx %>% 
  mutate_at(vars(maxt, avgt), m.replace) %>% 
  mutate(maxt = as.numeric(maxt), avgt = as.numeric(avgt))

uids <- unique(wx$uid)

df <- data.frame(uid = uids, result = NA)

i <- 1
for(i in 1:length(uids)){
  a <- subset(wx, uid == uids[i])
  days <- length(unique(a$date))
  nas <- sum(is.na(a$avgt))
  if(nas > days/2){
    df[i, "result"] <- "toss"
  }else{
    df[i, "result"] <- "keep"
  } 
}

wx <- left_join(wx, df, by = "uid")
wx <- wx %>% 
  filter(result == "keep")

summary(wx)

# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Filter out USGS streamflow stations
NRCSmeta <- NRCSmeta %>% 
  filter(ntwk %in% c("SNTL", "SCAN", "SNTLT", "COOP"))

# Get rid of ACIS sids column
ACISmeta <- ACISmeta %>% 
  dplyr::select(-sids)

# Load in pika site info
pika_sites <- read.csv("_data/pika_sites.csv")

# Convert to spatial objects
NRCSmeta <- st_as_sf(NRCSmeta, coords = c("longitude", "latitude"))
st_crs(NRCSmeta) <- 4326

pika_sites <- st_as_sf(pika_sites, coords = c("longitude", "Latitude"))
st_crs(pika_sites) <- 4326

ACISmeta <- st_as_sf(ACISmeta, coords = c("long", "lat"))
st_crs(ACISmeta) <- 4326

# Create a common list of weather stations
NRCSmeta <- NRCSmeta %>% 
  dplyr::select(ntwk, site_name, site_id, geometry)
NRCSmeta <- rename(NRCSmeta, source = ntwk)

# Filter for only stations we want to keep
uids <- unique(wx$uid)
ACISmeta <- ACISmeta %>% 
  dplyr::select(source, name, uid, geometry) %>% 
  filter(uid %in% uids)
ACISmeta <- rename(ACISmeta, site_name = name, site_id = uid)

WSmeta <- rbind(NRCSmeta, ACISmeta)

# RUNNING LIST OF STATIONS TO FILTER OUT BASED ON DATA AVAILABILITY --------
# ACIS:83041 - Denali Visitors Center

WSmeta <- WSmeta %>%
  filter(!site_id %in% c("83041"))

# Find the nearest weather station for each pika site
pika_sites <- pika_sites %>%
  group_by(Site) %>%
  mutate(WSmeta.id = st_nearest_feature(geometry, WSmeta),
         dist.WS = as.numeric(st_distance(geometry, WSmeta[WSmeta.id,])),
         WS.ID = WSmeta$site_id[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.name = WSmeta$site_name[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.source = WSmeta$source[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.geometry = WSmeta$geometry[match(WSmeta.id, 1:nrow(WSmeta))])

# Looking for site P619 
# Next nearest site with data is ACIS:83140  - Wigand Alaska, save this in a new df
winter2018_temp.sites <- as.data.frame(pika_sites[pika_sites$WS.ID == "83140",]) %>% 
  filter(Year == 2019)
winter2018_temp.sites$season <- "winter"
winter2018_temp.sites$year <- 2018
winter2018_temp.sites$type <- "temp"

# Precipitation Gaps ------------------------------------------------------

## Winter 2017 ##
# Load base database (ACIS only) with weather data
load( file = "_output/WxDbase(R10.01).rda" )

ACISmeta <- meta@data

# Select variables and filter for dates of interest
wx <- wx %>% 
  mutate(date = as.Date(date)) %>% 
  dplyr::select(-avgt, -maxt) %>% 
  filter(date >= "2017-12-01" & date <= "2018-03-31")

# Recode missing values "M" as NAs
m.replace <- function(x) x = ifelse(x=="M", NA, x)
wx <- wx %>% 
  mutate_at(vars(pcpn), m.replace)
# There are some issues with pcpn (text in strings) that we have to fix before converting to numeric
unique(wx$pcpn)
wx$pcpn <- ifelse(wx$pcpn == "S", NA, wx$pcpn)
wx$pcpn <- as.numeric(gsub("[^0-9.-]", "", wx$pcpn))

uids <- unique(wx$uid)

df <- data.frame(uid = uids, result = NA)

for(i in 1:length(uids)){
  a <- subset(wx, uid == uids[i])
  days <- length(unique(a$date))
  nas <- sum(is.na(a$pcpn))
  if(nas > days/2){
    df[i, "result"] <- "toss"
  }else{
    df[i, "result"] <- "keep"
  } 
}

wx <- left_join(wx, df, by = "uid")
wx <- wx %>% 
  filter(result == "keep")

summary(wx)

# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Filter out USGS streamflow stations
NRCSmeta <- NRCSmeta %>% 
  filter(ntwk %in% c("SNTL", "SCAN", "SNTLT", "COOP"))

# Get rid of ACIS sids column
ACISmeta <- ACISmeta %>% 
  dplyr::select(-sids)

# Load in pika site info
pika_sites <- read.csv("_data/pika_sites.csv")

# Convert to spatial objects
NRCSmeta <- st_as_sf(NRCSmeta, coords = c("longitude", "latitude"))
st_crs(NRCSmeta) <- 4326

pika_sites <- st_as_sf(pika_sites, coords = c("longitude", "Latitude"))
st_crs(pika_sites) <- 4326

ACISmeta <- st_as_sf(ACISmeta, coords = c("long", "lat"))
st_crs(ACISmeta) <- 4326

# Create a common list of weather stations
NRCSmeta <- NRCSmeta %>% 
  dplyr::select(ntwk, site_name, site_id, geometry)
NRCSmeta <- rename(NRCSmeta, source = ntwk)

# Filter for only stations we want to keep
uids <- unique(wx$uid)
ACISmeta <- ACISmeta %>% 
  dplyr::select(source, name, uid, geometry) %>% 
  filter(uid %in% uids)
ACISmeta <- rename(ACISmeta, site_name = name, site_id = uid)

WSmeta <- rbind(NRCSmeta, ACISmeta)

# RUNNING LIST OF STATIONS TO FILTER OUT BASED ON DATA AVAILABILITY --------
# ACIS:79398 - Denali 27 N
# ACIS:79408 - Glenallen 64N
WSmeta <- WSmeta %>%
  filter(!site_id %in% c("79398", "79408"))

# Find the nearest weather station for each pika site
pika_sites <- pika_sites %>%
  group_by(Site) %>%
  mutate(WSmeta.id = st_nearest_feature(geometry, WSmeta),
         dist.WS = as.numeric(st_distance(geometry, WSmeta[WSmeta.id,])),
         WS.ID = WSmeta$site_id[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.name = WSmeta$site_name[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.source = WSmeta$source[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.geometry = WSmeta$geometry[match(WSmeta.id, 1:nrow(WSmeta))])

# Looking for sites MV, P100, P146A, P618 & Polychrome
# Next nearest sites with data are SNTL:1072 - Kantishna (MV, P146A & Polychrome), 
# SNTL:1268 - Fielding Lake (P100 & P618); safe as new df
winter2017_pcpn.sites <- as.data.frame(pika_sites[pika_sites$Site %in% 
                                                    c("MV", "P100", "P146A", "P618", "Polychrome"),]) %>% 
  filter(Year == 2018)
winter2017_pcpn.sites$season <- "winter"
winter2017_pcpn.sites$year <- 2017
winter2017_pcpn.sites$type <- "pcpn"

# Combine new found stations into a single df for download ----------------
datagap.WS <- full_join(summer2018_temp.sites, winter2017_temp.sites) %>% 
  full_join(winter2018_temp.sites) %>% 
  full_join(winter2017_pcpn.sites)
datagap.WS <- datagap.WS %>%
  mutate(Latitude = unlist(map(datagap.WS$WS.geometry,1)),
         Longitude = unlist(map(datagap.WS$WS.geometry,2)))

# Save dataframe for reference when plugging in substitute wx data
saveRDS(datagap.WS, file = "../R11_CompileWx_pika/_output/datagap.WS.rds")

ACIS_datagap.WS <- datagap.WS %>%
  filter(WS.source == "ACIS") %>% 
  select(WSmeta.id, WS.ID, WS.name, WS.source, Latitude, Longitude, type)
saveRDS(ACIS_datagap.WS, file = "../R11_CompileWx_pika/_output/ACIS_datagap.WS.rds")


