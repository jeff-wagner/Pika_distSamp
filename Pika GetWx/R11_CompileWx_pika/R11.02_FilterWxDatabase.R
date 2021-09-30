# This script filters the WX database for only the stations and dates of
# interest for the Collared Pika distance sampling analysis.

# Install Required Packages
# Automatically install required packages if necessary
rqdPkgs <- c('rnoaa','rgeos','rgdal','maptools', 'leaflet','stringdist','mapview','sf','sp','dplyr', 'readr' )     
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


# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Load in pika site & nearest WS info
temp_WS <- read.csv("../R11_CompileWx_pika/_output/pika_sites_nearestWS_combined_Temp.csv")
precip_WS <- read.csv("../R11_CompileWx_pika/_output/pika_sites_nearestWS_combined_Precip.csv")

# Combine these and save for later use
temp_WS_com <- temp_WS %>% 
  mutate(type = "temp")
pcpn_WS_com <- precip_WS %>% 
  mutate(type = "pcpn")
WS <- full_join(temp_WS_com, pcpn_WS_com)
saveRDS(WS, file = "../R11_CompileWx_pika/_output/pikasites_WS.rds")

temp_WS$type <- "temp"
precip_WS$type <- "precip"

# Create common list
pika_sites_WS <- full_join(temp_WS, precip_WS)

# Load common list of weather stations
WSmeta <- read.csv("../R11_CompileWx_pika/_output/WSmeta.csv")

# Create list of NRCS & ACIS Wx stations of interest
NRCS_pikaWS <- pika_sites_WS[pika_sites_WS$WS.source 
                             %in% c("SCAN", "SNTL", "SNTLT", "OTHER", "COOP", "USGS", "MSNT"), ]

NRCS_pikaWS$WS.geometry <- sub("c\\(", "", NRCS_pikaWS$WS.geometry)
NRCS_pikaWS$WS.geometry <- sub("\\)", "", NRCS_pikaWS$WS.geometry)
NRCS_pikaWS$Longitude <- sapply(strsplit(NRCS_pikaWS$WS.geometry, ","), "[", 1)
NRCS_pikaWS$Latitude <- sapply(strsplit(NRCS_pikaWS$WS.geometry, ", "), "[", 2)
NRCS_pikaWS <- NRCS_pikaWS %>% 
  dplyr::select(WSmeta.id, WS.ID, WS.name, WS.source, Latitude, Longitude, type)

ACIS_pikaWS <- pika_sites_WS[pika_sites_WS$WS.source %in% "ACIS", ]
ACIS_pikaWS <- unique(ACIS_pikaWS)

ACIS_pikaWS$WS.geometry <- sub("c\\(", "", ACIS_pikaWS$WS.geometry)
ACIS_pikaWS$WS.geometry <- sub("\\)", "", ACIS_pikaWS$WS.geometry)
ACIS_pikaWS$Longitude <- sapply(strsplit(ACIS_pikaWS$WS.geometry, ","), "[", 1)
ACIS_pikaWS$Latitude <- sapply(strsplit(ACIS_pikaWS$WS.geometry, ", "), "[", 2)
ACIS_pikaWS <- ACIS_pikaWS %>% 
  dplyr::select(WSmeta.id, WS.ID, WS.name, WS.source, Latitude, Longitude, type)
ACIS_pikaWS <- unique(ACIS_pikaWS)

pikaWS <- merge(NRCS_pikaWS, ACIS_pikaWS, all = TRUE)
pikaWS <- unique(pikaWS)


saveRDS(ACIS_pikaWS, file = "../R11_CompileWx_pika/_output/ACIS_pikaWS.rds")
saveRDS(NRCS_pikaWS, file = "../R11_CompileWx_pika/_output/NRCS_pikaWS.rds")
saveRDS(pikaWS, file = "../R11_CompileWx_pika/_output/pikaWS.rds")
