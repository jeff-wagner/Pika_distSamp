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
load( file = "_output/WxDbase(R10.01)_singlesids.rda" )

ACISmeta <- meta@data


# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Load in pika site & nearest WS info
pika_sites_WS <- read.csv("../R11_CompileWx_pika/_output/pika_sites_nearestWS_combined.csv")

# Create a common list of weather stations
NRCSmeta <- NRCSmeta %>% 
  select(ntwk, site_name, site_id)
NRCSmeta <- rename(NRCSmeta, source = ntwk)

ACISmeta <- ACISmeta %>% 
  select(source, name, dbID)
ACISmeta <- rename(ACISmeta, site_name = name, site_id = dbID)

WSmeta <- rbind(NRCSmeta, ACISmeta)

# Create list of NRCS & ACIS Wx stations of interest
NRCS_pikaWS <- pika_sites_WS[pika_sites_WS$WS.source 
                             %in% c("SCAN", "SNTL", "SNTLT", "OTHER", "COOP", "USGS", "MSNT"), ]

NRCS_pikaWS$WS.geometry <- sub("c\\(", "", NRCS_pikaWS$WS.geometry)
NRCS_pikaWS$WS.geometry <- sub("\\)", "", NRCS_pikaWS$WS.geometry)
NRCS_pikaWS$Longitude <- sapply(strsplit(NRCS_pikaWS$WS.geometry, ","), "[", 1)
NRCS_pikaWS$Latitude <- sapply(strsplit(NRCS_pikaWS$WS.geometry, ", "), "[", 2)
NRCS_pikaWS <- NRCS_pikaWS %>% 
  select(WSmeta.id, WS.ID, WS.name, WS.source, Latitude, Longitude)

ACIS_pikaWS <- pika_sites_WS[pika_sites_WS$WS.source %in% "ACIS", ]
ACIS_pikaWS <- unique(ACIS_pikaWS)

ACIS_pikaWS$WS.geometry <- sub("c\\(", "", ACIS_pikaWS$WS.geometry)
ACIS_pikaWS$WS.geometry <- sub("\\)", "", ACIS_pikaWS$WS.geometry)
ACIS_pikaWS$Longitude <- sapply(strsplit(ACIS_pikaWS$WS.geometry, ","), "[", 1)
ACIS_pikaWS$Latitude <- sapply(strsplit(ACIS_pikaWS$WS.geometry, ", "), "[", 2)
ACIS_pikaWS <- ACIS_pikaWS %>% 
  select(WSmeta.id, WS.ID, WS.name, WS.source, Latitude, Longitude)
ACIS_pikaWS <- unique(ACIS_pikaWS)

pikaWS <- merge(NRCS_pikaWS, ACIS_pikaWS, all = TRUE)
pikaWS <- unique(pikaWS)


saveRDS(ACIS_pikaWS, file = "../R11_CompileWx_pika/_output/ACIS_pikaWS.rds")
saveRDS(NRCS_pikaWS, file = "../R11_CompileWx_pika/_output/NRCS_pikaWS.rds")
saveRDS(pikaWS, file = "../R11_CompileWx_pika/_output/pikaWS.rds")
