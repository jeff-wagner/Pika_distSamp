# This script finds NOAA weather observations in the vicinity 
#   of locations provided by Tom Paragi for IM evaluation

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

# FUNCTION: Apply stringsim function to each item of MatchLs list
MatchList <- function( MatchStr, MatchLs, Qual = 0.95 ){
  strdist <- stringsim( MatchStr, MatchLs, method = "jw" )
  mtch <- which( strdist >= Qual )
  return( list( idx = mtch,  qual = strdist[ mtch ] ) )
}


# Load base database (ACIS only) with weather data
load( file = "_output/WxDbase(R10.01).rda" )

ACISmeta <- meta@data


# Load NRCS weather data
load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
nrow( NRCSwx )
NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
nrow( NRCSwx )
NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )

# Filter out USGS streamflow stations
NRCSmeta <- NRCSmeta %>% 
  filter(ntwk %in% c("SNTL", "SCAN", "SNTLT", "COOP"))


# RUNNING LIST OF STATIONS TO FILTER OUT BASED ON DATA AVAILABILITY -------
# No Precip: SNTLT:1279 - Nicks Valley
# No Data: SNTL:641 - Frostbite Bottom
# No Data: SNTLT:768 - Look Eyrie
# No Precip: SNTL:1268 - Fielding Lake
NRCSmeta <- NRCSmeta %>% 
  filter(!site_id %in% c("SNTLT:1279", "SNTL:641", "SNTLT:768", "SNTL:1268"))

# Load in pika site info
pika_sites <- read.csv("_data/pika_sites.csv")


# Subset ACIS sid column for first item in each element -------------------
# We will need this to match with the ACIS wx data later
sids <- sapply(ACISmeta$sids, "[[", 1)
sids <- str_sub(sids, 1, nchar(sids)-2)
ACISmeta$sids <- sids

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

# #Find nearest NRCS weather station for each pika site 
# pika_sites <- pika_sites %>%
#   group_by(Site) %>%
#   mutate(NRCSmeta.id = st_nearest_feature(geometry, NRCSmeta),
#          dist.NRCS = as.numeric(st_distance(geometry, NRCSmeta[NRCSmeta.id,])),
#          NRCS.ID = NRCSmeta$site_id[match(NRCSmeta.id, 1:nrow(NRCSmeta))],
#          NRCS.name = NRCSmeta$site_name[match(NRCSmeta.id, 1:nrow(NRCSmeta))])
# 
# #Find nearest ACIS weather station for each pika site 
# pika_sites <- pika_sites %>%
#   group_by(Site) %>%
#   mutate(ACISmeta.id = st_nearest_feature(geometry, ACISmeta),
#          dist.ACIS = as.numeric(st_distance(geometry, ACISmeta[ACISmeta.id,])),
#          ACIS.ID = ACISmeta$dbID[match(ACISmeta.id, 1:nrow(ACISmeta))],
#          ACIS.name = ACISmeta$name[match(ACISmeta.id, 1:nrow(ACISmeta))])

# Create a common list of weather stations
NRCSmeta <- NRCSmeta %>% 
  dplyr::select(ntwk, site_name, site_id, geometry)
NRCSmeta <- rename(NRCSmeta, source = ntwk)

ACISmeta <- ACISmeta %>% 
  dplyr::select(source, name, dbID, geometry)
ACISmeta <- rename(ACISmeta, site_name = name, site_id = dbID)

WSmeta <- rbind(NRCSmeta, ACISmeta)

# Find the nearest weather station for each pika site
pika_sites <- pika_sites %>%
  group_by(Site) %>%
  mutate(WSmeta.id = st_nearest_feature(geometry, WSmeta),
         dist.WS = as.numeric(st_distance(geometry, WSmeta[WSmeta.id,])),
         WS.ID = WSmeta$site_id[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.name = WSmeta$site_name[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.source = WSmeta$source[match(WSmeta.id, 1:nrow(WSmeta))],
         WS.geometry = WSmeta$geometry[match(WSmeta.id, 1:nrow(WSmeta))])


# Map sites and nearest weather stations
sites <- dplyr::select(pika_sites, Site, Location, Year, geometry, WS.ID, WS.name, WS.source)
WS <- data.frame(nearest.site = pika_sites$Site, WS.ID = pika_sites$WS.ID, WS.name = pika_sites$WS.name,
                 WS.source = pika_sites$WS.source, geometry = pika_sites$WS.geometry)
WS <- st_as_sf(WS)
st_crs(WS) <- 4326

mapView(sites, col.regions = "blue") + mapView(WS, col.regions = "green")


write_csv(pika_sites, file = "C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/Pika GetWx/R11_CompileWx_pika/_output/pika_sites_nearestWS_combined.csv")
