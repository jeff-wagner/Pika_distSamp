source("scripts/initscript.r")
setwd("C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/data")

# Load manually downloaded ACIS & NRCS weather data
wx_ACISsummer2017 <- read.csv("./wx_data/ACIS/summer2017_ACIS.csv", header = T, skip = 2)
wx_ACISsummer2018 <- read.csv("./wx_data/ACIS/summer2018_ACIS.csv", header = T, skip = 2)
wx_ACISwinter2017 <- read.csv("./wx_data/ACIS/winter2017_ACIS.csv", header = T, skip = 2)
wx_ACISwinter2018 <- read.csv("./wx_data/ACIS/winter2018_ACIS.csv", header = T, skip = 2)

wx_NRCSsummer2017 <- read.table("./wx_data/NRCS/summer2017_NRCS_22sep21.txt", sep = ",", header = T, skip = 59)
wx_NRCSsummer2018 <- read.table("./wx_data/NRCS/summer2018_NRCS_22sep21.txt", sep = ",", header = T, skip = 59)
wx_NRCSwinter2017 <- read.table("./wx_data/NRCS/winter2017_NRCS_22sep21.txt", sep = ",", header = T, skip = 60)
wx_NRCSwinter2018 <- read.table("./wx_data/NRCS/winter2018_NRCS_22sep21.txt", sep = ",", header = T, skip = 60)


# Summarize NRCS data by site -----------------------------------------------
wx_NRCSsummer2017$Date <- as.numeric(as.Date(wx_NRCSsummer2017$Date))
nrcs <- unique(wx_NRCSsummer2017$Station.Id)
NRCS_summer2017 <- data.frame(Station.Name =NA, Station.Id = nrcs, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA,
                              mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA, total.precip = NA)
for(i in 1:length(nrcs)){
  a <- subset(wx_NRCSsummer2017, Station.Id == nrcs[i])
  mean.temp <- mean(a$Air.Temperature.Average..degF., na.rm = TRUE)
  number.max.temp.days <- sum(a$Air.Temperature.Maximum..degF. > 82.4)
  total.days <- max(wx_NRCSsummer2017$Date) - min(wx_NRCSsummer2017$Date) + 1
  percent.max.temp.days <- sum(a$Air.Temperature.Maximum..degF. > 82.4)/total.days
  total.precip <- sum(a$Precipitation.Increment..in.)
  
  NRCS_summer2017[i, "Station.Name"] <- a$Station.Name[i]
  NRCS_summer2017[i, "Network.Code"] <- a$Network.Code[i]
  NRCS_summer2017[i, "Latitude"] <- a$Latitude[i]
  NRCS_summer2017[i, "Longitude"] <- a$Longitude[i]
  NRCS_summer2017[i, "mean.temp.degF"] <- mean.temp
  NRCS_summer2017[i, "number.max.temp.days"] <- number.max.temp.days
  NRCS_summer2017[i, "percent.max.temp.days"] <- percent.max.temp.days
  NRCS_summer2017[i, "total.precip"] <- total.precip
}

NRCS.sites <- st_as_sf(NRCS_summer2017, coords = c("Longitude", "Latitude"))
st_crs(NRCS.sites) <- 4326
mapviewOptions(basemaps = "Esri.WorldGrayCanvas", native.crs = TRUE)
mapview(NRCS.sites)

# Create label for season/year and combine into one database ----------------
wx_ACISsummer2017$season <- "summer"
wx_ACISsummer2017$year <- 2017
wx_ACISsummer2018$season <- "summer"
wx_ACISsummer2018$year <- 2018
wx_ACISwinter2017$season <- "winter"
wx_ACISwinter2017$year <- 2017
wx_ACISwinter2018$season <- "winter"
wx_ACISwinter2018$year <- 2018

wx_ACIS <- Reduce(function(x, y) merge(x, y, all=TRUE), 
       list(wx_ACISsummer2017, wx_ACISsummer2018, wx_ACISwinter2017, wx_ACISwinter2018))

rm(wx_ACISsummer2017, wx_ACISsummer2018, wx_ACISwinter2017, wx_ACISwinter2018)

wx_NRCSsummer2017$season <- "summer"
wx_NRCSsummer2017$year <- 2017
wx_NRCSsummer2018$season <- "summer"
wx_NRCSsummer2018$year <- 2018
wx_NRCSwinter2017$season <- "winter"
wx_NRCSwinter2017$year <- 2017
wx_NRCSwinter2018$season <- "winter"
wx_NRCSwinter2018$year <- 2018

wx_NRCS <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                  list(wx_NRCSsummer2017, wx_NRCSsummer2018, wx_NRCSwinter2017, wx_NRCSwinter2018))

rm(wx_NRCSsummer2017, wx_NRCSsummer2018, wx_NRCSwinter2017, wx_NRCSwinter2018)

# Filter ACIS data for weather stations of interest ----------------------
ACIS_pikaWS <- readRDS("../Pika GetWx/R11_CompileWx_pika/_output/ACIS_pikaWS.rds")

# wx_ACIS.pika <- wx_ACIS[substr(wx_ACIS$Name, 1, 15) %in% substr(ACIS_pikaWS$WS.name, 1, 15), ]
# wx_ACIS.pika <- wx_ACIS.pika[substr(wx_ACIS.pika$Latitude, 2, 6) %in% substr(ACIS_pikaWS$Latitude, 1, 5), ]

# Convert to spatial objects
wx_ACIS <- wx_ACIS %>% 
  filter(Longitude != " -")
wx_ACIS <- st_as_sf(wx_ACIS, coords = c("Longitude", "Latitude"))
st_crs(wx_ACIS) <- 4326

ACIS_pikaWS <- st_as_sf(ACIS_pikaWS, coords = c("Longitude", "Latitude"))
st_crs(ACIS_pikaWS) <- 4326


# Filter NRCS data for weather stations of interest -----------------------
NRCS_pikaWS <- readRDS("../Pika GetWx/R11_CompileWx_pika/_output/NRCS_pikaWS.rds")
























# Find the nearest weather station
nearest.WS <- ACIS_pikaWS %>%
  mutate(wx_ACIS.rownum = st_nearest_feature(geometry, wx_ACIS),
         wx_ACIS.name = wx_ACIS[wx_ACIS.rownum,]$Name,
         wx_ACIS.geometry = wx_ACIS[wx_ACIS.rownum, 1]$geometry,
         dist_m = as.numeric(st_distance(geometry, wx_ACIS.geometry, by_element = TRUE)))

         ,
         WS.ID = wx_ACIS$site_id[match(wx_ACIS.id, 1:nrow(wx_ACIS))],
         WS.name = wx_ACIS$site_name[match(wx_ACIS.id, 1:nrow(wx_ACIS))],
         WS.source = wx_ACIS$source[match(wx_ACIS.id, 1:nrow(wx_ACIS))],
         WS.geometry = wx_ACIS$geometry[match(wx_ACIS.id, 1:nrow(wx_ACIS))])

m <- leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$Stamen.TonerHybrid)

m <- m %>% 
  addCircles(data = nearest.WS$geometry,
             radius = 5,
             opacity = 1,
             fillOpacity = 1,
             color = "yellow",
             label = nearest.WS$WS.name)

m <- m %>% 
  addCircles(data = nearest.WS$wx_ACIS.geometry,
             radius = 5,
             opacity = 1,
             fillOpacity = 1,
             color = "red",
             label = nearest.WS$wx_ACIS.name)
m
