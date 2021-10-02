# This script sorts and summarizes the data gap weather station data

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
setwd( "C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/Pika GetWx/R11_CompileWx_pika" )

# Load wx database and reference dataframe
load(file = "./_output/ACISdatagaps_download.rda")
datagap.WS <- readRDS( "./_output/datagap.WS.rds")

# Recombine ACIS meta and wx data into a format like the NRCS data
wx_ACIS <- left_join(ACISwx, ACISmeta, by = "uid")
wx_ACIS <- wx_ACIS %>%
  mutate(ntwk = "ACIS", Latitude = unlist(lapply(ll, `[[`, 1)), Longitude = unlist(lapply(ll, `[[`, 2)),
         avgt = as.numeric(avgt), maxt = as.numeric(maxt)) %>% 
  select(date, name, ntwk, uid, Latitude, Longitude, elev, avgt, maxt, pcpn) %>% 
  rename(Date = date, Station.Name = name, Station.Id = uid, Network.Code = ntwk)

# There are some issues with pcpn (text in strings) that we have to fix before converting to numeric
unique(wx_ACIS$pcpn)
wx_ACIS$pcpn <- ifelse(wx_ACIS$pcpn == "S", NA, wx_ACIS$pcpn)
wx_ACIS$pcpn <- as.numeric(gsub("[^0-9.-]", "", wx_ACIS$pcpn))

# Add in reference columns for season and year
wx_ACIS.ref <- wx_ACIS %>% 
  filter(Date >= "2017-12-01" & Date <= "2018-03-31" |
           Date >= "2018-06-01" & Date <= "2018-08-31" |
           Date >= "2018-12-01" & Date <= "2019-03-31") %>% 
  mutate(season = ifelse(Date >= "2018-06-01" & Date <= "2018-08-31", "summer", "winter"),
         year = ifelse(Date >= "2017-12-01" & Date <= "2018-03-31", 2017, 2018))

# Separate into data frames for pcpn and temp ------------------------
temp.uids <- subset(datagap.WS, type == "temp")$WS.ID
pcpn.uids <- subset(datagap.WS, type == "pcpn")$WS.ID 

# All snotel stations for pcpn, we only need to deal with temp here

wx_ACIS.temp <- subset(wx_ACIS, Station.Id %in% temp.uids) %>% 
  select(-pcpn)

# Separate into data frames for season: Summer 2018, Winters 2017 & 2018
temp_ACISsummer2018 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2018-06-01"), as.Date("2018-08-31")))
temp_ACISwinter2017 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2017-12-01"), as.Date("2018-03-31"))) 
temp_ACISwinter2018 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2018-12-01"), as.Date("2019-03-31")))

# Summarize ACIS data by site -----------------------------------------------
temp_ACISsummer2018$Date <- as.numeric(as.Date(temp_ACISsummer2018$Date))
temp_ACISwinter2017$Date <- as.numeric(as.Date(temp_ACISwinter2017$Date))
temp_ACISwinter2018$Date <- as.numeric(as.Date(temp_ACISwinter2018$Date))


ACIS_temp <- unique(temp_ACISsummer2018$Station.Id)



ACIS_summer2018_temp <- data.frame(season = "summer", year = "2018", Station.Name =NA, Station.Id = ACIS_temp, 
                                   Network.Code = NA,
                                   Latitude = NA, Longitude = NA,
                                   mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA)
for(i in 1:length(ACIS_temp)){
  a <- subset(temp_ACISsummer2018, Station.Id == ACIS_temp[i])
  mean.temp <- mean(a$avgt, na.rm = TRUE)
  number.max.temp.days <- sum(a$maxt > 82.4, na.rm = TRUE)
  total.days <- max(temp_ACISsummer2018$Date) - min(temp_ACISsummer2018$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  
  ACIS_summer2018_temp[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_summer2018_temp[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_summer2018_temp[i, "Latitude"] <- a$Latitude[i]
  ACIS_summer2018_temp[i, "Longitude"] <- a$Longitude[i]
  ACIS_summer2018_temp[i, "mean.temp.degF"] <- mean.temp
  ACIS_summer2018_temp[i, "number.max.temp.days"] <- number.max.temp.days
  ACIS_summer2018_temp[i, "percent.max.temp.days"] <- percent.max.temp.days
}

ACIS_winter2017_temp <- data.frame(season = "winter", year = "2017", Station.Name =NA, Station.Id = ACIS_temp, 
                                   Network.Code = NA,
                                   Latitude = NA, Longitude = NA,
                                   mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA)
for(i in 1:length(ACIS_temp)){
  a <- subset(temp_ACISwinter2017, Station.Id == ACIS_temp[i])
  mean.temp <- mean(a$avgt, na.rm = TRUE)
  number.max.temp.days <- sum(a$maxt > 82.4, na.rm = TRUE)
  total.days <- max(temp_ACISwinter2017$Date) - min(temp_ACISwinter2017$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  total.precip <- sum(a$pcpn, na.rm = TRUE)
  
  ACIS_winter2017_temp[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_winter2017_temp[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_winter2017_temp[i, "Latitude"] <- a$Latitude[i]
  ACIS_winter2017_temp[i, "Longitude"] <- a$Longitude[i]
  ACIS_winter2017_temp[i, "mean.temp.degF"] <- mean.temp
  ACIS_winter2017_temp[i, "number.max.temp.days"] <- number.max.temp.days
  ACIS_winter2017_temp[i, "percent.max.temp.days"] <- percent.max.temp.days
}

ACIS_winter2018_temp <- data.frame(season = "winter", year = "2018", Station.Name =NA, Station.Id = ACIS_temp, 
                                   Network.Code = NA,
                                   Latitude = NA, Longitude = NA,
                                   mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA)
for(i in 1:length(ACIS_temp)){
  a <- subset(temp_ACISwinter2018, Station.Id == ACIS_temp[i])
  mean.temp <- mean(a$avgt, na.rm = TRUE)
  number.max.temp.days <- sum(a$maxt > 82.4, na.rm = TRUE)
  total.days <- max(temp_ACISwinter2018$Date) - min(temp_ACISwinter2018$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  total.precip <- sum(a$pcpn, na.rm = TRUE)
  
  ACIS_winter2018_temp[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_winter2018_temp[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_winter2018_temp[i, "Latitude"] <- a$Latitude[i]
  ACIS_winter2018_temp[i, "Longitude"] <- a$Longitude[i]
  ACIS_winter2018_temp[i, "mean.temp.degF"] <- mean.temp
  ACIS_winter2018_temp[i, "number.max.temp.days"] <- number.max.temp.days
  ACIS_winter2018_temp[i, "percent.max.temp.days"] <- percent.max.temp.days
}

saveRDS(ACIS_summer2018_temp, file = "./_output/ACIS_summer2018_temp_datagap.rds")
saveRDS(ACIS_winter2017_temp, file = "./_output/ACIS_winter2017_temp_datagap.rds")
saveRDS(ACIS_winter2018_temp, file = "./_output/ACIS_winter2018_temp_datagap.rds")
