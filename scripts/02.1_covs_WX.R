source("scripts/initscript.r")
#setwd("C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/data")

# Load manually downloaded NRCS weather data and previously saved ACIS database

wx_NRCSsummer2017 <- read.table("./data/wx_data/NRCS/summer2017_NRCS_30sep21.txt", sep = ",", header = T, skip = 59)
wx_NRCSsummer2018 <- read.table("./data/wx_data/NRCS/summer2018_NRCS_30sep21.txt", sep = ",", header = T, skip = 59)
wx_NRCSwinter2017 <- read.table("./data/wx_data/NRCS/winter2017_NRCS_30sep21.txt", sep = ",", header = T, skip = 60)
wx_NRCSwinter2018 <- read.table("./data/wx_data/NRCS/winter2018_NRCS_30sep21.txt", sep = ",", header = T, skip = 60)

wx_NRCSwinter2017_gaps <- read.table("./data/wx_data/NRCS/winter2017_NRCS_datagaps_precip_29sep21.txt", sep = ",", header = T, skip = 53)
wx_NRCSwinter2017 <- full_join(wx_NRCSwinter2017, wx_NRCSwinter2017_gaps)

load(file = "./Pika GetWx/R11_CompileWx_pika/_output/ACISdownload.rda")

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

# Separate into dataframes for pcpn and temp ------------------------------
ACIS_pikaWS <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/ACIS_pikaWS.rds")

temp.uids <- subset(ACIS_pikaWS, type == "temp")$WS.ID
pcpn.uids <- subset(ACIS_pikaWS, type == "precip")$WS.ID

wx_ACIS.temp <- subset(wx_ACIS, Station.Id %in% temp.uids) %>% 
  select(-pcpn)

wx_ACIS.pcpn <- subset(wx_ACIS, Station.Id %in% pcpn.uids) %>% 
  select(-maxt, -avgt)

rm(ACIS_pikaWS, ACISmeta, ACISwx, wx_ACIS)

# Separate into seasons ---------------------------------------------------
temp_ACISsummer2017 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2017-06-01"), as.Date("2017-08-31")))
temp_ACISsummer2018 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2018-06-01"), as.Date("2018-08-31")))
temp_ACISwinter2017 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2017-12-01"), as.Date("2018-03-31"))) 
temp_ACISwinter2018 <- wx_ACIS.temp %>% 
  filter(between(Date, as.Date("2018-12-01"), as.Date("2019-03-31")))

pcpn_ACISsummer2017 <- wx_ACIS.pcpn %>% 
  filter(between(Date, as.Date("2017-06-01"), as.Date("2017-08-31")))
pcpn_ACISsummer2018 <- wx_ACIS.pcpn %>% 
  filter(between(Date, as.Date("2018-06-01"), as.Date("2018-08-31")))
pcpn_ACISwinter2017 <- wx_ACIS.pcpn %>% 
  filter(between(Date, as.Date("2017-12-01"), as.Date("2018-03-31"))) 
pcpn_ACISwinter2018 <- wx_ACIS.pcpn %>% 
  filter(between(Date, as.Date("2018-12-01"), as.Date("2019-03-31")))  

# Summarize NRCS data by site -----------------------------------------------
wx_NRCSsummer2017$Date <- as.numeric(as.Date(wx_NRCSsummer2017$Date))
wx_NRCSsummer2018$Date <- as.numeric(as.Date(wx_NRCSsummer2018$Date))
wx_NRCSwinter2017$Date <- as.numeric(as.Date(wx_NRCSwinter2017$Date))
wx_NRCSwinter2018$Date <- as.numeric(as.Date(wx_NRCSwinter2018$Date))

nrcs <- unique(wx_NRCSsummer2017$Station.Id)

NRCS_summer2017 <- data.frame(season = "summer", year = "2017", Station.Name =NA, Station.Id = nrcs, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA,
                              mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA, total.precip = NA)
for(i in 1:length(nrcs)){
  a <- subset(wx_NRCSsummer2017, Station.Id == nrcs[i])
  mean.temp <- mean(a$Air.Temperature.Average..degF., na.rm = TRUE)
  number.max.temp.days <- sum(a$Air.Temperature.Maximum..degF. > 82.4, na.rm = TRUE)
  total.days <- max(wx_NRCSsummer2017$Date) - min(wx_NRCSsummer2017$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  total.precip <- sum(a$Precipitation.Increment..in., na.rm = TRUE)
  
  NRCS_summer2017[i, "Station.Name"] <- a$Station.Name[i]
  NRCS_summer2017[i, "Network.Code"] <- a$Network.Code[i]
  NRCS_summer2017[i, "Latitude"] <- a$Latitude[i]
  NRCS_summer2017[i, "Longitude"] <- a$Longitude[i]
  NRCS_summer2017[i, "mean.temp.degF"] <- mean.temp
  NRCS_summer2017[i, "number.max.temp.days"] <- number.max.temp.days
  NRCS_summer2017[i, "percent.max.temp.days"] <- percent.max.temp.days
  NRCS_summer2017[i, "total.precip"] <- total.precip
}

nrcs <- unique(wx_NRCSsummer2018$Station.Id)
NRCS_summer2018 <- data.frame(season = "summer", year = "2018", Station.Name =NA, Station.Id = nrcs, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA,
                              mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA, total.precip = NA)
for(i in 1:length(nrcs)){
  a <- subset(wx_NRCSsummer2018, Station.Id == nrcs[i])
  mean.temp <- mean(a$Air.Temperature.Average..degF., na.rm = TRUE)
  number.max.temp.days <- sum(a$Air.Temperature.Maximum..degF. > 82.4, na.rm = TRUE)
  total.days <- max(wx_NRCSsummer2018$Date) - min(wx_NRCSsummer2018$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  total.precip <- sum(a$Precipitation.Increment..in., na.rm = TRUE)
  
  NRCS_summer2018[i, "Station.Name"] <- a$Station.Name[i]
  NRCS_summer2018[i, "Network.Code"] <- a$Network.Code[i]
  NRCS_summer2018[i, "Latitude"] <- a$Latitude[i]
  NRCS_summer2018[i, "Longitude"] <- a$Longitude[i]
  NRCS_summer2018[i, "mean.temp.degF"] <- mean.temp
  NRCS_summer2018[i, "number.max.temp.days"] <- number.max.temp.days
  NRCS_summer2018[i, "percent.max.temp.days"] <- percent.max.temp.days
  NRCS_summer2018[i, "total.precip"] <- total.precip
}

nrcs <- unique(wx_NRCSwinter2017$Station.Id)
NRCS_winter2017 <- data.frame(season = "winter", year = "2017", Station.Name =NA, Station.Id = nrcs, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA,
                              mean.temp.degF = NA, number.max.temp.days = NA, 
                              percent.max.temp.days = NA, total.precip = NA,
                              total.precip.snowadj = NA, mean.snow.depth = NA,
                              mean.swe = NA)
for(i in 1:length(nrcs)){
  a <- subset(wx_NRCSwinter2017, Station.Id == nrcs[i])
  mean.temp <- mean(a$Air.Temperature.Average..degF., na.rm = TRUE)
  number.max.temp.days <- sum(a$Air.Temperature.Maximum..degF. > 82.4, na.rm = TRUE)
  total.days <- max(wx_NRCSwinter2017$Date) - min(wx_NRCSwinter2017$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  total.precip <- sum(a$Precipitation.Increment..in., na.rm = TRUE)
  total.precip.snowadj <- sum(a$Precipitation.Increment...Snow.adj..in., na.rm = TRUE)
  mean.snow.depth <- mean(a$Snow.Depth..in..Start.of.Day.Values, na.rm = TRUE)
  mean.swe <- mean(a$Snow.Water.Equivalent..in..Start.of.Day.Values, na.rm = TRUE)
  
  NRCS_winter2017[i, "Station.Name"] <- a$Station.Name[i]
  NRCS_winter2017[i, "Network.Code"] <- a$Network.Code[i]
  NRCS_winter2017[i, "Latitude"] <- a$Latitude[i]
  NRCS_winter2017[i, "Longitude"] <- a$Longitude[i]
  NRCS_winter2017[i, "mean.temp.degF"] <- mean.temp
  NRCS_winter2017[i, "number.max.temp.days"] <- number.max.temp.days
  NRCS_winter2017[i, "percent.max.temp.days"] <- percent.max.temp.days
  NRCS_winter2017[i, "total.precip"] <- total.precip
  NRCS_winter2017[i, "total.precip.snowadj"] <- total.precip.snowadj
  NRCS_winter2017[i, "mean.snow.depth"] <- mean.snow.depth
  NRCS_winter2017[i, "mean.swe"] <- mean.swe
}

nrcs <- unique(wx_NRCSwinter2018$Station.Id)
NRCS_winter2018 <- data.frame(season = "winter", year = "2018", Station.Name =NA, Station.Id = nrcs, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA,
                              mean.temp.degF = NA, number.max.temp.days = NA, 
                              percent.max.temp.days = NA, total.precip = NA,
                              total.precip.snowadj = NA, mean.snow.depth = NA,
                              mean.swe = NA)
for(i in 1:length(nrcs)){
  a <- subset(wx_NRCSwinter2018, Station.Id == nrcs[i])
  mean.temp <- mean(a$Air.Temperature.Average..degF., na.rm = TRUE)
  number.max.temp.days <- sum(a$Air.Temperature.Maximum..degF. > 82.4, na.rm = TRUE)
  total.days <- max(wx_NRCSwinter2017$Date) - min(wx_NRCSwinter2017$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  total.precip <- sum(a$Precipitation.Increment..in., na.rm = TRUE)
  total.precip.snowadj <- sum(a$Precipitation.Increment...Snow.adj..in., na.rm = TRUE)
  mean.snow.depth <- mean(a$Snow.Depth..in..Start.of.Day.Values, na.rm = TRUE)
  mean.swe <- mean(a$Snow.Water.Equivalent..in..Start.of.Day.Values, na.rm = TRUE)
  
  NRCS_winter2018[i, "Station.Name"] <- a$Station.Name[i]
  NRCS_winter2018[i, "Network.Code"] <- a$Network.Code[i]
  NRCS_winter2018[i, "Latitude"] <- a$Latitude[i]
  NRCS_winter2018[i, "Longitude"] <- a$Longitude[i]
  NRCS_winter2018[i, "mean.temp.degF"] <- mean.temp
  NRCS_winter2018[i, "number.max.temp.days"] <- number.max.temp.days
  NRCS_winter2018[i, "percent.max.temp.days"] <- percent.max.temp.days
  NRCS_winter2018[i, "total.precip"] <- total.precip
  NRCS_winter2018[i, "total.precip.snowadj"] <- total.precip.snowadj
  NRCS_winter2018[i, "mean.snow.depth"] <- mean.snow.depth
  NRCS_winter2018[i, "mean.swe"] <- mean.swe
}

rm(a, wx_NRCSsummer2017, wx_NRCSsummer2018, wx_NRCSwinter2017, wx_NRCSwinter2018)

# Summarize ACIS data by site -----------------------------------------------
temp_ACISsummer2017$Date <- as.numeric(as.Date(temp_ACISsummer2017$Date))
temp_ACISsummer2018$Date <- as.numeric(as.Date(temp_ACISsummer2018$Date))
temp_ACISwinter2017$Date <- as.numeric(as.Date(temp_ACISwinter2017$Date))
temp_ACISwinter2018$Date <- as.numeric(as.Date(temp_ACISwinter2018$Date))
pcpn_ACISsummer2017$Date <- as.numeric(as.Date(pcpn_ACISsummer2017$Date))
pcpn_ACISsummer2018$Date <- as.numeric(as.Date(pcpn_ACISsummer2018$Date))
pcpn_ACISwinter2017$Date <- as.numeric(as.Date(pcpn_ACISwinter2017$Date))
pcpn_ACISwinter2018$Date <- as.numeric(as.Date(pcpn_ACISwinter2018$Date))

ACIS_temp <- unique(temp_ACISsummer2017$Station.Id)
ACIS_pcpn <- unique(pcpn_ACISsummer2017$Station.Id)

# Temperature
ACIS_summer2017_temp <- data.frame(season = "summer", year = "2017", Station.Name =NA, Station.Id = ACIS_temp, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA,
                              mean.temp.degF = NA, number.max.temp.days = NA, percent.max.temp.days = NA)
for(i in 1:length(ACIS_temp)){
  a <- subset(temp_ACISsummer2017, Station.Id == ACIS_temp[i])
  mean.temp <- mean(a$avgt, na.rm = TRUE)
  number.max.temp.days <- sum(a$maxt > 82.4, na.rm = TRUE)
  total.days <- max(temp_ACISsummer2017$Date) - min(temp_ACISsummer2017$Date) + 1
  percent.max.temp.days <- number.max.temp.days/total.days*100
  
  ACIS_summer2017_temp[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_summer2017_temp[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_summer2017_temp[i, "Latitude"] <- a$Latitude[i]
  ACIS_summer2017_temp[i, "Longitude"] <- a$Longitude[i]
  ACIS_summer2017_temp[i, "mean.temp.degF"] <- mean.temp
  ACIS_summer2017_temp[i, "number.max.temp.days"] <- number.max.temp.days
  ACIS_summer2017_temp[i, "percent.max.temp.days"] <- percent.max.temp.days
}

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

# Precipitation
ACIS_summer2017_pcpn <- data.frame(season = "summer", year = "2017", Station.Name =NA, Station.Id = ACIS_pcpn, 
                              Network.Code = NA,
                              Latitude = NA, Longitude = NA, total.precip = NA)

for(i in 1:length(ACIS_pcpn)){
  a <- subset(pcpn_ACISsummer2017, Station.Id == ACIS_pcpn[i])
  total.precip <- sum(a$pcpn, na.rm = TRUE)
  
  ACIS_summer2017_pcpn[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_summer2017_pcpn[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_summer2017_pcpn[i, "Latitude"] <- a$Latitude[i]
  ACIS_summer2017_pcpn[i, "Longitude"] <- a$Longitude[i]
  ACIS_summer2017_pcpn[i, "total.precip"] <- total.precip
}

ACIS_summer2018_pcpn <- data.frame(season = "summer", year = "2018", Station.Name =NA, Station.Id = ACIS_pcpn, 
                                   Network.Code = NA,
                                   Latitude = NA, Longitude = NA, total.precip = NA)

for(i in 1:length(ACIS_pcpn)){
  a <- subset(pcpn_ACISsummer2018, Station.Id == ACIS_pcpn[i])
  total.precip <- sum(a$pcpn, na.rm = TRUE)
  
  ACIS_summer2018_pcpn[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_summer2018_pcpn[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_summer2018_pcpn[i, "Latitude"] <- a$Latitude[i]
  ACIS_summer2018_pcpn[i, "Longitude"] <- a$Longitude[i]
  ACIS_summer2018_pcpn[i, "total.precip"] <- total.precip
}

ACIS_winter2017_pcpn <- data.frame(season = "winter", year = "2017", Station.Name =NA, Station.Id = ACIS_pcpn, 
                                   Network.Code = NA,
                                   Latitude = NA, Longitude = NA, total.precip = NA)

for(i in 1:length(ACIS_pcpn)){
  a <- subset(pcpn_ACISwinter2017, Station.Id == ACIS_pcpn[i])
  total.precip <- sum(a$pcpn, na.rm = TRUE)
  
  ACIS_winter2017_pcpn[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_winter2017_pcpn[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_winter2017_pcpn[i, "Latitude"] <- a$Latitude[i]
  ACIS_winter2017_pcpn[i, "Longitude"] <- a$Longitude[i]
  ACIS_winter2017_pcpn[i, "total.precip"] <- total.precip
}

ACIS_winter2018_pcpn <- data.frame(season = "winter", year = "2018", Station.Name =NA, Station.Id = ACIS_pcpn, 
                                   Network.Code = NA,
                                   Latitude = NA, Longitude = NA, total.precip = NA)

for(i in 1:length(ACIS_pcpn)){
  a <- subset(pcpn_ACISwinter2018, Station.Id == ACIS_pcpn[i])
  total.precip <- sum(a$pcpn, na.rm = TRUE)
  
  ACIS_winter2018_pcpn[i, "Station.Name"] <- a$Station.Name[i]
  ACIS_winter2018_pcpn[i, "Network.Code"] <- a$Network.Code[i]
  ACIS_winter2018_pcpn[i, "Latitude"] <- a$Latitude[i]
  ACIS_winter2018_pcpn[i, "Longitude"] <- a$Longitude[i]
  ACIS_winter2018_pcpn[i, "total.precip"] <- total.precip
}

rm(temp_ACISsummer2017, temp_ACISsummer2018, temp_ACISwinter2017, temp_ACISwinter2018,
   pcpn_ACISsummer2017, pcpn_ACISsummer2018, pcpn_ACISwinter2017, pcpn_ACISwinter2018)
# Data fixes and filling in gaps --------------------------------------------

# Remove rows (stations) with problems:
# Temperature: 
#   - Summer 2018: Replacement for ACIS:83041 - Denali Visitors Center
#   - Winter 2017: Replacement for ACIS:79408 - Glennallen 64N
#   - Winter 2018: Replacement for ACIS:83041 - Denali Visitors Center
# 
# Precipitation:
#   - Winter 2017: Replacement for ACIS:79398 - Denali 27 N
#                  Replacement for ACIS:79408 - Glenallen 64N

ACIS_summer2018_temp <- ACIS_summer2018_temp %>% 
  filter(Station.Id != 83041)
ACIS_winter2017_temp <- ACIS_winter2017_temp %>% 
  filter(Station.Id != 79408)
ACIS_winter2018_temp <- ACIS_winter2018_temp %>% 
  filter(Station.Id != 83041)

ACIS_winter2017_pcpn <- ACIS_winter2017_pcpn %>% 
  filter(!Station.Id %in% c(79398, 79408))

# Add in data from new stations
summer2018_tempgap <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/ACIS_summer2018_temp_datagap.rds")
winter2017_tempgap <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/ACIS_winter2017_temp_datagap.rds")
winter2018_tempgap <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/ACIS_winter2018_temp_datagap.rds")

ACIS_summer2018_temp <- rbind(ACIS_summer2018_temp, summer2018_tempgap)
ACIS_winter2017_temp <- rbind(ACIS_winter2017_temp, winter2017_tempgap)
ACIS_winter2018_temp <- rbind(ACIS_winter2018_temp, winter2018_tempgap)

rm(a, summer2018_tempgap, winter2017_tempgap, winter2018_tempgap, wx_ACIS.pcpn, wx_ACIS.temp)

# Cleanup and join into one dataframe ---------------------------------------
ACIS_summer2017 <- full_join(ACIS_summer2017_pcpn, ACIS_summer2017_temp)
ACIS_summer2018 <- full_join(ACIS_summer2018_pcpn, ACIS_summer2018_temp)
ACIS_winter2017 <- full_join(ACIS_winter2017_pcpn, ACIS_winter2017_temp)
ACIS_winter2018 <- full_join(ACIS_winter2018_pcpn, ACIS_winter2018_temp)

rm(ACIS_summer2017_pcpn, ACIS_summer2017_temp, ACIS_summer2018_pcpn, ACIS_summer2018_temp,
   ACIS_winter2017_pcpn, ACIS_winter2017_temp, ACIS_winter2018_pcpn, ACIS_winter2018_temp)

# Create label for season/year and combine into one database for each source 
wx_ACIS <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                  list(ACIS_summer2017, ACIS_summer2018, ACIS_winter2017, ACIS_winter2018))
wx_ACIS <- wx_ACIS %>% 
  mutate(percent.max.temp.days = as.numeric(percent.max.temp.days))

rm(ACIS_summer2017, ACIS_summer2018, ACIS_winter2017, ACIS_winter2018)

# NRCS data already has columns for season & year from previous step

wx_NRCS <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                  list(NRCS_summer2017, NRCS_summer2018, NRCS_winter2017, NRCS_winter2018))

rm(NRCS_summer2017, NRCS_summer2018, NRCS_winter2017, NRCS_winter2018)

# Select columns of interest
wx_NRCS <- wx_NRCS %>% 
  select(-total.precip.snowadj, -mean.snow.depth, -mean.swe)

# Combine into one final overall database
wx <- full_join(wx_ACIS, wx_NRCS)


# Cross-reference to Pika Sites and add into Site Covs --------------------
# Load reference dataframe
pcpn_ref <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/pcpn_ref.rds")
temp_ref <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/temp_ref.rds")
gap_ref <- readRDS("./Pika GetWx/R11_CompileWx_pika/_output/datagap.WS.rds")

ref <- full_join(pcpn_ref, temp_ref)
ref$label <- "original"

gap_ref$label <- "new"
ref <- full_join(ref, select(gap_ref, -Latitude, - Longitude))

overwrite <- ref %>% 
  filter(Site %in% gap_ref$Site &
         season %in% gap_ref$season &
         year %in% gap_ref$year &
         type %in% gap_ref$type, label == "original")

library(plyr)
overwrite <- match_df(ref, gap_ref, on = c("Site", "season", "year", "type")) %>% 
  filter(label == "original")
detach("package:plyr")

ref <- anti_join(ref, overwrite)

# Now, filter for seasons that we need for each site. Need summer & winter data 
# from the year that preceded the survey
reps <- 1:length(ref$Site)
df <- data.frame(rownum = reps, result = NA)

for(i in 1:length(reps)){
  a <- subset(ref, as.numeric(row.names(ref)) == reps[i])
  survey.year <- a$Year
  wx.year <- a$year
  if(wx.year == survey.year-1){
    df[i, "result"] <- "keep"
  }else{
    df[i, "result"] <- "toss"
  } 
}

ref <- merge(ref, df, by = 0) %>% 
  filter(result == "keep") %>% 
  select(Site, Location, Year, dist.WS, WS.ID, WS.name, WS.source, season, year, type)

wx$year <- as.numeric(wx$year)

wx.data <- left_join(ref, wx, by = c("WS.name" = "Station.Name", "season" = "season", "year" = "year")) %>% 
  select(Site, Location, Year, dist.WS, season, year, type, total.precip, mean.temp.degF, 
         number.max.temp.days, percent.max.temp.days)

wx.data$total.precip <- ifelse(wx.data$type == "temp", NA, wx.data$total.precip)
wx.data$mean.temp.degF <- ifelse(wx.data$type == "pcpn", NA, wx.data$mean.temp.degF)
wx.data$number.max.temp.days <- ifelse(wx.data$type == "pcpn", NA, wx.data$number.max.temp.days)
wx.data$percent.max.temp.days <- ifelse(wx.data$type == "pcpn", NA, wx.data$percent.max.temp.days)
wx.data$number.max.temp.days <- ifelse(wx.data$season == "winter", NA, wx.data$number.max.temp.days)
wx.data$percent.max.temp.days <- ifelse(wx.data$season == "winter", NA, wx.data$percent.max.temp.days)

summer.temp <- wx.data %>% 
  filter(season == "summer", type == "temp") %>% 
  select(Site, Location, Year, year, "mean.summer.temp" = mean.temp.degF, number.max.temp.days, percent.max.temp.days)
winter.temp <- wx.data %>% 
  filter(season == "winter", type == "temp") %>% 
  select(Site, Location, Year, year, "mean.winter.temp" = mean.temp.degF)
summer.pcpn <- wx.data %>% 
  filter(season == "summer", type == "pcpn") %>% 
  select(Site, Location, Year, year, "summer.pcpn" = total.precip)
winter.pcpn <- wx.data %>% 
  filter(season == "winter", type == "pcpn") %>% 
  select(Site, Location, Year, year, "winter.pcpn" = total.precip)

wx.data <- summer.temp %>% 
  left_join(winter.temp) %>% 
  left_join(summer.pcpn) %>% 
  left_join(winter.pcpn)

rm(a, df, gap_ref, overwrite, pcpn_ref, ref, summer.pcpn, summer.temp, temp_ref,
   winter.pcpn, winter.temp, wx, wx_ACIS, wx_NRCS, wx_NRCSwinter2017_gaps, ACIS_pcpn,
   ACIS_temp, i, mean.snow.depth, mean.swe, mean.temp, nrcs, number.max.temp.days, 
   pcpn.uids, percent.max.temp.days, reps, survey.year, temp.uids, total.days, total.precip,
   total.precip.snowadj, wx.year)

#setwd("C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp")
