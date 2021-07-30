getwd()
setwd("C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/data/wx_data")

NRCS_manual <- read.table("./NRCS/summer2017_NRCS.txt", sep = ",", header = T, skip = 60)

mean(NRCS_manual[430:460, 8])
