# set working directory  
  setwd( "C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/Pika GetWx/_Functions" )

  alldays <- format( seq( as.POSIXct( "1980-01-01"),
                          as.POSIXct( "2020-01-01" ), 86400 ), format = "%Y-%m-%d" )
  
  mth <- expand.grid( 1980:2019, 1:12, stringsAsFactors = F )
  mth <- mth[order(mth$Var1),]
  allmonths <- c()
  for( i in 1:length( mth[,1] ) ){
    allmonths <- c( allmonths,
                    paste0(paste0( sprintf( mth[i,], fmt = "%02.0f" ), collapse = "-" ),"-01") )
  }
  
  save( list = c("alldays","allmonths"), file = "alldays.rda" )
  