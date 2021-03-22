

    nd <- data.frame( yrmo = paste0( rep( seq( 1980, 2019, 1 ), each=12 ),"-", sprintf(fmt = "%02.0f", seq( 1, 12, 1 ) ) ) )
    nd$date <- as.POSIXct( paste0( nd$yrmo, "-01" ) )
    wntrs <- 1979:2019
    nd$wntr <- NA
    nd$dow <- NA
    for( i in 1:length(wntrs)){
      indx <- which( nd$yrmo %in% 
                       c( paste0(wntrs[i],"-", sprintf(fmt = "%02.0f",7:12 )),
                          paste0(wntrs[i]+1,"-", sprintf(fmt = "%02.0f", 1:6 )) ) )
      
      nd$wntr[ indx ] <- wntrs[ i ]
      nd$dow[ indx ] <- round( ( (( as.numeric(nd$date[indx]) -
                                        as.numeric( as.POSIXct( paste0( wntrs[i],"-07-01") ) ) )/86400)-0.499 ),0)+1
    }
    
    write.csv( nd, file = "REF_Winter.DOW.csv" )