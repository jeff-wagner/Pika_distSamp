
  # This script finds SNOTEL network weather observations in the vicinity 
  #   of locations provided by Tom Paragi for IM evaluation

# set working directory  
    setwd( "C:/Users/jpskinner/Documents/_Projects/20191220 IM Evaluation/20191220 GetWx/R03_SNOTEL" )

  # Install Required Packages
    # Automattically install required packages if necessary
    rqdPkgs <- c('snotelr','rgeos','rgdal','maptools', 'raster', 'leaflet' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   

  # Load required packages  
    sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )

  # Load function modifications for snotelr (to work around issues with Zscaler)
    source( "_functions/F00_SNOTELmods.r" )
    
  # load all days / months  
    load( file = "../_Functions/alldays.rda" )
    
    if( file.exists( "_data/sntl_Meta.rda" ) == F ){
    # download and list site information
      meta_data <- snotel_info()
      save( meta_data, file = "_data/sntl_Meta.rda" )
      write.csv( meta_data, file = "_data/sntl_Meta.csv", row.names = F )    
    } else {
      load( file = "_data/sntl_Meta.rda" )
    }
    
    AKstations <- meta_data[ which( meta_data$state == "AK" ), ]


    SNTLmeta <- data.frame()
    SNTLwx <- data.frame()
    for ( i in 1:length( AKstations[,1] ) ){

      dat <- snotel_sitedownload(ID = AKstations$site_id[ i ],
                                 ST = AKstations$state[ i ],
                                 NTWK = AKstations$network[ i ],
                             internal = TRUE)
      
      
      if( length( dat[,1] ) > 0 ){
        SNTLmeta <- rbind( SNTLmeta, data.frame( AKstations[i,],
                                         FirstDate = min( dat$date, na.rm = T ),
                                         LastDate = max( dat$date, na.rm = T ),
                                         tmax_Coverage = round( ( length( which( is.na( dat$temperature_max ) == F ) )/length(alldays) )*100, 1 ),
                                         snwd_Coverage = round( ( length( which( is.na( dat$precipitation_cummulative ) == F ) )/length(alldays) )*100, 1 ),
                                         stringsAsFactors = F
                                         )
         )
        
        SNTLwx <- rbind( SNTLwx, data.frame( SOURCE = "snotelr",
                                     network = AKstations$network[ i ],
                                     state = AKstations$state[ i ],
                                     siteid = AKstations$site_id[ i ],
                                     dat,
                                     stringsAsFactors = F )
        )
      }
      
    }
    save( list = c("SNTLmeta", "SNTLwx" ), file = "_data/sntl_Alaska.rda" )

    hist( SNTLmeta$snwd_Coverage )
    plot.ecdf( SNTLmeta$snwd_Coverage )
     