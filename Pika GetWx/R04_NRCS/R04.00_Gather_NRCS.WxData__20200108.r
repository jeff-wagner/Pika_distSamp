
  # This script finds NRCS network weather observations in the vicinity 
  #   of locations provided by Tom Paragi for IM evaluation
  # https://www.wcc.nrcs.usda.gov/snow/>

  # set working directory  
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx/R04_NRCS" )
    
  # load all days / months  
    load( file = "../_Functions/alldays.rda" )

  # Install Required Packages
    # Automatically install required packages if necessary
    rqdPkgs <- c('RNRCS','rgeos','rgdal','maptools', 'raster', 'leaflet' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   

  # Load required packages  
    sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )

  # load modification to RNRCS code
    source( file = "_functions/F00_NRCSmods.r" )
    
  # Load metadata
    if( file.exists( "_data/nrcs_Meta.rda" ) == F ){
      # download and list site information
      meta_data <- grabNRCS.meta(ntwrks = "ALL", cnvrt.elev = FALSE )
      AllMeta <- data.frame()
      for ( i in 1:length( meta_data ) ){
        AllMeta <- rbind( AllMeta,
                      data.frame( meta_data[[i]], stringsAsFactors = F )
        )
      }
      save( AllMeta, file = "_data/nrcs_Meta.rda" )
      write.csv( AllMeta, file = "_data/nrcs_Meta.csv", row.names = F )    
    } else {
      load( file = "_data/nrcs_Meta.rda" )
    }

    AKstations <- AllMeta[ which( AllMeta$state == "AK" ), ]
    elems <- c("Air.Temperature.Average..degF.",
               "Air.Temperature.Maximum..degF.",
               "Air.Temperature.Minimum..degF.",
               "Precipitation.Accumulation..in..Start.of.Day.Values",
               "Snow.Depth..in..Start.of.Day.Values",
               "Snow.Water.Equivalent..in..Start.of.Day.Values",
               "Precipitation.Increment..in.",
               "Wind.Speed.Average..mph.",
               "Wind.Speed.Maximum..mph.",
               "Precipitation.Increment...Snow.adj..in.")
    NRCSmeta <- data.frame()
    NRCSwx <- data.frame()
    for ( i in 1:length( AKstations[,1] ) ){
      
    # look up available site elements
    #  siteElmnt <- grabNRCS.elements(AKstations$site_id[i])
      
      sid = unlist(strsplit( AKstations$site_id[i], split = ":" ))[2]
      
      print( paste0( AKstations$site_name[ i ], " | Downloading..." ) )

      cnt <- 1
      dat <- NULL
      while( class( dat ) != "data.frame" & cnt < 4 ){
        dat <- tryCatch( grabNRCS.data(network=AKstations$ntwk[i], 
                                       site_id=sid,
                                       site_state = "AK",
                                       timescale="daily",
                                       DayBgn="1980-01-01",
                                       DayEnd="2020-01-01"), error=function(e) NULL )
        cnt <- cnt + 1
        if ( is.null( dat ) == T ){
          print( paste0( "try ", cnt ) )
          Sys.sleep(2)
        }
      }
      print( paste0( "...complete. | Writing data..." ) )
      
      if( nrow( dat ) > 0 ){
        
        r <- nrow( NRCSwx )+1
        k <- r+(length( dat[,1] )-1)
        
        NRCSwx[(r:k), "ntwk" ]  = AKstations$ntwk[ i ]
        NRCSwx[(r:k), "site_id" ] = sid
        NRCSwx[(r:k), "siteid" ]  = AKstations$site_id[ i ]
        NRCSwx[(r:k), "Date" ]  = dat$Date
        for( j in 1:length( elems ) ){
          if( length( which( elems[j] %in% colnames( dat ) ) ) > 0 ){
            NRCSwx[(r:k), elems[j] ] <- dat[,elems[j]]
          }
        }
        
        NRCSmeta <- rbind( NRCSmeta, data.frame( AKstations[i,],
                                         FirstDate = min( dat$Date , na.rm = T ),
                                         LastDate = max( dat$Date, na.rm = T ),
                                         maxt_Coverage = round( ( length( which( is.na( dat$`Air.Temperature.Maximum..degF.` ) == F ) )/length(alldays) )*100, 1 ),
                                         snwd_Coverage = round( ( length( which( is.na( dat$`Snow.Depth..in..Start.of.Day.Values` ) == F ) )/length(alldays) )*100, 1 ),
                                         stringsAsFactors = F
                                      )
        )
        

      } else {
        print( paste0( "NO RECORDS FOUND!!!" ) )
        
      }
      print( paste0( round( ( i / length( AKstations[,1] ) )*100, 1 ), "% complete." ) )
      
    }
    
    write.csv( NRCSwx, file = "_data/nrcs_WX__210128.csv", row.names = F )
    save( list = c("NRCSmeta", "NRCSwx" ), file = "_data/nrcs_Alaska__210128.rda" )
    
    all.siteid <- unique( NRCSwx$siteid )
    
    length( which( !AKstations$site_id %in% all.siteid ) )
    AKstations[ which( !AKstations$site_id %in% all.siteid ), ]
    
    write.csv( AKstations[ which( !AKstations$site_id %in% all.siteid ), ], 
               file = "_data/nrcs_Alaska(no data)__210128.csv", row.names = F )
    
    