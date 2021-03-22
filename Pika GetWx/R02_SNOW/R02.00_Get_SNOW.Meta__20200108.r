
# This script finds NRCS network weather observations in the vicinity 
#   of locations provided by Tom Paragi for IM evaluation
# https://www.wcc.nrcs.usda.gov/snow/>


# set working directory  
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx/R02_SNOW" )


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
    if( file.exists( "_data/SNOW_meta.rda" ) == F ){
      # download and list site information
      meta_data <- grabNRCS.meta(ntwrks = "ALL", cnvrt.elev = FALSE )
      AllMeta <- data.frame()
      for ( i in 1:length( meta_data ) ){
        AllMeta <- rbind( AllMeta,
                          data.frame( meta_data[[i]], stringsAsFactors = F )
        )
      }
      SNOW_meta <- subset( AllMeta, ntwk == "SNOW" & state == "AK" )
      save( SNOW_meta, file = "_data/SNOW_meta.rda" )
      write.csv( SNOW_meta, file = "_data/SNOW_meta.csv", row.names = F )    
    } else {
      load( file = "_data/SNOW_meta.rda" )
    }

    SNOWwx <- data.frame()
    for ( i in 1:length( SNOW_meta[,1] ) ){

      ID = gsub( "SNOW:", "", SNOW_meta$site_id[ i ], fixed = T )
      
      base_url <- paste0( "https://wcc.sc.egov.usda.gov/reportGenerator/",
                          "view_csv/customSingleStationReport/monthly/start_of_period/",
                          ID,":AK:SNOW%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/",
                          "stationId,state.code,state.name,elevation,latitude,longitude,SNWD::value?fitToScreen=false" )

      
      tempDF <- tryCatch( utils::read.csv(base_url, comment.char = "#", 
                                          quote = ""), error=function(e) NULL )
      
      SNOWwx <- rbind( SNOWwx,
                       data.frame( tempDF,
                                   stringsAsFactors = F )
      )
   
      
    }

    
    SNOWwx$datets <- as.POSIXct( paste("01", SNOWwx$Date), format = "%d %b %Y" )
    SNOWwx$yr <- as.numeric( format( SNOWwx$datets, format = "%Y" ) )
    SNOWwx$doy <- as.numeric( format( SNOWwx$datets, format = "%j" ) )
    

    
    
    nrow( SNOWwx )
    SNOWwx <- subset( SNOWwx, is.na( Snow.Depth..in..Start.of.Month.Values ) == F &
                        yr >= 1980 & yr <= 2019 )
    nrow( SNOWwx )
    
    table( SNOWwx$yr )
    
    save( list = c("SNOW_meta", "SNOWwx" ), file = "_data/SNOWwx_ValidationSet.rda" )

 
    
    
     