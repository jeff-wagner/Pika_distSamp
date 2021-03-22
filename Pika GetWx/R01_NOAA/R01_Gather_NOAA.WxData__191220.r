
  # This script finds NOAA weather observations in the vicinity 
  #   of locations provided by Tom Paragi for IM evaluation

  # Projections strings
    WGS84.prjinfo <- "+proj=longlat +datum=WGS84"

  # Install Required Packages
    # Automattically install required packages if necessary
    rqdPkgs <- c('rnoaa','rgeos','rgdal','maptools', 'raster', 'leaflet' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   
    
  # Load required packages  
    require( rgeos )
    require( rgdal )
    require( maptools )
    require( raster )
    require( rnoaa )
    require( readxl )
        
  # set working directory  
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx" )

    sites <- read.csv( file = "_Output/Wx_stations.csv", stringsAsFactors = F )
    
  # load weather variables of interest
    wxVars <- read.csv( file = "_Data/WxVaribles.csv", stringsAsFactors = F )
    
        
    alldays <- format( seq( as.POSIXct( "1980-01-01"),
                    as.POSIXct( "2020-01-01" ), 86400 ), format = "%Y-%m-%d" )
    
    mth <- expand.grid( 1980:2019, 1:12, stringsAsFactors = F )
    mth <- mth[order(mth$Var1),]
    allmonths <- c() 
    for( i in 1:length( mth[,1] ) ){
     allmonths <- c( allmonths,
                     paste0(paste0( sprintf( mth[i,], fmt = "%02.0f" ), collapse = "-" ),"-01") )
      
    }

    meta <- data.frame()
    wx <- data.frame()

    for ( i in 1:length( sites[,1] ) ){
      
      dat <- ghcnd_search( sites$id[i], date_min = "1980-01-01", 
                           date_max = "2020-01-01" )
      
      for ( j in 1:length( wxVars[,1] ) ){

        eldat <- data.frame( dat[ which( names( dat ) == tolower( wxVars$ELEMENT[j] ) ) ],
                             stringsAsFactors = F )
        
        if ( nrow( eldat ) > 0 ){
          colnames( eldat ) <- gsub( paste0(tolower( wxVars$ELEMENT[j] ), "."), "", colnames( eldat ) )
          colnames( eldat )[ 2 ] <- "Value"
  
          wx <- rbind( wx,
                       data.frame( Request = sites$Request[ i ],
                                   id = sites$id[ i ],
                                   Element = wxVars$ELEMENT[j],
                                   eldat[,2:5],
                                   stringsAsFactors = F )
          )
          
          
          meta <- rbind( meta,
                         data.frame( sites[ i, ],
                                     ELEMENT = wxVars$ELEMENT[j],
                                     FirstDate = min( eldat$date, na.rm = T ),
                                     LastDate = max( eldat$date, na.rm = T ),
                                     pcnt_good = round( ( length( which( is.na( eldat$Value ) == F ) )/length(alldays) )*100, 1 ),
                                     pcnt_qflags = round( ( length( which( eldat$qflag != " " ) )/length( allmonths ))*100,1),
                                     stringsAsFactors = F 
                         )
        )
        
        } else {
          meta <- rbind( meta,
                         data.frame( sites[ i, ],
                                     ELEMENT = wxVars$ELEMENT[j],
                                     FirstDate = NA,
                                     LastDate = NA,
                                     pcnt_good = 0,
                                     pcnt_qflags = NA,
                                     stringsAsFactors = F 
                         )
          )
          
        }
      }

    }

    save( list = c("meta", "wx" ), file = "_Output/Weather.rda" )

    
    