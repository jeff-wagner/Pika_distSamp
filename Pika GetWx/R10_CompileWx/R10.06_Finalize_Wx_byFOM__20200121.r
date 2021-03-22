

    # Install Required Packages
    # Automattically install required packages if necessary
    rqdPkgs <- c('rgeos','rgdal','maptools', 'raster', 'leaflet','stringdist' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   
    
    # Load required packages  
    sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )

    # Projections strings
    WGS84.prjinfo <- "+proj=longlat +datum=WGS84"
    
    # working projection string for Alaska
    projNAD83 <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

  # set working directory  
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx/R10_CompileWx" )

  # Load base database with weather by Day data
    load( file = "../_Data/Wx_byDay_20210204.rda" )

    snwd <- data.frame()
    for( i in 1:length( meta[,1] )){
      
      sbwx <- subset( wx, dbID == meta$dbID[ i ] & 
                        snwd >= 0 &
                        date >= as.POSIXct("1980-01-01") &
                        date <as.POSIXct("2020-01-01") )
      
      if( nrow( sbwx ) > 0 ){
        snwd <- rbind( snwd,
                       data.frame( sbwx[,c("dbID","date","mint","avgt","snow","snwd")],
                                   northing = meta@coords[i,2],
                                   easting = meta@coords[i,1],
                                   elev = meta$elev[i],
                                   stringsAsFactors = F
                       )
        )
      }
    }
    snwd$dbID <- as.factor( snwd$dbID )
    snwd$date <- as.POSIXct( snwd$date )
    snwd$yr <- format( snwd$date, format = "%Y" )
    snwd$yrmo <- format( snwd$date, format = "%Y-%m" )
    snwd$doy <- as.numeric( format( snwd$date, format = "%j" ) )
    snwd$elev <- as.numeric( as.character( snwd$elev ) )
    snwd$elev [ which( is.na(snwd$elev ) ==T ) ] <- median( snwd$elev, na.rm = T) 
    
    
    snwd$mo <- format( snwd$date, format = "%m" )
    AllIDs <- unique( snwd$dbID ) 
    SNWDmo <- data.frame()
    for ( i in 1:length( AllIDs ) ){
      
      subwx <- subset( snwd, dbID == AllIDs[ i ] )
      SNWDmo <- rbind( SNWDmo,
                       data.frame( dbID = subwx$dbID[1],
                                   northing = subwx$northing[1],
                                   easting =subwx$easting[1],
                                   elev = subwx$elev[1],
                                   yrmo = aggregate( snwd ~ yrmo, data = subwx, FUN = mean, na.rm=T )[,1],
                                   date = aggregate( date ~ yrmo, data = subwx, FUN = min, na.rm=T )[,2],
                                   snwd_top = aggregate( snwd ~ yrmo, data = subwx, FUN = head, n=1, na.rm=T )[,2],
                                   snwd_avg = aggregate( snwd ~ yrmo, data = subwx, FUN = mean, na.rm=T )[,2],
                                   snwd_min = aggregate( snwd ~ yrmo, data = subwx, FUN = min, na.rm=T )[,2],
                                   snwd_max = aggregate( snwd ~ yrmo, data = subwx, FUN = max, na.rm=T )[,2],
                                   stringsAsFactors = F 
                       )
      )
    }
    
    
    # Load SNOW data collected by first of month (FOM)
    load( file = "../R02_SNOW/_data/SNOWwx_ValidationSet.rda" ) 
    SNOWwx <- SNOWwx[c(2,5:11)]
    colnames( SNOWwx )[c(2,5,6)] <- c("elev","snwd","date")
    
    # Create coordinates for validation set
    snwVd <- spTransform( SpatialPointsDataFrame( coords = cbind( as.numeric( SNOWwx$Longitude ),
                                                                  as.numeric( SNOWwx$Latitude ) ),
                                                  proj4string = CRS( WGS84.prjinfo ),
                                                  data = SNOWwx ),
                          projNAD83 )
    
    SNOWwx$easting <- coordinates( snwVd )[,1]
    SNOWwx$northing <- coordinates( snwVd )[,2]
    SNOWwx$dbID <- as.factor( SNOWwx$Station.Id )
    SNOWwx$yrmo <- format( as.POSIXct( SNOWwx$date ), format = "%Y-%m" )

    SNWD <- rbind( data.frame( src = "Daily",
                               SNWDmo[,c(1:7)],
                               stringsAsFactors = F ),
                   data.frame( src = "SNOW",
                               dbID = SNOWwx$Station.Id,
                               northing = SNOWwx$northing,
                               easting = SNOWwx$easting,
                               elev = SNOWwx$elev,
                               yrmo = SNOWwx$yrmo,
                               date = SNOWwx$date,
                               snwd_top = SNOWwx$snwd,
                               stringsAsFactors = F )
    )
    
    
    
    wntrs <- 1979:2020
    SNWD$wntr <- NA
    SNWD$dow <- NA
    for( i in 1:length(wntrs)){
      indx <- which( SNWD$yrmo %in% 
                       c( paste0(wntrs[i],"-", sprintf(fmt = "%02.0f",7:12 )),
                          paste0(wntrs[i]+1,"-", sprintf(fmt = "%02.0f", 1:6 )) ) )
      
      SNWD$wntr[ indx ] <- wntrs[ i ]
      SNWD$dow[ indx ] <- round( ( (( as.numeric(SNWD$date[indx]) -
                                        as.numeric( as.POSIXct( paste0( wntrs[i],"-07-01") ) ) )/86400)-0.499 ),0)+1
    }
    
    
    
    
    plot( meta, col="black", pch=20 )
    plot( snwVd, col="red", add=T )
    
  # Save final version of database      
    save( list=c("SNWD"), file = "../_Data/Wx_byFOM_20210204.rda" )

    nrow( meta )