
  # This script finds NOAA weather observations in the vicinity 
  #   of locations provided by Tom Paragi for IM evaluation

  # Projections strings
    WGS84.prjinfo <- "+proj=longlat +datum=WGS84"
    
  # working projection string for Alaska
    projNAD83 <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"


  # Install Required Packages
    # Automattically install required packages if necessary
    rqdPkgs <- c('rnoaa','rgeos','rgdal','maptools', 'raster', 'leaflet','stringdist' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   
    
   # Load required packages  
    sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )
        
  # set working directory  
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx" )

    
  # Load NOAA weather data
    load( file = "R01_NOAA/_data/Weather.rda" )
    
    # Load NRCS weather data
    load( file = "R04_NRCS/_data/nrcs_Alaska__210107.rda" )
    nrow( NRCSwx )
    NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
    nrow( NRCSwx )
    NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )
    NRCSwx$Date <- as.POSIXct( NRCSwx$Date )
    
    nrow( NRCSmeta )
    NRCSmeta <- NRCSmeta[ which( NRCSmeta$site_id %in% unique( NRCSwx$siteid ) ), ]
    nrow( NRCSmeta )
    NRCSmeta$SITENAME <- toupper( NRCSmeta$site_name )
    
    # Load ACIS weather data
    load( file = "R06_ACIS webservice/_data/ACIS_Alaska_20210107.rda" )
    nrow( ACISwx )
    ACISwx <- subset( ACISwx, is.na(snwd) == F & snwd != "M" )
    nrow( ACISwx )
    ACISwx$snwd <- as.numeric( ACISwx$snwd )
    
    nrow( ACISmeta )
    ACISmeta <- ACISmeta[ which( ACISmeta$uid %in% unique( ACISwx$uid ) ), ]
    nrow( ACISmeta )
    

  # look up data for matched station across two sources 
    a <- subset( NRCSwx, siteid == "SNTL:1267"  &
                   Date >= as.POSIXct( "2015-11-28" ) & Date <= as.POSIXct( "2015-12-31" ) )
    
    tail( a )

    b <- subset( ACISwx, uid == 77630 &
                 date >= as.POSIXct( "2015-11-28" ) & date <= as.POSIXct( "2015-12-31" ) )
    
    tail( b )
    
    
    plot( a$Date, a$snwd, pch=20 )
    points( b$date+86400, b$snwd, col="red" )
    
    
    tail( a )
    
