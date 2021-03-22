
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

    if( file.exists( "_Output/ghcnd_META.rda" ) == F ){
    # get all stations
      stations <- as.data.frame( ghcnd_stations() )
      
    # Save Station Metadata 
      save( list = c("stations"), file = "_Output/ghcnd_META.rda" )
    
    } else {
      load( file = "_Output/ghcnd_META.rda" )
      
    }
    

    # Site data      
    sites <- read.csv( file = "_Data/pika_sites_20210128.csv", stringsAsFactors = F ) 
    colnames(sites)[1] <- "id"
    
    a <- meteo_nearby_stations( sites, lat_colname = "Latitude", lon_colname = "longitude",
                           radius = 100, station_data = stations, var = c("SNWD","SNOW"),
                           year_max = 2019, year_min = 1980 )
    
    ndf <- data.frame()
    for ( i in 1:length( a ) ){
      
      indx <- which( sites$id == names( a )[i] )
      ndf <- rbind( ndf,
                    data.frame( Request = names( a )[i],
                                sites[indx, -c(1)],
                                a[[i]][1,] )
      )
    }
    
    write.csv( ndf, file = "_Output/Wx_stations.csv", row.names = F )
    
    
    
    