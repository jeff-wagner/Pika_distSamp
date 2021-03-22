

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

  # Load base database (ACIS only) with weather data
    load( file = "_output/WxDbase(R10.02).rda" )

  # NOTES ABOUT ERRORS
    # all Threadex data don't have lat/long values because they are values from several
    # stations stitched together to produce a long term dataset. These sites (sids end in "thr") 
    # may contain the same data as other weather stations in this dataset and might need to be 
    # excluded during analysis.
    
  # This code was used to identify bad lat/long data
    plot( meta )
    nrow( meta[ which( coordinates( meta )[,1] > 2500000 ), ] )
    plot( meta[ which( coordinates( meta )[,1] > 2500000 ), ], add=T, col="red" )
    a <- meta@data[ which( coordinates( meta )[,1] > 2500000 ), ]
    a$sids <- unlist( lapply( a$sids, FUN = function(x)  paste0(unlist(x), collapse = ";" ) ) )
    write.csv( a,
               file = "_data/MetaCorrections_20210204.csv", row.names = F )
    
    crdat <- read.csv( file = "_data/MetaCorrections_20210204.csv",
                       stringsAsFactors = F )
    
   # apply corrections 
    for( i in 1:length( crdat[,1] ) ){
      indx <- which( meta$dbID == crdat$dbID[ i ] )
      print( meta[indx,] )
      meta$lat[ indx ] <- crdat$lat[i]
      meta$long[ indx ] <- crdat$long[i]
      print( meta[indx,] )
    }
    
    meta <- spTransform( SpatialPointsDataFrame( coords = cbind( meta$long, meta$lat ),
                                                 proj4string = CRS( WGS84.prjinfo ),
                                                 data = meta@data ),
                         projNAD83 )

    
    
    load( file = "../_Functions/alldays.rda" )
    
    for( i in 1:length( meta[,1] )){
      
      sbwx <- subset( wx, dbID == meta$dbID[ i ] & 
                        snwd >= 0 &
                        date >= as.POSIXct("1980-01-01") &
                        date <as.POSIXct("2019-01-01") )
      
      meta@data[i,"Start"] <- min( sbwx$date, na.rm = T )
      meta@data[i,"End"] <- max( sbwx$date, na.rm = T )
      meta@data[i,"swnd_coverage"] <- length( unique( sbwx$date ) )/length(alldays)
      
      
    }
    
    plot( meta, col="grey", pch=20 )
    mapview(meta)
    plot( meta[ which( meta$swnd_coverage == 0 ), ], col="red", add=T )
    
  # Save final version of database      
    save( list=c("meta", "wx"), file = "../_Data/Wx_byDay_20210204.rda" )

    nrow( meta )