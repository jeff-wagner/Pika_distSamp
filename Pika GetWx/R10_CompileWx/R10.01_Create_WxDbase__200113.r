
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
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx/R10_CompileWx" )

  # Load ACIS weather data
    load( file = "../R06_ACIS webservice/_data/ACIS_Alaska_20210107.rda" )
    nrow( ACISwx )
    ACISwx <- subset( ACISwx, is.na(snwd) == F & snwd != "M" )
    nrow( ACISwx )
    ACISwx$snwd <- as.numeric( ACISwx$snwd )
    
    nrow( ACISmeta )
    ACISmeta <- ACISmeta[ which( ACISmeta$uid %in% unique( ACISwx$uid ) ), ]
    nrow( ACISmeta )
    
  # Create Weather database
    ndf <- data.frame()
    wx <- data.frame()
    for( i in 1:length( ACISmeta[,1] ) ){
      
      ll <- c(0,0)
      if( is.null( unlist(  ACISmeta$ll[ i ] ) ) == F ){
        ll <- unlist( ACISmeta$ll[ i ] )
      }
      ndf <- rbind( ndf,
                    data.frame( dbID = i,
                                source = "ACIS",
                                ACISmeta[i,c(6,1,3,5)], 
                                lat = ll[2],
                                long = ll[1],
                                swnd_coverage = ACISmeta$snwd_Coverage[i],
                                stringsAsFactors = F )
      )
      
      sbwx <- subset( ACISwx, uid == ACISmeta$uid[i] )
      wx <- rbind( wx,
                    data.frame( dbID = i,
                                sbwx,                                
                                stringsAsFactors = F )
      )
      
    }
    
    meta <- spTransform( SpatialPointsDataFrame( coords = cbind( ndf$long, ndf$lat ),
                                                 proj4string = CRS( WGS84.prjinfo ),
                                                 data = ndf ),
                         projNAD83 )
    
    save( list=c("meta", "wx"), file = "_output/WxDbase(R10.01).rda" )
    
