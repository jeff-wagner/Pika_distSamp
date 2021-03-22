

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
    setwd( "C:/Users/jpskinner/Documents/_Projects/20191220 IM Evaluation/20191220 GetWx/R10_CompileWx" )

    load( file = "../_Data/WxDbase_20200114.rda" )
    
    dat <- read.csv( "_data/Innoko/Innoko_NWR_snow_stakes.csv", stringsAsFactors = F )
    datSP <- spTransform( SpatialPointsDataFrame( coords = cbind( dat$LON, dat$LAT ),
                                                 proj4string = CRS( WGS84.prjinfo ),
                                                 data = dat),
                         projNAD83 )
    
    
    ndf <- data.frame()
    for ( i in 1:length( datSP[,1] )){
      
      d <- gDistance( datSP[i,], meta, byid = T )
      
      ndf <- rbind( ndf,
                    data.frame( datSP@data[i,],
                                NearestStation_Name = meta$name[ which( d == min(d) ) ],
                                NearestStation_DBid = meta$dbID[ which( d == min(d) ) ],
                                NearestStation_sid = paste0(unlist(meta$sids[ which( d == min(d) ) ]), collapse = ";" ),
                                NearestStation_dist = min( d ),
                                stringsAsFactors = F
                    )
      )
    }
    
    write.csv( ndf, file = "_data/Innoko/NearbyStations.csv", row.names = F )