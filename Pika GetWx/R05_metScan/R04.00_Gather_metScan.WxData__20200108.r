
  # This script finds Alaska weather observations from multiple networks 

  # Projections strings
    WGS84.prjinfo <- "+proj=longlat +datum=WGS84"

  # Install Required Packages
    # Automattically install required packages if necessary
    rqdPkgs <- c('metScanR','rgeos','rgdal','maptools', 'raster', 'leaflet' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   

  # Load required packages  
    sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )

  # set working directory  
    setwd( "/Users/jeff/Library/Mobile Documents/com~apple~CloudDocs/R/GetWx/20191220 GetWx/R05_metScan" )

    AKmeta <- siteFinder( country = "United States", territory = "AK", radius = 20000,
                             vars = "snow depth" ) 
    
## Show results
    mapResults(AKmeta)
    
    stcodes <- c("GHCND","ICAO","WBAN","NCDC")

# Load metadata into flat file
    meta <- data.frame()
    for ( i in 1:length( AKmeta ) ){
      
      idarr <- array()
      for ( j in 1:length( stcodes ) ){
        a <- AKmeta[[i]]$identifiers$id[ which(AKmeta[[i]]$identifiers$idType == stcodes[j]) ]
        if ( length( a ) > 0 ){
          idarr[j] <- a
        } else {
          idarr[j] <- NA
        }
      }
      
      meta <- rbind( meta,                                        
                  data.frame( name = AKmeta[[i]]$namez,
                           platform = AKmeta[[i]]$platform,
                           GHCND = a[1],
                           ICAO = a[2],
                           WBAN = a[3],
                           NCDC = a[4],
                           AKmeta[[i]]$location,
                           stringsAsFactors = F )
      )
      
    }
    save( list = c("meta" ), file = "_data/metScan(snow).rda" )
    