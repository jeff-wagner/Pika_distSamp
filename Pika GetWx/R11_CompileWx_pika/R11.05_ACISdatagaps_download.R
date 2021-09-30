
  # This script uses the Applied Climate Information System (ACIS) web services
  # to find all weather stations in Alaska from multiple platforms/networks
  # see more info at: https://www.rcc-acis.org/docs_webservices.html

  #### WARNING - This code takes considerable time to run       #####
  #### and connection to the server will occasionally cutout    #####
  #### You'll need to monitor and manually restart the process  #####
  
  # set working directory  
    setwd( "C:/Users/jeffw/Dropbox/GitHub/Pika_distSamp/Pika GetWx/R06_ACIS webservice" )

  # load all days / months  
    load( file = "../_Functions/alldays_2017_2020.rda" )
    
  # Install Required Packages
  # Automatically install required packages if necessary
    rqdPkgs <- c('leaflet','jsonlite' )     
    a <- which( !rqdPkgs %in% installed.packages()[,1])
    if ( length( a ) > 0 ){
      install.packages( rqdPkgs[ a ] )
    }   
  # Load required packages  
    sapply( rqdPkgs, FUN = function(x){ library( x, character.only = T ) } )

  # elements 
    elems <- c("mint","maxt","avgt","pcpn","snow","snwd","13")
    

  # Get all AK weather stations in ACIS system
    base_url <- paste0("http://data.rcc-acis.org/StnMeta?state=AK&output=json") 
    mta <- fromJSON(base_url )$meta
    
    # Filter for stations of interest
    ACIS_pikaWS <- readRDS("../R11_CompileWx_pika/_output/ACIS_datagap.WS.rds")
    
    mta <- mta %>% 
      filter(uid %in% ACIS_pikaWS$WS.ID)
    
    ACISmeta <- data.frame()
    ACISwx <- data.frame()
    for ( i in 1:length( mta[,1] ) ){
      
    # Get data for current station
      sid <- unlist( strsplit( mta$sids[i][[1]][1], split = " " ) )
      base_url <- paste0("http://data.rcc-acis.org/StnData?sid=",sid[1],"&sdate=2017-01-01&edate=2020-01-01&elems=",
                         paste0(elems, collapse = ","),"&output=json")
      rslt <- fromJSON( base_url )
      
      if ( is.null( rslt$error ) == T ){
      
        dat <- data.frame( rslt$data, stringsAsFactors = F )
        dat[,1] <- as.POSIXct( as.character( dat[,1] ) )
        colnames( dat ) <- c("date", elems )
        
        if( length( dat[,1] ) > 0 ){
          ACISmeta <- rbind( ACISmeta, data.frame( mta[i,],
                                           FirstDate = min( dat$date, na.rm = T ),
                                           LastDate = max( dat$date, na.rm = T ),
                                           snwd_Coverage = round( ( length( which( is.na( dat$snwd ) == F &  dat$snwd != "M" ) )/length(alldays) )*100, 1 ),
                                           maxt_Coverage = round( ( length( which( is.na( dat$maxt ) == F &  dat$maxt != "M" ) )/length(alldays) )*100, 1 ),
                                           stringsAsFactors = F
                                )
          )
          
          ACISwx <- rbind( ACISwx, data.frame( uid = mta$uid[ i ],
                                               sid = sid[1],
                                               dat,
                                               stringsAsFactors = F )
          )
        }
      }
    }

    # Select variables of interest for this analysis
    ACISwx <- ACISwx %>% 
      dplyr::select(-mint, -snow, -snwd, -X13)
    
    # Replace trace values (T) with zeros
    t.replace <- function(x) x = ifelse(x=="T", 0, x)
    ACISwx <- ACISwx %>% 
      mutate_at(vars(maxt, avgt, pcpn), t.replace)
    
    # Replace missing values (M) with NA
    m.replace <- function(x) x = ifelse(x=="M", NA, x)
    ACISwx <- ACISwx %>% 
      mutate_at(vars(maxt, avgt, pcpn), m.replace)

    
    save( list = c("ACISmeta","ACISwx" ), file = "../R11_CompileWx_pika/_output/ACISdatagaps_download.rda" )
    