
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
    setwd( "C:\\Users\\jeffw\\Dropbox\\GitHub\\Pika_distSamp\\Pika GetWx\\R10_CompileWx" )

    # FUNCTION: Apply stringsim function to each item of MatchLs list
    MatchList <- function( MatchStr, MatchLs, Qual = 0.95 ){
      strdist <- stringsim( MatchStr, MatchLs, method = "jw" )
      mtch <- which( strdist >= Qual )
      return( list( idx = mtch,  qual = strdist[ mtch ] ) )
    }
    

  # Load base database (ACIS only) with weather data
    load( file = "_output/WxDbase(R10.01).rda" )

    
  # Load NRCS weather data
    load( file = "../R04_NRCS/_data/nrcs_Alaska__210128.rda" )
    nrow( NRCSwx )
    NRCSwx <- subset( NRCSwx, is.na(Snow.Depth..in..Start.of.Day.Values) == F )
    nrow( NRCSwx )
    NRCSwx$snwd <- as.numeric( NRCSwx$Snow.Depth..in..Start.of.Day.Values )
    
  # NRCS dates are one day off from the same data stored in ACIS 
    NRCSwx$Date <- as.POSIXct( NRCSwx$Date )+86400
    
    nrow( NRCSmeta )
    NRCSmeta <- NRCSmeta[ which( NRCSmeta$site_id %in% unique( NRCSwx$siteid ) ), ]
    nrow( NRCSmeta )
    NRCSmeta$SITENAME <- toupper( NRCSmeta$site_name )
    
    
    nrcsSP <- spTransform( SpatialPointsDataFrame( coords = cbind( as.numeric( NRCSmeta$longitude ),
                                                                   as.numeric( NRCSmeta$latitude ) ),
                                                   proj4string = CRS( WGS84.prjinfo ),
                                                   data = NRCSmeta ),
                           projNAD83 )
    
  # Search for matches
    mtch <- data.frame()
    r <- 0
    for ( i in 1:length( nrcsSP[,1] ) ){

      d <- gDistance( nrcsSP[i, ], meta, byid = T )
     nrby <- meta[ which( d < 2000 ), ]      
      if( nrow( nrby ) > 0 ){
        for ( j in 1:length( nrby[,1] ) ){
          NameMatch <- MatchList( toupper( nrby$name[j] ),
                                  toupper( nrcsSP$site_name[i] ), Qual = 0.5 )
          NameCor <- round( NameMatch$qual*100, 0 )
          if ( length( NameCor ) == 0 ) NameCor = 0
          if ( NameCor > 98 ){
          # check for data correlation
            a <- NRCSwx[ which( NRCSwx$siteid == nrcsSP$site_id[i] ), ]
            b <- wx[ which( wx$uid == nrby$uid[j] ), ]
            tst <- data.frame(date=as.Date(NA),a=NA,b=NA)
            for( k in 1:length( a[,1] ) ){
              indx <- which( b$date == a$Date[ k ] )
    
              tst[ k, "date" ] <- as.POSIXct( a$Date[k] )
              tst[ k, "a" ] <- a$snwd[k]
              if( length( indx ) > 0 ){
                tst[ k, "b" ] <- b$snwd[indx]
              }
            }
            tst <- na.omit( tst )
            ValCor <- round( ( cor( tst$a, tst$b )*100), 0)
            if( is.na( ValCor ) == T ) ValCor = 0
            if( ValCor > 55 ){
              r <- r + 1
              png( width = 800, height = 680, 
                   filename = paste0("_plots/NRCS/Match-",r,"__DBID-",nrby$dbID[j],"_vs_",nrcsSP$ntwk[i],"-",gsub(":","",nrcsSP$site_id[i]),".png" ),
                   pointsize = 20 )
              if( nrow( tst ) == 0 ){
                tst <- data.frame(date=a$Date[1],a=0,b=0)
              }
                plot( tst$date, tst$a, pch=20, xlab="Date", ylab="Snow Depth",
                      main="Database versus NRCS")
                points( tst$date, tst$b, col="red", type="l" )
                legend( x="topright", legend = c( nrcsSP$site_id[i],
                                                  paste0("DBID:",nrby$dbID[j])),
                        pch=c(20,NA), lty = c(NA,"solid"), col=c("black","red" ) )
                
                mtext( text = paste0( "NameMatch = ", NameCor, "%\n", "Corr = ", ValCor, "%" ),
                       side = 3, adj = 0.5, line = -2 )
              dev.off()
              mtch[ r, "MatchID" ] <- r
              mtch[ r, "dbID" ] <- nrby$dbID[j]
              mtch[ r, "NRCSID" ] <- nrcsSP$site_id[i]
              mtch[ r, "DBname" ] <- nrby$name[j] 
              mtch[ r, "DB_days" ] <- length( unique( b$date ) )
              mtch[ r, "NRCS_days" ] <- length( unique( a$Date ) )
              mtch[ r, "Distance_m" ] <- round(d[which( d < 2000 )][j],0)
              mtch[ r, "Name_Match" ] <- NameCor
              mtch[ r, "Data_Match" ] <- ValCor
            }
           
          } 
        }
      }
      
    }
    write.csv( mtch, file = "_output/NRCS_Matches_210204.csv", row.names = F ) 

    
  # drop data determined to be already in database
    nrow( NRCSmeta )
    NRCSmeta <- NRCSmeta[ which( !NRCSmeta$site_id %in% mtch$NRCSID ), ]
    nrow( NRCSmeta )
    
    nrow( NRCSwx )
    NRCSwx <- NRCSwx[ which( NRCSwx$siteid %in% NRCSmeta$site_id ), ]
    nrow( NRCSwx )
    

    id <- max( meta$dbID )
    ndf <- data.frame()
    wxdf <- data.frame()
    for( i in 1:length( NRCSmeta[,1] ) ){

      id <- id + 1
      ndf <- rbind( ndf,
                    data.frame( dbID = id,
                                source = "NRCS",
                                uid = NA,
                                name = NRCSmeta$site_name[i],
                                sids = NRCSmeta$site_id[i],
                                elev = NRCSmeta$elev_ft[i],
                                lat = NRCSmeta$latitude[i],
                                long = NRCSmeta$longitude[i],
                                swnd_coverage = NRCSmeta$snwd_Coverage[i],
                                stringsAsFactors = F )
      )
      
      sbwx <- subset( NRCSwx, siteid == NRCSmeta$site_id[i] )
      wxdf <- rbind( wxdf,
                   data.frame( dbID = id,
                               uid = NA,
                               sid = NRCSmeta$site_id[i],
                               date = sbwx$Date,
                               mint = sbwx$Air.Temperature.Minimum..degF.,
                               maxt = sbwx$Air.Temperature.Maximum..degF.,
                               avgt = sbwx$Air.Temperature.Average..degF.,
                               pcpn = sbwx$Precipitation.Increment..in.,
                               snow = sbwx$Precipitation.Increment...Snow.adj..in.,
                               snwd = sbwx$Snow.Depth..in..Start.of.Day.Values,
                               X13 = sbwx$Snow.Water.Equivalent..in..Start.of.Day.Values,                               
                               stringsAsFactors = F )
      )
      
    }
    
    cbndf <- rbind( meta@data, ndf )
    cbndf$lat <- as.numeric( cbndf$lat )
    cbndf$long <- as.numeric( cbndf$long )
    which( is.na( cbndf$lat ) == T )
    
    meta <- spTransform( SpatialPointsDataFrame( coords = cbind( cbndf$long, cbndf$lat ),
                                                 proj4string = CRS( WGS84.prjinfo ),
                                                 data = cbndf ),
                         projNAD83 )
    
    
    cbwx <- rbind( wx, wxdf )

    wx <- cbwx
    save( list=c("meta", "wx"), file = "_output/WxDbase(R10.02).rda" )
        
   