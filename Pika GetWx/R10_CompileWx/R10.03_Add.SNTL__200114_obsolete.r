
  # This script finds NOAA weather observations in the vicinity 
  #   of locations provided by Tom Paragi for IM evaluation

 ###########################################################################
 ###########################################################################
 ##########    THESE DATA ARE REDUNDANT ( SEE READ ME FILE )     ###########
 ###########################################################################
 ###########################################################################
 


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
    setwd( "C:/Users/jpskinner/Documents/_Projects/20191220 IM Evaluation/20191220 GetWx/R10_CompileWx" )

    # FUNCTION: Apply stringsim function to each item of MatchLs list
    MatchList <- function( MatchStr, MatchLs, Qual = 0.95 ){
      strdist <- stringsim( MatchStr, MatchLs, method = "jw" )
      mtch <- which( strdist >= Qual )
      return( list( idx = mtch,  qual = strdist[ mtch ] ) )
    }
    

  # Load base database (ACIS only) with weather data
    load( file = "_output/WxDbase(R10.02).rda" )

    
  # Load SNOTEL weather data
    load( file = "../R03_SNOTEL/_data/sntl_Alaska.rda" )
    nrow( SNTLwx )
    SNTLwx <- subset( SNTLwx, is.na(precipitation_cummulative) == F )
    nrow( SNTLwx )
    SNTLwx$snwd <- as.numeric( SNTLwx$precipitation_cummulative )
    
    nrow( SNTLmeta )
    SNTLmeta <- SNTLmeta[ which( SNTLmeta$site_id %in% unique( SNTLwx$siteid ) ), ]
    nrow( SNTLmeta )
    SNTLmeta$SITENAME <- toupper( SNTLmeta$site_name )
  
    meta[which( meta$name == "Frostbite Bottom"), ]
  
  
    wxsb <- subset( SNTLwx, siteid == 641 )
    
    dbsb <- subset( wx, sid == "SNTL:641" )  
    
    
    nrcsSP <- spTransform( SpatialPointsDataFrame( coords = cbind( as.numeric( NRCSmeta$longitude ),
                                                                   as.numeric( NRCSmeta$latitude ) ),
                                                   proj4string = CRS( WGS84.prjinfo ),
                                                   data = NRCSmeta ),
                           projNAD83 )
    
  # # Load NOAA weather data
  #   load( file = "R01_NOAA/_data/Weather.rda" )
  #   
  # # check for snow and temp max      
  #   NOAAmeta <- subset( meta, ELEMENT %in% c("TMAX","SNWD") )
  #   NOAAwx <- subset( wx, Element %in% c("TMAX","SNWD") )
  #   # length( which( meta$id %in% unique( subwx$id ) ) )
  #   # nrow( meta )
  #   # head( meta )
  #   rm(list = c("wx","meta") )
  #   
  #   

    
    mtch <- data.frame()
    r <- 0
    for ( i in 1:length( nrcsSP[,1] ) ){

      d <- gDistance( nrcsSP[i, ], wxSP, byid = T )

      nrby <- wxSP[ which( d < 2000 ), ]      
      if( nrow( nrby ) > 0 ){

        for ( j in 1:length( nrby[,1] ) ){
          r <- r + 1
          # nrby@data
          # nrcsSP[i,]@data
           
          NameMatch <- MatchList( toupper( nrby$name[j] ),
                                  toupper( nrcsSP$site_name[i] ), Qual = 0.5 )
          NameCor <- round( NameMatch$qual*100, 0 )
          if ( length( NameCor ) == 0 ) NameCor = NA
        # check for data correlation
          a <- NRCSwx[ which( NRCSwx$siteid == nrcsSP$site_id[i] ), ]
          b <- ACISwx[ which( ACISwx$uid == nrby$uid[j] ), ]
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
        
          png( width = 800, height = 680, 
               filename = paste0("_plots/compare/Match-",r,"__DBID-",nrby$dbID[j],"_vs_",nrcsSP$ntwk[i],"-",gsub(":","",nrcsSP$site_id[i]),".png" ),
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
      
    write.csv( mtch, file = "NRCS_Matches.csv", row.names = F ) 

        
    plot( as.Date(b$date), b$snwd, pch=20 )
    points( as.Date( a$Date ), a$Snow.Depth..in..Start.of.Day.Values, type="l", col="blue" )
    
    
  # look up data for a single example (match number 5 ) 
    mtch [ which( mtch$dbID == 486 ), ]
    
    a <- subset( NRCSwx, siteid == "SNTL:1267"  &
                   Date >= as.POSIXct( "2015-11-28" ) & Date <= as.POSIXct( "2015-12-31" ) )
    
    nrow( a )
    
    
    ndf[which( ndf$dbID==486),]
      
    b <- subset( ACISwx, uid == 77630 &
                 date >= as.POSIXct( "2015-11-28" ) & date <= as.POSIXct( "2015-12-31" ) )
    
    tail( b )
    
    
    plot( a$Date, a$snwd )
    points( b$date, b$snwd )
    
    
    tail( a )
    
