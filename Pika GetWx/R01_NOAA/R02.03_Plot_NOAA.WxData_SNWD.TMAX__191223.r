
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

  # Load raw weather data
    load( file = "_Output/Weather.rda" )
    
  # load weather variables of interest
    wxVars <- read.csv( file = "_Data/WxVaribles.csv", stringsAsFactors = F )
    
    
  # convert raw values
    wx$Measure <- NA
    wx$Unit <- NA
    for ( i in 1:length( wxVars[,1] ) ){
      
      indx <- which( wx$Element == wxVars$ELEMENT[ i ] )
      
      wx$Measure[ indx ] <- sapply( wx$Value[ indx ], FUN = function(X){ if( is.na( X ) == F ){
        eval( parse(text=wxVars$Convert[ i ] ) ) } else { NA } } )
      wx$Unit[ indx ] <- wxVars$Units_sym[ i ]

    }
     
  # all months
    mth <- expand.grid( 1980:2019, 1:12, stringsAsFactors = F )
    mth <- mth[order(mth$Var1),]
    allmonths <- c() 
    for( i in 1:length( mth[,1] ) ){
      allmonths <- c( allmonths,
                      paste0(paste0( sprintf( mth[i,], fmt = "%02.0f" ), collapse = "-" ),"-01") )
      
    }
    
    hist( wx$Measure[ wx$Element == "SNWD" & wx$qflag == " " ], xlab = "Average snow depth (inches)", main="" )
    hist( wx$Measure[ wx$Element == "TMIN" & wx$qflag == " " ], xlab = "Average minimum temperature (F)", main="" )
    

    x_rng <- c( as.POSIXct("1980-01-01", tz="America/Anchorage"),
                as.POSIXct("2020-01-01", tz="America/Anchorage") )
    y1_rng <- c( 0, 120 )
    y2_rng <- c( -60, 60 )
    ndf <- data.frame()
    for ( i in 1:length( meta[,1] ) ){
      
      y1 <- subset( wx, id == meta$id[ i ] & Element == "SNWD" )
      y2 <- subset( wx, id == meta$id[ i ] & Element == "TMIN" )
      
      
      # create monthly summary
      nd <- data.frame()
      for ( j in 1:(length( allmonths )-1) ){
       
        m_y1 <- subset( y1, date >= allmonths[j] &
                          date < allmonths[j+1] )
        
        m_y2 <- subset( y2, date >= allmonths[j] &
                          date < allmonths[j+1] )
        
        
        dySeq <- as.POSIXct( format( seq( as.POSIXct( allmonths[j] ),
                                          as.POSIXct( allmonths[j+1] )-80000, 86400 ), "%Y-%m-%d" ) )
        

        nd <- rbind( nd, data.frame( ID = meta$id[ i ],
                          Name = meta$name[ i ],
                          Region = meta$Region[ i ],
                          Unit = meta$UnitSub[ i ],
                          YEAR = format( dySeq[1], format="%Y" ),
                          MONTH = format( dySeq[1], format="%m" ),
                          START = allmonths[j],
                          END = allmonths[j+1],
                          TMINavg_F = mean( m_y2$Measure, na.rm=T ),
                          TMINmin_F = min( m_y2$Measure, na.rm=T ),
                          TMINmax_F = max( m_y2$Measure, na.rm=T ),
                          TMINnull_pcnt = round( (1-(length( which( is.na( m_y2$Measure ) == F ) )/length( dySeq )))*100, 0 ),
                          SNWDavg_in = mean( m_y1$Measure, na.rm=T ),
                          SNWDmin_in = min( m_y1$Measure, na.rm=T ),
                          SNWDmax_in = max(m_y1$Measure, na.rm=T ),
                          SNWDnull_pcnt = round( (1-(length( which( is.na(  m_y1$Measure ) == F ) )/length( dySeq )))*100, 0 )
            )
        )
        
       
      }
      nd$SNWDmin_in[ which( is.infinite( nd$SNWDmin_in ) == T ) ] <- NA
      nd$SNWDmax_in[ which( is.infinite( nd$SNWDmax_in ) == T ) ] <- NA
      nd$TMINmin_F[ which( is.infinite( nd$TMINmin_F ) == T ) ] <- NA
      nd$TMINmax_F[ which( is.infinite( nd$TMINmax_F ) == T ) ] <- NA
      
      # save data 
      ndf <- rbind( ndf, nd )

        Filename <-  paste0( "r", meta$Region[i], "-u", meta$UnitSub[i],"_",  meta$name[i]," (", meta$id[ i ], " )" )
        png( width = 2400, height = 1200, 
             filename = paste0( "_Plots/", Filename, ".png" ),
             pointsize = 24 )
          par( mar=c(4,4,3,4), new=F )
          plot( x=x_rng, y=y1_rng, type="n", xaxt="n", yaxt="n",
                xlab="", ylab="")
          mtext( side = 3, paste0( meta$name[i]," ( Region ", meta$Region[ i ],", Unit ",meta$UnitSub[i], " )" ) , line=0.5, cex=2 )

          mtext( side = 1, "Date", line=2.5 )
          axis( side = 1, at=seq(x_rng[1], x_rng[2], length.out = 41),
                labels=substr( seq(1980,2020),3,4 ) )

          mtext( side = 2, "Average snow depth (inches)", line=2.5, col="black" )         
          axis( side = 2, at=seq(y1_rng[1], y1_rng[2], length.out = 11), las=1 )
          
          points( as.POSIXct( nd$START ), nd$SNWDavg_in, type="l", col="black" )
          
          abline( h = 28, lwd=2, lty="dotted" )
          abline( h = 35, lwd=3, lty="dashed" )
          
          
          par( new=T )
          plot( x=x_rng, y=y2_rng, type="n", xaxt="n", yaxt="n",
                xlab="", ylab="")
          
          mtext( side = 4, "Average minimum temperature (F)", line=2.5, col="blue" )          
          axis( side = 4, at=seq(y2_rng[1], y2_rng[2], length.out = 11), las=1, col.axis = "blue" )
          
          points( as.POSIXct( nd$START ), nd$TMINavg_F, type="l", col="blue" )

                                
        dev.off()
       
        write.csv( nd, file = paste0( "_Plots/", Filename, ".csv" ), row.names = F )
        
    }
    
    write.csv( ndf, file = paste0("_Output/WxData_byStation_byMonth.csv" ), row.names = F )    
        
   
    write.csv( wx, file = paste0("_Output/WxData_byStation_byDay.csv" ), row.names = F )    
    
        
        
      
      