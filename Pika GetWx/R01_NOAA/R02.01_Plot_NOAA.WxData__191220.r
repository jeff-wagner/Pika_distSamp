
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

    load( file = "_Output/Weather.rda" )
    wx$snwd <- as.numeric( wx$snwd/10 )/2.54
    wx$tstamp <- as.POSIXct( wx$date )
    
  # all months
    mth <- expand.grid( 1980:2019, 1:12, stringsAsFactors = F )
    mth <- mth[order(mth$Var1),]
    allmonths <- c() 
    for( i in 1:length( mth[,1] ) ){
      allmonths <- c( allmonths,
                      paste0(paste0( sprintf( mth[i,], fmt = "%02.0f" ), collapse = "-" ),"-01") )
      
    }
    
  #  hist( wx$snwd[ wx$qflag == " " ], xlab = "Snow depth (inches)", main="" )

    x_rng <- c( as.POSIXct("1980-01-01", tz="America/Anchorage"),
                as.POSIXct("2020-01-01", tz="America/Anchorage") )
    y_rng <- range( wx$snwd, na.rm = T )
    ndf <- data.frame()
    for ( i in 1:length( meta[,1] ) ){
      
      subdat <- subset( wx, id == meta$id[ i ] )
      
      # create monthly summary
      nd <- data.frame()
      for ( j in 1:(length( allmonths )-1) ){
       
        mDat <- subset( subdat, tstamp >= allmonths[j] &
                          tstamp < allmonths[j+1] )
        dySeq <- as.POSIXct( format( seq( as.POSIXct( allmonths[j] ),
                                          as.POSIXct( allmonths[j+1] )-80000, 86400 ), "%Y-%m-%d" ) )
        

        nd <- rbind( nd, data.frame( ID = meta$id[ i ],
                          Name = meta$name[ i ],
                          Region = meta$Region[ i ],
                          Unit = meta$UnitSub[ i ],
                          YEAR = format( mDat$tstamp[1], format="%Y" ),
                          MONTH = format( mDat$tstamp[1], format="%m" ),
                          START = allmonths[j],
                          END = allmonths[j+1],
                          SNWDavg_in = mean( mDat$snwd, na.rm=T ),
                          SNWDmin_in = min( mDat$snwd, na.rm=T ),
                          SNWDmax_in = max( mDat$snwd, na.rm=T ),
                          MISSING_pcnt = round( (1-(length( which( is.na( mDat$snwd ) == F ) )/length( dySeq )))*100, 0 )
        ))
        
       
      }
      nd$SNWDmin_in[ which( is.infinite( nd$SNWDmin_in ) == T ) ] <- NA
      nd$SNWDmax_in[ which( is.infinite( nd$SNWDmax_in ) == T ) ] <- NA
      
      # save data 
      ndf <- rbind( ndf, nd )

        Filename <-  paste0( "r", meta$Region[i], "-u", meta$UnitSub[i],"_",  meta$name[i]," (", meta$id[ i ], " )" )
        y_rng <- c(0,120) # c(0, round((( max( nd$SNWDavg_in, na.rm = T )/10 )+0.5),0)*10 )
        png( width = 2400, height = 1200, 
             filename = paste0( "_Plots/QAQC/", Filename, ".png" ),
             pointsize = 24 )
          par( mar=c(4,4,3,4) )
          plot( x=x_rng, y=y_rng, type="n", xaxt="n", yaxt="n",
                xlab="", ylab="")
          mtext( side = 3, paste0( meta$name[i]," ( Region ", meta$Region[ i ],", Unit ",meta$UnitSub[i], " )" ) , line=0.5, cex=2 )
          mtext( side = 4, "Observations available (%)", line=2.5, col="red" )
          mtext( side = 1, "Date", line=2.5 )
          mtext( side = 2, "Snow depth (inches)", line=2.5, col="blue" )
          axis( side = 1, at=seq(x_rng[1], x_rng[2], length.out = 41),
                labels=substr( seq(1980,2020),3,4 ) )
         
          axis( side = 2, at=seq(y_rng[1], y_rng[2], length.out = 11), las=1 )
          
          axis( side = 4, at=seq(y_rng[1], y_rng[2], length.out = 11),
                labels = seq(0,100,10), las=1 )
          
          points( as.POSIXct( nd$START ), nd$SNWDavg_in, type="l", col="blue" )
          points( as.POSIXct( nd$START ), (1-(nd$MISSING_pcnt/100))*y_rng[2], type="l", col="red" )
          
          abline( h = 28, lwd=2, lty="dotted" )
          abline( h = 35, lwd=3, lty="dashed" )
                                
        dev.off()
        
        png( width = 2400, height = 1200, 
             filename = paste0( "_Plots/Final/", Filename, ".png" ),
             pointsize = 24 )
          par( mar=c(4,5,2,4) )
          plot( x=x_rng, y=y_rng, type="n", xaxt="n", yaxt="n",
                xlab="", ylab="")
          mtext( side = 3, paste0( nd$Name[1]," ( Region ", nd$Region[ 1 ],", Unit ",nd$Unit[1], " )" ) , line=0.5, cex=2 )
          mtext( side = 1, "Date", line=3, cex=2 )
          mtext( side = 2, "Snow depth (inches)", line=3, col="black", cex=2 )
          axis( side = 1, at=seq(x_rng[1], x_rng[2], length.out = 41),
                labels=substr( seq(1980,2020),3,4 ) )
          
          axis( side = 2, at=seq(y_rng[1], y_rng[2], length.out = 11), las=1 )
          
          points( as.POSIXct( nd$START ), nd$SNWDavg_in, type="l", col="black", lwd=3 )
          
          abline( h = 28, lwd=2, lty="dotted" )
          abline( h = 35, lwd=3, lty="dashed" )
        
        dev.off()
        
        write.csv( nd, file = paste0( "_Plots/", Filename, ".csv" ), row.names = F )
        
    }
    
    write.csv( ndf, file = paste0("_Output/WxData_byStation_byMonth.csv" ), row.names = F )    
        
   
    write.csv( wx, file = paste0("_Output/WxData_byStation_byDay.csv" ), row.names = F )    
    
        
        
      
      