
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

  # load weather database  
    wxdb <- read.csv( "_Output/WxData_byStation_byMonth.csv", stringsAsFactors = F )    
    
    
    wx <- read_xlsx( path = "_Data/mean monthly snow depth xmacis3.xlsx", sheet = 5 )
    wx <- data.frame( wx[,-c(3:6)] )

    wx$snwd <- as.numeric( wx$Snowdepth_in )
    wx$tstamp <- as.POSIXct( wx$Date, tz="America/Anchorage",
                            format="%b %d %Y" )
    
    wxDat <- data.frame( ID = "UNKNOWN",
                         Name = "Yankee Slough",
                         Region = 5,
                         Unit = "21E",
                         Year = format( wx$tstamp, format = "%Y" ),
                         MONTH = format( wx$tstamp, format = "%m" ),
                         START = format( wx$tstamp, format = "%Y-%m-%d" ),
                         END = paste0( format( wx$tstamp, format = "%Y" ), "-",
                                       sprintf( "%02.0f", as.numeric(format( wx$tstamp, format = "%m" )) + 1),
                                       "-01" ),
                         SNWDavg_in = wx$snwd,
                         SNWDmin_in = NA,
                         SNWDmax_in = NA,
                         MISSING_pcnt = NA )


    Filename <-  paste0( "r", wxDat$Region[1], "-u", wxDat$Unit[1],"_",  wxDat$Name[1]," (", wxDat$id[ 1 ], " )" )
    
    x_rng <- c( as.POSIXct("1980-01-01", tz="America/Anchorage"),
                as.POSIXct("2019-01-01", tz="America/Anchorage") )
    y_rng <- c(0,120) 
    png( width = 2400, height = 1200, 
         filename = paste0( "_Plots/QAQC/", Filename, ".png" ),
         pointsize = 24 )
      par( mar=c(4,4,3,4) )
      plot( x=x_rng, y=y_rng, type="n", xaxt="n", yaxt="n",
            xlab="", ylab="")
      mtext( side = 3, paste0( wxDat$Name[1]," ( Region ", wxDat$Region[ 1 ],", Unit ",wxDat$Unit[1], " )" ) , line=0.5, cex=2 )
      mtext( side = 4, "Observations available (%)", line=2.5, col="red" )
      mtext( side = 1, "Date", line=2.5 )
      mtext( side = 2, "Snow depth (inches)", line=2.5, col="blue" )
      axis( side = 1, at=seq(x_rng[1], x_rng[2], length.out = 40),
            labels=substr( seq(1980,2019),3,4 ) )
     
      axis( side = 2, at=seq(y_rng[1], y_rng[2], length.out = 11), las=1 )
      
      axis( side = 4, at=seq(y_rng[1], y_rng[2], length.out = 11),
            labels = seq(0,100,10), las=1 )
      
      points( as.POSIXct( wxDat$START ), wxDat$SNWDavg_in, type="l", col="blue" )
      points( as.POSIXct( wxDat$START ), (1-(wxDat$MISSING_pcnt/100))*y_rng[2], type="l", col="red" )
      
      abline( h = 28, lwd=2, lty="dotted" )
      abline( h = 35, lwd=3, lty="dashed" )
                            
    dev.off()
    
    png( width = 2400, height = 1200, 
         filename = paste0( "_Plots/Final/", Filename, ".png" ),
         pointsize = 24 )
      par( mar=c(4,5,2,4) )
      plot( x=x_rng, y=y_rng, type="n", xaxt="n", yaxt="n",
            xlab="", ylab="")
      mtext( side = 3, paste0( wxDat$Name[1]," ( Region ", wxDat$Region[ 1 ],", Unit ",wxDat$Unit[1], " )" ) , line=0.5, cex=2 )
      mtext( side = 1, "Date", line=3, cex=2 )
      mtext( side = 2, "Snow depth (inches)", line=3, col="black", cex=2 )
      axis( side = 1, at=seq(x_rng[1], x_rng[2], length.out = 40),
            labels=substr( seq(1980,2019),3,4 ) )
      
      axis( side = 2, at=seq(y_rng[1], y_rng[2], length.out = 11), las=1 )
      
      points( as.POSIXct( wxDat$START ), wxDat$SNWDavg_in, type="l", col="black", lwd=3 )
      
      abline( h = 28, lwd=2, lty="dotted" )
      abline( h = 35, lwd=3, lty="dashed" )
    
    dev.off()
    
    
    write.csv( wxDat, file = paste0( "_Plots/", Filename, ".csv" ), row.names = F )
    
    
    #write.csv( wxDat, file = paste0("_Output/WxData_byStation_byMonth_(Yankee Slough).csv" ), row.names = F )    
        
   
        
        
      
      