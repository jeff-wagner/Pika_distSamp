grabNRCS.data <- function (network, site_id, site_state, timescale, DayBgn, DayEnd) 
    {
      options(timeout = 400)
      options(stringsAsFactors = F)
      eCodes <- RNRCS::elementCodes
      tempDF <- NULL
     # meta <- grabNRCS.meta(ntwrks = network)
     # site_state <- as.character(meta[[1]]$state[grep(pattern = site_id, 
     #                                                 x = meta[[1]]$site_id)])
     # comboCode <- paste(network, site_id, sep = ":")
     # siteElmnt <- RNRCS::grabNRCS.elements(comboCode)
     # siteEnames <- tryCatch( trimws(siteElmnt[[1]]$element, which = "both"), error=function(e) NULL )
     # if( is.null( siteEnames ) == F ){
        # siteEnames = gsub(pattern = "-", replacement = "", 
        #                   x = siteEnames)
        # siteEnames = gsub(pattern = "  ", replacement = " ", 
        #                   x = siteEnames)
        # eCodeIndx <- c()
        # for (i in 1:length(siteEnames)) {
        #   eCodeIndx <- append(eCodeIndx, grep(tolower(siteEnames[i]), 
        #                                       tolower(eCodes$ElementName)))
        # }
        siteEcodes <- trimws(eCodes$ElementCode)
        eCodeString <- do.call(paste, c(as.list(siteEcodes), sep = "::value,"))
        eCodeString <- paste0(eCodeString, "::value")
        stoString <- "STO:-2:value,STO:-4:value,STO:-8:value,STO:-20:value,STO:-40:value"
        smsString <- "SMS:-2:value,SMS:-4:value,SMS:-8:value,SMS:-20:value,SMS:-40:value"
        eCodeString <- gsub(pattern = "STO::value", replacement = stoString, 
                            eCodeString)
        eCodeString <- gsub(pattern = "SMS::value", replacement = smsString, 
                            eCodeString)
        eCodeString = gsub(pattern = "SRADT", replacement = "SRADV::value,SRADT", 
                           x = eCodeString)

        baseURL <- "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport/"
        fullURL <- paste0(baseURL, timescale, "/", site_id, 
                          ":", site_state, ":", network, "|id=\"\"|name/", 
                          DayBgn, ",", DayEnd, "/", eCodeString)
        tempDF <- tryCatch( utils::read.csv(fullURL, comment.char = "#", 
                                  quote = ""), error=function(e) NULL )
     # }
      
      if ( is.null( tempDF ) == F ){
        if (ncol(tempDF) == 1) {
          stop("The NRCS API is currently unavailable, please try again later.")
        }
        NRCS.df <- tempDF[, -which(colSums(is.na(tempDF)) == nrow(tempDF))]
        return(NRCS.df)
      } else {
        return( data.frame() )
      }
}


