    
    
    snotel_info <- function (path){
      server <- try(wdman::phantomjs(verbose = FALSE))
      remDr <- RSelenium::remoteDriver(browserName = "phantomjs", 
                                       port = 4567L)
      remDr$open(silent = TRUE)
      remDr$navigate("https://wcc.sc.egov.usda.gov/nwcc/yearcount?network=sntl&counttype=listwithdiscontinued&state=")
      main <- xml2::read_html(remDr$getPageSource()[[1]])
      remDr$close()
      if (!inherits(server, "try-error")) {
        server$stop()
      }
      sel_data <- "h5~ table+ table"
      df <- rvest::html_nodes(main, sel_data) %>% rvest::html_table() %>% 
        data.frame()
      df$site_id <- as.numeric(gsub("[\\(\\)]", "", 
                                    regmatches(df$site_name, regexpr("\\(.*?\\)", df$site_name))))
      df$site_name <- tolower(lapply(strsplit(df$site_name, "\\("), 
                                     "[[", 1))
      df$start <- as.Date(paste(df$start, "1"), "%Y-%B %d")
      df$end <- as.Date(paste(df$enddate, "1"), "%Y-%B %d")
      df <- df[, -grep("enddate", colnames(df))]
      colnames(df)[which(colnames(df) == "ntwk")] <- "network"
      colnames(df)[which(colnames(df) == "huc")] <- "description"
      df <- df[-which(colnames(df) == "ts" | colnames(df) == 
                        "wyear")]
      df <- df[, c(1, 2, 3, 9, 4, 11, 5, 6, 7, 8, 10)]
      df$elev <- round(df$elev * 0.3048)
      df$end[df$end == "2100-01-01"] <- Sys.Date()
      if (base::missing(path)) {
        return(df)
      }
      else {
        utils::write.table(df, sprintf("%s/snotel_metadata.csv", 
                                       path), col.names = TRUE, row.names = FALSE, quote = FALSE, 
                           sep = ",")
      }
    }
    
    
    snotel_sitedownload <- function (ID, ST, NTWK, path = tempdir(), internal = FALSE) 
    {
      if (base::missing(ID)) {
        stop("no site specified")
      }
 
     message(sprintf("Downloading SiteID = %s\n", ID))
     base_url <- paste0("https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport,metric/daily/", 
                        ID, ":", ST, ":", NTWK, "%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value,PREC::value,TMAX::value,TMIN::value,TAVG::value,PRCP::value")
     error <- httr::GET(url = base_url, 
                        httr::write_disk(path = file.path(tempdir(), 
                        "snotel_tmp.csv"), overwrite = TRUE))
     
     if (httr::http_error(error)) {
       warning(sprintf("Downloading site %s failed, removed empty file.", 
                       meta_data$ID[i]))
     }
     Sys.sleep(0.25)
     df <- tryCatch( utils::read.table(file.path(tempdir(), "snotel_tmp.csv"), 
                             header = TRUE, sep = ",", stringsAsFactors = FALSE), error=function(e) NULL )
     if ( is.null( df ) == F ){
      df <- snotelr::snotel_metric(df)
     }

     return(df)

    }