downloadFremont <- function(csvdest = "data/fremont.csv", 
                            RDatadest = "data/fremont.RData") {
  url <- "https://data.seattle.gov/api/views/65db-xm6k/rows.csv?accessType=DOWNLOAD"
  download.file(url = url, csvdest)
  fremont <- readr::read_csv(csvdest)
  fremont$Date <- as.POSIXct( strptime(fremont$Date, "%m/%d/%Y %I:%M:%S %p") )
  save(fremont, file = RDatadest)
  return(fremont)
}


downloadSpokaneSt <- function(csvdest = "data/spokane.csv",
                              RDatadest = "data/spokane.RData") {
  url <- "https://data.seattle.gov/api/views/upms-nr8w/rows.csv?accessType=DOWNLOAD"
  download.file(url = url, csvdest)
  spokane <- readr::read_csv(csvdest)
  spokane$Date <- as.POSIXct( strptime(spokane$Date, "%m/%d/%Y %I:%M:%S %p") )
  save(spokane, file = RDatadest)
  return(spokane)
}

download2ndAve <- function(csvdest = "data/second.csv",
                           RDatadest = "data/second.RData") {
  url <- "https://data.seattle.gov/api/views/avwm-i8ym/rows.csv?accessType=DOWNLOAD"
  download.file(url = url, csvdest)
  second <- readr::read_csv(csvdest)
  second$Date <- as.POSIXct( strptime(second$Date, "%m/%d/%Y %I:%M:%S %p") )
  save(second, file = RDatadest)
  return(second)
}

