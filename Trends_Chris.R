URL_GT=function(keyword=NA, country=NA, region=NA, year=NA, month=1, length=3){
  # keyword can contain up to five words
  # country is the 2 letter country code
  # region is also a 2 letter code
  # year: if you want a specific year, put it here
  # month: starting month if you have specified the year
  # length: the number of months you want if you have specified the year
  
  start="http://www.google.com/trends/trendsReport?hl=en-US&q="
  end="&cmpt=q&content=1&export=1"
  geo=""
  date=""
  
  #Geographic restrictions
  if(!is.na(country)) {
    geo="&geo="
    geo=paste(geo, country, sep="")
    if(!is.na(region)) geo=paste(geo, "-", region, sep="")
  }
  
  queries=keyword[1]
  if(length(keyword)>1) {
    for(i in 2:length(keyword)){
      queries=paste(queries, "%2C ", keyword[i], sep="")
    }
  }
  
  #Dates
  if(!is.na(year)){
    date="&date="
    date=paste(date, month, "%2F", year, "%20", length, "m", sep="")
  }
  
  URL=paste(start, queries, geo, date, end, sep="")
  URL <- gsub(" ", "%20", URL)
  return(URL)
}

concerts <- c("paris", "london")
for (i in seq_along(concerts)){
  url <- URL_GT(concerts[i])  
  browseURL(url)
}


### Cleaning

setwd("C:/Users/Christopher/Downloads")
temp <- list.files(pattern="*.csv")
listings <- do.call("rbind", lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header = FALSE, 
                                                               sep = ",")))
