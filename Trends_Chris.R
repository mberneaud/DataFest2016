library(zoo)
library(plyr)
library(dplyr)
library(ggplot2)

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

concerts <- c("stubhub sting", "stubhub Maroon 5", "stubhub country megaticket", 
            "stubhub jennifer lopez", "stubhub van halen", "stubhub adam lambert",
            "stubhub acdc", "stubhub black sabbath", "stubhub beyonce", "stubhub madonna",
            "stubhub billy idol")
for (i in seq_along(concerts)){
url <- URL_GT(concerts[i])  
browseURL(url)
}


### Cleaning

setwd("C:/Users/Christopher/Downloads")
temp <- list.files(pattern="*.csv")
listings <- do.call("rbind", lapply(temp, function(x) read.csv(x, stringsAsFactors = FALSE, header = FALSE, 
                                                               sep = ",", skip = 4)))

listings$V3[listings$V1 == "Week"] <- listings$V2[listings$V1 == "Week"]
listings$V3[listings$V3 == ""] <- NA
listings$V3 <- na.locf(listings$V3)

listings$indicator <- substr(listings$V1, 1, 2)
listings <- listings %>% filter(indicator == "20")
listings$indicator <- NULL
write.csv(listings, "Complete.csv")

## Compare sales and search queries
sales <- read.csv("daily_sales.csv")


queries <- read.csv("Maroon 5 daily.csv", sep = ";")
queries$dates <- queries$X2015.01.01
queries$Count <- seq(1:nrow(queries))

dat <- join(queries, sales, type = "left", by = "dates")

dat$daily.sales.percent <- NA
for (i in 1:nrow(dat)){
  dat$daily.sales.percent[i] <- (dat$daily.sales[i]/26)*100
}

ggplot(dat, mapping = aes(x = Count)) + 
  geom_line(mapping = aes(y = X22)) +
  geom_point(mapping = aes(y = daily.sales.percent))

