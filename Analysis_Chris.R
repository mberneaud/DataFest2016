### Load packages
packages <- c("data.table", "plyr")

### Load and if required install packages
 for (p in packages) {
   if (p %in% installed.packages()[,1]) require(p, character.only=T)
   else {
     install.packages(p)
     require(p, character.only=T)
   }
 }

### Load data, insert your path here
setwd("C:/Users/Christopher/Google Drive/GitHub/DataFest Munich")

adwords <- fread("adwords_sample.csv")
purchase <- fread("purchase_sample.csv")
dat <- fread("data_sample.csv")

### Format distance to venue
purchase$dist_to_ven <- as.numeric(purchase$dist_to_ven)
table(purchase$dist_to_ven, useNA = "always")
table(purchase$dist_to_ven, purchase$event_years)

### Extract year from date variable
purchase$event_dates <- as.Date(purchase$event_dt)
purchase$event_years <- format(purchase$event_dates, "%Y")
table(purchase$occpn_val, purchase$event_years)


### Merge Google Trends data
dat$date <- gsub(pattern = "-", replacement = "", dat$date) # Remove - from date

# Import trends data
trends <- read.csv("C:/Users/Christopher/Google Drive/GitHub/DataFest2016/Trends/Coldplay Chicago.csv", header = FALSE)
trends <- rename(trends, replace = c("V1" = "date"))
trends$date <- gsub(pattern = "-", replacement = "", trends$date) # Remove - from date
trends$date <- substr(trends$date, 1, 8)
trends$geonetwork_metro <- rep("Chicago IL", c(nrow(trends))) # Add region

test <- join(dat, trends, by = c("geonetwork_metro", "date"), type = "left")









