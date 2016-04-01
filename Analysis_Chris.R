### Load packages
packages <- c("data.table")

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
data <- fread("data_sample.csv")

### Extract year from date variable
purchase$event_dates <- as.Date(purchase$event_dt)
purchase$event_years <- format(purchase$event_dates, "%Y")
table(purchase$occpn_val, purchase$event_years)
