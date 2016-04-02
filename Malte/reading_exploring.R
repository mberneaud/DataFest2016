# Reading in and exploring the data
# Author: Malte Berneaud

library(data.table)
library(dplyr)
setwd("/home/malte/Git/DataFest2016")
pur <- fread("Data/data and codebook/approved_data_purchase-v5.csv")

ga <- fread("Data/data and codebook/approved_ga_data_v2.csv")

ads <- fread("Data/data and codebook/approved_adwords_v3.csv")

# alternatives einlesen mit read.csv
#purchase <- read.csv("Data/data and codebook/approved_data_purchase-v5.csv")

acdc <- filter(pur, event_name== "AC/DC")
acdc$price_ticket <- acdc$trans_face_val_amt / acdc$tickets_purchased_qty

acdc <- group_by(acdc, venue_state)


prices <- summarise(acdc, mean_price = mean(price_ticket, na.rm = TRUE))


# trying it on the entire data set 
pur$price_ticket <- pur$trans_face_val_amt / pur$tickets_purchased_qty

summary(pur$price_ticket)
quantile(pur$price_ticket)
hist(pur$price_ticket, breaks = 20)


# 
distances <- pur[!is.na(pur$dist_to_ven)]

table(pur$major_cat_name)
table(pur$minor_cat_name)

misc.events <- pur %>% filter(major_cat_name == "MISC") %>% select(minor_cat_name)


# trying it on the entire data set 
pur$price_ticket <- pur$trans_face_val_amt / pur$tickets_purchased_qty


pur <- arrange(pur, desc(price_ticket))
head(unique(pur$primary_act_name), n=25)
head(pur$price_ticket)

