library(data.table)
library(ggplot2)
library(doBy)

## Prepare folder and load data

setwd("C:/Users/Benji/Desktop/DataFest2016")
#adwords <- fread("adwords_sample.csv")
#data <- fread("data_sample.csv")
purchase <- fread("purchase_sample.csv")

## Only focus on Concerts
Concerts <- subset(purchase, major_cat_name=="CONCERTS" & la_event_type_cat=="CONCERTS")

## Get rid of unnecessary/NA variables
Concerts$event_id <- NULL
Concerts$secondary_act_id <- NULL
Concerts$purch_party_lkup_id <- NULL
Concerts$primary_act_id <- NULL
Concerts$secondary_act_name <- NULL
Concerts$major_cat_name <- NULL
Concerts$la_event_type_cat <- NULL
Concerts$event_disp_name <- NULL
Concerts$ticket_text <- NULL
Concerts$print_flg <- NULL
Concerts$la_valid_tkt_event_flg <- NULL
Concerts$web_session_cookie_val <- NULL
Concerts$gndr_cd <- NULL
Concerts$age_yr <- NULL
Concerts$income_amt <- NULL
Concerts$edu_val <- NULL
Concerts$edu_1st_indv_val <- NULL
Concerts$edu_2nd_indv_val <- NULL
Concerts$adults_in_hh_num <- NULL
Concerts$married_ind <- NULL
Concerts$child_present_ind <- NULL
Concerts$home_owner_ind <- NULL
Concerts$occpn_val <- NULL
Concerts$occpn_1st_val <- NULL
Concerts$occpn_2nd_val <- NULL
Concerts$dist_to_ven <- NULL

## Create a variable of Ticket Price (Price/Quantity purchased)
Concerts$TicketPr <- Concerts$trans_face_val_amt/Concerts$tickets_purchased_qty

## Collapse on unique events with average ticket price
conc2 <- summaryBy(TicketPr ~ primary_act_name + event_date_time, data = Concerts, FUN=function(x) {c(m=mean(x))})
conc2 <- conc2[order(-TicketPr.m),]

## Check if duplicates
sum(duplicated(conc2, by=c("primary_act_name", "event_date_time")))

