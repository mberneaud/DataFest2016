library(dplyr)

pur <- fread("Data/data and codebook/approved_data_purchase-v5.csv")
pur <- filter(pur, major_cat_name == "CONCERTS" &
                la_event_type_cat == "CONCERTS")

# working on Van Halen data

van.halen <- filter(pur, primary_act_name == "Van Halen")
min(van.halen$event_date_time)
max(van.halen$event_dt)
unique(van.halen$event_dt)

# calculating ticket price
van.halen$tprice <- van.halen$trans_face_val_amt / van.halen$tickets_purchased_qty


van.halen <- group_by(van.halen, event_id)
van.halen.d <- summarise(van.halen, last.sale = max(sales_ord_create_dttm),
                         first.concert = min(event_date_time))
van.halen.d <- arrange(van.halen.d, first.concert)
van.halen <- merge(van.halen, van.halen.d, by= "event_id")

# Ticket prices for all obs
pur$tprice <- pur$trans_face_val_amt / pur$tickets_purchased_qty
pur <- group_by(pur, event_id)
pur.p <- summarise(pur, mean.tprice = mean(tprice, na.rm = TRUE))
pur <- merge(pur, pur.p, by = "event_id")
pur <- arrange(pur, desc(mean.tprice))
head(pur$primary_act_name)


# Sting, Maroon 5, Jennifer Lopez, Van Halen

sting <- filter(pur, primary_act_name == "Sting")
min(sting$event_dt)
max(sting$sales_ord_create_dttm)

# Maroon 5
maroon <- filter(pur, primary_act_name == "Maroon 5")
table(maroon$event_name)
min(maroon$event_dt)
unique(maroon$event_dt)
length(unique(maroon$event_dt))
max(maroon$sales_ord_create_dttm)

maroon2 <- select(maroon, event_dt, event_id)
maroon2 <- arrange(maroon2, desc(event_dt))

table(maroon2$event_dt, maroon2$event_id)

# sourcing dates functionsales_ord_create_dttm
source("Malte/first_concert_last_sale.R")

first.concert(pur, "Maroon 5")

maroon2 <- filter(maroon, event_dt == "2015-03-05")
maroon2$dates <- substr(maroon2$sales_ord_create_dttm, 1, 10)
maroon2 <- group_by(maroon2, dates)
daily.sales <- summarise(maroon2, daily.sales = sum(tickets_purchased_qty))

