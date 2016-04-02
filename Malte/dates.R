

gr.purch <- group_by(pur, event_id)

gr.purch <- summarise(gr.purch, last.purch = max(sales_ord_create_dttm))


van.halen <- filter(pur, primary_act_name == "Van Halen")
min(van.halen$event_dt)
max(van.halen$event_dt)
unique(van.halen$event_dt)

# calculating ticket price



van.halen <- group_by(van.halen, event_id)
van.halen.d <- summarise(van.halen, last.sale = max(sales_ord_create_dttm),
                         first.concert = min(event_date_time))
van.halen.d <- arrange(van.halen.d, first.concert)

van.halen <- merge(van.halen, van.halen.d, by= "event_id")
