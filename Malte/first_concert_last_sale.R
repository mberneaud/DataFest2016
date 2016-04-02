# Min and Max functions
first.concert <- function(df, artist) {
  tmp <- filter(df, primary_act_name == artist)
  max <- max(tmp$sales_ord_create_dttm)
  min <- min(tmp$event_dt)
  collector <- c(min, max)
  return(collector)
}

