# Merging csv data together

setwd("/home/malte/Git/DataFest2016/Malte")


files <- list.files(pattern = "*.csv")
df <- data.frame()
for(i in seq_along(files)) {
  tmp <- read.csv(files[i], skip = 4L, header = TRUE)
  tmp <- head(tmp, n=-12)
  tmp$artist <- substr(names(tmp)[2], 9, 100L)
  names(tmp) <- c("week", "interest", "artist")
  df <- rbind(df, tmp)
}

# deleting files which are too small
system(". -name *.csv -size -24k", intern = TRUE)
