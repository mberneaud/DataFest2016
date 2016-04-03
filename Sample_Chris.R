#packages <- c()

### Load and if required install packages
# for (p in packages) {
#   if (p %in% installed.packages()[,1]) require(p, character.only=T)
#   else {
#     install.packages(p)
#     require(p, character.only=T)
#   }
# }

library(data.table)

setwd("C:/Users/Christopher/Google Drive/GitHub/DataFest Munich")

adwords <- fread("data and codebook/approved_adwords_v3.csv")
adwords_sample <- adwords[sample(1:nrow(adwords), 10000, replace=FALSE),]
write.csv(adwords_sample, "adwords_sample.csv")

purchase <- fread("data and codebook/approved_data_purchase-v5.csv")
purchase_sample <- purchase[sample(1:nrow(purchase), 10000, replace=FALSE),]
write.csv(purchase_sample, "purchase_sample.csv")


data <- fread("data and codebook/approved_ga_data_v2.csv")
data_sample <- data[sample(1:nrow(data), 10000, replace=FALSE),]
write.csv(data_sample, "data_sample.csv")

