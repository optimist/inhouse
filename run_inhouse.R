source("posiciones.R")
source("utils.R")


pos <- pos_query(c("2016-10-14"))
#saveRDS(pos_data, "./data/pos_data.RDS")
#pos_data <- readRDS("./data/pos_data.RDS")

pos <- pos_clean(pos)

asset_stats <- pos_asset_stats(pos)

