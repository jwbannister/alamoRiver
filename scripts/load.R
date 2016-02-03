# load.R -- load data related to Alamo River emissions estimate
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 
load_all()
library(dplyr)

data_path <- "./data-raw/"
data_files <- c("1002_Hour_Manual_20160129.dat",
                "1003_Hour_Manual_20160128_C.dat",
                "1004_Hour_Manual_20160201.dat")
df1 <- multi_read(data_path, data_files)
df1 <- select(df1, -V2, -V9, -V10, -V11)
colnames(df1) <- c("timestamp", "ws.6m", "wd.6m", "wd.6m.sd",
                   "ws.2m", "ws.1m", "batt", "file")
df1$site <- substr(df1$file, 1, 4)
df1 <- select(df1, -file)
df1 <- assign_cardinal(df1, 3)

save(df1, file="./data-clean/met_data.RData")

