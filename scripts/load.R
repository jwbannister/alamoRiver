# load.R -- load data related to Alamo River emissions estimate
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 
load_all()
library(dplyr)

data_path <- "./data-raw/"
data_files <- c("1001_CR6_Hour.dat", "1002_CR6_Hour.dat",
                "1003_CR6_Hour.dat")
df1 <- multi_read(data_path, data_files)
df1 <- select(df1, V1, V3, V4, V5, V6, V7, V8, V9)
colnames(df1) <- c("timestamp", "ws.6m", "wd.6m", "wd.6m.sd",
                   "ws.2m", "ws.0m", "batt", "site")
df2 <- read.csv("./data-raw/1004_CR6_Hour.dat", skip=4, header=FALSE)
df2 <- select(df2, V1, V3, V4, V5, V6, V7, V8, V9, V10, V11)
colnames(df2) <- c("timestamp", "ws.6m", "wd.6m", "wd.6m.sd",
                   "ws.4m", "ws.2m", "ws.1m", "ws.0m", "batt", "site")
# for some data in 1004 data file, site is incorrectly tagged as 1001
df2$site <- rep(1004, nrow(df2))
df1$ws.4m <- rep(NA, nrow(df1))
df1$ws.1m <- rep(NA, nrow(df1))
df3 <- rbind(df1, df2)
df3 <- assign_cardinal(df3, 3)

df4 <- read.csv("./data-raw/dump.csv")
df4 <- assign_cardinal(df4, 3)
df4[df4$ws_1m=='\\N', ]$ws_1m <- NA
df4$ws_1m <- round(as.numeric(df4$ws_1m), 2)
df4[df4$ws_15cm=='\\N', ]$ws_15cm <- NA
df4$ws_15cm <- round(as.numeric(df4$ws_15cm), 2)
names(df4) <- gsub("_", ".", names(df4))

save(df3, df4,  file="./data-clean/met_data.RData")
