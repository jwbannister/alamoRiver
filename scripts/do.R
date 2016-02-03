# do.R -- data analysis for Alamo River met data
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 
# Analyze data from met stations near Alamo River to determine u* and z0 values
load_all()
library(dplyr)
library(reshape2)
library(ggplot2)
load("./data-clean/met_data.RData")

# check for potential problem data points by looking at station battery voltage
batt_check <- df1$batt > mean(df1$batt) - 4*sd(df1$batt) | 
  df1$batt < mean(df1$batt) + 4*sd(df1$batt)
print(paste0(sum(df1$batt < mean(df1$batt) - 4*sd(df1$batt) | 
                 df1$batt > mean(df1$batt) + 4*sd(df1$batt)),
             " data points with unusual battery voltage.")) 
df1 <- df1[batt_check, ]

# eliminate data hours with wind direction standard deviation greater than
# 22.5deg (indicates swirling winds for that hour). 
df1 <- df1[df1$wd.6m.sd < 22.5, ]

df_melt <- df1 %>% 
  melt(id.vars=c("timestamp", "site", "wd.named"), 
       measure.vars=c("ws.6m", "ws.2m", "ws.1m"),
       value.name="v")
df_melt$h <- as.numeric(substr(df_melt$variable, 4, 4))
df_melt <- df_melt[df_melt$v!=0, ]
df_melt$log.h <- log(df_melt$h)

df_sum <- df_melt %>% group_by(site, timestamp) %>%
  do(model = lm(log.h ~ v, data=.), n = nrow(.), 
     num.dirs = length(unique(.$wd.named)),
     wd = unique(.$wd.named)) %>%
  ungroup()
df_sum$n <- unlist(df_sum$n)
df_sum$num.dirs <- unlist(df_sum$num.dirs)
df_sum$wd <- unlist(df_sum$wd)
df_sum$intercept <- lapply(df_sum$model, function(x) coefficients(x)[1])
df_sum$intercept <- unlist(df_sum$intercept)
df_sum$slope <- lapply(df_sum$model, function(x) coefficients(x)[2])
df_sum$slope <- unlist(df_sum$slope)
df_sum$z_0 <- exp(df_sum$intercept)
k <- 0.4 # Von Karman's constant
df_sum$u_star <- k / df_sum$slope
df_sum <- filter(df_sum, n==3)
df_sum <- filter(df_sum, num.dirs==1)
print(paste0(nrow(filter(df_sum, slope < 0)), 
             " data points thrown out due to negative slope."))
df_sum <- filter(df_sum, slope > 0)

df_tbl <- df_sum %>% group_by(site, wd) %>%
  summarize(u_star.avg = mean(u_star), z_0.avg = mean(z_0))
