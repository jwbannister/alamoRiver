# do.R -- data analysis for Alamo River met data
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 
# Analyze data from met stations near Alamo River to determine u* and z0 values
load_all()
library(dplyr)
library(reshape2)
library(ggplot2)
library(lubridate)
load("./data-clean/met_data.RData")

ws_min <- 1 # low windspeed data cutoff
dir_sd_max <- 11.25 / 2 # wind direction standard deviation cutoff
dir_sd_flag <- FALSE # should data be limited by directional variation?

# eliminate all low wind speed data - hours with speed at 2m anemometer
# less than assigned minimum (above)
df4 <- filter(df4, ws.2m > ws_min)

df4[df4$deployment==1003 & df4$datetime < '2016-01-28 16:00', ]$wd.6m <- 
   flip_wind(df4[df4$deployment==1003 & 
            df4$datetime < '2016-01-28 16:00', ]$wd.6m)
df4 <- assign_cardinal(df4, 3)

# eliminate data hours with wind direction standard deviation greater than
# 22.5deg (indicates swirling winds for that hour). 
if (dir_sd_flag==TRUE) df4 <- df4[df4$wd.sd.6m < dir_sd_max, ]

names(df4)[names(df4)=="ws.15cm"] <- "ws.0m"
df_all <- df4
df_dec <- filter(df4, month(datetime)==12)
all_melt <- df_all %>% 
  melt(id.vars=c("datetime", "deployment", "wd.named"), 
       measure.vars=c("ws.6m", "ws.2m", "ws.1m", "ws.0m"),
       value.name="v")
all_melt$h <- as.numeric(substr(all_melt$variable, 4, 4))
# lowest anemometer is a height 0.15m (not 0m)
all_melt[all_melt$h==0, ]$h <- 0.15
all_melt$log.h <- log(all_melt$h)
dec_melt <- df_dec %>% 
  melt(id.vars=c("datetime", "deployment", "wd.named"), 
       measure.vars=c("ws.6m", "ws.2m", "ws.1m", "ws.0m"),
       value.name="v")
dec_melt$h <- as.numeric(substr(dec_melt$variable, 4, 4))
# lowest anemometer is a height 0.15m (not 0m)
dec_melt[dec_melt$h==0, ]$h <- 0.15
dec_melt$log.h <- log(dec_melt$h)

all_hourly <- build_hourly_lm(all_melt, 0.4) %>%
  select(datetime, deployment, wd, z_0, u_star) %>%
  inner_join(select(df4, datetime, deployment, ws.6m, wd.6m), 
             by=c("datetime", "deployment"))
dec_hourly <- build_hourly_lm(dec_melt, 0.4) %>%
  select(datetime, deployment, wd, z_0, u_star) %>%
  inner_join(select(df4, datetime, deployment, ws.6m, wd.6m), 
             by=c("datetime", "deployment"))

dir.ref <- direction_reference()

all_tbl <- build_average_table(all_hourly)
dec_tbl <- build_average_table(dec_hourly)

path <- "~/dropbox/Salton Sea CALPUFF/toKent/"
write.csv(all_hourly, file=paste0(path, "all_hourly_ustar.csv"))
write.csv(dec_hourly, file=paste0(path, "dec_hourly_ustar.csv"))
write.csv(dec_tbl, file=paste0(path, "dec_directional_ustar.csv"))
write.csv(all_tbl, file=paste0(path, "all_directional_ustar.csv"))

p_bar <- df_tbl %>% 
  ggplot(aes(x = angle, y = z_0.avg)) +
  geom_point() +
  scale_x_continuous(breaks=unlist(lapply(dir.ref, function(x) x[[3]])),
                     labels = waiver()) +
  facet_grid(deployment ~ ., scales="free_y") +
  geom_errorbar(aes(ymax=z_0.avg + z_0.sd, 
                    ymin=ifelse(z_0.avg - z_0.sd>0, z_0.avg-z_0.sd, 0),
                  color="+/- sd"), width=10) 

p_rose_all <- all_tbl %>% 
  ggplot(aes(x = angle, y = u_star.avg)) +
  geom_bar(stat='identity', color="black") + 
  scale_x_discrete(breaks=unlist(lapply(dir.ref, function(x) x[[3]])),
                   labels = waiver(), 
                   drop=FALSE) +
  coord_polar(start=-11.25*2*pi/360) +
  facet_grid(deployment ~ ., scales="free_y") +
  ggtitle("All Available Met Data") + xlab("") + ylab("u*") 
png(filename=paste0(path, "all_met_ustar_rose.png"), height=9, width=4,
    units="in", res=300)
print(p_rose_all)
dev.off()
  
p_rose_dec <- dec_tbl %>% 
  ggplot(aes(x = angle, y = u_star.avg)) +
  geom_bar(stat='identity', color="black") + 
  scale_x_discrete(breaks=unlist(lapply(dir.ref, function(x) x[[3]])),
                   labels = waiver(), 
                   drop=FALSE) +
  coord_polar(start=-11.25*2*pi/360) +
  facet_grid(deployment ~ ., scales="free_y") +
  ggtitle("December Met Data") + xlab("") + ylab("u*") 
png(filename=paste0(path, "dec_met_ustar_rose.png"), height=9, width=4,
    units="in", res=300)
print(p_rose_dec)
dev.off()

#j <- "SW"
#p1 <- df_melt %>% filter(deployment==1001, wd.named==j) %>% arrange(h) %>%
#  ggplot(aes(x=v, y=log.h, group=datetime)) +
#  geom_path() +
#  geom_point(aes(color=datetime))

