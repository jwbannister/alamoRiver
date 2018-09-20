# do.R -- data analysis for Alamo River met data
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 
# Analyze data from met stations near Alamo River to determine u* and z0 values
airsci_loc <- Sys.getenv("R_AIRSCI")
suppressMessages(devtools::load_all(airsci_loc))
load_all()
library(tidyverse)
library(ggplot2)
library(lubridate)

query1 <- paste0("SELECT d.deployment, datetime, ",
                "ws_1m, ws_2m, ws_6m, wd_6m, wd_sd_6m ",
                "FROM met.met_1hour m JOIN info.deployments d ",
                "ON m.deployment_id=d.deployment_id ", 
                "WHERE d.deployment IN ('1101', '1102', '1103');")
df1 <- query_db("saltonsea", query1)
df2 <- df1[complete.cases(df1), ]
colnames(df2) <- gsub("_", ".", colnames(df2))

cuts <- seq(0, 5, 0.5)
plts <- vector(mode="list", length=length(cuts))
names(plts) <- as.character(cuts)

ws_cut <- 8 # wind speed cutoff in m/s
ws_min <- ws_cut # low windspeed data cutoff
dir_sd_max <- 11.25 / 2 # wind direction standard deviation cutoff
dir_sd_flag <- FALSE # should data be limited by directional variation?

# eliminate all low wind speed data - hours with speed at 2m anemometer
# less than assigned minimum (above)
df4 <- filter(df2, ws.2m > ws_min)

#df4[df4$deployment==1003 & df4$datetime < '2016-01-28 16:00', ]$wd.6m <- 
#   flip_wind(df4[df4$deployment==1003 & 
#            df4$datetime < '2016-01-28 16:00', ]$wd.6m)
df4 <- assign_cardinal(df4, 6)

# eliminate data hours with wind direction standard deviation greater than
# 22.5deg (indicates swirling winds for that hour). 
if (dir_sd_flag==TRUE) df4 <- df4[df4$wd.sd.6m < dir_sd_max, ]

melt_df <- df4 %>% select(-wd.sd.6m) %>%
    gather(key="key", value="value", -deployment, -datetime, -wd.6m, -wd.named) 
melt_df$h <- as.numeric(substr(melt_df$key, 4, 4))
melt_df$log.h <- log(melt_df$h)

trim_1m=T
if (trim_1m){
    lm_data <- filter(melt_df, h!=1)
} else{
    lm_data <- melt_df
}

hourly_df <- build_hourly_lm(lm_data, 0.4) %>%
  select(datetime, deployment, wd, z_0, u_star) %>%
  inner_join(select(df4, datetime, deployment, ws.6m, wd.6m), 
             by=c("datetime", "deployment"))
hourly_df$wd <- factor(hourly_df$wd, levels=c("E", "ESE", "SE", "SSE", "S", 
                                              "SSW", "SW", "WSW", "W", "WNW", "NW", 
                                              "NNW", "N", "NNE", "NE", "ENE"))

all_tbl <- build_average_table(hourly_df)
z0_avg <- all_tbl %>% group_by(deployment) %>%
  summarize(mean.z0=mean(z_0.avg))

dir.ref <- direction_reference()
mets <- unique(all_tbl$deployment)
rose_plots <- vector(mode="list", length=length(mets))
names(rose_plots) <- mets
wind_plots <- rose_plots
dir_plots <- rose_plots
trim_plots <- rose_plots
sub_ttle <- paste0("Anemometer heights = ", 
                   paste(unique(lm_data$h), collapse=", "), " m")
for (j in mets){
  rng <- round(range(filter(all_tbl, deployment==j)$z_0.avg), 4)
    tmp <- all_tbl %>% filter(deployment==j) %>%
        mutate(lower_bar=z_0.avg-z_0.sd)
    tmp$bar_min <- ifelse(tmp$lower_bar<0, 0, tmp$lower_bar)
rose_plots[[j]] <- tmp %>%
  ggplot(aes(x = angle, y = z_0.avg)) +
  geom_bar(stat='identity') + 
  scale_x_discrete(breaks=unlist(lapply(dir.ref, function(x) x[[3]])),
                   labels = waiver(), 
                   drop=FALSE) +
  coord_polar(start=-11.25*2*pi/360) +
  ggtitle(paste0(j, " - wind v > ", ws_cut, "m/s"),
          subtitle=sub_ttle) +
  ylim(c(0, max(all_tbl$z_0.avg+all_tbl$z_0.sd))) +
  xlab("") + ylab("z_0 (m)") +
  geom_errorbar(mapping=aes(x=angle,  
                           ymin=bar_min, ymax=z_0.avg+z_0.sd),
               color='red', width=2) +
  geom_text(mapping=aes(x=angle, y=max(all_tbl$z_0.avg+all_tbl$z_0.sd), 
                        label=n), 
            color='blue')
  
wind_plots[[j]] <- df4 %>% filter(deployment==j) %>%
  plot_rose(., 'ws.6m', 'wd.6m', 
            plot.title=paste0(j, " - wind v > ", ws_cut, "m/s"))
df_dir <- hourly_df %>% filter(deployment==j) %>% group_by(wd) %>%
  summarize(n=length(z_0), mean.z0=mean(z_0), median.z0=median(z_0), 
            geom.z0=psych::geometric.mean(z_0))
dir_plots[[j]] <- hourly_df %>% filter(deployment==j) %>%
  ggplot(aes(x=wd, y=z_0)) +
  geom_jitter(alpha=0.5, width=0.2, height=0) +
  geom_point(data=df_dir, mapping=aes(y=mean.z0, color="Mean z0")) +
  geom_point(data=df_dir, mapping=aes(y=median.z0, color="Median z0")) +
  geom_point(data=df_dir, mapping=aes(y=geom.z0, color="Geom z0")) +
  geom_text(data=df_dir, mapping=aes(x=wd, y=-0.001, label=n)) +
  ggtitle(paste0(j, " - wind v > ", ws_cut, "m/s (no outliers removed)"))

df_trim <- hourly_df %>% filter(deployment==j, z_0<0.01, 
                                !(wd %in% c("E", "ENE", "ESE"))) %>%
group_by(wd) %>%
summarize(n=length(z_0), mean.z0=mean(z_0), median.z0=median(z_0), 
          geom.z0=psych::geometric.mean(z_0)) %>% ungroup()
trim_avg <- df_trim %>% 
  summarize(mean.z0=mean(mean.z0), median.z0=mean(median.z0), 
            geom.z0=mean(geom.z0))
trim_plots[[j]] <- hourly_df %>% filter(deployment==j, z_0<0.01,
                                 !(wd %in% c("E", "ENE", "ESE"))) %>%
  ggplot(aes(x=wd, y=z_0)) +
  geom_jitter(alpha=0.5, width=0.2, height=0) +
  geom_point(data=df_trim, mapping=aes(y=mean.z0, color='arith'), size=6) +
  geom_point(data=df_trim, mapping=aes(y=median.z0, color='median'), size=6) +
  geom_point(data=df_trim, mapping=aes(y=geom.z0, color='geom'), size=6) +
  geom_text(data=df_trim, mapping=aes(x=wd, y=-0.0005, label=n)) +
  ggtitle(paste0(j, " - wind v > ", ws_cut, "m/s (z0 > 0.01 removed)")) +
  geom_hline(yintercept=trim_avg$mean.z0, color='red') + 
  geom_hline(yintercept=trim_avg$median.z0, color='blue') +
  geom_hline(yintercept=trim_avg$geom.z0, color='green') +
  scale_color_manual(values=c('red', 'blue', 'green')) +
  geom_label(mapping=aes(x='SE', y=0.01, hjust='left'),
                        label=paste0("Average of Arithmetic Means = ", 
                                     round(trim_avg$mean.z0, 6)),
                        color='red') +
  geom_label(mapping=aes(x='SE', y=0.009, hjust='left'),
                        label=paste0("Average of Medians = ", 
                                     round(trim_avg$median.z0, 6)),
                         color='blue') +
  geom_label(mapping=aes(x='SE', y=0.008, hjust='left'), 
                        label=paste0("Average of Geometric Means = ", 
                                     round(trim_avg$geom.z0, 6)),
                         color='green') +
  scale_y_continuous(breaks=seq(0, 0.01, .001))
}
for (j in unique(df4$deployment)){
    png(paste0("~/Desktop/z0_", j, "_cut", ws_cut, ".png"), 
        height=6, width=6, units="in", res=300)
    print(rose_plots[[j]])
    dev.off()
    png(paste0("~/Desktop/wind_", j, "_cut", ws_cut, ".png"), 
        height=6, width=6, units="in", res=300)
    print(wind_plots[[j]])
    dev.off()
    png(paste0("~/Desktop/dir_", j, "_cut", ws_cut, ".png"), 
        height=6, width=6, units="in", res=300)
    print(dir_plots[[j]])
    dev.off()
}
