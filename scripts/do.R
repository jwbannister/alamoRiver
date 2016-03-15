# do.R -- data analysis for Alamo River met data
# John Bannister
# Created 02/02/2016 -- see git for revision history
# 
# Analyze data from met stations near Alamo River to determine u* and z0 values
load_all()
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
load("./data-clean/met_data.RData")

# check for potential problem data points by looking at station battery voltage
batt_check <- df1$batt > mean(df1$batt) - 4*sd(df1$batt) | 
  df1$batt < mean(df1$batt) + 4*sd(df1$batt)
print(paste0(sum(df1$batt < mean(df1$batt) - 4*sd(df1$batt) | 
                 df1$batt > mean(df1$batt) + 4*sd(df1$batt)),
             " data points with unusual battery voltage.")) 
df1 <- df1[batt_check, ]

ws_min <- 1 # low windspeed data cutoff
dir_sd_max <- 11.25 / 2 # wind direction standard deviation cutoff
dir_var_flag <- FALSE # should data be limited by directional variation?

# eliminate all low wind speed data - hours with speed at lowest anemometer
# less than 1 m/s
df1 <- filter(df1, ws.1m > ws_min)

# eliminate data hours with wind direction standard deviation greater than
# 22.5deg (indicates swirling winds for that hour). 
if (dir_var_flag==TRUE) df1 <- df1[df1$wd.6m.sd < dir_sd_max, ]

df_melt <- df1 %>% 
  melt(id.vars=c("timestamp", "site", "wd.named"), 
       measure.vars=c("ws.6m", "ws.2m", "ws.1m"),
       value.name="v")
df_melt$h <- as.numeric(substr(df_melt$variable, 4, 4))
df_melt[df_melt$h==1, ]$h <- 0.15
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
  do(u_star.avg = mean(.$u_star), u_star.sd = sd(.$u_star),
     z_0.avg = mean(.$z_0), z_0.sd = sd(.$z_0), n=nrow(.)) %>% ungroup()
dir.ref <- direction_reference()
for (i in 1:nrow(df_tbl)){
  df_tbl$angle[i] <- dir.ref[names(dir.ref)==df_tbl$wd[i]][[1]][3]
}
df_tbl$u_star.avg <- unlist(df_tbl$u_star.avg)
df_tbl$u_star.sd <- unlist(df_tbl$u_star.sd)
df_tbl$z_0.sd <- unlist(df_tbl$z_0.sd)
df_tbl$z_0.avg <- unlist(df_tbl$z_0.avg)
df_tbl$n <- unlist(df_tbl$n)
df_tbl$angle <- unlist(df_tbl$angle)

for (i in unique(df_tbl$site)){
  p_u <- df_tbl %>% filter(site==i) %>%
    ggplot(aes(x = angle, y = u_star.avg)) +
    geom_bar(stat='identity', color="black") + 
    scale_x_continuous(breaks=unlist(lapply(dir.ref, function(x) x[[3]])),
                       labels = waiver()) +
    ylim(0, max(df_tbl$u_star.avg)) +
#    coord_polar(start=-11.25*2*pi/360) +
    ggtitle(paste0(i, " u*")) + xlab("") + ylab("u*") 
  p_z <- df_tbl %>% filter(site==i) %>%
    ggplot(aes(x = angle, y = z_0.avg)) +
    geom_bar(stat='identity', color="black") + 
    scale_x_continuous(breaks=unlist(lapply(dir.ref, function(x) x[[3]])),
                       labels = waiver()) +
    ylim(0, max(df_tbl$z_0.avg)) +
 #   coord_polar(start=-11.25*2*pi/360) +
    geom_errorbar(aes(ymax=z_0.avg + z_0.sd, 
                      ymin=ifelse(z_0.avg - z_0.sd>0, z_0.avg-z_0.sd, 0),
                      color="+/- sd"), width=10) +
    geom_label(aes(label=n, fill="# of data points")) +
    labs(fill="", color="") + 
    ggtitle(paste0(i, " z0")) + xlab("") + ylab("z0") 
  assign(paste0("p", i, "_u"), p_u)
  assign(paste0("p", i, "_z"), p_z)
}
grid.arrange(p1002_z, p1003_z, p1004_z, 
             ncol=1, nrow=3, 
             top=paste0("Salton Met Stations\n",
                        "1m wind speed > ", ws_min, " m/s\n",
                        "Hourly Direction Variance Filter = ", 
                        as.character(dir_var_flag))) 

p1 <- df_melt %>% filter(site==1003, wd.named=="W") %>%
  ggplot(aes(x=v, y=log.h, color=timestamp)) + 
  geom_path() +
  geom_point()
