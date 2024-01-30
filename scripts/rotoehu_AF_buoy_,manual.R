library(tidyverse)
library(ggplot2)
source('./scripts/R/calc_AF.R')

threshold <- 2

###################################################################################
# buoy data
buoy <- read.csv('./data/Rotoehu_202101-202311_profiles.csv')

ggplot(buoy, aes(x = as.Date(DateTime), y = DOconc, color = DptSns)) +
  geom_point()

buoy <- buoy %>% 
  select(DateTime, DptSns, DOconc) %>% 
  rename(date = DateTime, 
         depth_m = DptSns,
         DO_gm3 = DOconc)
buoy <- na.omit(buoy)

# round buoy depths to nearest meter and average over the day
buoy <- buoy %>% 
  mutate(depth_rnd = round(depth_m),
         day = as.Date(date)) %>% 
  group_by(depth_rnd, day) %>% 
  mutate(avg_do = mean(DO_gm3, na.rm = TRUE))

ggplot(buoy, aes(x = DO_gm3, y = avg_do, color = depth_m)) +
  geom_point() +
  geom_abline(aes(intercept = 0, slope = 1))

buoy_oxy <- buoy %>% 
  distinct(day, depth_rnd, .keep_all = TRUE) %>% 
  select(day, depth_rnd, avg_do) %>% 
  rename(date = day,
         depth_m = depth_rnd,
         DO_gm3 = avg_do)

AF_buoy <- calc_af(threshold = threshold, oxy_df = buoy_oxy, lake_name = 'Rotoehu', dat_type = 'hf')
AF_buoy

#####################################################################################
# monitoring data
monthly_df <- 'C:/Users/wwoelmer/Desktop/BayOfPlenty/data/processed_data/BoP_ctd_2003_2022.csv'
lake_name <- 'Rotoehu'
max_depth <- 13.5

monitor_data <- read.csv(monthly_df)
monitor_data <- monitor_data %>% 
  dplyr::filter(lake==lake_name) %>% 
  select(date, depth_m, DO_gm3) %>% 
  dplyr::filter(depth_m < max_depth)

ggplot(monitor_data, aes(x = as.Date(date), y = DO_gm3, color = depth_m)) +
  geom_point() 

ggplot(monitor_data, aes(x = as.Date(date), y = DO_gm3, color = as.factor(depth_m))) +
  geom_point() +
  geom_line() +
  facet_wrap(~as.factor(depth_m))

AF_monitoring <- calc_af(threshold = threshold, oxy_df = monitor_data,
                         lake_name = lake_name, dat_type = 'monthly')
AF_monitoring





