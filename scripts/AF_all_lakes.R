# loop through all Bay of Plenty lakes with both buoy and monthly monitoring data and calculate anoxic factor

library(ggplot2)
library(tidyverse)
source('./scripts/R/calc_AF.R')

lakes <- c('Okareka', 'Okaro','Rerewhakaaitu', 'Rotoehu', 'Rotoiti', 'Rotorua', 'Tarawera')
method <- c('hf', 'monthly')
threshold <- 2
data_dir <- './data'

out_df <- data.frame('year' = NULL,
                     'AF' = NULL,
                     'n_months' = NULL,
                     'max_depth' = NULL,
                     'threshold' = NULL,
                     'lake' = NULL,
                     'method' = NULL)

for(j in 1:length(lakes)){
  print(lakes[j])
  for (k in 1:length(method)) {
    print(method[k])
    if(method[k]=='hf'){
      fls <- list.files(data_dir, pattern = lakes[j])
      
      dat <- read.csv(paste0(data_dir, "/", fls))
      dat <- dat %>% 
        select(DateTime, DptSns, DOconc) 
      dat <- na.omit(dat)
      
      # round buoy depths to nearest meter and average over the day
      dat <- dat %>% 
        mutate(depth_rnd = round(DptSns),
               day = as.Date(DateTime)) %>% 
        group_by(depth_rnd, day) %>% 
        mutate(avg_do = mean(DOconc, na.rm = TRUE))
      
      dat <- dat %>% 
        distinct(day, depth_rnd, .keep_all = TRUE) %>% 
        select(day, depth_rnd, avg_do) %>% 
        rename(date = day,
               depth_m = depth_rnd,
               DO_gm3 = avg_do)
    }else{
      monthly_df <- './data//BoP_ctd_2003_2022.csv' # file created in the BayOfPlenty project folder
      dat <- read.csv(monthly_df)
      dat <- dat %>% 
        dplyr::filter(lake==lakes[j]) %>% 
        select(date, depth_m, DO_gm3) 
      
      max_depth <- max(dat$depth_m)
      dat <- dat %>% 
        dplyr::filter(depth_m < max_depth)
    }
    AF_buoy <- calc_af(threshold = threshold, 
                       oxy_df = dat, 
                       lake_name = lakes[j], 
                       method = method[k])
    out_df <- rbind(out_df, AF_buoy)
  }
}


# filter to when there are estimates for both hf and monthly, only two years at the moment, womp
out_sub <- out_df %>% 
  filter(year > 2020 & year < 2023) %>% 
  mutate(AF = ifelse(AF==0, 0.1, AF))

# add mixing status
mix <- data.frame(lake = c("Okareka", "Okaro", "Rerewhakaaitu", "Rotoehu", "Rotoiti",
                           "Rotorua", "Tarawera"),
                  mixing_state = c('mono', 'mono', 'poly', 'poly', 'mono',
                                   'poly', 'mono'))

out_df <- left_join(out_df, mix, by = 'lake')

out_df %>% 
  filter(year > 2020 & year < 2023) %>% 
  mutate(AF = ifelse(AF==0, 0.1, AF)) %>% 
  ggplot(aes(x = as.factor(year), y = AF, fill = method)) +
  geom_col(position = 'dodge') +
  facet_wrap(mixing_state~lake, scales = 'free') +
  theme_bw()

library(plotly)

ggplotly(out_df %>% 
  filter(method=='monthly') %>% 
  ggplot(aes(x = as.factor(year), y = AF, group = lake, color = lake)) +
  geom_line() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(~mixing_state) + 
    ggtitle('monthly data')) 

ggplotly(out_df %>% 
           ggplot(aes(x = as.factor(year), y = AF, group = lake, color = lake, linetype = fct_rev(method))) +
           geom_line() +
           geom_point() +
           theme_bw() +
           theme(axis.text.x = element_text(angle = 45)) +
           facet_wrap(~mixing_state) + 
           ggtitle('monthly data + buoy data'))

out_df %>% 
  ggplot(aes(x = as.factor(year), y = AF, group = paste0(lake, method), color = lake, linetype = fct_rev(method))) +
  geom_line(size = 1, lineend = 'round') +
  geom_point(size = 3) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = -0.001),
        text = element_text(size = 18)) +
  facet_wrap(~mixing_state) + 
  ggtitle('monthly data + buoy data') +
  labs(linetype = 'method') +
  xlab('')

ggplotly(out_df %>% 
           filter(method=='hf') %>% 
           ggplot(aes(x = as.factor(year), y = AF, group = lake, color = lake)) +
           geom_line() +theme_bw() + 
           facet_wrap(~mixing_state) + 
           ggtitle('buoy data'))
