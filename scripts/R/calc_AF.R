# note that a return of NA for AF indicates there were no observations below the threshold during the specified time period
# oxy_df = dataframe of oxygen observations with columns date, depth_m, DO_gm3
# method = the method used to collect data in oxy_df, either 'hf' for high-frequency or 'monthly' for monthly monitoring data
  # if 'monthly' observations will be linearly interpolated to the daily scale...is this a good idea??


calc_af <- function(threshold = 2, oxy_df, lake_name, method){
  
  library(tidyverse)
  library(readxl)
  library(pracma)
  
  
  years <- unique(year(oxy_df$date))
  AF_out = data.frame('year' = NULL,
                      'AF' = NULL,
                      'n_months' = NULL,
                      'max_depth' = NULL,
                      'threshold' = NULL,
                      'lake' = NULL,
                      'method' = NULL)
  
  ####### subset to each year
  for(i in 1:length(years)){
    print(paste0("year ", years[i]))
    oxy_sub <- oxy_df %>% 
      dplyr::mutate(year = year(date)) %>% 
      dplyr::filter(year==years[i]) %>% 
      select(-year) %>% 
      distinct(date, depth_m, .keep_all = TRUE)
    
    # make wide
    oxy_wide <- oxy_sub %>% 
      ungroup() %>% 
      pivot_wider(names_from = date, values_from = DO_gm3) %>% 
      select(-depth_m)
    
    oxy_wide <- as.matrix(oxy_wide)
    oxy_wide[oxy_wide=="NULL"] <- NA
    
    # interpolate missing days...probably not a good idea in with monthly observations
    num_days <- length(seq(yday(min(oxy_sub$date)), yday(max(oxy_sub$date)), 1))
    
    if(method!='hf'){
      oxy_interp = matrix(NA, 
                          ncol = num_days, 
                          nrow = nrow(oxy_wide))
      for (m in 1:nrow(oxy_interp)){
        oxy_interp[m,] <- interp1(yday(unique(oxy_sub$date)),
                                  oxy_wide[m, ],
                                  xi = seq(yday(min(oxy_sub$date)), yday(max(oxy_sub$date)), 1),
                                  method = 'spline')
      }
      
      oxy_interp <- na.omit(oxy_interp)
      
    }else{
      oxy_interp <- oxy_wide
    }
    
    # dataframe of NAs if above threshold and 1 if below threshold
    oxy_thresh <- ifelse(oxy_interp <= threshold, 1, NA)
    
    
    ########################################################################################
    # read in area
    bty <- read_excel('./data/Rotlakes_bathymetry.xls', skip = 1)
    colnames(bty) <- c('lake', 'depth_m', 'vol_to_bottom_m3', 'vol_at_countour_m3', 'planar_sa_m2', 
                       'model_sd_m2')
    
    bty <- bty %>% dplyr::filter(lake==lake_name)
    bty$depth_m <- abs(bty$depth_m)
    bty$planar_sa_m2 <- as.numeric(bty$planar_sa_m2)
    
    # interpolate to every  m to match oxygen data
    ####### sequence depths from surface (0 m) to lowest depth measurement in bathymetry
    areas.af <- approx(bty$depth_m, bty$planar_sa_m2, seq(0, max(bty$depth_m), 1))$y
  
    ## identify depths at which anoxia occurs
    seq.af <- NA
    try(seq.af <- na.contiguous(apply(oxy_thresh, 2, function(x) which.min(x)[1])), silent = TRUE)    
        # for each column, find the row with the minimum value which corresponds to the depth where anoxia begins
    
    if(!is.na(seq.af[1])){
      for(m in 1:length(seq.af)){
        if(max(bty$depth_m) < seq.af[m]){
          print('bathy not sufficient')
        }
        
      }
      
    }
    
    
    AF <- sum(areas.af[seq.af], na.rm = TRUE)/max(areas.af, na.rm = TRUE)
    AF_out <- rbind(AF_out, data.frame('year' = years[i],
                                       'AF' =AF,
                                       'n_months' = length(unique(month(oxy_sub$date))),
                                       'max_depth' = max(oxy_sub$depth_m),
                                       'threshold' = threshold,
                                       'lake' = lake_name,
                                       'method' = method))
    
  }
  
  return(AF_out)
  
}
