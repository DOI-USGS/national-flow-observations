munge_nwis_uv <- function(in_ind, min_value, max_value, max_daily_range, out_ind) {
  nwis_df <- readRDS(sc_retrieve(in_ind)) %>%
    filter(!is.na(temp_value), !is.na(dateTime)) %>%
    filter(temp_value > min_value,
           temp_value < max_value) %>%
    mutate(dateTime = as.Date(dateTime)) %>%
    group_by(site_no, col_name, dateTime) %>%
    summarize(temperature_mean_daily = round(mean(temp_value), 3), 
              cd_value = paste0(unique(cd_value), collapse = ','),
              n_day = n())
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(nwis_df, data_file)
  gd_put(out_ind, data_file)
}

munge_nwis_dv <- function(in_ind, min_value, max_value, max_daily_range, out_ind) {

  dat <- readRDS(sc_retrieve(in_ind)) %>%
    filter(grepl('A', cd_value, ignore.case = FALSE)) %>%
    filter(temp_value > min_value,
           temp_value < max_value)
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(dat, data_file)
  gd_put(out_ind, data_file)
  
}

combine_all_dat <- function(wqp_ind, nwis_dv_ind, nwis_uv_ind, out_ind) {
  wqp <- readRDS(sc_retrieve(wqp_ind)) %>%
    mutate(date = as.Date(ActivityStartDate), source = 'wqp') %>%
    select(site_id = MonitoringLocationIdentifier, 
           date,
           temp_degC = temperature_mean_daily,
           n_obs = n_day, source)
  
  nwis_dv <- readRDS(sc_retrieve(nwis_dv_ind)) %>%
    mutate(date = as.Date(dateTime),
           source = 'nwiw_dv',
           site_id = paste0('USGS-', site_no)) %>%
    select(site_id, date, temp_degC = temp_value, source)
  
  nwis_uv <- readRDS(sc_retrieve(nwis_uv_ind)) %>%
    ungroup() %>%
    mutate(date = as.Date(dateTime),
           source = 'nwiw_uv',
           site_id = paste0('USGS-', site_no)) %>%
    select(site_id, date, temp_degC = temperature_mean_daily, n_obs = n_day, source)
  
  all_dat <- bind_rows(wqp, nwis_dv, nwis_uv)
    
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(all_dat, data_file)
  gd_put(out_ind, data_file)
  
}
  
