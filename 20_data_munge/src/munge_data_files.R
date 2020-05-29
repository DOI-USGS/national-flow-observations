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
  gd_put(out_ind)
}

munge_nwis_dv <- function(in_ind, min_value, max_value, max_daily_range, out_ind) {

  dat <- readRDS(sc_retrieve(in_ind)) %>%
    filter(grepl('A', cd_value, ignore.case = FALSE)) %>%
    filter(temp_value > min_value,
           temp_value < max_value)
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(dat, data_file)
  gd_put(out_ind)
  
}

combine_all_dat <- function(nwis_dv_ind, nwis_uv_ind, out_ind) {
  
  nwis_dv <- readRDS(sc_retrieve(nwis_dv_ind)) %>%
    mutate(date = as.Date(dateTime),
           source = 'nwis_dv') %>%
    select(site_id = site_no, date, temp_degC = temp_value, source)
  
  nwis_uv <- readRDS(sc_retrieve(nwis_uv_ind)) %>%
    ungroup() %>%
    mutate(date = as.Date(dateTime),
           source = 'nwis_uv') %>%
    select(site_id = site_no, date, temp_degC = temperature_mean_daily, n_obs = n_day, source)
  
  all_dat <- bind_rows(nwis_dv, nwis_uv)
    
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(all_dat, data_file)
  gd_put(out_ind)
  
}
  
