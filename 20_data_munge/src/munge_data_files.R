combine_all_dat <- function(nwis_dv_ind, nwis_uv_ind, out_ind) {
  
  nwis_uv <- readRDS(sc_retrieve(in_ind)) %>%
    filter(!is.na(temp_value), !is.na(dateTime)) %>%
    mutate(dateTime = as.Date(dateTime)) %>%
    group_by(site_no, col_name, dateTime) %>%
    summarize(temp_degC = round(mean(temp_value), 3), 
              cd_value = paste0(unique(cd_value), collapse = ','),
              n_obs = n()) %>%
    ungroup() %>%
    mutate(source = 'nwis_uv') %>%
    select(site_id = site_no, date, temp_degC = temperature_mean_daily, n_obs, source)
  
  nwis_dv <- readRDS(sc_retrieve(nwis_dv_ind)) %>%
    mutate(date = as.Date(dateTime),
           source = 'nwis_dv', 
           n_obs = 1) %>%
    select(site_id = site_no, date, temp_degC = temp_value, n_obs, source)
  
  all_dat <- bind_rows(nwis_dv, nwis_uv)
    
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(all_dat, data_file)
  gd_put(out_ind)
  
}
  
