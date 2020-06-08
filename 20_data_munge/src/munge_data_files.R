combine_all_dat <- function(nwis_dv_ind, nwis_uv_ind, out_ind) {
  
  nwis_uv <- readRDS(sc_retrieve(nwis_uv_ind)) %>%
    filter(!is.na(flow_value), !is.na(dateTime)) %>%
    mutate(date = as.Date(dateTime)) %>%
    group_by(site_no, col_name, date) %>%
    summarize(daily_flow = round(mean(flow_value), 3), 
              cd_value = paste0(unique(cd_value), collapse = ','),
              n_obs = n()) %>%
    ungroup() %>%
    mutate(source = 'nwis_uv') %>%
    select(site_id = site_no, date, flow_cfs = daily_flow, n_obs, source)
  
  nwis_dv <- readRDS(sc_retrieve(nwis_dv_ind)) %>%
    mutate(date = Date,
           source = 'nwis_dv', 
           n_obs = 1) %>%
    select(site_id = site_no, date, flow_cfs = flow_value, n_obs, source)
  
  all_dat <- bind_rows(nwis_dv, nwis_uv)
    
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(all_dat, data_file)
  s3_put(out_ind)
  
}
