tally_active_flow_gages <- function(out_ind, all_dat_ind, min_days_active) {
  
  active_gages_per_year <- readRDS(sc_retrieve(all_dat_ind)) %>% 
    mutate(year = format(date, "%Y")) %>% 
    group_by(site_id, year) %>% 
    summarize(n_obs_per_year = n()) %>% 
    ungroup() %>% 
    # Keep only gages with the minimum observations required per year
    filter(n_obs_per_year >= min_days_active) %>% 
    group_by(year) %>% 
    # Get the number of active gages per year &
    # a list of which gages are active per year
    summarize(n_gages_per_year = n(),
              site_ids_per_year = list(site_id))
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(active_gages_per_year, data_file)
  s3_put(out_ind)
  
}

