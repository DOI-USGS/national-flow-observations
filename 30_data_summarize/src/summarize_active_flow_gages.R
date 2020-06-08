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

summarize_activity_by_gage <- function(out_ind, annual_active_gages_ind) {
  
  active_gages_per_year <- readRDS(sc_retrieve(annual_active_gages_ind))
  
  # Get vector of unique active sites
  unique_sites <- unique(unlist(apply(active_gages_per_year, 1, function(r) {
    r$site_ids_per_year
  })))
  
  # Summarize info about each active gages record 
  #   e.g. earliest date, if not continuous where were there gaps?
  gages_activity <- purrr::map(unique_sites, function(s) {
    years_active_i <- which(apply(active_gages_per_year, 1, function(r) {
      s %in% r$site_ids_per_year
    }))
    years_active <- as.numeric(active_gages_per_year$year)[years_active_i]
    
    gaps_i <- which(diff(years_active) > 1)
    start_gap <- years_active[gaps_i] + 1 
    end_gap <- years_active[gaps_i+1] - 1 
    gap_range_str <- sprintf("%s - %s", start_gap, end_gap)
    gap_range_str[start_gap == end_gap] <- as.character(end_gap) # only use single value if there is only one year
    gap_range_str[length(gap_range_str) == 0] <- NA
    
    activity_summary <- tibble(
      site = s, 
      n_years_active = length(years_active),
      earliest_active_year = min(years_active),
      which_years_active = list(years_active),
      any_gaps = length(gaps_i) > 0,
      gap_years = list(gap_range_str)
    )
    return(activity_summary)
  }) %>% 
    purrr::reduce(bind_rows)
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(gages_activity, data_file)
  s3_put(out_ind)
}
