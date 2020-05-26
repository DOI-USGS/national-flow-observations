# extract and post the data (dropping the site info attribute), creating an
# .ind file that we will want to share because it represents the shared cache
combine_nwis_data <- function(ind_file, ...){
  
  rds_files <- c(...)
  df_list <- list()
  
  
  for (i in seq_len(length(rds_files))){
    
    temp_dat <- readRDS(rds_files[i]) 
    
    reduced_dat <- choose_flow_column(temp_dat)
    
    df_list[[i]] <- reduced_dat
  }
  
  nwis_df <- do.call("bind_rows", df_list)
  
  
  data_file <- scipiper::as_data_file(ind_file)
  saveRDS(nwis_df, data_file)
  gd_put(ind_file, data_file)
}



choose_flow_column <- function(temp_dat) {
  # take all flow columns and put into long df
  values <- temp_dat %>%
    select(-ends_with('_cd'), -agency_cd) %>%
    tidyr::gather(key = 'col_name', value = 'temp_value', -site_no, -dateTime) %>%
    filter(!is.na(temp_value))
  
  # take all flow cd columns and do the same thing
  codes <- temp_dat %>%
    select(site_no, dateTime, ends_with('_cd'), -tz_cd, -agency_cd) %>%
    tidyr::gather(key = 'col_name', value = 'cd_value', -site_no, -dateTime) %>%
    mutate(col_name = gsub('_cd', '', col_name)) %>%
    filter(!is.na(cd_value))
  
  # bring together so I have a long df with both temp and cd values
  all_dat <- left_join(values, codes, by = c('site_no', 'dateTime', 'col_name'))
  
  # find which col_name has the most records for each site,
  # and keep that column
  top_cols <- all_dat %>%
    group_by(site_no, col_name) %>%
    summarize(count_nu = n()) %>%
    group_by(site_no) %>%
    slice(which.max(count_nu))
  
  # reduce the data down to those site-top col combos
  reduced_dat <- inner_join(all_dat, select(top_cols, site_no, col_name)) %>%
    distinct()
  
  return(reduced_dat)
}
