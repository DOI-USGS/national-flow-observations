# extract and post the data (dropping the site info attribute), creating an
# .ind file that we will want to share because it represents the shared cache
combine_nwis_data <- function(ind_file, ...){
  
  rds_files <- c(...)
  df_list <- list()
  message('reading in partition files...')
  for (i in seq_len(length(rds_files))){
    print(rds_files[i])
    flow_dat <- readRDS(rds_files[i]) 
    
    reduced_dat <- convert_to_long(flow_dat)
    
    df_list[[i]] <- reduced_dat
  }
  
  nwis_df <- data.table::rbindlist(df_list)
  
  data_file <- scipiper::as_data_file(ind_file)
  message('writing combined file...')
  saveRDS(nwis_df, data_file)
  sc_indicate(ind_file, data_file = data_file)
}



convert_to_long <- function(flow_dat) {
  
  # For "dv", dates are in a column called `Date`
  # For "uv" dates are in `dateTime` and there is also a `tz_cd` column
  date_col_nm <- names(flow_dat)[grepl("date|Date", names(flow_dat))]
  tz_col_nm <- names(flow_dat)[grepl("tz_cd", names(flow_dat))] # If empty, matches(tx_col_nm) still works
  
  # take all flow columns and put into long df
  values <- flow_dat %>%
    select(-ends_with('_cd'), -agency_cd) %>%
    tidyr::gather(key = 'col_name', value = 'flow_value', -site_no, -matches(date_col_nm))
  
  # take all flow cd columns and do the same thing
  codes <- flow_dat %>%
    select(site_no, ends_with('_cd'), -matches(tz_col_nm), -agency_cd, matches(date_col_nm)) %>%
    tidyr::gather(key = 'col_name', value = 'cd_value', -site_no, -matches(date_col_nm)) %>%
    mutate(col_name = gsub('_cd', '', col_name)) %>%
    filter(!is.na(cd_value))
  
  # bring together so I have a long df with both temp and cd values
  all_dat <- left_join(values, codes, by = c('site_no', date_col_nm, 'col_name')) %>% 
    # Only filter out NA flow if it doesn't have a Provisional Ice flag
    # "P Ice" means that a WSC hasn't finished processing records but there 
    # might be an actual flow value after processing. If you can,
    # dataRetrieval::setAccess("internal") would show the under ice flows.
    filter(!is.na(flow_value) | cd_value == "P Ice")
  
  # find which col_name has the most records for each site,
  # and keep that column
  # top_cols <- all_dat %>%
  #   group_by(site_no, col_name) %>%
  #   summarize(count_nu = n()) %>%
  #   group_by(site_no) %>%
  #   slice(which.max(count_nu))
  # 
  # reduce the data down to those site-top col combos
  # reduced_dat <- inner_join(all_dat, select(top_cols, site_no, col_name)) %>%
  #   distinct()
  # 
  # return(reduced_dat)
  
  return(all_dat)
}
