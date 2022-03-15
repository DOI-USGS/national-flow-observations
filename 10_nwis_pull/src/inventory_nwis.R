inventory_nwis <- function(inv_ind, nwis_pull_params, service) {
  
  hucs <- stringr::str_pad(1:22, width = 2, pad = "0")
  nwis_pull_params$service <- service
  nwis_pull_params$statCd <- '00003'
  
  # what NWIS data does not accept start/end dates
  nwis_pull_params$startDate <- NULL
  nwis_pull_params$endDate <- NULL
  
  all_dat <- data.frame()
  for(huc in hucs){
    nwis_pull_params$huc = huc
    sites <- do.call(whatNWISdata, nwis_pull_params)
    
    all_dat <- rbind(all_dat, sites)
    
  }
  
  data_file <- scipiper::as_data_file(inv_ind)
  saveRDS(all_dat, data_file)
  s3_put(inv_ind)
  
}
