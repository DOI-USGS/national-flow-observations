#one row per site, including ID, lat/lon, site type, maybe n records from the data pull

summarize_site_observations <- function(target_ind, daily_flow_ind, 
                                        dv_inventory, uv_inventory) {
  
  daily_flow <- readRDS(sc_retrieve(daily_flow_ind, remake_file = '20_data_munge.yml')) 
  #daily_flow <- readRDS('20_data_munge/out/daily_flow.rds')
  #dv_inventory <- readRDS('10_nwis_pull/inout/nwis_dv_inventory.rds')
  #uv_inventory <- readRDS('10_nwis_pull/inout/nwis_uv_inventory.rds')
  #dv_inventory <- readRDS(sc_retrieve(dv_inventory_ind, remake_file = '20_data_munge.yml'))
  #uv_inventory <- readRDS(sc_retrieve(uv_inventory_ind, remake_file = '20_data_munge.yml'))
  
  
  daily_summary <- daily_flow %>% 
    group_by(site_id, source) %>% 
    summarize(n_days = n(),
              median_flow = median(flow_cfs))
  stopifnot(length(unique(daily_summary$site_id)) == nrow(daily_summary))
  
  daily_summary_dv_site_info <- daily_summary %>% 
    filter(source == 'nwis_dv') %>% 
    left_join(dv_inventory, by = c(site_id = 'site_no'))
  
  daily_summary_uv_site_info <- daily_summary %>% 
    filter(source == 'nwis_vv') %>% 
    left_join(uv_inventory, by = c(site_id = 'site_no'))
  daily_summary_all_site_info <- bind_rows(daily_summary_dv_site_info,
                                           daily_summary_uv_site_info)
  stopifnot(sum(is.na(daily_summary_all_site_info$station_nm)) == 0)
  
  saveRDS(file = as_data_file(target_ind), object = daily_summary_all_site_info)
  s3_put(target_ind)
}