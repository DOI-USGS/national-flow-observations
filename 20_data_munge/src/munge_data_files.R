f_to_c <- function(f) {
  c <- (f - 32) * (5/9)
  return(c)
}

munge_wqp_withdepths <- function(in_ind, min_value, max_value, max_daily_range, out_ind) {
  dat <- readRDS(sc_retrieve(in_ind)) %>%
    filter(!is.na(`ResultMeasure/MeasureUnitCode`)) %>%
    filter(ActivityMediaName %in% 'Water') %>%
    filter(!is.na(`ActivityDepthHeightMeasure/MeasureValue`)|
                         !is.na(`ResultDepthHeightMeasure/MeasureValue`)|
                         !is.na(`ActivityTopDepthHeightMeasure/MeasureValue`)|
                         !is.na(`ActivityBottomDepthHeightMeasure/MeasureValue`)) %>% 
    filter(ActivityStartDate > as.Date('1800-01-01') & ActivityStartDate < as.Date('2020-01-01'))
  
  # create depth column
  dat <- mutate(dat, sample_depth = case_when(
    !is.na(`ActivityDepthHeightMeasure/MeasureValue`) ~ `ActivityDepthHeightMeasure/MeasureValue`,
    is.na(`ActivityDepthHeightMeasure/MeasureValue`) & !is.na(`ResultDepthHeightMeasure/MeasureValue`) ~ `ResultDepthHeightMeasure/MeasureValue`,
    is.na(`ActivityDepthHeightMeasure/MeasureValue`) & is.na(`ResultDepthHeightMeasure/MeasureValue`) & !is.na(`ActivityTopDepthHeightMeasure/MeasureValue`) ~ `ActivityTopDepthHeightMeasure/MeasureValue`,
    is.na(`ActivityDepthHeightMeasure/MeasureValue`) & is.na(`ResultDepthHeightMeasure/MeasureValue`) & is.na(`ActivityTopDepthHeightMeasure/MeasureValue`) ~ `ActivityBottomDepthHeightMeasure/MeasureValue`
  ))
  
  dat <- mutate(dat, sample_depth_unit_code = case_when(
    !is.na(`ActivityDepthHeightMeasure/MeasureValue`) ~ `ActivityDepthHeightMeasure/MeasureUnitCode`,
    is.na(`ActivityDepthHeightMeasure/MeasureValue`) & !is.na(`ResultDepthHeightMeasure/MeasureValue`) ~ `ResultDepthHeightMeasure/MeasureUnitCode`,
    is.na(`ActivityDepthHeightMeasure/MeasureValue`) & is.na(`ResultDepthHeightMeasure/MeasureValue`) & !is.na(`ActivityTopDepthHeightMeasure/MeasureValue`) ~ `ActivityTopDepthHeightMeasure/MeasureUnitCode`,
    is.na(`ActivityDepthHeightMeasure/MeasureValue`) & is.na(`ResultDepthHeightMeasure/MeasureValue`) & is.na(`ActivityTopDepthHeightMeasure/MeasureValue`) ~ `ActivityBottomDepthHeightMeasure/MeasureUnitCode`
  ))
  

  # now reduce to daily measures
  dat_reduced <- dat %>%
    filter(grepl('deg C|deg F', `ResultMeasure/MeasureUnitCode`, ignore.case = TRUE)) %>%
    mutate(ResultMeasureValue = ifelse(grepl('deg F', `ResultMeasure/MeasureUnitCode`, ignore.case = TRUE),
                                       f_to_c(ResultMeasureValue), ResultMeasureValue)) %>%
    filter(ResultMeasureValue > min_value,
           ResultMeasureValue < max_value) %>%
    mutate(`ResultMeasure/MeasureUnitCode` = 'deg C')
    
  dat_daily <- group_by(dat_reduced, MonitoringLocationIdentifier, ActivityStartDate, sample_depth) %>%
    summarize(temperature_mean_daily = mean(ResultMeasureValue),
              activity_start_times = paste(`ActivityStartTime/Time`, collapse = ', '),
              n_day = n())
  
  dat_daily_meta <- select(dat_reduced, -ResultMeasureValue, -`ActivityStartTime/Time`) %>%
    group_by(MonitoringLocationIdentifier, ActivityStartDate, sample_depth) %>%
    summarize_all(first)
  
  # bring it together
  dat_daily <- left_join(dat_daily, dat_daily_meta)
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(dat_daily, data_file)
  gd_put(out_ind, data_file)
    
}

munge_wqp_withoutdepths <- function(in_ind, min_value, max_value, max_daily_range, out_ind) {
  dat <- readRDS(sc_retrieve(in_ind)) %>%
    select(MonitoringLocationIdentifier, ActivityStartDate, ResultMeasureValue, 
           `ResultMeasure/MeasureUnitCode`, `ActivityStartTime/Time`, ActivityMediaName, 
           `ActivityDepthHeightMeasure/MeasureValue`, `ResultDepthHeightMeasure/MeasureValue`, 
           `ActivityTopDepthHeightMeasure/MeasureValue`, `ActivityBottomDepthHeightMeasure/MeasureValue`) %>%
    filter(!is.na(`ResultMeasure/MeasureUnitCode`)) %>%
    filter(ActivityMediaName %in% 'Water') %>%
    filter(is.na(`ActivityDepthHeightMeasure/MeasureValue`)&
             is.na(`ResultDepthHeightMeasure/MeasureValue`)&
             is.na(`ActivityTopDepthHeightMeasure/MeasureValue`)&
             is.na(`ActivityBottomDepthHeightMeasure/MeasureValue`)) %>%
    filter(ActivityStartDate > as.Date('1800-01-01') & ActivityStartDate < as.Date('2020-01-01'))
  
 
  # now reduce to daily measures
  # first clean up obviously bad data
  dat_reduced <- dat %>%
    filter(grepl('deg C|deg F', `ResultMeasure/MeasureUnitCode`, ignore.case = TRUE)) %>%
    mutate(ResultMeasureValue = ifelse(grepl('deg F', `ResultMeasure/MeasureUnitCode`, ignore.case = TRUE),
                                       f_to_c(ResultMeasureValue), ResultMeasureValue)) %>%
    mutate(`ResultMeasure/MeasureUnitCode` = 'deg C') %>%
    filter(ResultMeasureValue > min_value, ResultMeasureValue < max_value)
  
  dat_daily <- group_by(dat_reduced, MonitoringLocationIdentifier, ActivityStartDate) %>%
    summarize(temperature_mean_daily = mean(ResultMeasureValue),
              activity_start_times = paste(`ActivityStartTime/Time`, collapse = ', '),
              n_day = n())
  
  # save
  data_file <- scipiper::as_data_file(out_ind)
  saveRDS(dat_daily, data_file)
  gd_put(out_ind, data_file)
  
}

munge_nwis_uv <- function(in_ind, min_value, max_value, max_daily_range, out_ind) {
  nwis_df <- readRDS(sc_retrieve(in_ind)) %>%
    filter(!is.na(temp_value), !is.na(dateTime)) %>%
    filter(grepl('A', cd_value, ignore.case = FALSE)) %>%
    filter(temp_value > min_value,
           temp_value < max_value) %>%
    mutate(dateTime = as.Date(dateTime)) %>%
    group_by(site_no, col_name, dateTime) %>%
    summarize(temperature_mean_daily = round(mean(temp_value), 3), n_day = n())
  
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
  
