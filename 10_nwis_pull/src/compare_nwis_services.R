# compare NWIS dv and uv services

compare_services <- function(dv_file, uv_file) {
  uv <- readRDS(uv_file)
  dv <- readRDS(dv_file)
  
  uv <- uv %>%
    select(site_no, uv_begin_date = begin_date, uv_end_date = end_date, uv_count = count_nu) %>%
    distinct()
  
  dv <- dv %>%
    select(site_no, dv_begin_date = begin_date, dv_end_date = end_date, dv_count = count_nu) %>%
    distinct()
  
  compare <- full_join(uv, dv) %>%
    rowwise() %>%
    mutate(in_both = ifelse(any(is.na(c(dv_begin_date, uv_begin_date))), FALSE, TRUE),
           begin_dates_diff = dv_begin_date - uv_begin_date,
           end_dates_diff = dv_end_date - uv_end_date)
  
  return(compare)
}

reduce_redundancy <- function(compare) {

  uv_pull_sites <- filter(compare, is.na(dv_begin_date)) %>%
    select(site_no, count_nu = uv_count) %>%
    distinct(site_no, .keep_all = TRUE)
  
  return(uv_pull_sites)
}