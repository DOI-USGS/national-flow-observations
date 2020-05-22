# compare NWIS dv and uv services

compare_services <- function(dv_ind, uv_ind, compare_ind) {
  uv <- feather::read_feather(sc_retrieve(uv_ind))
  dv <- feather::read_feather(sc_retrieve(dv_ind))
  
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
  
  feather::write_feather(compare, as_data_file(compare_ind))
  gd_put(compare_ind)
}

reduce_redundancy <- function(uv_inv_ind, compare_ind, out_ind) {
  compare <- feather::read_feather(sc_retrieve(compare_ind))
  uv_inv <- feather::read_feather(sc_retrieve(uv_inv_ind))
  
  uv_pull_sites <- filter(compare, is.na(dv_begin_date)) %>%
    select(site_no) %>%
    distinct() %>% pull()
  
  uv_inv_reduced <- uv_inv %>%
    filter(site_no %in% uv_pull_sites)
  
  feather::write_feather(uv_inv_reduced, as_data_file(out_ind))
  gd_put(out_ind)
}