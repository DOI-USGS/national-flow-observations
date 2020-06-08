# Summarize data that was pulled during the build to use for later

summarize_nwis_pull <- function(target_file, nwis_dv_ind, nwis_uv_ind) {
  
  nwis_dv <- readRDS(sc_retrieve(nwis_dv_ind)) 
  nwis_uv <- readRDS(sc_retrieve(nwis_uv_ind))
  
  diagnostics <- list(
    DV = list(
      Num_obs = nrow(nwis_dv),
      Num_sites = length(unique(nwis_dv$site_no))
    ),
    UV = list(
      Num_obs = nrow(nwis_uv),
      Num_sites = length(unique(nwis_uv$site_no))
    )
  )
  
  yaml::write_yaml(diagnostics, file = target_file)
}
