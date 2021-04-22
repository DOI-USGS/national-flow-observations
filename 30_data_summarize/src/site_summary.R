#one row per site, including ID, lat/lon, site type, maybe n records from the data pull

function(target_file, nwis_dv_ind, nwis_uv_ind) {
  
  # nwis_dv <- readRDS(sc_retrieve(nwis_dv_ind, remake_file = '10_nwis_pull.yml')) 
  # nwis_uv <- readRDS(sc_retrieve(nwis_uv_ind, remake_file = '10_nwis_pull.yml'))
    nwis_dv <- readRDS('10_nwis_pull/out/nwis_dv_data.rds')
    nwis_uv <- readRDS('10_nwis_pull/out/nwis_uv_data.rds')
}