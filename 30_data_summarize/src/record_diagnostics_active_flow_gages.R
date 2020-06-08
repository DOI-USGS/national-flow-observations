summarize_active_flow_gages <- function(target_file, active_gages_ind, active_gages_summary_ind) {
  
  nwis_active_gages <- readRDS(sc_retrieve(active_gages_ind))
  nwis_active_gages_info <- readRDS(sc_retrieve(active_gages_summary_ind))
  
  n_active_gages <- nrow(nwis_active_gages_info)
  continuous_gages <- nwis_active_gages_info %>% filter(!any_gaps)
  n_continuous_gages <- nrow(continuous_gages)

  min_yr <- min(nwis_active_gages$year)
  max_yr <- max(nwis_active_gages$year)
  
  png(target_file, width = 1000, height = 800)
  par(mar = c(4, 4, 6, 2))
  plot(as.numeric(nwis_active_gages$year), nwis_active_gages$n_gages_per_year,
       xaxt = "n", xlab = "Year", ylab = "Number of active gages")
  mtext(side = 3, line = 5, cex = 1.2, "Count of active gages through time")
  mtext(side = 3, line = 1, cex = 1,
        sprintf(paste0("Total # active gages in data set: %s\n",
                       "Total # continously active gages: %s\n",
                       "Year range of active gages: %s - %s"),
                n_active_gages, n_continuous_gages, min_yr, max_yr))
  axis(side = 1, at = seq(min_yr, max_yr, by = 10))
  dev.off()
  
}
