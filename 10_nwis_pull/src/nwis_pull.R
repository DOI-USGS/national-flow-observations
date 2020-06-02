#### pull ####

# prepare a plan for downloading (from NWIS) and posting (to S3) one data file
# per state
plan_nwis_pull <- function(partitions, service) {

  folders <- list(tmp='10_nwis_pull/tmp')
  
  # create partition step
  partitions_name <- deparse(substitute(partitions))
  partition <- scipiper::create_task_step(
    step_name = 'partition',
    target_name = function(task_name, step_name, ...) {
      sprintf('%s_partition_%s', service, task_name)
    },
    command = function(task_name, ...) {
      sprintf("filter_partitions(partitions = %s, I('%s'))", partitions_name, task_name)
    }
  )

  # download from NWIS, save locally

  download <- scipiper::create_task_step(
    step_name = 'download',
    target_name = function(task_name, step_name, ...) {
      file.path(folders$tmp, sprintf('%s_%s.rds', service, task_name))
    },
    command = function(steps, ...) {
      paste(
        "get_nwis_data(",
        "data_file=target_name,",
        paste0("service = I('", service, "'),"),
        sprintf("partition=%s,", steps$partition$target_name),
       "nwis_pull_params = nwis_pull_parameters)",
        sep="\n      ")
    }
  )

  # put the steps together into a task plan
  task_plan <- scipiper::create_task_plan(
    task_names=sort(partitions$PullTask),
    task_steps=list(partition, download),
    final_steps=c('download'),
    add_complete=FALSE,
    ind_dir=folders$tmp)
  
  return(task_plan)

}

create_nwis_pull_makefile <- function(makefile, task_plan, final_targets) {


  create_task_makefile(
    makefile=makefile, task_plan=task_plan,
    include = c('10_nwis_pull.yml'),
    sources = c('10_nwis_pull/src/nwis_pull.R', '10_nwis_pull/src/nwis_combine_functions.R'),
    packages=c('dplyr', 'dataRetrieval', 'scipiper', 'yaml', 'stringr'),
    file_extensions=c('ind'), finalize_funs = 'combine_nwis_data', final_targets = final_targets)
}

# --- functions used in task table ---

# read the partitions file and pull out just those sites that go with a single
# PullTask 
filter_partitions <- function(partitions, pull_task) {
  
  these_partitions <- dplyr::filter(partitions, PullTask==pull_task, count_nu > 0)
  
  return(these_partitions)  
}

# pull a batch of NWIS observations, save locally
get_nwis_data <- function(data_file, partition, nwis_pull_params, service, verbose = TRUE) {
  
  nwis_pull_params$service <- service
  nwis_pull_params$site <- partition$site_no
  
  dataRetrieval::setAccess(access = 'internal')
  if (service == 'dv') { nwis_pull_params$statCd <- '00003' }
  
  # do the data pull
  nwis_dat_time <- system.time({
    nwis_dat <- do.call(readNWISdata, nwis_pull_params)
  })
  
  if (verbose){
    message(sprintf(
      'NWIS pull for %s took %0.0f seconds and returned %d rows',
      partition$PullTask[1],
      nwis_dat_time['elapsed'],
      nrow(nwis_dat)))
  }
  # make nwis_dat a tibble, converting either from data.frame (the usual case) or
  # NULL (if there are no results)
  nwis_dat <- as_tibble(nwis_dat)
  
  # write the data to rds file. do this even if there were 0
  # results because remake expects this function to always create the target
  # file
  saveRDS(nwis_dat, data_file)
}