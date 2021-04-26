FROM rocker/geospatial:4.0.4

#the binder step changes some directory permissions which blocks R package installs,
#so installing packages here first
#add additional R packages to install here:
RUN install2.r --error \
  igraph \
  targets \
  tarchetypes \
  feather \
  visNetwork \
  aws.signature \
  aws.s3 \ 
  dataRetrieval 
  
RUN Rscript -e 'remotes::install_github("richfitz/remake"); remotes::install_github("USGS-R/scipiper")'
  


