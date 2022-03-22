FROM rocker/geospatial:4.0.4

#add additional R packages to install here:
RUN install2.r --error \
  aws.signature \
  aws.s3 \ 
  dataRetrieval \
  feather \
  igraph \
  qs \
  tarchetypes \
  targets \
  visNetwork \
  && rm -rf /tmp/downloaded_packages /tmp/*.rds /tmp/Rtmp*
  
RUN Rscript -e 'remotes::install_github("richfitz/remake"); remotes::install_github("USGS-R/scipiper")' \
  && rm -rf /tmp/downloaded_packages /tmp/*.rds /tmp/Rtmp*
