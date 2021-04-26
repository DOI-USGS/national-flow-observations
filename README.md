# national-flow-observations
This repository pulls national flow data from NWIS. The pipeline pushes to a shared cache in an S3 bucket called [ds-pipeline-national-flow-observations](https://s3.console.aws.amazon.com/s3/buckets/ds-pipeline-national-flow-observations/?region=us-west-2&tab=overview).

## To run on Tallgrass or Yeti with Singularity:
These are exact commands for Yeti, Tallgrass may require small changes.

After cloning the repo, pull the docker image from code.chs.usgs.gov:

```
module load singularity/3.4.1
singularity pull --docker-login docker://code.chs.usgs.gov:5001/wma/wp/national-data-pulls:v0.0
```

Two Slurm scripts are included in this repo.  If you want to run Rstudio to do development work, run `sbatch launch_rstudio.slurm` and then follow the instructions in the Slurm output file to make an SSH tunnel and log in to Rstudio via a browser.

To run the pipline as a non-interactive batch job, open `run_scmake.slurm` and modify parameters like job length (`--time`) and partition (`-p`) as needed for how long you expect the job to run (see [this issue](https://github.com/USGS-R/national-flow-observations/issues/4) for estimates). Submit the job with `sbatch --mail-user=$USER@usgs.gov run_scmake.slurm`.
