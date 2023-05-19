# national-flow-observations
This repository pulls national flow data from NWIS for 50 states and all territories. The pipeline pushes to a shared cache in a USGS-internal S3 bucket called [ds-pipeline-national-flow-observations](https://s3.console.aws.amazon.com/s3/buckets/ds-pipeline-national-flow-observations/?region=us-west-2&tab=overview) (note you need to be logged into the console before this link will work).

As of 2022, the repo is set-up for use on Tallgrass because Yeti will be removed from service in August 2022. The 2022 version of this repo is currently cloned here: `/caldera/projects/usgs/water/iidd/datasci/data-pulls/national-flow-observations`

## To setup on Tallgrass with Singularity:
These are exact commands for Tallgrass. This is needed only when the image needs to be updated or when we move the repo somewhere other than Tallgrass.

After cloning the repo, pull the docker image from code.chs.usgs.gov:

```
module load singularity
singularity pull --docker-login docker://code.chs.usgs.gov:5001/wma/wp/national-data-pulls:v0.1
```
After you run the commands above you will see a prompt for a password. The password it is looking for is your [personal access token (PAT)](https://code.chs.usgs.gov/-/profile/personal_access_tokens). If you do not have a PAT set up you will need to do so.

## To run on Tallgrass with Singularity:

Two Slurm scripts are included in this repo.  If you want to run Rstudio to do development work, run `sbatch launch_rstudio.slurm` and then follow the instructions in the Slurm output file (`shellLog/rstudio.out`) to make an SSH tunnel and log in to Rstudio via a browser.

To run the pipeline as a non-interactive batch job, open `run_scmake.slurm` and modify parameters like job length (`--time`) and partition (`-p`) as needed for how long you expect the job to run (see [this issue](https://github.com/USGS-R/national-flow-observations/issues/4) for estimates). Submit the job with `sbatch --mail-user=$USER@usgs.gov run_scmake.slurm`.

To be able to run the full pipeline, you will need to have AWS credentials setup on your user's home directory on Tallgrass. This will only be possible for USGS users. You need to have the file `/home/username/.saml2aws`, which looks like this:

```
[default]
   url                  = http://awsconsole.usgs.gov/
   username             = USER@usgs.gov
   provider             = ADFS
   mfa                  = Auto
   skip_verify          = false
   timeout              = 0
   aws_urn              = urn:amazon:webservices
   aws_session_duration = 28800
   aws_profile          = default
   resource_id          =
   subdomain            =
   role_arn             =
   region               =
   http_attempts_count  = 3
   http_retry_delay     = 1
```

Be sure to update the `username` field to match your own email. You will need to log in either before running `scmake()` in an interactive session or before you submit your non-interactive batch job. Note that credentials will work for a maximum of 8 hours, which means that you may get a failure and need to re-authenticate for jobs that take longer. Run the following to use the appropriate credentials to authenticate to AWS for this pipeline: 

```
saml2aws login --role=arn:aws:iam::807615458658:role/adfs-wma-developer --region us-west-2
```
