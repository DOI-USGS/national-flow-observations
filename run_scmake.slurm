#!/bin/bash
#BATCH --job-name=national_flow_pull    # name that you chose
#SBATCH -p UV,normal,short #the partition you want to use, for this case prod is best
#SBATCH -A iidd                 # your account
#SBATCH -o shellLog/slurm-%A_%a.out
#SBATCH --time=2:00:00        # time at which the process will be cancelled if unfinished
#SBATCH --mail-type=ALL
#SBATCH --export=ALL
##SBATCH --array=1        # process IDs 
#SBATCH -n 1
#SBATCH --mem=60GB

module load singularity/3.4.1

srun singularity exec national-data-pulls_v0.0.sif Rscript -e '
library(scipiper);
options(scipiper.getters_file = "10_nwis_pull.yml"); 
scmake("20_data_munge/out/daily_flow.rds.ind")'

