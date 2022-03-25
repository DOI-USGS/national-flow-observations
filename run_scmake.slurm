#!/bin/bash
#BATCH --job-name=national_flow_pull    # name that you chose
#SBATCH -p cpu
#SBATCH -A iidd                 # your account
#SBATCH -o shellLog/slurm-%A_%a.out
#SBATCH --time=48:00:00        # time at which the process will be cancelled if unfinished
#SBATCH --mail-type=ALL
#SBATCH --export=ALL
##SBATCH --array=1        # process IDs 
#SBATCH -n 1
#SBATCH --mem=60GB

module load singularity

srun singularity exec national-data-pulls_v0.1.sif Rscript -e '
library(scipiper);
options(scipiper.getters_file = "remake.yml"); 
scmake("30_data_summarize")'

