#!/bin/bash
#SBATCH -J rstudio
#SBATCH -t 2-00:00:00               # time
#SBATCH -o shellLog/rstudio.out
#SBATCH -p cpu
#SBATCH -A iidd          # your account code
#SBATCH -n 1
#SBATCH -c 1
#SBATCH --mem 120GB

export PASSWORD=$(openssl rand -base64 15)

# get unused socket per https://unix.stackexchange.com/a/132524
# tiny race condition between the python & singularity commands
readonly PORT=$(python -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()')
cat 1>&2 <<END
1. SSH tunnel from your workstation using the following command:

   ssh -N -L 8787:${HOSTNAME}:${PORT} ${USER}@tallgrass.cr.usgs.gov

   and point your web browser to http://localhost:8787

2. log in to RStudio Server using the following credentials:

   user: ${USER}
   password: ${PASSWORD}

When done using RStudio Server, terminate the job by:

1. Exit the RStudio Session ("power" button in the top right corner of the RStudio window)
2. Issue the following command on the login node:

      scancel -f ${SLURM_JOB_ID}
END

#rstudio needs writable /run and /tmp directories
#creating these outside the container, will be bound to normal paths
mkdir -p .mytmp
mkdir -p .myrun
mkdir -p .myvar

export SLURM_CPUS_ON_NODE

START_DIR=$(pwd) #start Rstudio in current directory (wherever sbatch run from)
module load singularity
# This example bind mounts the /cxfs directory on the host into the Singularity container.
# By default the only host file systems mounted within the container are $HOME, /tmp, /proc, /sys, and /dev.
srun singularity exec --bind .mytmp:/tmp --bind .myrun:/run --bind .myvar:/var \
    national-data-pulls_v0.1.sif \
    rserver --www-port ${PORT} --auth-none=0 --auth-pam-helper-path=pam-helper \
    --server-working-dir=${START_DIR}
printf 'rserver exited' 1>&2


