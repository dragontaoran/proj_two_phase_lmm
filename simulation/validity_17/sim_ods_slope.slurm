#!/bin/bash

#SBATCH --mail-user=r.tao@vanderbilt.edu  # email address
#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts
#SBATCH --nodes=1   # Number of nodes required
#SBATCH --ntasks=1   # Number of nodes required
#SBATCH --mem=4G  # Total memory (RAM) required, per node
#SBATCH --time=02-00:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]
#SBATCH --array=1-40
#SBATCH --output=res/sim_ods_slope_%A_%a.slog  # output and error messages go to this file
#SBATCH --job-name=slope # job name

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID

setpkgs -d /usr/local/cgg/software/R/.config_pkg/ -a R_3.3.2.cgg

Rscript sim_ods_slope.R $SLURM_ARRAY_TASK_ID
