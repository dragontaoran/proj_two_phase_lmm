#!/bin/bash

r_set=("2" "3" "4")
goal_set=("intercept" "slope" "both")
dir="res"

mkdir -p ${dir}

for goal in ${goal_set[@]}; 
do
	for r in ${r_set[@]};
	do
		subdir=${dir}/acml_ods_${goal}_r${r}
		mkdir -p ${subdir}
		
		echo "#!/bin/bash" > tmp.slurm
		echo "" >> tmp.slurm
		echo "#SBATCH --mail-user=taor@live.unc.edu  # email address" >> tmp.slurm
		echo "#SBATCH --mail-type=NONE  # Alerts sent when job begins, ends, or aborts" >> tmp.slurm
		echo "#SBATCH --nodes=1   # Number of nodes required" >> tmp.slurm
		echo "#SBATCH --ntasks=1   # Number of nodes required" >> tmp.slurm
		echo "#SBATCH --mem=4G  # Total memory (RAM) required, per node" >> tmp.slurm
		echo "#SBATCH --time=07-0:00:00  # Wall Clock time (dd-hh:mm:ss) [max of 14 days]" >> tmp.slurm
		echo "#SBATCH --array=1-10" >> tmp.slurm
		echo "#SBATCH --output=${subdir}/%a.slog  # output and error messages go to this file" >> tmp.slurm
		echo "#SBATCH --job-name=ACML # job name" >> tmp.slurm
		echo "#SBATCH --partition=general" >> tmp.slurm
		echo "" >> tmp.slurm
		echo "echo \"SLURM_JOBID: \" \$SLURM_JOBID" >> tmp.slurm
		echo "echo \"SLURM_ARRAY_TASK_ID: \" \$SLURM_ARRAY_TASK_ID" >> tmp.slurm
		echo "echo \"SLURM_ARRAY_JOB_ID: \" \$SLURM_ARRAY_JOB_ID" >> tmp.slurm
		echo "" >> tmp.slurm
		echo "R CMD BATCH --no-save --no-restore --slave \"--args \${SLURM_ARRAY_TASK_ID} ${r} ${goal} ${subdir}\" sim_acml.R ${subdir}/\${SLURM_ARRAY_TASK_ID}.Rout" >> tmp.slurm
		
		# cat tmp.slurm
		sbatch tmp.slurm
		rm -rf tmp.slurm				
	done
done
