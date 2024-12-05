#here you need to specify how many instances you want to run simulations on. 
#currently it si set up as such that for any "real" collected data from our pilot there will be Simulations.
#this is not to fit a model yet. this makes extensive Simulations. I fit the model back at another place.
HowManyModels=6
agegroups=3

PATH_LOG_E="./logsE/$(date '+%Y%m%d_%H%M%S')"
PATH_LOG_O="./logsY/$(date '+%Y%m%d_%H%M%S')"

# CREATE RELEVANT DIRECTORIES:
# ==============================================================================
# create output directory:
if [ ! -d ${PATH_LOG_E} ]; then
	mkdir -p ${PATH_LOG_E}
fi
# create directory for log files:
if [ ! -d ${PATH_LOG_O} ]; then
	mkdir -p ${PATH_LOG_O}
fi

# Fit model
# ==============================================================================
	for i in `seq 1 $HowManyModels`;
	do
		for j in `seq 1 $agegroups`;
	do

		  echo '#!/bin/bash'                                > job.slurm
      echo "#SBATCH --job-name fit_${i}_${j}"         >> job.slurm
      echo "#SBATCH --partition short"                   >> job.slurm
      echo "#SBATCH --mem 30GB"                          >> job.slurm
      echo "#SBATCH --cpus-per-task 4"                           >> job.slurm
      echo "#SBATCH --time 5:0:0"                     >> job.slurm
      #echo "#SBATCH --workdir ."                        >> job.slurm
      echo "#SBATCH --error ${PATH_LOG_E}/slurm-%j.err"              >> job.slurm
      echo "#SBATCH --output ${PATH_LOG_O}/slurm-%j.out"             >> job.slurm
			echo "module load R/4.2; Rscript fit_models_stan_gamble_est.R $i $j"	>>  job.slurm
    sbatch job.slurm
    rm -f job.slurm
	done
done
