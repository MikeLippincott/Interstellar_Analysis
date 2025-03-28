#!/bin/bash
# This script is used to train the regression models for the elastic network

#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --partition=amilan
#SBATCH --qos=normal
#SBATCH --time=5:00
#SBATCH --output=sample-%j.out
#SBATCH --array=1-2%2

module load anaconda

conda activate Interstellar_python

# define the search parameters
cell_types=( "PBMC" "SHSY5Y" )

# calculate the number of jobs
job_id=$((SLURM_ARRAY_TASK_ID - 1))
cell_type_idx=$((job_id % ${#cell_types[@]}))

cell_type=${cell_types[$cell_type_idx]}

command="sbatch split_data_call.sh"

$command "$cell_type"



echo "Complete"
