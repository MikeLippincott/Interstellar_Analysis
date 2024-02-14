#!/bin/bash

#SBATCH --nodes=1
#SBATCH --mem=500G
#SBATCH --partition=amem
#SBATCH --qos=mem
#SBATCH --account=amc-general
#SBATCH --time=48:00:00
#SBATCH --output=sample-%j.out

module load anaconda

conda activate Interstellar_python

python 2.get_channel_splits.py --cell_type "PBMC"

echo "Done"
