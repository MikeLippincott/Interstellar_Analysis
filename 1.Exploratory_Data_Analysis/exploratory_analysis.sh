#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=256G
#SBATCH --partition=amem
#SBATCH --qos=mem
#SBATCH --time=24:00:00
#SBATCH --output=sample-%j.out

module load anaconda

conda activate Interstellar

papermill 4.cell_count_analysis.ipynb 4.cell_count_analysis.ipynb -p celltype "SHSY5Y"
Rscript 7.cell_count_visualization.r --celltype "SHSY5Y"
papermill 1.umap_analysis_plate2.ipynb 1.umap_analysis_plate2.ipynb -p celltype "SHSY5Y"

papermill 4.cell_count_analysis.ipynb 4.cell_count_analysis.ipynb -p celltype "PBMC"
Rscript 7.cell_count_visualization.r --celltype "PBMC"
papermill 1.umap_analysis_plate2.ipynb 1.umap_analysis_plate2.ipynb -p celltype "PBMC"

jupyter nbconvert --to=script --FilesWriter.build_directory=scripts *.ipynb
