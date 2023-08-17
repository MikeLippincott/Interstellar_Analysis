#!/bin/bash

jupyter nbconvert --to=script --FilesWriter.build_directory=. ../notebooks/*.ipynb*

Rscript binary_classification_training_visualization.r --celltype "SHSY5Y" --model_name "DMSO_0.025_vs_LPS_100"
