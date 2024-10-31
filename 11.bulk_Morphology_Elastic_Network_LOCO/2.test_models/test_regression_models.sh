#!/bin/bash
# This script is used to train the regression models for the elastic network

# 32 channel combination * 2 cell types * 2 shuffles * 187 cytokines = 23936

conda activate Interstellar_python

# get the array of cytokiens
feature_combination_key_file="../0.split_data/results/channel_feature_combinations_keys.txt"
# get the array of cytokines
filename="../0.split_data/cytokine_list/cytokine_list.txt"

# read all lines of the file to an array
readarray -t cytokine_array < $filename
# get all of the feature combinations
# read all lines of the file to an array
readarray -t feature_combination_keys < $feature_combination_key_file

jupyter nbconvert --to=script --FilesWriter.build_directory=./scripts/ ./notebooks/*.ipynb
cd scripts || exit

shuffles=( True False )
cell_types=( SHSY5Y PBMC )

for cell_type in "${cell_types[@]}"
do
    for shuffle in "${shuffles[@]}"
    do
        for feature_combination in "${feature_combination_keys[@]}"
        do
            for cytokine in "${cytokine_array[@]}"
            do
		        echo "$cell_type $shuffle $feature_combination '${cytokine}'"
                python 1.test_regression_multi_output.py --cell_type "$cell_type" --shuffle "$shuffle" --feature_combinations_key "$feature_combination" --cytokine "${cytokine}"
	    done
        done
    done
done


cd .. || exit

echo "All models have been tested"


