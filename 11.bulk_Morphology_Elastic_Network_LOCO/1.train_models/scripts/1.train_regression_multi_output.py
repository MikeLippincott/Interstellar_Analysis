#!/usr/bin/env python
# coding: utf-8

# In[1]:


import argparse
import ast
import itertools
import pathlib
import warnings

import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import toml
import tqdm
from joblib import dump
from sklearn.exceptions import ConvergenceWarning
from sklearn.linear_model import ElasticNetCV, LogisticRegression, MultiTaskElasticNetCV

# import RepeatedKFold
from sklearn.model_selection import (
    GridSearchCV,
    LeaveOneOut,
    RepeatedKFold,
    StratifiedKFold,
    cross_val_score,
    train_test_split,
)
from sklearn.utils import parallel_backend


# In[2]:


argparser = argparse.ArgumentParser()
argparser.add_argument("--cell_type", type=str, default="all")
argparser.add_argument("--shuffle", type=str, default=False)
argparser.add_argument("--cytokine", type=str, default="cytokine")
argparser.add_argument("--feature_combinations_key", type=str, default="all")

args = argparser.parse_args()

cell_type = args.cell_type
shuffle = args.shuffle
cytokine = args.cytokine
feature_combinations_key = args.feature_combinations_key


# cell_type = "PBMC"
# cytokine = "IL-1 beta [NSU]"
# shuffle = "False"
# feature_combinations_key = "CorrDNA"


if shuffle == "True":
    shuffle = True
elif shuffle == "False":
    shuffle = False
else:
    raise ValueError("shuffle must be True or False")

print(f"cell_type: {cell_type}")
print(f"cytokine: {cytokine}")
print(f"shuffle: {shuffle}")
print(f"feature_combinations_key: {feature_combinations_key}")


# In[3]:


aggregation = True
nomic = True


# In[4]:


# set shuffle value
if shuffle:
    shuffle = "shuffled_baseline"
else:
    shuffle = "final"


# In[5]:


MODEL_TYPE = "regression"


# In[6]:


# load training data from indexes and features dataframe
data_split_path = pathlib.Path(
    f"../../0.split_data/indexes/{cell_type}/regression/aggregated_sc_and_nomic_data_split_indexes.tsv"
).resolve(strict=True)

feature_combinations_file_path = pathlib.Path(
    f"../../0.split_data/results/feature_combinations_{cell_type}.toml"
).resolve(strict=True)

data_path = pathlib.Path(
    f"../../../data/{cell_type}_preprocessed_sc_norm_aggregated_nomic.parquet"
).resolve(strict=True)

feature_combination_key_file = pathlib.Path(
    f"../../0.split_data/results/feature_combinations_keys.txt"
).resolve(strict=True)

# load the feature combinations file
feature_combinations = toml.load(feature_combinations_file_path)
feature_combinations_columns = feature_combinations[feature_combinations_key]

# dataframe with only the labeled data we want (exclude certain phenotypic classes)
data_df = pd.read_parquet(data_path)
data_df = data_df[feature_combinations_columns]

data_split_indexes = pd.read_csv(data_split_path, sep="\t")


# In[7]:


# select tht indexes for the training and test set
train_indexes = data_split_indexes.loc[data_split_indexes["label"] == "train"]
# subset data_df by indexes in data_split_indexes
training_data = data_df.loc[train_indexes["labeled_data_index"]]
# define metadata columns
# subset each column that contains metadata
metadata = training_data.filter(regex="Metadata")
# drop all metadata columns
data_x = training_data.drop(metadata.columns, axis=1)
labeled_data = training_data["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"]
# get all columns that contain "NSU" in the column name
data_y_cols = data_x.filter(regex="NSU").columns
train_y = training_data[data_y_cols]
train_x = data_x.drop(data_y_cols, axis=1)
# drop the oneb_Treatment_Dose_Inhibitor_Dose column if it exists
if "oneb_Treatment_Dose_Inhibitor_Dose" in train_x.columns:

    train_x = train_x.drop(columns="oneb_Treatment_Dose_Inhibitor_Dose")
loo = LeaveOneOut()
loo.get_n_splits(train_y)

train_data_y = train_y[cytokine]
model = ElasticNetCV(
    random_state=0,
    max_iter=10000,
    cv=loo,
    l1_ratio=[0.01, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.9, 0.99],
    alphas=[0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000],
    fit_intercept=True,
    selection="random",
)


# In[8]:


# train model on training data on all combinations of model types, feature types, and phenotypic classes
if shuffle == "shuffled_baseline":
    print("Shuffling data")
    for column in train_x:
        np.random.shuffle(train_x[column].values)
else:
    print("Not shuffling data")
# define parameters to search over
with parallel_backend("multiprocessing"):
    with warnings.catch_warnings():
        warnings.filterwarnings("ignore", category=ConvergenceWarning, module="sklearn")
        # create a logistic regression model
        model.fit(train_x, train_data_y)
        scores = cross_val_score(
            model,
            train_x,
            train_data_y,
            scoring="neg_mean_absolute_error",
            cv=loo,
            n_jobs=-1,
        )
        print(scores)
        print(f"Mean MAE: {scores.mean()}")
        print(f"Std MAE: {scores.std()}")
        print(f"R2: {model.score(train_x, train_data_y)}")

if (aggregation == True) and (nomic == True):
    results_dir = f"../models/regression/{cell_type}/aggregated_with_nomic/"
elif (aggregation == True) and (nomic == False):
    results_dir = f"../models/regression/{cell_type}/aggregated/"
elif (aggregation == False) and (nomic == True):
    results_dir = f"../models/regression/{cell_type}/sc_with_nomic/"
elif (aggregation == False) and (nomic == False):
    results_dir = f"../models/regression/{cell_type}/sc/"
else:
    print("Error")

# create results directory if it doesn't exist
pathlib.Path(results_dir).mkdir(parents=True, exist_ok=True)

# save final estimator
if shuffle == "shuffled_baseline":
    dump(
        model,
        f"{results_dir}/{cytokine}_{feature_combinations_key}_shuffled_baseline__all_nomic.joblib",
    )
elif shuffle == "final":
    dump(
        model,
        f"{results_dir}/{cytokine}_{feature_combinations_key}_final__all_nomic.joblib",
    )
else:
    print("Error")

# save condfig copy specific to this model to the folder with the results
# use pathlib
if shuffle == "shuffled_baseline":
    config_copy_path = pathlib.Path(
        f"{results_dir}/{cytokine}_{feature_combinations_key}_shuffled_baseline__all_nomic.toml"
    )
elif shuffle == "final":
    config_copy_path = pathlib.Path(
        f"{results_dir}/{cytokine}_{feature_combinations_key}_final__all_nomic.toml"
    )
else:
    print("Error")

# write toml file with parameters used from injected parameters

with open(config_copy_path, "w") as f:
    f.write(f"model_type='{shuffle}'\n")
    f.write(f"aggregation={aggregation}\n")
    f.write(f"nomic={nomic}\n")
    f.write(f"cell_type='{cell_type}'\n")
    f.write(f"cytokine='{cytokine}'\n")
    f.write(f"feature_combinations_key='{feature_combinations_key}'\n")

