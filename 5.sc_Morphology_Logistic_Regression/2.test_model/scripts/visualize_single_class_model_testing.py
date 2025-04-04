#!/usr/bin/env python
# coding: utf-8

# In[1]:


import ast
import itertools
import pathlib

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import pyarrow.parquet as pq
import seaborn as sns
import toml
from joblib import dump, load
from sklearn.exceptions import ConvergenceWarning
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    accuracy_score,
    classification_report,
    confusion_matrix,
    f1_score,
)
from sklearn.model_selection import GridSearchCV, StratifiedKFold, train_test_split
from sklearn.utils import parallel_backend, shuffle

# In[2]:


# Parameters
cell_type = "SHSY5Y"
aggregation = True
nomic = True
flag = True
control = "DMSO_0.100_%_DMSO_0.025_%"
treatment = "LPS_100.000_ug_per_ml_DMSO_0.025_%"


# In[3]:


print(cell_type, aggregation, nomic, flag, control, treatment)


# In[4]:


MODEL_TYPE = "binary_classification"
if flag == False:
    # read in toml file and get parameters
    toml_path = pathlib.Path("../1.train_models/single_class_config.toml")
    with open(toml_path, "r") as f:
        config = toml.load(f)
    control = config["logistic_regression_params"]["control"]
    treatment = config["logistic_regression_params"]["treatments"]
    aggregation = ast.literal_eval(config["logistic_regression_params"]["aggregation"])
    nomic = ast.literal_eval(config["logistic_regression_params"]["nomic"])
    cell_type = config["logistic_regression_params"]["cell_type"]
    print(aggregation, nomic, cell_type)


# In[5]:


if flag == False:
    # read in toml file and get parameters
    toml_path = pathlib.Path("../1.train_models/single_class_config.toml")
    with open(toml_path, "r") as f:
        config = toml.load(f)
    f.close()
    control = config["logistic_regression_params"]["control"]
    treatment = config["logistic_regression_params"]["treatments"]
    aggregation = ast.literal_eval(config["logistic_regression_params"]["aggregation"])
    nomic = ast.literal_eval(config["logistic_regression_params"]["nomic"])
    cell_type = config["logistic_regression_params"]["cell_type"]
    print(aggregation, nomic, cell_type)


# In[6]:


# set path for figures
if (aggregation == True) and (nomic == True):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/aggregated_with_nomic/{control}__{treatment}"
    )
    results_path = pathlib.Path(
        f"./results/single_class/{cell_type}/aggregated_with_nomic/{control}__{treatment}"
    )
elif (aggregation == True) and (nomic == False):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/aggregated/{control}__{treatment}"
    )
    results_path = pathlib.Path(
        f"./results/single_class/{cell_type}/aggregated/{control}__{treatment}"
    )
elif (aggregation == False) and (nomic == True):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/sc_with_nomic/{control}__{treatment}"
    )
    results_path = pathlib.Path(
        f"./results/single_class/{cell_type}/sc_with_nomic/{control}__{treatment}"
    )
elif (aggregation == False) and (nomic == False):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/sc/{control}__{treatment}"
    )
    results_path = pathlib.Path(
        f"./results/single_class/{cell_type}/sc/{control}__{treatment}"
    )
else:
    print("Error")


# In[7]:


# read in data
df = pd.read_csv(f"{results_path}/compiled_predictions.csv")


# In[8]:


# set model path from parameters
if (aggregation == True) and (nomic == True):
    model_path = pathlib.Path(
        f"models/single_class/{cell_type}/aggregated_with_nomic/{MODEL_TYPE}/{control}__{treatment}"
    )
elif (aggregation == True) and (nomic == False):
    model_path = pathlib.Path(
        f"models/single_class/{cell_type}/aggregated/{control}__{treatment}"
    )
elif (aggregation == False) and (nomic == True):
    model_path = pathlib.Path(
        f"models/single_class/{cell_type}/sc_with_nomic/{control}__{treatment}"
    )
elif (aggregation == False) and (nomic == False):
    model_path = pathlib.Path(
        f"models/single_class/{cell_type}/sc/{control}__{treatment}"
    )
else:
    print("Error")


# In[9]:


# set path for figures
if (aggregation == True) and (nomic == True):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/aggregated_with_nomic/{control}__{treatment}"
    )

elif (aggregation == True) and (nomic == False):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/aggregated/{control}__{treatment}"
    )

elif (aggregation == False) and (nomic == True):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/sc_with_nomic/{control}__{treatment}"
    )

elif (aggregation == False) and (nomic == False):
    figure_path = pathlib.Path(
        f"./figures/single_class/{cell_type}/sc/{control}__{treatment}"
    )

else:
    print("Error")
figure_path.mkdir(parents=True, exist_ok=True)


# In[10]:


model_types = ["final", "shuffled_baseline"]
feature_types = ["CP"]
phenotypic_classes = [treatment]
evaluation_types = ["train", "test"]


# In[11]:


for model_type, feature_type, phenotypic_class, evaluation_type in itertools.product(
    model_types, feature_types, phenotypic_classes, evaluation_types
):
    print(model_type, feature_type, phenotypic_class, evaluation_type)
    # load model
    model = load(f"../1.train_models/{model_path}/{model_type}__{feature_type}.joblib")
    print(model)

    if evaluation_type == "train":
        # get data_split train
        df_train = df[df["data_split"] == "train"]

        if model_type == "final":
            df_train_final = df_train[df_train["shuffled"] == False]
            model = load(
                f"../1.train_models/{model_path}/{model_type}__{feature_type}.joblib"
            )

            sns.heatmap(
                confusion_matrix(
                    df_train_final["Phenotypic_Class_True"],
                    df_train_final["Phenotypic_Class_Predicted"],
                ),
                annot=True,
                fmt="g",
            )
            plt.xlabel("Predicted")
            plt.ylabel("True")
            plt.title(
                f"Confusion matrix of {model_type} model on {feature_type} features for {phenotypic_class}"
            )
            plt.savefig(
                f"{figure_path}/{model_type}__train__{feature_type}__{phenotypic_class}.png"
            )
            plt.show()
        elif model_type == "shuffled_baseline":
            df_train_shuffled = df_train[df_train["shuffled"] == True]
            model = load(
                f"../1.train_models/{model_path}/{model_type}__{feature_type}.joblib"
            )
            sns.heatmap(
                confusion_matrix(
                    df_train_shuffled["Phenotypic_Class_True"],
                    df_train_shuffled["Phenotypic_Class_Predicted"],
                ),
                annot=True,
                fmt="g",
            )
            plt.xlabel("Predicted")
            plt.ylabel("True")
            plt.title(
                f"Confusion matrix of {model_type} model on {feature_type} features for {phenotypic_class}"
            )
            plt.savefig(
                f"{figure_path}/{model_type}__train__{feature_type}__{phenotypic_class}.png"
            )
            plt.show()
        else:
            print("Error")
    elif evaluation_type == "test":
        # get data_split test
        df_test = df[df["data_split"] == "test"]
        if model_type == "final":
            df_test_final = df_test[df_test["shuffled"] == False]
            model = load(
                f"../1.train_models/{model_path}/{model_type}__{feature_type}.joblib"
            )
            sns.heatmap(
                confusion_matrix(
                    df_test_final["Phenotypic_Class_True"],
                    df_test_final["Phenotypic_Class_Predicted"],
                ),
                annot=True,
                fmt="g",
            )
            plt.xlabel("Predicted")
            plt.ylabel("True")
            plt.title(
                f"Confusion matrix of {model_type} model on {feature_type} features for {phenotypic_class}"
            )
            plt.savefig(
                f"{figure_path}/{model_type}__test__{feature_type}__{phenotypic_class}.png"
            )
            plt.show()
        elif model_type == "shuffled_baseline":
            df_test_shuffled = df_test[df_test["shuffled"] == True]
            model = load(
                f"../1.train_models/{model_path}/{model_type}__{feature_type}.joblib"
            )
            sns.heatmap(
                confusion_matrix(
                    df_test_shuffled["Phenotypic_Class_True"],
                    df_test_shuffled["Phenotypic_Class_Predicted"],
                ),
                annot=True,
                fmt="g",
            )
            plt.xlabel("Predicted")
            plt.ylabel("True")
            plt.title(
                f"Confusion matrix of {model_type} model on {feature_type} features for {phenotypic_class}"
            )
            plt.savefig(
                f"{figure_path}/{model_type}__test__{feature_type}__{phenotypic_class}.png"
            )
            plt.show()
        else:
            print("Error")


# In[12]:


"../1.train_models/models/single_class/SHSY5Y/aggregated_with_nomic/DMSO_0.100_%_DMSO_0.025_%__LPS_100.000_ug_per_ml_DMSO_0.025_%/final__CP.joblib"
"../1.train_models/models/single_class/SHSY5Y/aggregated_with_nomic/binary_classification/DMSO_0.100_%_DMSO_0.025_%__LPS_100.000_ug_per_ml_DMSO_0.025_%/final__CP.joblib"
