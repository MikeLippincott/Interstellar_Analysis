#!/usr/bin/env python
# coding: utf-8

# This notebook which is run as a script for each feature is used to compute the ANOVA and Tuket HSD test for the feature specified.
# We perform the ANOVA across death types and then perform the Tukey HSD test to see which death types are significantly different from each other.

# In[1]:


import argparse
import pathlib
import warnings

import numpy as np
import pandas as pd
import statsmodels.api as sm
import toml

warnings.filterwarnings("ignore")
from statsmodels.formula.api import ols
from statsmodels.stats.multicomp import pairwise_tukeyhsd

# In[2]:


# set up command line argument parser
parser = argparse.ArgumentParser(
    description="Run ANOVA and Tukey's HSD on all groupings"
)
parser.add_argument(
    "-c",
    "--cell_type",
    type=str,
    help="Cell type to run ANOVA and Tukey's HSD on",
)

parser.add_argument(
    "-f",
    "--feature",
    type=str,
    help="feature to run ANOVA and Tukey's HSD on",
)
parser.add_argument(
    "-s",
    "--shuffle_labels",
    action="store_true",
    help="Shuffle labels to create a null distribution",
)
# parse arguments from command line
args = parser.parse_args()
cell_type = args.cell_type
feature = args.feature
shuffle_labels = args.shuffle_labels


# In[3]:


Metadata_columns = [
    "Metadata_cell_type",
    "Metadata_Well",
    "Metadata_number_of_singlecells",
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose",
]
Metadata_columns = Metadata_columns + [feature]


# In[4]:


# Import Data
# set data file path under pathlib path for multi-system use
file_path = pathlib.Path(f"../../data/{cell_type}_preprocessed_sc_norm.parquet")
# df = pd.read_parquet(file_path, columns=Metadata_columns)
df = pd.read_parquet(file_path, columns=Metadata_columns)


# In[5]:


# get 10% of the data from each well.
df = (
    df.groupby("Metadata_Well")
    .apply(lambda x: x.sample(frac=0.1, random_state=0))
    .reset_index(drop=True)
)
print(df.shape)


# In[6]:


# toml file path
ground_truth_file = pathlib.Path(
    "../../4.sc_Morphology_Neural_Network_MLP_Model/MLP_utils/ground_truth.toml"
).resolve(strict=True)
# read toml file
ground_truth = toml.load(ground_truth_file)
apopotosis_trts = ground_truth["Apoptosis"]["apoptosis_groups_list"]
pyroptosis_trts = ground_truth["Pyroptosis"]["pyroptosis_groups_list"]
healthy_trts = ground_truth["Healthy"]["healthy_groups_list"]

# make a column that has the class of each treatment


df["apoptosis"] = df.apply(
    lambda row: row["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] in apopotosis_trts,
    axis=1,
)
df["pyroptosis"] = df.apply(
    lambda row: row["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] in pyroptosis_trts,
    axis=1,
)
df["healthy"] = df.apply(
    lambda row: row["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] in healthy_trts,
    axis=1,
)

# merge apoptosis, pyroptosis, and healthy columns into one column

df["labels"] = df.apply(
    lambda row: "apoptosis"
    if row["apoptosis"]
    else "pyroptosis"
    if row["pyroptosis"]
    else "healthy",
    axis=1,
)
# drop apoptosis, pyroptosis, and healthy columns
df.drop(columns=["apoptosis", "pyroptosis", "healthy"], inplace=True)


# In[7]:


# show the data prior to shuffle
df.head()


# In[8]:


np.random.seed(0)
if shuffle_labels is True:
    for col in df.columns:
        df[col] = np.random.permutation(df[col].values)


# In[9]:


# and after shuffle
df.head()


# In[10]:


df_metadata = df.filter(regex="Metadata")
df_data = df.drop(df_metadata.columns, axis=1)


# In[11]:


# anova for each feature in the dataframe with posthoc tukey test to determine which groups are different from each other
lst = []

formula = f"{feature} ~ C(labels)"
model = ols(formula, df_data).fit()
aov_table = sm.stats.anova_lm(
    model, typ=2
)  # type 2 for unbalanced data and no regard for order

posthoc = pairwise_tukeyhsd(  # posthoc tukey test independent observations, and normal distribution of data
    df_data[feature],
    df_data["labels"],
    alpha=0.001,
)
lst.append([posthoc, feature])


# In[12]:


tukey_df_list = []
for i in lst:
    j = pd.DataFrame(i[0]._results_table.data[1:])
    j["features"] = np.repeat(i[1], len(j))
    tukey_df_list.append(j)
tukey_df = pd.concat(tukey_df_list)


tukey_df.columns = [
    "group1",
    "group2",
    "meandiff",
    "lower",
    "upper",
    "p-adj",
    "reject",
    "features",
]
# drop the other organelle
# make new column with the absolute value of the p-adj
tukey_df["p-adj_abs"] = abs(tukey_df["p-adj"])
# make new column that states if the relationship is positive or negative
tukey_df["pos_neg"] = np.where(tukey_df["p-adj"] > 0, "positive", "negative")
# order the features by p-adj value
tukey_df = tukey_df.sort_values(by="p-adj_abs", ascending=False)
if shuffle_labels:
    tukey_df["shuffled"] = True
else:
    tukey_df["shuffled"] = False


# In[13]:


# save the dataframe as a parquet file
anova_results_path = pathlib.Path(
    f"../results/{cell_type}/{feature}_{shuffle_labels}_anova_results_all_treatments.parquet"
)
# if the directory does not exist, create it
if not anova_results_path.parent.exists():
    anova_results_path.parent.mkdir(parents=True)
# save the dataframe as a parquet file
tukey_df.to_parquet(anova_results_path)
tukey_df.head()
