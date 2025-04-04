#!/usr/bin/env python
# coding: utf-8

# This noteboook pre-processes the single cell morphology data to be ready for exploratory analysis and machine learning.

# In[ ]:


import pathlib

import numpy as np
import pandas as pd
import papermill as pm
import pyarrow as pa
import pyarrow.parquet as pq

# In[2]:


# Parameters
cell_type = "SHSY5Y"


# In[ ]:


# Define inputs
feature_file = pathlib.Path(f"../data/{cell_type}_sc_norm_fs.parquet")
feature_df = pd.read_parquet(feature_file)


# In[ ]:


# replace all " " with "_" in all values of the dataframe
feature_df = feature_df.replace(to_replace=" ", value="_", regex=True)


# In[ ]:


# remove uM in each row of the Metadata_inducer1_concentration column
feature_df["Metadata_inducer1_concentration"] = feature_df[
    "Metadata_inducer1_concentration"
].str.replace("µM", "")


# In[ ]:


feature_df["Metadata_inducer1_concentration"].unique()


# In[ ]:


# define output file path
feature_df_out_path = pathlib.Path(f"../data/{cell_type}_preprocessed_sc_norm.parquet")


# In[ ]:


print(feature_df.shape)
feature_df.head()


# In[ ]:


# removing costes features as they behave with great variance across all data
feature_df = feature_df.drop(feature_df.filter(regex="Costes").columns, axis=1)
print(feature_df.shape)
feature_df.head()


# In[ ]:


# replacing '/' in treatment dosage column to avoid errors in file interpolation including such strings
feature_df = feature_df.replace(to_replace="/", value="_per_", regex=True)


# In[ ]:


# replace nan values with 0

columns_to_fill = [
    "Metadata_inducer1_concentration",
    "Metadata_inducer2_concentration",
    "Metadata_inhibitor_concentration",
]
feature_df[columns_to_fill].fillna(0, inplace=True)


# In[ ]:


# replace all None values with 0
feature_df["Metadata_inducer1_concentration"].fillna(0, inplace=True)


# In[ ]:


# create a list of columns to be converted to float
col_list = [
    "Metadata_inducer1_concentration",
    "Metadata_inducer2_concentration",
    "Metadata_inhibitor_concentration",
]
# loop through the list and convert each column to float
for i in col_list:
    feature_df[i] = feature_df[i].apply(
        lambda x: f"{float(x):.3f}" if float(x) != 0 else float(x)
    )


# #### Combine Inducer1 and Inducer2 into one column

# In[ ]:


# treatment column merge
conditions = [
    (feature_df["Metadata_inducer2"].isnull()),
    feature_df["Metadata_inducer2"].notnull(),
]

results = [
    (feature_df["Metadata_inducer1"]).astype(str),
    (
        feature_df["Metadata_inducer1"]
        + "_"
        + feature_df["Metadata_inducer2"].astype(str)
    ),
]
feature_df["Metadata_Treatment"] = np.select(condlist=conditions, choicelist=results)


# dose column merge
results = [
    (
        feature_df["Metadata_inducer1_concentration"].astype(str)
        + "_"
        + feature_df["Metadata_inducer1_concentration_unit"].astype(str)
    ),
    (
        feature_df["Metadata_inducer1_concentration"].astype(str)
        + "_"
        + feature_df["Metadata_inducer1_concentration_unit"].astype(str)
        + "_"
        + feature_df["Metadata_inducer2_concentration"].astype(str)
        + "_"
        + feature_df["Metadata_inducer2_concentration_unit"].astype(str)
    ),
]
feature_df["Metadata_Dose"] = np.select(condlist=conditions, choicelist=results)


# In[ ]:


# one beta of inudcer1, inducer1 concentration, inhibitor, and inhibitor concentration all as 1 beta term
feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = (
    feature_df["Metadata_Treatment"]
    + "_"
    + feature_df["Metadata_Dose"].astype(str)
    # + "_"
    # + feature_df['Metadata_inducer1_concentration_unit'].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor"].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor_concentration"].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor_concentration_unit"].astype(str)
).astype(str)


# two beta of inducer1, inhibitor, and inhibitor concentration all as 1 beta term + inducer1 concentration as 2nd beta term
feature_df["twob_Metadata_Treatment_Dose_Inhibitor_Dose"] = (
    feature_df["Metadata_Treatment"]
    + "_"
    + feature_df["Metadata_inhibitor"].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor_concentration"].astype(str)
    + "__"
    + feature_df["Metadata_Dose"].astype(str)
).astype(str)

# three beta of inducer 1 as 1 beta term, inducer1 concentration as 2nd beta term, inhibitor and inhibitor concentration as 3rd beta term
feature_df["threeb_Metadata_Treatment_Dose_Inhibitor_Dose"] = (
    feature_df["Metadata_Treatment"]
    + "__"
    + feature_df["Metadata_Dose"].astype(str)
    + "__"
    + feature_df["Metadata_inducer1_concentration_unit"].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor"].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor_concentration"].astype(str)
).astype(str)

# four beta of inducer 1 as 1 beta term, inducer1 concentration as 2nd beta term, inhibitor as 3rd beta term, and inhibitor concentration as 4th beta term
feature_df["fourb_Metadata_Treatment_Dose_Inhibitor_Dose"] = (
    feature_df["Metadata_Treatment"]
    + "__"
    + feature_df["Metadata_Dose"].astype(str)
    + "__"
    + feature_df["Metadata_inducer1_concentration_unit"].astype(str)
    + "_"
    + feature_df["Metadata_inhibitor"].astype(str)
    + "__"
    + feature_df["Metadata_inhibitor_concentration"].astype(str)
).astype(str)


# In[ ]:


replacement_dict = {
    "None": "0",
    "µ": "u",
    "nan": "0",
}
for pattern, replacement in replacement_dict.items():
    print(pattern, replacement)
    feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
        "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
    ].replace(to_replace=str(pattern), value=str(replacement), regex=True)


# In[ ]:


feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace("media_ctr_0.0_0_Media_ctr_0_0", "Media", regex=False)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace("media_ctr_0.0_0_Media_0_0", "Media", regex=False)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace("media_ctr_0.0_0_Media_ctr_0.0_0", "Media", regex=False)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace(
    "Flagellin_1.000_0_Disulfiram_1.000_uM",
    "Flagellin_1.000_ug_per_ml_Disulfiram_1.000_uM",
    regex=False,
)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace(
    "Flagellin_1.000_0_DMSO_0.025_%",
    "Flagellin_1.000_ug_per_ml_DMSO_0.025_%",
    regex=False,
)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace(
    "Flagellin_0.100_ug_per_ml_DMSO_0.000_%",
    "Flagellin_0.100_ug_per_ml_DMSO_0.025_%",
    regex=False,
)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace(
    "Flagellin_1.000_ug_per_ml_DMSO_0.000_%",
    "Flagellin_1.000_ug_per_ml_DMSO_0.025_%",
    regex=False,
)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace("media_ctr_Media_ctr_nan__0.0_µg_per_ml", "Media", regex=False)

feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"] = feature_df[
    "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"
].str.replace("media_ctr_0.0_ug_per_ml_Media_ctr_0_0", "Media", regex=False)


# In[18]:


print(len(feature_df["oneb_Metadata_Treatment_Dose_Inhibitor_Dose"].unique()))


# In[19]:


# need to convert to strings to save as parquet
# if the column is an object then convert it to a string
for column in feature_df.columns:
    if feature_df[column].dtype == "object":
        feature_df[column] = feature_df[column].astype(str)


# In[20]:


# clean up any NaN values
print(feature_df.shape)
# drop rows that contain any NaN values
feature_df = feature_df.dropna()
print(feature_df.shape)


# In[21]:


# write to parquet file
feature_df.to_parquet(feature_df_out_path)
