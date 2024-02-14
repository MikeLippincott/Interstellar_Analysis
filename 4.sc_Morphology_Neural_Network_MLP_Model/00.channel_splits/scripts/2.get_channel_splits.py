#!/usr/bin/env python
# coding: utf-8

# In[2]:


import argparse
import gc
import itertools
import pathlib

import numpy as np
import pandas as pd
import toml

# In[3]:


argparser = argparse.ArgumentParser()
argparser.add_argument("--cell_type", default="all")

args = argparser.parse_args()

cell_type = args.cell_type


# In[5]:


# Parameters
aggregation = True
nomic = True


# In[6]:


MODEL_TYPE = "regression"


# In[7]:


file_path = pathlib.Path(
    f"../../../data/{cell_type}_preprocessed_sc_norm.parquet"
).resolve(strict=True)
data_df = pd.read_parquet(file_path)

data_df.head()


# In[8]:


# subset each column that contains metadata
metadata = data_df.filter(regex="Metadata")

# get all columns that are not metadata except for metadata_Well
data = data_df.drop(metadata.columns, axis=1)

# get the metadata_Well column
metadata_well = metadata[
    ["Metadata_Well", "oneb_Metadata_Treatment_Dose_Inhibitor_Dose"]
]

data_df = pd.merge(data, metadata_well, left_index=True, right_index=True)
data_df


# In[9]:


# define the list of the channels
channel_list = ["DNA", "Gasdermin", "ER", "Mito", "PM"]


# In[10]:


# combiantions of channels
channel_combinations = []
for i in range(1, len(channel_list) + 1):
    tmp_list = list(itertools.combinations(channel_list, i))
    channel_combinations += tmp_list


# In[11]:


# set up the LOO channel with recursion for dropping multiple channels


def channel_drop(df, channel):
    df = df.drop(df.filter(regex=channel).columns, axis=1)
    return df


# In[12]:


# dictionary for each df to go into
# results_dict = {}


# In[24]:


index_path = pathlib.Path(f"../indexes/{cell_type}/regression/channels").resolve()
index_path.mkdir(parents=True, exist_ok=True)

list_of_combinations = []
for i in channel_combinations:
    tmp = data_df.copy()

    if len(i) == 5:
        channel_list_index = "No Channels"
        print(i)
        for j in range(1, len(i)):
            tmp = channel_drop(tmp, i[j])
        list_of_combinations.append(channel_list_index)
        print(channel_list_index)
    elif len(i) < 5:
        for j in range(1, len(i)):
            tmp = channel_drop(tmp, i[j])
        channel_list_index = [x for x in channel_list if x in i]
        channel_list_index = "_".join(channel_list_index)
        list_of_combinations.append(channel_list_index)
        print(channel_list_index)
    new_df = pd.merge(tmp, metadata_well, left_index=True, right_index=True)
    # set file path
    file_path = pathlib.Path(
        f"../indexes/{cell_type}/regression/channels/{channel_list_index}.parquet"
    )
    # save the dataframe
    new_df.to_parquet(file_path)


# In[17]:


index_list = []
for i in list_of_combinations:
    index_list.append(i + ".parquet")
# write the list to a text file
# file path
file_write_path = pathlib.Path(f"../cytokine_list/channel_splits.txt")
with open(file_write_path, "w") as f:
    for i in index_list:
        f.write("%s\n" % i)
