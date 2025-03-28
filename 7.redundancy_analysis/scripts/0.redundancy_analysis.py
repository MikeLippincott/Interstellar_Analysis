#!/usr/bin/env python
# coding: utf-8

# # Canonical Correlation Analysis (CCA)
# Here I calculate the Canonical Correlation Coefficients and the canonical variables for the two datasets.
# I also plot the correlation coefficients and the canonical variables.

# This analysis is based on the following article:
# https://brainder.org/2019/12/27/redundancy-in-canonical-correlation-analysis/
#

# This is an involved analysis, so I will try to explain it as best as I can. There is a lot going on in the background that scikit learn does for us, so I will try to explain that as well.
#
# Extra resources and references:
# [CCA explaination](https://medium.com/@pozdrawiamzuzanna/canonical-correlation-analysis-simple-explanation-and-python-example-a5b8e97648d2)
# [CCA Sklearn](https://scikit-learn.org/stable/modules/generated/sklearn.cross_decomposition.CCA.html#sklearn.cross_decomposition.CCA.fit_transform)
# [Eigen value decomposition](https://gregorygundersen.com/blog/2018/07/17/cca/)
# [High level explaination of CCA](https://medium.com/analytics-vidhya/what-is-canonical-correlation-analysis-58ef4349c0b0)
# [CCA Tutorial](https://www.cs.cmu.edu/~tom/10701_sp11/slides/CCA_tutorial.pdf)
# [Redundancy Analysis](https://brainder.org/2019/12/27/redundancy-in-canonical-correlation-analysis/)

# #### A great visualization of CCA from user `ttnphns` on [stats.stackexchange.com](https://stats.stackexchange.com/questions/65692/how-to-visualize-what-canonical-correlation-analysis-does-in-comparison-to-what)
#
# ##### Principal Component Analysis (PCA)
# ![PCA](../images_for_notebook/PCA.jpg)
#
# ##### Multiple Linear Regression (MLR)
# ![MLR](../images_for_notebook/MLR.jpg)
#
# ##### Canonical Correlation Analysis (CCA)
# ![CCA](../images_for_notebook/CCA.jpg)

# In[1]:


# import libraries
import pathlib

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns
from sklearn.cross_decomposition import CCA
from sklearn.metrics import r2_score
from sklearn.preprocessing import StandardScaler
from tqdm import tqdm

# In[2]:


# Parameters
cell_type = "PBMC"
Shuffle = True


# In[3]:


# set paths to data
morphology_data_path = pathlib.Path(
    f"../../data/{cell_type}_preprocessed_sc_norm_aggregated.parquet"
).resolve(strict=True)
nomic_data_path = pathlib.Path(
    f"../../2.Nomic_nELISA_Analysis/Data/clean/Plate2/nELISA_plate_430420_{cell_type}_clean.parquet"
).resolve(strict=True)

# output path
results_file_path = pathlib.Path(f"../results/{cell_type}_redundancy_analysis.csv")
results_file_path.parent.mkdir(parents=True, exist_ok=True)

# read data
morphology_data = pd.read_parquet(morphology_data_path)
nomic_data = pd.read_parquet(nomic_data_path)


# In[4]:


# get the columns that contain metadata
morphology_metadata = morphology_data[
    morphology_data.columns[morphology_data.columns.str.contains("Metadata")]
]
morphology_data = morphology_data.drop(morphology_metadata.columns, axis=1)

nomic_data_values = nomic_data[
    nomic_data.columns[nomic_data.columns.str.contains("[NSU]", regex=True)]
]
nomic_metadata = nomic_data.drop(nomic_data_values.columns, axis=1)


# #### Data Needs to be in standard scalar format for CCA

# In[5]:


# standardize the data for nomic standard scalar
scaler = StandardScaler()
nomic_data_values = scaler.fit_transform(nomic_data_values)
nomic_data_values = pd.DataFrame(
    nomic_data_values,
    columns=nomic_data.columns[nomic_data.columns.str.contains("[NSU]", regex=True)],
)


# In[6]:


# check the scale of the data
nomic_data_values.describe()


# In[7]:


# shuffle the data for each column
# this changes the single well values but keeps the distribution of each column
if Shuffle:
    for column in nomic_data_values:
        np.random.shuffle(nomic_data_values[column].values)
    for column in morphology_data:
        np.random.shuffle(morphology_data[column].values)


# Note this analysis only works for paired data.
# Here the two datasets are the same size and the rows are paired.
# Where each row represents one well in the plate.
# Though the morphology data is at the single cell resolution, the data is averaged for each well.
# This is to match the resolution of the Nomic data.

# ### Variables
# $Y_{N \times P} = MorphologyData$
# $X_{N \times Q} = NomicData$
# Where
# $N = Rows of each data set$
# note that each data set is paired so N is the same for both
# $P = Columns of MorphologyData$
# $Q = Columns of NomicData$
# $K = Number of Canonical Variables$
# Where
# $K = min(P,Q)$
# unless $N < min(P,Q)$
# then $K = min(N, P, Q)$

# Note that K is the number of canonical variables that we will get out of the analysis.
# We calculate the canonical variables for both the X and Y data sets later in the analysis.

# We use the $min(N,P,Q)$ because the number of canonical variables is limited by the smallest dimension of the data sets.
# This includes the number of rows in the data sets.
# Usually the feature space is smaller than the number of samples, so $K = min(P,Q)$.
# In our case here, the number of samples is smaller than the feature space, so $K = min(N, P, Q)$.
# If we do not do this, we will get an error when we try to calculate the canonical variables.
# This is due to the fact that the eigenvalue decomposition will not work if the number of eigenvalues is greater than the number of samples.
# We transpose the data vectors when solving for the eigenvalues.

# In[8]:


# define the variables
N = morphology_data.shape[0]
P = morphology_data.shape[1]
Q = nomic_data_values.shape[1]
print("N:", N, "P:", P, "Q:", Q)
K = min(N, P, Q)
print("K:", K)


# #### Calculate the Canonical Correlation Coefficients
# X = Morphology Data
# Y = Nomic Data

# In[9]:


# define the cca model
cca = CCA(n_components=K)
# fit the model to the paired data sets
cca.fit(morphology_data, nomic_data_values)
# transform the data to the canonical space
# get the canonical coefficients for both data sets
X_c, Y_c = cca.transform(morphology_data, nomic_data_values)
# r2 score of the model fit
r2_model = [cca.score(morphology_data, nomic_data_values), X_c, Y_c][0]
print("The R2 score for the Canonical Correlation is:", r2_model)


# #### Extract the canonical loadings from the CCA
# In the absence of scikit-learn canonical loadings.
# We would calculate the loads as follows:
# ##### $\tilde{A} = corr(Y,U)$
# ##### $\tilde{B} = corr(X,V)$
# Where $X$ and $Y$ are the original data matrices
# and $U$ and $V$ are the canonical variates

# In[10]:


A_tilde = cca.x_loadings_.T
B_tilde = cca.y_loadings_.T


# From the canonical coefficients we can calculate the variance extracted by each canonical variable.
# #### $u_k = \frac{1}{P} \sum^P_{p=1} \tilde a^2_{pk}$
#
# #### $v_k = \frac{1}{Q} \sum^Q_{q=1} \tilde b^2_{qk}$
# Where $k$ is the canonical variable number and $p$ and $q$ are the variables in the original data sets.
#

# In[11]:


u_k = []
v_k = []
for i in A_tilde:
    u_k.append(np.mean(i**2))
for i in B_tilde:
    v_k.append(np.mean(i**2))


# We can caluculate the r2 score for each canonical variable as follows:

# In[12]:


# coefficients of determination for each canonical variable
r2 = r2_score(u_k, v_k)
r2


# We then caluclate the Redundancy Index (RI) for each canonical variable as follows:
# $RI_u = u_k * r^2_k$
# $RI_v = v_k * r^2_k$

# In[13]:


# calculate the redundancy index for each canonical variable
RI_u = []
RI_v = []

for i in u_k:
    RI_u.append(i * r2)
for i in v_k:
    RI_v.append(i * r2)


# We then calculate the total redundancy of both data sets as follows:
# #### $RI_{total} = \sum^K_{k=1} RI_u +  RI_v$
# From the total redundancy we can calculate the percentage contribution of each data set to the total redundancy as follows:
# #### $RI_{u\%} = \frac{\sum^K_{k=1} RI_u}{RI_{total}}$
# #### $RI_{v\%} = \frac{\sum^K_{k=1} RI_v}{RI_{total}}$

# In[14]:


RI_u_min = np.min(RI_u)
RI_v_min = np.min(RI_v)
RI_u_max = np.max(RI_u)
RI_v_max = np.max(RI_v)
global_min = np.min([RI_u_min, RI_v_min])
global_max = np.max([RI_u_max, RI_v_max])

# Calulate the global redundancy index
global_RI_u_v = np.sum(RI_u) + np.sum(RI_v)
global_RI_u = np.sum(RI_u) / global_RI_u_v * 100
global_RI_v = np.sum(RI_v) / global_RI_u_v * 100


# In[15]:


# make a dataframe of the results
results_df = pd.DataFrame(
    {
        "RI_u": RI_u,
        "RI_v": RI_v,
        "u_k": u_k,
        "v_k": v_k,
        "r2": r2,
        "Shuffle": Shuffle,
        "global_RI_u": global_RI_u,
        "global_RI_v": global_RI_v,
        "global_RI_u_v": global_RI_u_v,
        "global_min": global_min,
        "global_max": global_max,
    }
)
results_df.head(5)


# In[16]:


# check for file existence
if results_file_path.is_file():
    print("The results file exists.")
    #  read the results file
    existing_file_df = pd.read_csv(results_file_path)
    # check for if it is full for shuffle type
    if len(existing_file_df["Shuffle"].unique()) > 1:
        # delete the existing file
        results_file_path.unlink()
    elif not existing_file_df["Shuffle"].unique() == Shuffle:
        pd.concat([existing_file_df, results_df]).to_csv(results_file_path, index=False)
else:
    results_df.to_csv(results_file_path, index=False)
    print("The results file is created.")
