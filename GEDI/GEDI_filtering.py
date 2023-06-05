#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jan 22 21:33:21 2023

@author: nomes
"""

import os
import pandas as pd
import numpy as np
import glob

gediFiles = [g for g in os.listdir() if g.startswith('SEA') and g.endswith('.csv')]  # List GEDI L2A .h5 files in the inDir

df = pd.DataFrame()

for i in range(len(gediFiles)):
    
    full = pd.read_csv(gediFiles[i], header= 0)
    
    # add an s to the shot number 
    full["Shot_Number"] = 's' + full["Shot_Number"].astype(str)

    # Filter based on quality flag and sensitivity > 95
    filt95 = full.where(full['Quality_Flag']==1)  # Set any poor quality returns to NaN
    filt95 = filt95.where(filt95['Degrade_Flag']==0)
    filt95 = filt95.where(filt95['Sensitivity'] > 0.95)
    filt95 = filt95.dropna()  # Drop all of the rows (shots) that did not pass the quality filtering above
    
    df = df.append(filt95)

a = np.array(df['Beam'])
b = []
for i in range(len(a)):
    if a[i] in ['BEAM0000', 'BEAM0001', 'BEAM0010', 'BEAM0011']:
        b.append("coverage")
    else:
        b.append("full_power")
df['beam_type'] = b

df.to_csv('/n/holyscratch01/moorcroft_lab/nhegwood/SEAsia_filtered.csv', index=False)

