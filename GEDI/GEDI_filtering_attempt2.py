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
len(gediFiles)

with open("Output.txt", "w") as text_file:
    text_file.write("Number of Files: %i" % len(gediFiles))

df = pd.DataFrame()
for i in range(len(gediFiles)):
    if (i!=0 and np.mod(i, 5) == 0) or (i == len(csvfile) - 1): # save the dataframe by every 500 h5 files or if it iterates to the final h5file
        df.to_csv('/n/holyscratch01/moorcroft_lab/nhegwood/Africa_edge_filtered/Africa_edge_filt+' + f'{i:05}' + '.csv', index=False)
    
    full = pd.read_csv(gediFiles[i], header= 0)
    
    # add an s to the shot number 
    full["Shot_Number"] = 's' + full["Shot_Number"].astype(str)

    # Filter based on quality flag and sensitivity > 95
    filt95 = full.where(full['Quality_Flag']==1)  # Set any poor quality returns to NaN
    filt95 = filt95.where(filt95['Degrade_Flag']==0)
    filt95 = filt95.where(filt95['Sensitivity'] > 0.95)
    filt95 = filt95.dropna()  # Drop all of the rows (shots) that did not pass the quality filtering above
    
    a = np.array(filt95['Beam'])
    b = []
    for i in range(len(a)):
        if a[i] in ['BEAM0000', 'BEAM0001', 'BEAM0010', 'BEAM0011']:
            b.append("coverage")
        else:
            b.append("full_power")
    filt95['beam_type'] = b
    
    df = df.append(filt95)

filt95.to_csv('/n/holyscratch01/moorcroft_lab/nhegwood/Africa_edge_filtered/Africa_edge_filtered.csv', index=False)

