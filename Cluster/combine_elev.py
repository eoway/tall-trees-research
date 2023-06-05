#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Feb 18 11:22:32 2023

@author: nomes
"""

import glob
import os


path = '/n/holyscratch01/moorcroft_lab/nhegwood/copernicus'

file_list = glob.glob(path + '*.tif')
        
files_string = " ".join(file_list)

command = 'gdal_merge.py -o ' + 'SEAsia_cop_elev' + '.tif' +' -of gtiff ' + files_string

os.system(command)