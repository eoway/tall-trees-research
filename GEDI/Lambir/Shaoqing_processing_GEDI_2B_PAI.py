import os
import glob
import h5py
import numpy as np
import pandas as pd


os.chdir('/n/holylfs04/LABS/moorcroft_lab/Users/sqliu/GEDI_data/SouthEast_Asia/Lambir')
#os.chdir('/n/holylfs04/LABS/moorcroft_lab/Users/sqliu/GEDI_data/TonziRanch')

h5file = sorted(glob.glob('*.h5'))


xmin = 114.018
xmax = 114.048
xmid = 114.033
ymin = 4.187
ymax = 4.217
ymid = 4.202

"""
xmin = -120.905
xmax = -120.895
ymin = 38.395
ymax = 38.405
"""
#38.40, -120.80
for i in range(len(h5file)):
    gediL2B = h5py.File(h5file[i], 'r')
    # Get the beam information
    beamNames = [g for g in gediL2B.keys() if g.startswith('BEAM')]
    gediL2B_objs = []
    gediL2B.visit(gediL2B_objs.append)                                           # Retrieve list of datasets
    gediSDS = [o for o in gediL2B_objs if isinstance(gediL2B[o], h5py.Dataset)]
	# iterate in each beam
    df = pd.DataFrame()
    
    for j in range(4, len(beamNames)):
# The whole beam names are:
# BEAM0000 is a Coverage beam
# BEAM0001 is a Coverage beam
# BEAM0010 is a Coverage beam
# BEAM0011 is a Coverage beam
# BEAM0101 is a Full power beam
# BEAM0110 is a Full power beam
# BEAM1000 is a Full power beam
# BEAM1011 is a Full power beam
# we only care about Full power beam
        beamSDS = [g for g in gediSDS if beamNames[j] in g]
		# get the lon, lat first
        zLat = gediL2B[[g for g in beamSDS if g.endswith('/lat_lowestmode')][0]][()]
        zLon = gediL2B[[g for g in beamSDS if g.endswith('/lon_lowestmode')][0]][()]
        iy = np.where((zLat<ymax)&(zLat>ymin))
        ix = np.where((zLon<xmax)&(zLon>xmin))
        iy = list(iy[0])
        ix = list(ix[0])
        id = set(ix)&set(iy)
        zLat = gediL2B[[g for g in beamSDS if g.endswith('/lat_lowestmode')][0]][sorted(list(id))]
        zLon = gediL2B[[g for g in beamSDS if g.endswith('/lon_lowestmode')][0]][sorted(list(id))]
        dem = gediL2B[[g for g in beamSDS if g.endswith('/digital_elevation_model')][0]][sorted(list(id))]
        zElevation = gediL2B[[g for g in beamSDS if g.endswith('/elev_lowestmode')][0]][sorted(list(id))]
        zHigh = gediL2B[[g for g in beamSDS if g.endswith('/elev_highestreturn')][0]][sorted(list(id))]
        canopyHeight = gediL2B[[g for g in beamSDS if g.endswith('/rh100')][0]][sorted(list(id))]
        quality = gediL2B[[g for g in beamSDS if g.endswith('/l2b_quality_flag')][0]][sorted(list(id))]
        degrade = gediL2B[[g for g in beamSDS if g.endswith('/degrade_flag')][0]][sorted(list(id))]
        sensitivity = gediL2B[[g for g in beamSDS if g.endswith('/sensitivity')][0]][sorted(list(id))]
        pai = gediL2B[[g for g in beamSDS if g.endswith('/pai')][0]][sorted(list(id))]
        pai_z = gediL2B[[g for g in beamSDS if g.endswith('/pai_z')][0]][sorted(list(id))]
        shotNums = gediL2B[f'{beamNames[j]}/shot_number'][sorted(list(id))]
        shotIndex = np.arange(shotNums.size)
        beamNums = [beamNames[j]]*shotNums.size
        canopyHeight = canopyHeight / 100 
		
        transectDF = pd.DataFrame({'Shot Index': shotIndex, 'Shot Number': shotNums, 'Beam': beamNums, 'Latitude': zLat, 'Longitude': zLon,
                           'Tandem-X DEM': dem, 'Elevation (m)': zElevation, 'Canopy Elevation (m)': zHigh,
                           'Canopy Height (rh100)': canopyHeight, 'Quality Flag': quality, 'Degrade Flag': degrade,
                           'Plant Area Index': pai, 'Sensitivity': sensitivity})
        pai_z_DF = pd.DataFrame(pai_z, columns=["pai z_%03d_cm" % i for i in range(0,150,5)])
        transectDF = pd.concat([transectDF, pai_z_DF], axis=1)
        df = df.append(transectDF)
        df.to_csv('GEDI'+str(i)+'.csv', index=False)

	
