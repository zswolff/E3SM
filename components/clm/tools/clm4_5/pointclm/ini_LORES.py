#!/sw/xk6/python/2.7.9/sles11.3_gnu4.3.4/bin/python
import netCDF4 as nc4
import numpy as np
import os, sys
from scipy.spatial import distance
from optparse import OptionParser

parser = OptionParser()

parser.add_option("--casename", dest="casename", default="", \
                  help="case name?")
parser.add_option("--diricase", dest="diricase", default="", \
                  help="case input directory")
parser.add_option("--dirocase", dest="dirocase", default="", \
                  help="case output directory")
parser.add_option('--restart_year', dest='restart_year', default="", \
                    help='Year of restart file to modify')
parser.add_option('--acme_input', dest='acme_input', default="", \
                  help='acme input data directory')
(options, args) = parser.parse_args()

rest_str = str(10000+int(options.restart_year))
frestrt=options.diricase+'/'+options.casename+'.clm2.r.'+ \
                rest_str[1:]+'-01-01-00000.nc'
finsert=options.acme_input+"/lnd/clm2/rawdata/pforms_LORES_lmchk.nc"

print frestrt
#print os.path.basename(frestrt)
#-os.system("cp " + frestrt + " ./") 

ncr = nc4.Dataset(frestrt, "a")
nci = nc4.Dataset(finsert, "r")

clat1d = ncr.variables["cols1d_lat"]
clon1d = ncr.variables["cols1d_lon"]
cidx1d = ncr.variables["cols1d_gridcell_index"]

glat1d = ncr.variables["grid1d_lat"]
glon1d = ncr.variables["grid1d_lon"]

# 0-360 to -180 180
#-glon1d[:] = np.where(glon1d[:]>180., glon1d[:]-360., glon1d[:])

glat05 = nci.variables["lat"]
glon05 = nci.variables["lon"]

xcor, ycor = np.meshgrid(glon05, glat05)


grcell = ncr.dimensions['gridcell']
column = ncr.dimensions['column' ]
levgrd = ncr.dimensions['levgrnd']


zcor = zip(xcor.reshape(96*144), ycor.reshape(96*144))


print grcell.name, grcell.__len__()


ig = 0 
jj_idx = []
ii_idx = []


for glat, glon in zip(glat1d[:], glon1d[:]):

    indx = distance.cdist([(glon,glat)], zcor).argmin()
    jj, ii = np.unravel_index(indx, (96,144))

    print glat, "-->", glat05[jj], glon, "-->", glon05[ii], jj, ii, indx, ig

    jj_idx.append(jj)
    ii_idx.append(ii)

    ig = ig + 1


print len(ii_idx), len(jj_idx)

np.save('xindex', np.array(ii_idx))
np.save('yindex', np.array(jj_idx))

#find index

#-ii_idx = np.load('xindex.npy')
#-jj_idx = np.load('yindex.npy')


print "finish loading the index"

for ic in range(column.__len__()):
    ig = cidx1d[ic] - 1
    print "ig= ", ig
    jj = jj_idx[ig] 
    ii = ii_idx[ig]

    ncr.variables['labilep_vr'][ic,:] = nci.variables['lab'][jj, ii]
    ncr.variables['secondp_vr'][ic,:] = nci.variables['sec'][jj, ii]
    ncr.variables['occlp_vr'  ][ic,:] = nci.variables['occ'][jj, ii]
    ncr.variables['primp_vr'  ][ic,:] = nci.variables['apa'][jj, ii]

ncr.close()
#-ncr.variables['labilep_vr'][:,:] = nci.variables['lab'][jj_idx[cidx1d[:]-1], ii_idx[cidx1d[:]-1]]
#-ncr.variables['secondp_vr'][:,:] = nci.variables['sec'][jj_idx[cidx1d[:]-1], ii_idx[cidx1d[:]-1]]
#-ncr.variables['occlp_vr'  ][:,:] = nci.variables['occ'][jj_idx[cidx1d[:]-1], ii_idx[cidx1d[:]-1]]
#-ncr.variables['primp_vr'  ][:,:] = nci.variables['apa'][jj_idx[cidx1d[:]-1], ii_idx[cidx1d[:]-1]]
