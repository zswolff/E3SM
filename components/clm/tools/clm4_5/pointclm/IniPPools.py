#!/usr/bin/env python

from optparse import OptionParser
import numpy as np
import os, shutil
from netcdf_functions import putvar,getvar
import glob

parser = OptionParser()
parser.add_option("--casesite", dest="casesite", default="", \
                  help="case plus site name?")
parser.add_option("--casename", dest="casename", default="", \
                  help="case name?")
parser.add_option("--diricase", dest="diricase", default="", \
                  help="case input directory")
parser.add_option("--dirocase", dest="dirocase", default="", \
                  help="case output directory")
parser.add_option("--sitephos", dest="sitephos", default="", \
                  help="site P pool data")
parser.add_option('--restart_year', dest='restart_year', default="", \
                    help='Year of restart file to modify')
parser.add_option('--acme_input', dest='acme_input', default="", \
                  help='acme input data directory')
parser.add_option('--site_lat', dest='site_lat', default=-999.)
parser.add_option('--site_lon', dest='site_lon', default=-999.)
(options, args) = parser.parse_args()


# the directory of input
casesite = options.casesite
casename = options.casename
diricase = options.diricase
if (options.dirocase == ''):
    dirocase = options.diricase
else:
    dirocase = options.dirocase
sitephos = options.sitephos



if (options.restart_year == ''):
        #if restart_year not provided, take the last existing restart file
        restart_file = glob.glob(diricase+'/'+casename+'.clm2.r.*.nc')
        restart_file_last = restart_file[-1]
        year = int(restart_file_last[-19:-15])
else:
        year = int(options.restart_year)


#site = casesite[len(casesite)-6:len(casesite)]

site = casesite

solutionP = {}
labileP   = {}
secondP   = {}
occlP     = {}
primP     = {}

with open(sitephos) as f:
     lines = f.readlines()
     contents = [x.rstrip('\n') for x in lines]
     for content in contents[1:]:
         print not content.strip()
         if content.strip():
            sl = content.split(',')
            sitename = sl[0]
            solutionP[sitename]=float(sl[1])
            labileP  [sitename]=float(sl[2])
            secondP  [sitename]=float(sl[3])
            occlP    [sitename]=float(sl[4])
            primP    [sitename]=float(sl[5])
            #print sitename, solutionP[sitename], labileP  [sitename], secondP  [sitename], occlP    [sitename], primP    [sitename]
f.close()

fileinp = diricase + '/' + casename + ".clm2.r."+ str(10000+year)[1:] + "-01-01-00000.nc"
fileout = dirocase + '/' + casename + ".clm2.r."+ str(10000+year)[1:] + "-01-01-00000.nc"

if diricase != dirocase:
   shutil.copy(fileinp, dirocase)
shutil.copy(fileinp, fileinp+".b")

if site in solutionP.keys():
   soultionP = solutionP[site]
   labileP   = labileP[site]
   secondP   = secondP[site]
   occlP     = occlP[site]
   primP     = primP[site]
else:
  global_file = options.acme_input+'/lnd/clm2/rawdata/mksrf_soilphos_0.5x0.5_simyr1850.c170623.nc'
  #get corresponding 0.5x0.5 and 1.9x2.5 degree grid cells
  longxy = getvar(global_file, 'LONGXY')
  latixy = getvar(global_file, 'LATIXY')
  xgrid_min = -1
  ygrid_min = -1
  lon_bounds = [float(options.site_lon), float(options.site_lon)]
  lat_bounds = [float(options.site_lat), float(options.site_lat)]

  for i in range(0,longxy.shape[1]):
      if (longxy[0,i] >= lon_bounds[0] and xgrid_min == -1):
          xgrid_min = i
          xgrid_max = i
      elif (longxy[0,i] <= lon_bounds[1]):
          xgrid_max = i
  if (lon_bounds[0] == 180 and lon_bounds[1] == 180):  #global
      xgrid_min = 0
      xgrid_max = longxy.shape[1]-1

  for i in range(0,latixy.shape[0]):
      if (latixy[i,0] >= lat_bounds[0] and ygrid_min == -1):
          ygrid_min = i
          ygrid_max = i
      elif (latixy[i,0] <= lat_bounds[1]):
          ygrid_max = i
  tempphosfile = './temp/tempsitephos.nc'
  os.system('ncks -d lon,'+str(xgrid_min)+','+str(xgrid_max)+' -d lat,'+str(ygrid_min)+ \
                ','+str(ygrid_max)+' '+global_file+' '+tempphosfile)
  solutionP = 0.0015
  labileP = getvar(tempphosfile, 'LABILE_P')
  secondP = getvar(tempphosfile, 'SECONDARY_P')
  occlP   = getvar(tempphosfile, 'OCCLUDED_P')
  primP   = 10.

putvar(fileout, 'solutionp_vr', solutionP)
putvar(fileout, 'labilep_vr'  , labileP  )
putvar(fileout, 'secondp_vr'  , secondP  )
putvar(fileout, 'occlp_vr'    , occlP    )
putvar(fileout, 'primp_vr'    , primP    )

