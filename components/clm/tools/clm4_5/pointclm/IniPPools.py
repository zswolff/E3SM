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
        print 'Restart year not provided.  Using year '+str(year)
else:
        year = int(options.restart_year)


#site = casesite[len(casesite)-6:len(casesite)]

site = casesite

solutionP = {}
labileP   = {}
secondP   = {}
occlP     = {}
primP     = {}


if (sitephos != ''):
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
   mysolutionP = solutionP[site]
   mylabileP   = labileP[site]
   mysecondP   = secondP[site]
   myocclP     = occlP[site]
   myprimP     = primP[site]
else:
  global_file = options.acme_input+'/lnd/clm2/rawdata/mksrf_soilphos_0.5x0.5_simyr1850.c170623.nc'
  #get corresponding 0.5x0.5 and 1.9x2.5 degree grid cells
  longxy = getvar(global_file, 'LONGXY')
  latixy = getvar(global_file, 'LATIXY')

  cols1d_lon = getvar(fileinp, 'cols1d_lon')
  cols1d_lat = getvar(fileinp, 'cols1d_lat')

  restsolutionP = getvar(fileinp, 'solutionp_vr')
  restlabileP   = getvar(fileinp, 'labilep_vr')
  restsecondP   = getvar(fileinp, 'secondp_vr')
  restocclP     = getvar(fileinp, 'occlp_vr')
  restprimP     = getvar(fileinp, 'primp_vr')

  obssolutionP  = 0.0015
  obslabileP    = getvar(global_file, 'LABILE_P')
  obssecondP    = getvar(global_file, 'SECONDARY_P')
  obsocclP      = getvar(global_file, 'OCCLUDED_P')
  obsprimP      = getvar(global_file, 'APATITE_P')

  for i in range(0,len(cols1d_lon)):
     mylon = cols1d_lon[i]
     if (mylon < 0):
         mylon = mylon+360.
     mylat = cols1d_lat[i]
     for j in range(0,longxy.shape[1]):
         lon_data = longxy[0,j]
         if (lon_data < 0):
             lon_data = lon_data+360.
         if (mylon >= lon_data-0.25 and mylon < lon_data+0.25):
            lon_ind = j
            for k in range(0,latixy.shape[0]):
                if (mylat >= latixy[k,0]-0.25 and mylat < latixy[k,0]+0.25):
                    lat_ind = k
     restsolutionP[i,:] = obssolutionP
     restlabileP[i,:]   = obslabileP[lat_ind,lon_ind]
     restsecondP[i,:]   = obssecondP[lat_ind,lon_ind]
     restocclP[i,:]     = obsocclP[lat_ind,lon_ind]
     restprimP[i,:]     = obsprimP[lat_ind,lon_ind]

putvar(fileout, 'solutionp_vr', restsolutionP)
putvar(fileout, 'labilep_vr'  , restlabileP  )
putvar(fileout, 'secondp_vr'  , restsecondP  )
putvar(fileout, 'occlp_vr'    , restocclP    )
putvar(fileout, 'primp_vr'    , restprimP    )

