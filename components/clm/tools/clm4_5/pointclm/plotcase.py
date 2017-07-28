#!/usr/bin/python

import os, sys, csv, glob
import numpy, scipy
from scipy.io import netcdf
from optparse import OptionParser
import matplotlib.pyplot as plt

def getvar(fname, varname, npf, index, scale_factor):
    usescipy = False
    try:
        import Scientific.IO.NetCDF as netcdf
    except ImportError:
        import scipy
        from scipy.io import netcdf
        usescipy = True
    if (usescipy):
        nffile = netcdf.netcdf_file(fname,"r",mmap=False)
        var = nffile.variables[varname]
        varvals = var[0:npf,index].copy() * scale_factor    #works for vector only?
        nffile.close()
    else:
        nffile = netcdf.NetCDFFile(fname,"r")
        var = nffile.variables[varname]
        varvals = var.getValue()[0:npf,index] * scale_factor
        nffile.close()
    return varvals


parser = OptionParser()
parser.add_option("--csmdir", dest="mycsmdir", default='../../../../..', \
                  help = 'Base CESM directory (default = ..)')
parser.add_option("--cases", dest="mycase", default='', \
                  help = "name of case id prefixs to plot (comma delmited)")
parser.add_option("--compset", dest="compset", default="I20TRCLM45CN", \
                  help = "Compset to plot")
parser.add_option("--titles", dest="titles", default='', \
                  help = "titles of case to plot (for legend)")
parser.add_option("--obs", action="store_true", default=False, \
                  help = "plot observations", dest="myobs")
parser.add_option("--sites", dest="site", default="none", \
                  help = 'site (to plot observations)')
parser.add_option("--varfile", dest="myvarfile", default='varfile', \
                  help = 'file containing list of variables to plot')
parser.add_option("--vars", dest="myvar", default='', \
                  help="variable to plot (overrides varfile, " \
                  +"sends plot to screen")
parser.add_option("--avpd", dest="myavpd", default=1, \
                  help = 'averaging period in # of output timesteps' \
                  +' (default = 1)')
parser.add_option("--hist_mfilt", dest="myhist_mfilt", default=-999, \
                  help = 'beginning model year to plot')
parser.add_option("--hist_nhtfrq", dest="myhist_nhtfrq", default=-999, \
                  help = 'beginning model year to plot')
parser.add_option("--ystart", dest="myystart", default=1, \
                  help = 'beginning model year to plot')
parser.add_option("--yend", dest="myyend", default=9999, \
                  help = 'final model year to plot')
parser.add_option("--diurnal", dest="mydiurnal", default=False, \
                  action="store_true", help = 'plot diurnal cycle')
parser.add_option("--dstart", dest="dstart", default=1, \
                  help = 'beginning model DOY to plot (for diruanl average)')
parser.add_option("--dend", dest="dend", default=365, \
                  help = 'final model DOY to plot (for diurnal average)')
parser.add_option("--seasonal", dest="myseasonal", default=False, \
                  action="store_true", help = 'plot seasonal cycle')
parser.add_option("--h1", dest="h1", default=False, \
                  action="store_true", help = 'Use h1 history files')
parser.add_option("--h2", dest="h2", default=False, \
                  action="store_true", help = 'Use h2 history files')
parser.add_option("--index", dest="index", help = 'index (site or pft)', \
                   default=0)
parser.add_option("--spinup", dest="spinup", help = 'plot Ad and final spinup', \
                   default=False, action="store_true")
parser.add_option("--scale_factor", dest="scale_factor", help = 'scale factor', \
                   default=-999)

(options,args) = parser.parse_args()
               

cesmdir=os.path.abspath(options.mycsmdir)                 
#if (options.mycase == ''): # or os.path.exists(options.mycase) == False):
#    print('Error: invalid CESM root directory')
#    sys.exit()

mycases = options.mycase.split(',')
mysites = options.site.split(',')
mycompsets = options.compset.split(',')

ncases = 1
if (len(mycases) > 1):
  ncases = len(mycases)
  mysites=[]
  mycompsets=[]
  for c in range(0,ncases):
    mysites.append(options.site)
    mycompsets.append(options.compset)
  mytitles = mycases
elif (len(mysites) > 1):
  ncases = len(mysites)
  mycases=[]
  mycompsets=[]
  for c in range(0,ncases):
    mycases.append(options.mycase)
    mycompsets.append(options.compset)
  mytitles = mysites
elif (len(mycompsets) > 1):
  ncases = len(mycompsets)
  mycases=[]
  mysites=[]
  for c in range(0,ncases):
    mycases.append(options.mycase)
    mysites.append(options.site)  
  mytitles = mycompsets
else:
  mytitles=[]
  mytitles.append(mycases[0]+'_'+mysites[0]+'_'+mycompsets[0])

if (options.titles != ''):
  mytitles = options.titles.split(',')

obs     = options.myobs

#get list of variables from varfile
myvars=[]

if (options.myvar == ''):
    if os.path.isfile('./'+options.myvarfile):
        input = open('./'+options.myvarfile)
        for s in input:
            myvars.append(s.strip())
    else:
        print('Error:  invalid varfile')
        sys.exit()
    terminal = 'postscript'
else:
    terminal=''
    myvars = options.myvar.split(',')
    

avpd      = int(options.myavpd)        # desired averaging period in output timestep
ystart    = int(options.myystart)      # beginning year to plot/average
yend      = int(options.myyend)        # final year to plot/average 

avtype = 'default'
if (options.mydiurnal):
    avtype = 'diurnal'
    avpd=1
if (options.myseasonal):
    avtype = 'seasonal'

#------------------------------------------------------------------------------

if (obs):
    ncases=ncases+1

#site = options.site
#compset = options.compset

#dirs=[]
nvar = len(myvars)    
x_toplot    = numpy.zeros([ncases, 2000000], numpy.float)
data_toplot = numpy.zeros([ncases, nvar, 2000000], numpy.float)
snum        = numpy.zeros([ncases], numpy.int)

for c in range(0,ncases):
    if (mycases[c] == ''):
        mydir = cesmdir+'/'+mysites[c]+'_'+mycompsets[c]+'/run/'
    else:
        mydir = cesmdir+'/'+mycases[c]+'_'+mysites[c]+'_'+mycompsets[c]+'/run/'
    print 'Processing '+mydir

    #query lnd_in file for output file information
    if ((options.myhist_mfilt == -999 or options.myhist_nhtfrq == -999) or (obs and  c  < ncases-1)):
        #print('Obtaining output resolution information from lnd_in')
        input = open(mydir+"/lnd_in")
        npf=-999
        tstep=-999
        input = open(mydir+"/lnd_in")
        for s in input:
	    if ('hist_mfilt' in s):
	        mfiltinfo = s.split()[2]
                npf = int(mfiltinfo.split(',')[0])
                if (options.h1): 
                    npf = int(mfiltinfo.split(',')[1])
                if (options.h2):
                    npf = int(mfiltinfo.split(',')[2])
            if ('hist_nhtfrq' in s):
                nhtfrqinfo = s.split()[2]
                tstep = int(nhtfrqinfo.split(',')[0])
                if (options.h1):
                    tstep = int(nhtfrqinfo.split(',')[1])
                if (options.h2):
                    tstep = int(nhtfrqinfo.split(',')[2])
        input.close()
    elif (c == ncases-1 and obs):
        npf   = 8760
        tstep = -1
    else:
        npf   = int(options.myhist_mfilt)
        tstep = int(options.myhist_nhtfrq)
   
    #print npf, tstep 
    if (npf == -999 or tstep == -999):
        print('Unable to obtain output file information from lnd_in.  Exiting')
        sys.exit()

    yststr=str(100000+ystart)
    #determine type of file to plot
    if (tstep == 0):
        ftype = 'default'
        if (options.h2):
            hst = 'h2'
        elif (options.h1):
            hst = 'h1'
        else:
            hst = 'h0'
        if (mycases[c] == ''):
          testfile = mydir+'/'+mysites[c]+'_'+mycompsets[c]+'.clm2.'+hst+ \
                     '.'+yststr[2:6]+'-01.nc'
        else:
          testfile = mydir+'/'+mycases[c]+'_'+mysites[c]+'_'+mycompsets[c]+ \
                     '.clm2.'+hst+'.'+yststr[2:6]+'-01.nc'
    else:
        ftype = 'custom'
        nhtot=-1*tstep*npf
        nypf = max(1, nhtot/8760)
        if (options.h2):
            hst='h2'
        elif (options.h1):
            hst='h1'
        else:
            hst='h0'
        if (mycases[c] == ''):
          testfile = mydir+'/'+mysites[c]+'_'+mycompsets[c]+ \
                     '.clm2.'+hst+'.'+yststr[2:6]+'-01-01-00000.nc'
        else:
          testfile = mydir+'/'+mycases[c]+'_'+mysites[c]+'_'+mycompsets[c]+ \
                     '.clm2.'+hst+'.'+yststr[2:6]+'-01-01-00000.nc'
        
    if (obs and c == ncases-1):
        ftype='obs'
        if (options.site == 'none'):
            print('Error:  No site specified.  Cannot load observations')
        testfile = options.site+'obs.nc'

    #check for output files (if not here, change to archive directory)
    if (os.path.isfile(testfile) == False):
        print('Output not in run directory.  Switching to archive directory')
        archdir=cesmdir+'/archive/'+mycases[c]+'_'+mysites[c]+'_'+mycompsets[c]+'/lnd/hist'
        if (os.path.exists(archdir) == False):
            print('Archive directory does not exist.  Exiting')
            sys.exit()
        else:
            os.chdir(archdir)
            dirs[c] = archdir
            if (os.path.isfile(testfile) == False):
                print('Output not found.  Exiting')
                sys.exit()
                

    #initialize data arrays
    mydata      = numpy.zeros([nvar,2000000], numpy.float)
    x           = numpy.zeros([2000000], numpy.float)
    nsteps=0
 
    if (c == 0):   
        var_units=[]
        var_long_names=[]
        myscalefactors=[]
    #read monthly .nc files (default output)
    if (ftype == 'default'):
        jobs=[]
        for v in range(0,nvar):
            nsteps = 0
            for y in range(ystart,yend+1):
                yst=str(10000+y)[1:5]
                for m in range(0,12):
                    mst=str(101+m)[1:3]
                    myfile = os.path.abspath(mydir+'/'+mycases[c]+'_'+mysites[c]+'_'+mycompsets[c]+ \
                                             ".clm2."+hst+"."+yst+"-"+mst+".nc")
                    #get units/long names from first file
                    if (os.path.exists(myfile)):
                        if (y == ystart and m == 0 and c == 0):
                            nffile = netcdf.netcdf_file(myfile,"r")
                            varout=nffile.variables[myvars[v]]
                            var_long_names.append(varout.long_name)
                            nffile.close()
                            if (float(options.scale_factor) < -900):
                                if ('gC/m^2/s' in varout.units):
                                    myscalefactors.append(3600*24)
                                    var_units.append('gC/m^2/day')
                                else:
                                    myscalefactors.append(1.0)
                                    var_units.append(varout.units)
                            else:
                                myscalefactors.append(float(options.scale_factor))
                                var_units.append(varout.units)

                        x[nsteps] = y+m/12.0
                        myvar_temp = getvar(myfile, myvars[v],npf,int(options.index), \
                                            myscalefactors[v])
                        mydata[v,nsteps] = myvar_temp
                        nsteps = nsteps + 1
    
    #read annual .nc files
    if (ftype == 'custom'):
        for v in range(0,nvar):
            jobs = []
            nsteps=0
            nfiles = (yend-ystart)/nypf
            nc=1
            ylast=0
            if (options.spinup):
                nc=2
            for n in range(0,nc):
                if ((options.spinup)and n== 0):
                    if (mycases[c] == ''):
                        mydir = cesmdir+'/'+mysites[c]+'_'+mycompsets[c].replace('CNP','CN')+ \
	                          '_ad_spinup/run/'
                    else:
                        mydir = cesmdir+'/'+mycases[c]+'_'+mysites[c]+'_'+ \
                                mycompsets[c].replace('CNP','CN')+'_ad_spinup/run/'
                    thiscompset = mycompsets[c].replace('CNP','CN')+'_ad_spinup'
                else:
                    if (mycases[c] == ''):
                        mydir = cesmdir+'/'+mysites[c]+'_'+mycompsets[c]+'/run/'
                    else:
                        mydir = cesmdir+'/'+mycases[c]+'_'+mysites[c]+'_'+ \
                                mycompsets[c]+'/run/'
                    thiscompset = mycompsets[c]
                for y in range(n,nfiles+1):   #skip first file on final spinup
                    yst=str(10000+ystart+y*nypf)[1:5]
                    if (mycases[c].strip() == ''):
                        myfile = os.path.abspath(mydir+'/'+mycases[c]+'_'+thiscompset+".clm2."+hst+ \
                                                 "."+yst+"-01-01-00000.nc")
                    else:
                        myfile = os.path.abspath(mydir+'/'+mycases[c]+"_"+mysites[c]+'_'+thiscompset+ \
                                                 ".clm2."+hst+"."+yst+"-01-01-00000.nc")
                    if (os.path.exists(myfile)):
                        if (n == 0):
                            ylast = y
                        if (y == 0 and c == 0):
                            nffile = netcdf.netcdf_file(myfile,"r")
                            varout=nffile.variables[myvars[v]]
                            var_long_names.append(varout.long_name)
                            if (float(options.scale_factor) < -900):
                                if ('gC/m^2/s' in varout.units):
                                    if (npf >= 365):
                                        myscalefactors.append(3600*24)
                                        var_units.append('gC/m^2/day')
                                    else:
                                        myscalefactors.append(3600*24*365)
                                        var_units.append('gC/m^2/yr')
                                else:
                                    myscalefactors.append(1.0)
                                    var_units.append(varout.units)
                            else:
                                 myscalefactors.append(float(options.scale_factor))
                                 var_units.append(varout.units)

                            nffile.close()
                        myvar_temp = getvar(myfile,myvars[v],npf,int(options.index), \
                                            myscalefactors[v])
                        for i in range(0,npf):
                            if (n == 0):
                                myind = y*npf+i
                                x[myind] = ystart+(y*nypf) + nypf*(i*1.0-0.5)/npf
                            else:
                                myind = ylast*npf+y*npf+i
                                x[myind] = ystart+(ylast*nypf+y*nypf) + nypf*(i*1.0-0.5)/npf
                            mydata[v,myind] = myvar_temp[i]
                            nsteps=nsteps+1

    #read obervation file, assumes it is in case directory (years must match!)
    #will work for NEE only!
    if (ftype == 'obs'):
        npd=24                #number of time steps per day
        lst=-5 #local standard time (diff from UTC)
        npf=(yend-ystart+1)*365*npd
        nffile = NetCDF.NetCDFFile(options.site+'obs.nc',"r")
        nsteps=-(lst-1)
        nstepslast=nsteps
        for v in range(0,nvar):
            if (myvars[v] == 'NEE' or myvars[v] == 'GPP'):
                nsteps=nstepslast
                varout=nffile.variables[myvars[v]+'_filled']
                indata = varout.getValue()[0:npf]*12/1e6
                x[0:9]=ystart
                for i in range(0,npf*24/npd):
                    x[nsteps] = ystart+(i*1.0)/8760
                    if (npd == 24):
                        mydata[v,max(nsteps,0)] = float(indata[i])
                    if (npd == 48):
                        mydata[v,max(nsteps,0)] = (float(indata[i*2])+ \
                                                       float(indata[i*2+1]))/2
                    nsteps=nsteps+1
        nffile.close()
        nsteps=nsteps+lst-1
    
    #perform averaging and write output files for gnuplot
    if (avtype == 'default'):
        for v in range(0,nvar):
            snum[c] = 0
            for s in range(0,int(nsteps/avpd)): 
                x_toplot[c, snum[c]]       = sum(x[s*avpd:(s+1)*avpd])/avpd
                data_toplot[c, v, snum[c]] = sum(mydata[v,s*avpd:(s+1)*avpd])/avpd      
                snum[c] = snum[c]+1

    #diurnal average (must have hourly output)
    if (avtype == 'diurnal'):
        snum[c]=36
        for v in range(0,nvar):
            mysum = numpy.zeros(snum[c], numpy.float)
            myct = numpy.zeros(snum[c],numpy.float)
            for y in range(0,(yend-ystart+1)):
                for d in range (int(options.dstart),int(options.dend)):
                    for s in range(0,snum[c]):        
                        h=s
                        if (h >= 24):
                            h=h-24
                        mysum[s] = mysum[s]+mydata[v,y*8760+(d-1)*24+h]/((yend-ystart+1)* \
                                                 (int(options.dend)-int(options.dstart)+1))
                        myct[s] = myct[s]+1
            for s in range(0,snum[c]):
                x_toplot[c,s] = s+0.5
                data_toplot[c, v, s] = mysum[s]/myct[s]
      
    #seasonal average (assumes default monthly output)
    if (avtype == 'seasonal'):
        for v in range(0,nvar):
            snum[c] = 12
            mysum=numpy.zeros(snum[c], numpy.float)
            for y in range(0,(yend-ystart+1)):
                for s in range(0,snum[c]):
                    mysum[s]=mysum[s]+mydata[v,(y*12+s)]/(yend-ystart+1)
        
            for s in range(0,snum[c]):
                x_toplot[c,s] = s+0.5
                data_toplot[c,v,s] = mysum[s]
        
#matplotlib plot
for v in range(0,len(myvars)):
    fig = plt.figure()
    ax = plt.subplot(111)
    colors=['b','g','r','c','m','y','k','b','g','r','c','m','y','k','b','g','r','c','m','y','k']
    styles=['-','-','-','-','-','-','-','--','--','--','--','--','--','--','-.','-.','-.','-.','-.','-.','-.']
    for c in range(0,ncases): 
        ax.plot(x_toplot[c, 1:snum[c]], (data_toplot[c,v,1:snum[c]]), label=mytitles[c], color=colors[c], \
	  linestyle=styles[c], linewidth=3)
    if (avtype == 'seasonal'):
        plt.xlabel('Model Month')
    elif (avtype == 'diurnal'):
        plt.xlabel('Model Hour (UTC)')
    else:
        plt.xlabel('Model Year')

    plt.ylabel(myvars[v]+' ('+var_units[v]+')')
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
    ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
    plt.title(var_long_names[v])
    #plt.yscale('log')
plt.show()
