import os

#output_dir = '/lustre/or-hydra/cades-ccsi/scratch/xyk/TESTRD4'
output_dir = '/lustre/or-hydra/cades-ccsi/scratch/dmricciuto/'

#sites=['AU-Tum','BR-Sa1','FI-Hyy','FR-Pue','GF-Guy','RU-Cok','RU-Fyo','RU-Sam','US-Blo','US-Ha1','US-PFa','US-SRM','US-UMB','US-Wkg','ZA-Kru']
sites=['BR-Sa1','FI-Hyy','GF-Guy','US-Ha1','US-UMB']

#cases=['170802_orig','170802_no2nduptake','170803_rootprof','170802_ECA']
cases=['DEF170802_fixiniP2','NPOOL170802','CRU170803']

compsets=['ICB1850CNPRDCTCBC','ICB1850CNRDCTCBC', 'ICB1850CNPRDCTCBC']
#compsets = ['ICB1850CNPRDCTCBC','ICB1850CNPRDCTCBC','ICB1850CNPRDCTCBC','ICB1850CNPECACNTBC']


vars_spinup=['NEE','GPP','NPP','TLAI','TOTSOMC','TOTVEGC']
vars_seasonal=['TLAI','GPP','NEE','FPI_P','FPI','FPG_P','FPG']


for s in sites:
    mycases=''
    for c in cases:
        mycases=mycases+str(c)+','
    mycompsets=''
    for p in compsets:
        mycompsets=mycompsets+str(p)+','
    myvars=''
    for v in vars_spinup:
        if (v != 'NEE'):
            myvars=myvars+str(v)+','
#    os.system('python plotcase.py --case '+mycases[:-1]+' --site '+s+ \
#             '  --csmdir '+output_dir+' --pdf --vars '+myvars[:-1]+ \
#              ' --compset '+mycompsets[:-1]+' --spinup')
#    if ('NEE' in vars_spinup):
#            os.system('python plotcase.py --case '+mycases[:-1]+' --site '+ \
#                      s+'  --csmdir '+output_dir+' --pdf --vars NEE'+ \
#                      ' --compset '+mycompsets[:-1]+' --spinup --ylog')
    
    mycompsets_tr = mycompsets.replace('1850','20TR')
    myvars=''
    for v in vars_seasonal:
        myvars=myvars+str(v)+','
    
    os.system('python plotcase.py --case '+mycases[:-1]+' --site '+s+ \
              '  --csmdir '+output_dir+' --pdf --vars '+myvars[:-1]+ \
              ' --compset '+mycompsets_tr[:-1]+' --seasonal --obs')
    
