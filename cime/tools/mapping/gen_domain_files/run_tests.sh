#!/bin/bash

# Set paths
script_root=$PWD
cime_root=${script_root}/../../
# TODO: get this cleverly from CIME somehow, otherwise this will need to be
# changed by hand for each supported system this is run on.
inputdata_root=/projects/ccsm/inputdata

# Setup the test case. gen_domain takes as inputs a mapping file and the names
# of the ocean and land grids. Note that we could probably get these by parsing
# the specified baseline filenames or global netcdf attributes, so that we
# could maybe simplify this to specifying just ocn_baseline and lnd_baseline,
# and then figuring out which mapping file and grid names we need.
ocn_name=oQU240
lnd_name=ne4np4
mapping_file=${inputdata_root}/cpl/gridmaps/oQU240/map_oQU240_to_ne4np4_aave.160614.nc
ocn_baseline=${inputdata_root}/share/domains/domain.ocn.ne4np4_oQU240.160614.nc
lnd_baseline=${inputdata_root}/share/domains/domain.lnd.ne4np4_oQU240.160614.nc

# Make a temporary folder to manage all temporary files created
test_root=${script_root}/test_temp
mkdir -p ${test_root} && cd ${test_root}

# We will redirect verbose test log output to a file; remove any existing
# versions of this file first
test_log=${script_root}/test.out
rm -f ${test_log}

# Build the gen_domain executable
echo "" >> ${test_log}
echo "Building gen_domain..." >> ${test_log}
${cime_root}/configure --macros-format Makefile --mpilib mpi-serial >> ${test_log} 2>&1
(. ./.env_mach_specific.sh ; cd ${script_root}/src ; gmake) >> ${test_log} 2>&1

# Build the cprnc executable (for comparison of netcdf files)
echo "" >> ${test_log}
echo "Building cprnc..." >> ${test_log}
cprnc_root=${cime_root}/cprnc
(. .env_mach_specific.sh ; cd ${cprnc_root} ; make) >> ${test_log} 2>&1

# Run example gen_domain code on test case
(. .env_mach_specific.sh ; ${script_root}/gen_domain -m ${mapping_file} -o ${ocn_name} -l ${lnd_name}) >> ${test_log} 2>&1

# Compare outputs from test case against baselines
datestring=`date +'%y%m%d'`
for baseline in ${ocn_baseline} ${lnd_baseline}; do
    # Find file that matches prefix of specified baseline; do this by stripping
    # off last two tokens from baseline filename (file extension and datestring)
    # and adding in datestring for current day and .nc file extension.
    testfile=`basename ${baseline} | rev | cut -d. -f3- | rev`.${datestring}.nc

    # Compare against baseline and print report from cprnc comparison
    echo "Comparing $testfile against ${baseline}..."
    (. .env_mach_specific.sh ; ${cprnc_root}/cprnc -m ${testfile} ${baseline}) >> ${test_log} 2>&1
    tail -n2 ${test_log}
done

# Exit gracefully. Alternatively, we could return a non-zero exit status if any
# of the cprnc comparisons failed.
exit 0
