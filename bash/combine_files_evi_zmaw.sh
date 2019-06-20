#!/bin/bash

here=`pwd`

cd /rds/general/user/bstocker/home/data/modis_monthly-evi/zmaw_data/0_05deg
for iyr in `seq 2001 2015`
do 
    cd "$iyr"
    list=`ls modis_vegetation__LPDAAC__v5__0.05deg__*_halfdeg.nc`
    cdo mergetime modis_vegetation__LPDAAC__v5__0.05deg__*_halfdeg.nc modis_vegetation__LPDAAC__v5__halfdegMAX_${iyr}.nc
    mv modis_vegetation__LPDAAC__v5__halfdegMAX_${iyr}.nc ..
    cd ..
done
cdo mergetime modis_vegetation__LPDAAC__v5__halfdegMAX_????.nc modis_vegetation__LPDAAC__v5__halfdegMAX.nc
#cdo -O invertlat modis_vegetation__LPDAAC__v5__halfdegMAX.nc modis_vegetation__LPDAAC__v5__halfdegMAX.nc
mv modis_vegetation__LPDAAC__v5__halfdegMAX.nc ../halfdeg/
cd $here

# now run Ferret script fill_gaps_modis_evi.jnl
