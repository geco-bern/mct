#!/bin/bash

here=`pwd`

cd "/rds/general/user/bstocker/home/data/modis_monthly-evi/zmaw_data/0_05deg"
for iyr in `seq 2001 2015`
do 
    cd "$iyr"
    list=`ls modis_vegetation__LPDAAC__v5__0.05deg__*_halfdeg.nc`
  #   for imon in `seq -f "%02g" 1 12`
  #   do
  #   	echo $imon
		# cdo remapbil,${here}/halfdeg.txt modis_vegetation__LPDAAC__v5__0.05deg__${iyr}${imon}.nc modis_vegetation__LPDAAC__v5__0.5deg__${iyr}${imon}.nc
  #   done
    cdo modis_vegetation__LPDAAC__v5__0.05deg__*_halfdeg.nc modis_vegetation__LPDAAC__v5__halfdeg_${iyr}.nc
    mv modis_vegetation__LPDAAC__v5__halfdeg_${iyr}.nc ..
    cd ..
done
cdo modis_vegetation__LPDAAC__v5__halfdeg_????.nc modis_vegetation__LPDAAC__v5__halfdeg.nc
cd $here

# now run Ferret script fill_gaps_modis_evi.jnl