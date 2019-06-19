#!/bin/bash

here=`pwd`

cd "/Users/benjaminstocker/data/modis_monthly-evi/zmaw_data"
for iyr in `seq 2001 2015`
do 
    cd "$iyr"
    list=`ls modis_vegetation__LPDAAC__v5__0.05deg*.nc`
    for imon in `seq -f "%02g" 1 12`
    do
    	echo $imon
		cdo remapbil,${here}/halfdeg.txt modis_vegetation__LPDAAC__v5__0.05deg__${iyr}${imon}.nc modis_vegetation__LPDAAC__v5__0.5deg__${iyr}${imon}.nc
    done
    cdo mergetime modis_vegetation__LPDAAC__v5__0.5deg__*.nc modis_vegetation__LPDAAC__v5__0.5deg__${iyr}.nc
    mv modis_vegetation__LPDAAC__v5__0.5deg__${iyr}.nc ..
    cd ..
done
cdo mergetime modis_vegetation__LPDAAC__v5__0.5deg__${iyr}.nc modis_vegetation__LPDAAC__v5__0.5deg.nc
cd $here

# now run Ferret script fill_gaps_modis_evi.jnl