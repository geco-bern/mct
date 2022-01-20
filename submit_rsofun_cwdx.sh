#!/bin/bash

# jobs to run
njobs=50

# check dirs
pwd=`pwd`
base=`basename $pwd`

# check if we are in the project folder!
# jobs need to be executed in the main
# directory, not anywhere else to maintain
# relative paths
if [ ${pwd} -ne "mct" ]; then
   echo "You are not executing the script in the main project directory, exiting!"
   exit 1
fi

for ((n=1;n<=${njobs};n++)); do
   echo "Submitting pixel $n ..."
   bsub -W 72:00 -u $USER -J "test cell_${n}" -R "rusage[mem=48000]" "Rscript --vanilla ./rscript_rsofun_cwdx.R $n 9999"
done

for ((n=1;n<=${njobs};n++)); do
   echo "Submitting pixel $n ..."
   bsub -W 72:00 -u $USER -J "test cell_${n}" -R "rusage[mem=48000]" "Rscript --vanilla ./rscript_rsofun_cwdx.R $n 100"
done

for ((n=1;n<=${njobs};n++)); do
   echo "Submitting pixel $n ..."
   bsub -W 72:00 -u $USER -J "test cell_${n}" -R "rusage[mem=48000]" "Rscript --vanilla ./rscript_rsofun_cwdx.R $n 200"
done