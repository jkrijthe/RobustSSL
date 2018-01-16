#!/bin/bash
#SBATCH --partition=long --qos=long 
#SBATCH --time=7-00:00:00
#SBATCH --cpus-per-task=16
#SBATCH --mem-per-cpu=1000
Rscript learningcurves-fraction.R