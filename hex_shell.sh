#!/bin/bash
#
#SBATCH --job-name=hex_play
#
#SBATCH --time=10079:59
#SBATCH --cpus-per-task=64
#SBATCH --qos=long

ml rstudio
Rscript DS/hex_script.R