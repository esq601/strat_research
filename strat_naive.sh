#!/bin/bash
#SBATCH --job-name=hex_naive
#SBATCH --output=hex_naive%j.out
#SBATCH --error=hex_naive%j.err
#SBATCH --nodes=5
#SBATCH --ntasks-per-node=16  # number of tasks per node
#SBATCH --cpus-per-task=2  # each task gets 2 CPUs
#SBATCH --time=24:00:00 # for 12 hours
ml R/4.1.2 # load R module, this can differ based on your system's module name for R
for i in {1..80} # run the R script 50 times
do
srun --exclusive -n1 Rscript MCTS_strat/strategic_naive.R &
  done
wait