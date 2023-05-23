#!/bin/bash
#SBATCH --job-name=hex_naive
#SBATCH --output=hex_naive%j.out
#SBATCH --error=hex_naive%j.err
#SBATCH --nodes=1
#SBATCH --ntasks=2 # for two processes in parallel
#SBATCH --time=24:00:00 # for 12 hours

ml R/4.1.2 # load R module, this can differ based on your system's module name for R

for i in {1..50} # run the R script 50 times
do
srun --exclusive -n1 Rscript MCTS_naive/mcts_two_player_naive_sherlock.R &
  done
wait
