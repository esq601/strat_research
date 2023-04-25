#!/bin/bash
#SBATCH --job-name=myjob           # Job name
#SBATCH --ntasks=100               # Number of tasks (parallel jobs)
#SBATCH --nodes=1                  # Number of nodes
#SBATCH --cpus-per-task=6          # Number of CPU cores per task
#SBATCH --time=00:10:00            # Time limit (hh:mm:ss)
#SBATCH --output=output.%j.out     # Standard output log file
#SBATCH --error=error.%j.err       # Standard error log file

# Load the required R module and change to the working directory
module load R/4.1.0
cd /path/to/working/directory

# Run the R script for each task in the background
for i in {1..100}
do
  Rscript myscript.R &
done

# Wait for all background jobs to finish
wait
