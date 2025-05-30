#!/bin/bash
#SBATCH --job-name=r_rjags           # Job name
#SBATCH --output=r_rjags_01.out      # Standard output and error log
#SBATCH --ntasks=1                   # Run a single task
#SBATCH --cpus-per-task=4            # Number of CPU cores per task
#SBATCH --mem=8gb                    # Total memory
#SBATCH --time=24:00:00              # Time limit (hh:mm:ss)
#SBATCH --partition=main             

# Add email notifications
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=jli183@crimson.ua.edu


# Load your conda environment where R is installed
conda activate /bighome/jli183/my_r_env

# Run R script
Rscript /panfs/panhpc/home/jli183/EyeTracking/Code/Jags_HPC_09_27_24_PPMCinMCMC.R
