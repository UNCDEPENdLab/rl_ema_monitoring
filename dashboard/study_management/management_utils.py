'''
Utilities for the backend python scripts for the rl ema dashboard
'''
# imort dependencies
import os
import rpy2.robjects as robjects

# imports from /dashboard/dashboard_utils.R
# finds the given root directory for a file hierarchy, loaded from dashboard_utils.R
def findRoot(root_dir):
    # Defining the R script and loading the instance in Python
    r = robjects.r
    r['source']('preprocess.R')
    # Loading the function we have defined in R
    find_root_r = robjects.globalenv['findRoot']