'''
Utilities for the backend python scripts for the rl ema dashboard.
Some dependencies are imported from dashboard/dashboard_utils.R
'''
# imort dependencies
import os
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects.vectors import StrVector
from pathlib import Path

# imports from /dashboard/dashboard_utils.R, NOTE: will break if this file moves from /dashboard/study_management/
def importFromDashUtils(funcName):
    # Check to ensure that the direct parent of study_management/ is dashboard/
    path = Path(os.getcwd())
    parent = path.parent.parts[-1]
    if parent != "dashboard":
        # raise exception if the direct parent is not dashboard/
        raise Exception("Error: the direct parent of 'study_management/' should be 'dashboard/'.")
    # import R's utility package
    utils = rpackages.importr('utils')
    # select a mirror for R packages
    utils.chooseCRANmirror(ind=1) # select the first mirror in the list
    # R package names, copied from dashboard/dashboard_utils.R
    packnames = ("RSQLite", "plyr", "dplyr", "ggplot2", "zoo", "sqldf", "rjson", "reticulate")
    # Selectively install what needs to be install.
    names_to_install = [x for x in packnames if not rpackages.isinstalled(x)]
    if len(names_to_install) > 0:
        utils.install_packages(StrVector(names_to_install))
    # Defining the R script and loading the instance in Python
    r = robjects.r
    r['source']('../dashboard_utils.R')
    # Loading the function we have defined in R
    r_func = robjects.globalenv[funcName]
	# return the selected function
    return r_func

# finds the given root directory for a file hierarchy, loaded from dashboard_utils.R
def findRoot(root_dir):
    # Load findRoot from dashboard_utils.R
    findRoot_r = importFromDashUtils("findRoot")
    # Execute the r function in python
    pathStr = findRoot_r(root_dir)
    # Cleanup the final result to be pythonic
    pathStr = pathStr[0]
    # return the path
    return pathStr