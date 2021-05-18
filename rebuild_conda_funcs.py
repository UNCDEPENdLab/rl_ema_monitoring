import os
import subprocess
import sys

# Finds the root to the conda environment built by the reticulate r package
# By default, the reticulate names its conda env: r-reticulate
# Assumes conda is executable in your local environment
def findReticulatePath():
    # run a conda function on the system to find the reticulate env
    output = str(subprocess.check_output("conda env list", shell=True))
    # generate a list where the above string is split by white-space
    outList = output.split(' ')
    # initialize the return variabke
    retPath = None
    # loop through the list of items in the list (some are empty strings, some are paths)
    for item in outList:
        # if r-reticulate is in the given item
        if 'r-reticulate' in item:
            # save the uncleaned path
            retPath = item
            # break from the for loop
            break
    # handle for windows
    if sys.platform == "win32":
        # clean up the string to remove special characters
        retPath = retPath.split('\\r\\n')[0]
        retPath = retPath.strip()
        # clean up the '\\' to be '/'
        retPath = retPath.replace('\\', '/').replace('//','/')
    # handle for mac
    if sys.platform == "darwin":
        # clean up the string to remove special characters
        retPath = retPath.split('\\n')[0]
        retPath = retPath.strip()
    
    # return the path to the reticulate conda environment
    return retPath

# method to install pip into a reticulate conda environment
def reticulateInstallPip(reticulatePath):
    # create the string for the system call
    sys_call = "conda install -p {retPath} pip -y".format(retPath=reticulatePath)
    # run a system call to download pip to the environment
    os.system(sys_call)

# methdo to install a list of python packages inot a reticulate conda environment
def reticulatePipInstall(reticulatePath, dependListFile):
    # initialize the variable for the package list
    packages = None
    # load the package names from the text file as a list
    with open(dependListFile, 'r') as pip_file:
        # read each line into a list element
        packages = pip_file.readlines()
        # remove any special text characters
        packages = [x.strip() for x in packages]
    # for each dependecy
    for package in packages:
        # create the string for the system call
        sys_call = "conda run -p {retPath} pip install {pack}".format(retPath=reticulatePath, pack=package)
        # run a system call to download pip package to the environment
        os.system(sys_call)

# method rebuilds a conda environment with a default name of "conda-env"
def rebuild_from_yaml():
    # if the conda environment does not exist
    if os.path.isdir(os.getcwd() + '/conda-env') == False:
        # create the conda environment
        os.system("conda env create --prefix conda-env -f environment.yml")
    # if the conda environment already exists
    else:
        # then update the environment
        os.system("conda env update --prefix conda-env -f environment.yml")  

# Method that goes through all three steps of rebuilding a project
def build_reticulate_conda(dependListFile):
    # find the conda path
    retPath = findReticulatePath()
    # attempt to install pip
    reticulateInstallPip(retPath)
    # attempt to install packages from pip
    reticulatePipInstall(retPath, dependListFile)