import os

def rebuild_from_yaml():
    # if the conda environment does not exist
    if os.path.isdir(os.getcwd() + '/conda_env') == False:
        # create the conda environment
        os.system("conda env create --prefix conda-env -f environment.yml")
    # if the conda environment already exists
    else:
        # then update the environment
        os.system("conda env update --prefix conda_env -f environment.yml")  

# Execution of main method
if __name__ == '__main__':
    #main()
    rebuild_from_yaml()