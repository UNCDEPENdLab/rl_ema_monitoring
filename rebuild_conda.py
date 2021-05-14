import os
import subprocess
import sys
import rebuild_conda_funcs

# Main method for the function
def main():
    # run the build function
    rebuild_conda_funcs.build_reticulate_conda(dependListFile='pip.txt')
		
# Execution of main method
if __name__ == '__main__':
    main()