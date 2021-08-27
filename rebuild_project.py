import argparse
import os
import sys
import json
import subprocess
from rebuild_conda_funcs import build_reticulate_conda
from rebuild_config_funcs import build_config
from rebuild_yaml_funcs import build_yaml
from rebuild_project_funcs import build_system
import yaml

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description="Tool for setting cfg.json and conda_env, should be called from the system directly.")
    # root_name argument
    parser.add_argument('--root_name', help='Name of the designated root directory of the project, will change the root name.', default = None)
    # root_dir argument
    parser.add_argument('--root_dir', help='Name of the designated root directory of the project.', default = None)
    # name of the data directory
    parser.add_argument('--data_dir', help='Name and full path to the directory will all app data, expect videos, will be stored.', default = None)
    # name of the directory for videos to be stored
    parser.add_argument('--vid_dir', help='Name and full path to where the videos will be stored (these are considered PID, must be secured appropriately).', default = None)
    # name of the directory for videos to be stored
    parser.add_argument('--vid_url', help='Base url for where the videos will be stored (these are considered PID, must be secured appropriately).', default = None)
    # url where the site will be hosted
    parser.add_argument('--host', help='Full path to push the site to.', default = None)
    return parser

# simple function to return '' if None is given
def to_emptystr(in_str=None):
    if in_str == None:
        return 'NA'
    else:
        return in_str

def main():
    '''
    Main method for this program.
    '''
    # Create the argparser
    arg_parser = create_arg_parser()
    # Parse the args from terminal input
    parsed_args = arg_parser.parse_args(sys.argv[1:])
    # builds the system paths and files in the user's home directory
    build_system()
    # run the build_config function
    build_config(rootDir=parsed_args.root_dir)
    # run the build_config on the data folder
    build_config(rootDir=parsed_args.data_dir, file_name='data')
    # run the build_config on the video folder
    build_config(rootDir=parsed_args.vid_dir, file_name='videos')
    # create the conda env if it does not exist
    os.system("Rscript create_conda.R")
    # create the conda start file
    #start_str = '''
    #!{shell_type}
    #conda activate r-reticulate
    #'''.format(shell_type= "bash") #os.environ['SHELL'])
    # save the start script to a file
    #with open("start_conda", "w") as f:
    #    f.write(start_str)
    # make the start script executable
    #os.system("chmod +x start_conda")
    # switch the to the conda environment
    #os.system("./start_conda")
    # rebuild the conda_env
    build_reticulate_conda('pip.txt')
    # if all steps completed successfully, end the function by updating cfg.yaml
    build_yaml(root=parsed_args.root_dir, data=parsed_args.data_dir, videos=parsed_args.vid_dir, video_url=parsed_args.vid_url, host=parsed_args.host, fmri=None, bids=None)

# Execution of main method
if __name__ == '__main__':
    main()
