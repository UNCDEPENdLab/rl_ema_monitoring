import argparse
import os
import sys
import json
from rebuild_conda_funcs import build_reticulate_conda
from rebuild_config_funcs import build_config

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description="Tool for setting cfg.json and conda_env, should be called from the system directly.")
    # root_name argument
    parser.add_argument('--root_name', help='name of the designated root directory of the project', default = None)
    return parser

def main():
    '''
    Main method for this program.
    '''
    # Create the argparser
    arg_parser = create_arg_parser()
    # Parse the args from terminal input
    parsed_args = arg_parser.parse_args(sys.argv[1:])
    # rebuild the conda_env
    build_reticulate_conda('pip.txt')
    # run the build_config function
    build_config(root_name=parsed_args.root_name)

# Execution of main method
if __name__ == '__main__':
    main()