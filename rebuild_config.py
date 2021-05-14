import argparse
import os
import sys
import json
import rebuild_config_funcs

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description="Tool for setting cfg.json, should be called from the system directly.")
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
    # run the build_config function
    rebuild_config_funcs.build_config(root_name=parsed_args.root_name)

# Execution of main method
if __name__ == '__main__':
    main()
