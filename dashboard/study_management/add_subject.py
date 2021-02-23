import argparse
import os
import sys
from add_subject_func import add_subject

'''
1. Creae a directory for the subject.
2. Authorize the gmail if need be.
3. Initialize the subject within the json file.
'''

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description='Tool for initializing a subject into the current project.')
    # id argument
    parser.add_argument('--id', help='the subject id', default = None, required=True)
    # gmail argument
    parser.add_argument('--gmail', help='the gmail associated with the subjects phone and google drive', default = None, required=True)
    # status argument
    parser.add_argument('--status', help='status to set the subject to (should be active or inactive)', default = None, required=True)
    # return the parser object
    return parser

def main():
    '''
    Main method for this program.
    '''
    # Create the argparser
    arg_parser = create_arg_parser()
    # Parse the args from terminal input
    parsed_args = arg_parser.parse_args(sys.argv[1:])
    # The subject id
    id = parsed_args.id
    # Ensure an id was given
    if id is None:
        raise Exception("Error: id must be given.")
        exit()
    # The subject's associated gmail
    gmail = parsed_args.gmail
    # Ensure a gmail was given
    if gmail is None:
        raise Exception("Error: gmail must be given.")
        exit()
    # Add the subject
    add_subject(id=id, gmail=gmail)
    
# Execution of main method
if __name__ == '__main__':
    main()