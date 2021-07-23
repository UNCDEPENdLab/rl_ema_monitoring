import argparse
from momentum_pull_func import *

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description='Tool for pulling subject data into a project.')
    # id argument
    parser.add_argument('--id', help='the subject id', default = None, required=True)
    # path argument
    parser.add_argument('--path', help='path to the subject ema data directory', default = None, required=False)
    # clinical user path argument
    parser.add_argument('--clinical_path', help='path to the subject video data directory', default = None, required=False)
    # clinical user path argument
    parser.add_argument('--clinical_url', help='url to the subject video data directory', default = None, required=False)
    # gmail argument
    #parser.add_argument('--gmail', help='the gmail associated with the subjects phone and google drive', default = None, required=True)
    # return the parser object
    return parser

def main():
    '''
    Main method for this program.
    '''
    # Get the current working directory.as a global variable within main
    currDir = os.getcwd()
    # Create the argparser
    arg_parser = create_arg_parser()
    # Parse the args from terminal input
    parsed_args = arg_parser.parse_args(sys.argv[1:])
    # The subject id
    id = parsed_args.id
    # If the there is a path given, set as currDir
    if parsed_args.path != None:
        currDir = parsed_args.path
    # Pull the data and update the subject's json file
    pull_files(id=id, path=currDir, clinical_path=parsed_args.clinical_path, clinical_url=parsed_args.clinical_url)

# Execution of main method
if __name__ == '__main__':
    main()