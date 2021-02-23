import argparse
from set_status_func import add_subject_by_status
'''
Allows for the setting of a subject as active or inactive in the study.
'''

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description="Tool for setting the status of a subject as active or inactive.")
    # id argument
    parser.add_argument('--id', help='the subject id', default = None, required=True)
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
    status = parsed_args.status
    # Ensure an approriate status was given
    if gmail is None:
        raise Exception("Error: a status must be given.")
        exit()
    # Ensure the status given in 'active' or 'inactive'
    if (status == 'active') | (status == 'inactive'):
	    pass
    # Exit if an incorrect status is given
    else:
        raise Exception("Error: status must be 'active' or 'inactive', not " + status + ".")
        exit()
    add_subject_by_status(id=id, status=status)

# Execution of main method
if __name__ == '__main__':
    main()