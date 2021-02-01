import argparse
import json
import os
import sys
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

def add_subject_by_status(id, status):
    '''
    Method for adding a subject to a status
    '''
    # Initialize an empty data object
    data = None
    # Get the current working directory
    path = os.getcwd()
    # Open the json file and convert it to a dict
    with open(path + '/subject_status.json') as f:
        data = json.load(f)
        # Add the subject in the approriate place and remove if needed
        # Subject to be set as inactive
        if status == 'inactive':
            # Check if the subject already exists with an 'inactive' status
            if id in data['subjects']['inactive']:
                # Note that the subject is already set to inactive
                print("Subject " + id + " is already set as inactive.")
            # Check if the subject already exists with an 'active' status
            elif id in data['subjects']['active']:
                # Remove the subject from active status
                data['subjects']['active'].remove(id)
                # Add the subject to inactive status
                data['subjects']['inactive'].append(id)
                # Note the movement from active to inactive
                print("Subject " + id + " has been set from active to inactive.")
            # Subject has not been assigned a status before.
            else:
                # Add subject to inactive status
                data['subjects']['inactive'].append(id)
                # Note that the subject has been set to inactive
                print("Subject " + id + " has been set to inactive.")
        # Subject to be set as active
        elif status == 'active':
            # Check if the subject already exists with an 'active' status
            if id in data['subjects']['active']:
                # Note that the subject is already set to active
                print("Subject " + id + " is already set as active.")
            # Check if the subject already exists with an 'inactive' status
            elif id in data['subjects']['inactive']:
                # Remove the subject from inactive status
                data['subjects']['inactive'].remove(id)
                # Add the subject to active status
                data['subjects']['active'].append(id)
                # Note the movement from inactive to active
                print("Subject " + id + " has been set from inactive to active.")
            # Subject has not been assigned a status before.
            else:
                # Add subject to active status
                data['subjects']['active'].append(id)
                # Note that the subject has been set to active
                print("Subject " + id + " has been set to active.")
    # Overwrite the json file with the new dict
    with open(path + '/subject_status.json', 'w') as json_file:
        json.dump(data, json_file)

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
    elif (status == 'active') | (status == 'inactive'):
	    pass
    # Exit if an incorrect status is given
    else:
        raise Exception("Error: status must be 'active' or 'inactive', not " + status + ".")
        exit()
    add_subject_by_status(id=id, status=status)

# Execution of main method
if __name__ == '__main__':
    main()