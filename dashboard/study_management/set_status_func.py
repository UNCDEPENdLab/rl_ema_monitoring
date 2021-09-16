import datetime
import json
import os
import sys
import yaml
from data_management_functions import get_cfg_var_p

def add_subject_by_status(id, status, path=None):
    '''
    Method for updating a subject's status
    '''
    # Initialize an empty data object
    data = None
    # Get the current working directory
    # if the path variable is None
    if path == None:
        # then use the current working directory by default
        #path = os.getcwd()
        # then use the data path in the project config
        path = get_cfg_var_p(var="root")
    # handle for running on a windows operaing system (removes leading '/')
    if sys.platform == "win32":
        path = path.replace('\\','/').replace('//','/')
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
            # variable for the individula subject data
            subjectData = None
            # now that this is set at the project level, set the subject as inactive locally
            with open(path + '/' + id + '/subject.json') as s:
                # load the subject json file
                subjectData = json.load(s)
                # set the local status element as inactive
                subjectData['subject']['status'] = 'inactive'
            # reopen the json file in write mode
            with open(path + '/' + id + '/subject.json', 'w') as subject_json:
                # dump the modified data to the json file
                json.dump(subjectData, subject_json, indent=4)
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
            # variable for the individula subject data
            subjectData = None
            # now that this is set at the project level, set the subject as inactive locally
            with open(path + '/Subjects/' + id + '/subject.json') as s:
                # load the subject json file
                subjectData = json.load(s)
                # set the local status element as active
                subjectData['subject']['status'] = 'active'
            # reopen the json file in write mode
            with open(path + '/Subjects/' + id + '/subject.json', 'w') as subject_json:
                # dump the modified data to the json file
                json.dump(subjectData, subject_json, indent=4)
    # Overwrite the json file with the new dict
    with open(path + '/subject_status.json', 'w') as json_file:
        json.dump(data, json_file, indent=4)