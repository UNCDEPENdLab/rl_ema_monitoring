import argparse
import json
import os
import sys
from set_status import add_subject_by_status
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
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

def make_json(id, gmail, path):
    '''
    Method to create a json file for the subject.
    '''
    # Initialize an empty data object
    data = None
    # Open the json file and convert it to a dict
    with open(path + '/' + 'subjects_skeleton.json') as f:
        data = json.load(f)
        #print(data)
    # Set the id
    data["subject"]["id"] = id
    # Set the gmail
    data["subject"]["gmail"] = gmail
    # Export to a new json file in the subject's directory
    with open(path + '/Subjects/' + id + '/subject.json', 'w') as json_file:
        json.dump(data, json_file, indent=4)
    
def google_auth(id, path):
    # String to the credentials file
    credPath = path + '/Subjects/' + id + "/mycreds.txt"
    # Initialize Google Auth object
    gauth = GoogleAuth()
    # Try to load saved client credentials
    gauth.LoadCredentialsFile(credPath)
    # If this credential does not exist
    print("gauth.credentials")
    if gauth.credentials is None:
        # Authenticate if they're not there
        gauth.LocalWebserverAuth()
        #print("hello")
    elif gauth.access_token_expired:
        # Refresh them if expired
        gauth.Refresh()
    else:
        # Initialize the saved creds
        gauth.Authorize()
    # Save the current credentials to a file
    #print("here")
    gauth.SaveCredentialsFile(credPath)

def add_subject(id, gmail):
    '''
    Method to do everything for adding the subject.
        Updates json files, creates file hierarchy, authenticates with Google.
    '''
    # Get the current working directory.as a global variable within main
    currDir = os.getcwd()
    # Make independent to giving the '@gmail.com' suffix
    if gmail.endswith('@gmail.com') == False:
        gmail = gmail + '@gmail.com'
    # Make the Subject directory
    if os.path.isdir(currDir + '/Subjects') == False:
        # Make the Subjects directory
        os.mkdir(currDir + '/Subjects')
    # Make the directory for the subject
    if os.path.isdir(currDir + '/Subjects/' + id) == False:
        # Make the subject directory
        os.mkdir(currDir + '/Subjects/' + id)
    # Make the schedule directory for the subject
    if os.path.isdir(currDir + '/Subjects/' + id + '/schedule') == False:
        # Make the schedule directory
        os.mkdir(currDir + '/Subjects/' + id + '/schedule')
    # Make the schedule archive directory for the subject
    if os.path.isdir(currDir + '/Subjects/' + id + '/schedule/archive') == False:
        # Make the schedule archive directory
        os.mkdir(currDir + '/Subjects/' + id + '/schedule/archive')
    # Make the physio directory for the subject
    if os.path.isdir(currDir + '/Subjects/' + id + '/physio') == False:
        # Make the physio directory
        os.mkdir(currDir + '/Subjects/' + id + '/physio')
    # Make the video directory for the subject
    if os.path.isdir(currDir + '/Subjects/' + id + '/video') == False:
        # Make the video directory
        os.mkdir(currDir + '/Subjects/' + id + '/video')
    # Make the json file for the subject
    if os.path.isfile(currDir + '/Subjects/' + id + '/subject.json') == False:
        # Make the json file for the subject
        make_json(id=id, gmail=gmail, path=currDir)
    # Authorize the gmail with the subject id
    google_auth(id=id, path=currDir)
    #print("here 3")
    # Set the status of the subject
    add_subject_by_status(id=id, status="active")

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