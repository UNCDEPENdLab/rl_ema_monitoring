import json
import os
import sys
from set_status_func import add_subject_by_status
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

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