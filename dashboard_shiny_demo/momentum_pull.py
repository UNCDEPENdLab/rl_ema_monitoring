import argparse
import json
import os
import shutil
import sys
from datetime import datetime
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description='Tool for initializing a subject into the current project.')
    # id argument
    parser.add_argument('--id', help='the subject id', default = None, required=True)
    # gmail argument
    #parser.add_argument('--gmail', help='the gmail associated with the subjects phone and google drive', default = None, required=True)
    # return the parser object
    return parser

def google_drive_connect(id, path):
    '''
    Method for connecting to the subject's Google Drive
    '''
    # Initialize Google Auth object
    gauth = GoogleAuth()
    # String to the credentials file
    credPath = path + '/Subjects/' + id + "/mycreds.txt"
    # Try to load saved client credentials
    gauth.LoadCredentialsFile(credPath)
    # If this credential does not exist
    if gauth.credentials is None:
        # Authenticate if they're not there
        raise Exception("Error: the credentials for this subject do not exist.")
    elif gauth.access_token_expired:
        # Refresh them if expired
        print("Note: This subject's credentials are being renewed.")
        gauth.Refresh()
    # Initialize the Google Drive Connection
    drive = GoogleDrive(gauth)
    # Return the Google Drive object
    return drive

def subjectID2driveID(id, drive_list):
    '''
    Method to get the subject's unique Google Drive id from the subject's id number
    '''
    # Initialize variable to hold the folder id
    folder_id = None
    # Iterate through the files/folders at root to find the subject
    for file in drive_list:
        if file['title'] == id:
            folder_id = file['id']
    # Return the drive id
    return folder_id

def get_pull_files(id, path, drive):
    '''
    Method for determining the files to pull from the given Google Drive.
    '''
    # Get a set of files already downloaded
    json_dict = None
    with open(path + '/Subjects/' + id + '/subject.json') as f:
        json_dict = json.load(f)
    file_dict = json_dict["subject"]["files"]
    #print(file_dict)
    # Split the schedule, physio, and video files into sets
    schedule_local = file_dict["schedule"]
    physio_local = set(file_dict["physio"])
    video_local = set(file_dict["video"])
    # Get the PyDrive formatted list of files from 'root'
    drive_list = drive.ListFile({'q': "'root' in parents and trashed=false"}).GetList()
    #Get the subject's Google Drive directory id from the subject id
    drive_id = subjectID2driveID(id=id, drive_list=drive_list)
    # Get the list of files from the subject's folder in Google Drive
    file_list = drive.ListFile({'q': "'{}' in parents and trashed=false".format(drive_id)}).GetList()
    # Initialize sets for file names from Google Drive
    schedule_gdrive = None
    physio_gdrive = list()
    video_gdrive = list()
    # Split the schedule, physio, and video files into lists
    for i, file in enumerate(sorted(file_list, key = lambda x: x['title']), start=1):
        # Name of the file
        file_name = file['title']
        #print(file_name)
        # Store in the appropriate location
        if 'physio' in file_name:
            physio_gdrive.append(file)
        elif '.db' not in file_name:
            video_gdrive.append(file)
        elif 'schedule' in file_name:
            if schedule_gdrive == None:
                schedule_gdrive = file
            else:
                raise Warning("Warning: multiple schedule files were found. The first file found was kept: " + schedule_gdrive['title'])
    #print(schedule_gdrive)
    #print(physio_gdrive)
    #print(video_gdrive)
    # Initialize variables that are the sets of files to be pulled
    schedule_pull = None
    # See if the schedule file needs download
    if schedule_local == None:
        schedule_pull = schedule_gdrive
    # Remove physio files that do not need pulled
    physio_pull = list()
    for physio_file in physio_gdrive:
        if physio_file['title'] not in physio_local:
            physio_pull.append(physio_file)
    # Remove video files that do not need pulled
    video_pull = list()
    for video_file in video_gdrive:
        if video_file['title'].replace('/', '_') not in video_local:
            video_pull.append(video_file)
    #physio_pull = physio_gdrive - physio_local
    #video_pull = video_gdrive - video_local
    # Create the pull list
    pull_dict = {"schedule":schedule_pull, "physio":physio_pull, "video":video_pull}
    # Return the list of files to pull
    return pull_dict
    
def download_files(id, path, drive, fileDict, scheduleLatest):
    '''
    Method for downloading the files.
    '''
    # Attempt to Download the schedule file NOTE: this may need updated => is the schedule file constantly appended to?
    print('Downloading the updated schedule file from GDrive:')
    # Move old schedule file to archive if it exists
    if scheduleLatest != None and os.path.isfile(path + '/' + '/Subjects/' + id + '/schedule/' + scheduleLatest):
        shutil.move(path + '/' + '/Subjects/' + id + '/schedule/' + scheduleLatest, path + '/' + '/Subjects/' + id + '/schedule/archive' + scheduleLatest)
    # Download the updated schedule file and add a time stamp and add a time-stamp
    now = datetime.now()
    timestamp = datetime.timestamp(now)
    fileDict["schedule"].GetContentFile(path + '/' + '/Subjects/' + id + '/schedule/' + str(timestamp) + '_' + fileDict["schedule"]["title"])
    print('Downloading {} from GDrive'.format(fileDict["schedule"]['title']))
    # Variables for logging the download
    physio_len = len(fileDict["physio"])
    video_len = len(fileDict["video"])
    # Attempting to Download the physio files
    print('Downloading the new physio files from GDrive:')
    for i, file in enumerate(fileDict["physio"], start=1):
        print('Downloading {} from GDrive ({}/{})'.format(file['title'], i, physio_len))
        file.GetContentFile(path + '/Subjects/' + id + '/physio/' + file['title'])
    # Attempting to Download the video files
    print('Downloading the new video files from GDrive:')
    for i, file in enumerate(fileDict["video"], start=1):
        print('Downloading {} from GDrive ({}/{})'.format(file['title'], i, video_len))
        file.GetContentFile(path + '/Subjects/' + id + '/video/' + file['title'].replace('/', '_'))

def update_json(id, path, fileDict):
    '''
    Method for updating a subject's json file.
    '''
    # Have the files pulled, get the files from before pull
    with open(path + '/Subjects/' + id + '/subject.json') as f:
        json_dict = json.load(f)
	    # 

def get_schedule_name(id, path):
    '''
    Method to get the subject's current schedule file name.
    '''
    # Get a set of files already downloaded
    schedule_name = None
    with open(path + '/Subjects/' + id + '/subject.json') as f:
        json_dict = json.load(f)
    schedule_name = json_dict["subject"]["files"]["schedule"]
    # Return the name of the schedule file
    return schedule_name
    
def pull_files(id, path):
    '''
    Method for pulling data from the given Google Drive and updates the subjects json file.
    '''
	# Connect to Google Drive
    drive = google_drive_connect(id=id, path=path)
    # Get the set of files to pull based on what is not stored locally
    files2pull = get_pull_files(id=id, path=path, drive=drive)
    #print(files2pull)
    # Get the name of the current schedule file
    current_schedule = get_schedule_name(id=id, path=path)
    # Pull the files
    download_files(id=id, path=path, drive=drive, fileDict=files2pull, scheduleLatest=current_schedule)
    # Update the subject's json file
    #update_json(id=id, path=path, fileDict=files2pull)

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
    # Pull the data and update the subject's json file
    pull_files(id=id, path=currDir)

# Execution of main method
if __name__ == '__main__':
    main()