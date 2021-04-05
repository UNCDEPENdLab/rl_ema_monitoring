import json
import os
import shutil
import sys
from datetime import datetime
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive

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

# method to convert original app naming convention to new naming convention
def updatePhysioName(old_name=None):
    # initialize variable 
    old_string = old_name
    #print(old_string)
    # get the appropriate file name
    if (',' in old_string):
        # remove comma and odd-character replacement and split by white space
        old_string = old_string.replace(',', '').replace('\uf03a', ' ').replace(':', '_').split(' ')
        # remove a front chunk of the data
        front = old_string.pop(0)
        # split front by '_'
        front = front.split('_')
        # move the converted month into the old string list
        old_string.insert(2, front.pop(2))
        # remove '.db' from the PM or AM string
        old_string.append(old_string.pop(-1)[0:2])
        # collapes back to a string
        old_string = '_'.join(old_string)
        # convert this string to a datetime
        old_string = datetime.strptime(old_string, '%d_%Y_%b_%I_%M_%S_%p')
        # convert back to a string
        old_string = old_string.strftime('%Y%m%d%H%M%S') # '%Y_%m_%d_%H_%M_%S'
        # rejoin the front peices
        front = '_'.join(front)
        # rebuild the string
        new_string = front + '_' + old_string + '.db'
        # return the new string
        return new_string
    # otherwise
    else:
        # use the original file name
        return old_string

# method to convert original app naming convention to new naming convention
def updateVideoName(old_name=None):
    # initialize variable 
    old_string = old_name
    # get the appropriate file name
    if ('/' in old_string):
        # replace '/' with '_', split by '_', and 
        old_string = old_string.replace('/', '_').split('_')
        # save the first element at the subject id
        id = old_string[0]
        # take the last element and remove '.db'
        old_string = old_string[-1] #.replace('.db', '')
        # convert milliseconds to a datetime object
        #old_string = datetime.fromtimestamp(int(old_video)/1000.0)
        # convert date time into a string
        #old_string = old_video.strftime('%Y%m%d%H%M%S')
        # rebuild the string
        new_string = id + '_video_' + old_string
        # return the new string
        return new_string
    # otherwise
    else:
        # use the original file name
        return old_string
    
def get_pull_files(id, path, drive):
    '''
    Method for determining the files to pull from the given Google Drive.
    '''
    # Get a set of files already downloaded
    json_dict = None
    with open(path + '/Subjects/' + id + '/subject.json') as f:
        json_dict = json.load(f)
    file_dict = json_dict["subject"]["files"]
    # initialize our local varibales for the three data modalities
    schedule_local = None
    physio_local = None
    video_local = None
    # Split the schedule, physio, and video files into sets
    # try the instance where a schedule file would already exist
    try:
        schedule_local = file_dict["schedule"]["file_name"]
    # try the instance where a schedule file does not exist
    except:
        schedule_local = file_dict["schedule"]
    # try the instance where a physio file does not exist
    try:
        physio_local = set(file_dict["physio"])
    # try the instance where a physio file would already exist
    except:
        physio_local = set(physio_dict["file_name"] for physio_dict in file_dict["physio"])
    # try the instance where a video file would does not exist
    try:
        video_local = set(file_dict["video"])
    # try the instance where a video file would already exist
    except:
        video_local = set(video_dict["file_name"] for video_dict in file_dict["video"])
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
    # See if the schedule file needs downloaded
    #if schedule_local == None:
    # Assumes schedule file always needs downloaded
    schedule_pull = schedule_gdrive
    # Remove physio files that do not need pulled
    physio_pull = list()
    for physio_file in physio_gdrive:
        #print(updatePhysioName(old_name=physio_file['title']))
        #print('')
        if updatePhysioName(old_name=physio_file['title']) not in physio_local:
            physio_pull.append(physio_file)
    # Remove video files that do not need pulled
    video_pull = list()
    for video_file in video_gdrive:
        if updateVideoName(old_name=video_file['title']) not in video_local:
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
    # initialize the dictionary of logged results for each file
    logDict = dict()
    # Attempt to Download the schedule file NOTE: this may need updated => is the schedule file constantly appended to?
    print('Downloading the updated schedule file from GDrive:')
    # Move old schedule file to archive if it exists
    if scheduleLatest != None and os.path.isfile(path + '/' + '/Subjects/' + id + '/schedule/' + scheduleLatest):
        shutil.move(path + '/Subjects/' + id + '/schedule/' + scheduleLatest, path + '/' + '/Subjects/' + id + '/schedule/archive/' + scheduleLatest)
    # Move any other residual files in the schedule folder to the schedule file archive
    for file in [x for x in os.listdir(path + '/' + '/Subjects/' + id + '/schedule/') if os.path.isfile(path + '/' + '/Subjects/' + id + '/schedule/' + x) == True]:
        shutil.move(path + '/Subjects/' + id + '/schedule/' + file, path + '/' + '/Subjects/' + id + '/schedule/archive/' + file)
    # Download the updated schedule file and add a time stamp and add a time-stamp
    now = datetime.now()
    timestamp = datetime.timestamp(now)
    # attempt to download the file
    try:
        # attempt the download
        fileDict["schedule"].GetContentFile(path + '/' + '/Subjects/' + id + '/schedule/' + str(timestamp) + '_' + fileDict["schedule"]["title"])
        # log that the download occured successfully
        logDict.update({fileDict["schedule"]["title"]: "Download Successful"})
    # if the attempt failed
    except:
        # print to console that the download attempt failed
        print("Warning: download of " + fileDict["schedule"]["title"] + " was unsuccessfull!")
        # log that the download occured unsuccessfully
        logDict.update({fileDict["schedule"]["title"]: "Download Failed"})
    print('Downloading {} from GDrive'.format(fileDict["schedule"]['title']))
    # Variables for logging the download
    physio_len = len(fileDict["physio"])
    video_len = len(fileDict["video"])
    # Attempting to Download the physio files
    print('Downloading the new physio files from GDrive:')
    for i, file in enumerate(fileDict["physio"], start=1):
        print('Downloading {} from GDrive ({}/{})'.format(file['title'], i, physio_len))
        # attempt to download the file
        # initialize a file title varibale
        physio_title = updatePhysioName(old_name=file['title'])
        try:
            # attempt the download
            file.GetContentFile(path + '/Subjects/' + id + '/physio/' + physio_title)
            # log that the download occured successfully
            logDict.update({physio_title: "Download Successful"})
        # if the attempt failed
        except:
            # print to console that the download attempt failed
            print("Warning: download of " + physio_title + " was unsuccessfull!")
            # log that the download occured unsuccessfully
            logDict.update({physio_title: "Download Failed"})
    # Attempting to Download the video files
    print('Downloading the new video files from GDrive:')
    for i, file in enumerate(fileDict["video"], start=1):
        print('Downloading {} from GDrive ({}/{})'.format(file['title'], i, video_len))
        # initialize a file title varibale
        video_title = updateVideoName(old_name=file['title'])
        # attempt to download the file
        try:
            # attempt the download
            file.GetContentFile(path + '/Subjects/' + id + '/video/' + video_title)
            # log that the download occured successfully
            logDict.update({video_title: "Download Successful"})
        # if the attempt failed
        except:
            # print to console that the download attempt failed
            print("Warning: download of " + video_title + " was unsuccessfull!")
            # log that the download occured unsuccessfully
            logDict.update({video_title: "Download Failed"})
    # return the dictionary of logs
    return logDict

def update_json(id, path, fileDict, scheduleLatest, logDict):
    '''
    Method for updating a subject's json file.
    '''
    # get the pull datetime
    now = datetime.now()
    # convert datetime object to a string
    now = now.strftime("%m/%d/%Y, %H:%M:%S")
    # variable that will get the new dict to be printed to the json file
    new_json_dict = None
    # Have the files pulled in fileDict, get the files from before pull and combine them
    with open(path + '/Subjects/' + id + '/subject.json', 'r') as json_file: 
        # dict of the old data
        old_json_dict = json.load(json_file)
        # get the name of the schedule.db file without the time-stamp
        sched_name = scheduleLatest.split('_')[1] + '_' + scheduleLatest.split('_')[2]
        # use the new schedule file
        old_json_dict["subject"]["files"]["schedule"] = {"file_name": scheduleLatest, "datetime_of_pull": now, "pull_log": logDict[sched_name]}
        # get a list of the titles of the physio files
        physioList = [{"file_name": updatePhysioName(x['title']), "datetime_of_pull": now, "pull_log": logDict[updatePhysioName(x['title'])]} for x in fileDict["physio"]]
        # update the physio files
        old_json_dict["subject"]["files"]["physio"].extend(physioList)
        # get a list of the titles of the video files
        videoList = [{"file_name": updateVideoName(x['title']), "datetime_of_pull": now, "pull_log": logDict[updateVideoName(x['title'])]} for x in fileDict["video"]]
        # update the video files
        old_json_dict["subject"]["files"]["video"].extend(videoList)
        # set the variable out of this scope to feed in for writing
        new_json_dict = old_json_dict

    with open(path + '/Subjects/' + id + '/subject.json', 'w') as json_file:
        # overwite the file with the new files recorded
        json.dump(new_json_dict, json_file, indent=4)

def get_schedule_name(id, path):
    '''
    Method to get the subject's current schedule file name.
    '''
    # Get a set of files already downloaded
    schedule_name = None
    with open(path + '/Subjects/' + id + '/subject.json') as f:
        json_dict = json.load(f)
    # try to get the schedule name
    try:
        # path down the json file where the file name would exist if the file exists
        schedule_name = json_dict["subject"]["files"]["schedule"]["file_name"]
    # otherwise, just move on
    except:
        pass
    # Return the name of the schedule file (will return None if there was no file)
    return schedule_name

def get_schedule_name_system(id, path):
    '''
    Method to get the most recent schedule file on the local system
    '''
    # get all db files in the schedule directory
    db_files = [f for f in os.listdir(path + '/' + '/Subjects/' + id + '/schedule/') if f.endswith('_' + id + '_schedule.db')]
    # raise an error if there is more than one db file
    if len(db_files) > 1:
        #raise Exception("Error: there are more than 1 db files in this directory. Purge the extra files and rerun.")
        # get a dictionary from datetime stamps to file names
        time_stamps2files = {int(f.split('_')[0].split('.')[0]): f for f in db_files}
        # get a list of the time stamps as ints
        #time_stamps = [t for t in time_stamps2files.keys()]
        # get the largest int (largest timestamp will be the newest file)
        newest = max(time_stamps2files.keys()) #str(max(time_stamps.keys()))
        # return the name of the newest file
        return time_stamps2files[newest]
    # otherwise, return the file name
    return db_files[0]

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
    #print(current_schedule)
    # if there are files to be download
    if files2pull != None:
        # Pull the files
        logs = download_files(id=id, path=path, drive=drive, fileDict=files2pull, scheduleLatest=current_schedule)
        # Update the name of the current schedule file
        current_schedule = get_schedule_name_system(id=id, path=path)
        # Update the subject's json file
        update_json(id=id, path=path, fileDict=files2pull, scheduleLatest=current_schedule, logDict=logs)