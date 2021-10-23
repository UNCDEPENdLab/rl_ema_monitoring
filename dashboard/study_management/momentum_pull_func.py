import json
import os
import shutil
import sys
import hashlib
import glob
from datetime import datetime
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
from data_management_functions import get_cfg_var_p

def google_drive_connect(id, path, auto_auth=True):
    '''
    Method for connecting to the subject's Google Drive
    '''
    # Initialize Google Auth object
    try:
        gauth = GoogleAuth(settings_file=path + '/Subjects/' + id + '/pydrive.yaml')
        local_set = True
    except:
        gauth = GoogleAuth()
        local_set = False
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
        # try to refresh the access token
        try:
            gauth.Refresh()
        # may fail if the refresh token does not exist
        except:
            #gauth.LocalWebserverAuth()
            #gauth.Authenticate()
            return None
    # Save the current credentials to the file
    #gauth.SaveCredentialsFile(credPath)
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

# method to confirm same file by timestamp/datetime for physios
def ts_physio(gdrive_fname):
    # try the strings with the same format
    #try: 
    #    if file_name.split("_")[-1].replace(".db", "") == gdrive_fname.replace('/', '_').split("_")[-1]:
    #        return True
    #except:
    #    pass
    # try for the case of the old naming convention
    try:
        # remove comma and odd-character replacement and split by white space
        old_string = gdrive_fname.replace(',', '').replace('\uf03a', ' ').replace(':', '_').split(' ')
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
        #if file_name.split("_")[-1].replace(".db", "") == old_string:
        #print(old_string)
        return old_string
    except:
        #print(gdrive_fname.split("_")[-1].replace('.db', ''))
        return gdrive_fname.split("_")[-1].replace('.db', '')
    #return False

# method to confirm same file by timestamp/datetime for videos
def ts_video(gdrive_fname):
    # try the strings with the same format
    #print(file_name.split("_")[-1].replace(".mp4", ""))
    #print(gdrive_fname.replace('/', '_').split("_")[-1])
    return gdrive_fname.replace('/', '_').split("_")[-1].replace(".mp4", '')
    #try:    
    #    if file_name.split("_")[-1].replace(".mp4", "") == gdrive_fname.replace('/', '_').split("_")[-1]:
    #        return True
    #except:
    #    pass
    #return False


def compare_checksum(local_text, gdrive_checksum):
    '''
    Method to compare the checksums of the files.
    '''
    checksum1 = str(hashlib.md5(local_text.encode('utf-8')).hexdigest())
    checksum2 = str(hashlib.md5(gdrive_checksum.encode('utf-8')).hexdigest())
    print("Local: " + checksum1)
    print("Gdrive: " + checksum2)
    return checksum1 == checksum2
    
def get_pull_files(id, path, clinical_path, drive, include_failed=True, check_local=True, check_md5=True, pull_video=True, pull_all=False):
    '''
    Method for determining the files to pull from the given Google Drive.
    '''
    # Reconnect to Google Drive
    # if checking md5, must check local
    if check_md5 == True:
        check_local = True
    drive = google_drive_connect(id=id, path=path)
    # Get a set of files already downloaded
    json_dict = None
    with open(path + '/Subjects/' + id + '/subject.json') as f:
        json_dict = json.load(f)
    # get the actual files that exist for the subject
    try:
        on_system_sched = [os.path.basename(x) for x in glob.glob(path + '/Subjects/' + id + '/**/*schedule*.db', recursive=True) if "archive" not in x][0]
    except:
        on_system_sched = None
    on_system_physio = [os.path.basename(x) for x in glob.glob(path + '/Subjects/' + id + '/**/*physio*.db', recursive=True)]
    on_system_video = [os.path.basename(x) for x in glob.glob(clinical_path + '/' + id + '**/**video*.mp4', recursive=True)]
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
        # ensure that this is not an empty file (Some files have 0 Byte duplicated)
        if int(file["fileSize"]) > 0:
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
        else:
            print(file_name + " is size: " + str(int(file["fileSize"])) + "; is empty...was not added to files to be pulled.")
    # if pulling all files, otherwise, do not pull files in json cache file
    if pull_all == True:
        # set the pull_dict to everything from the GDrive
        pull_dict = {"schedule":schedule_gdrive, "physio":physio_gdrive, "video":video_gdrive}
        # return this dictionary
        return [pull_dict, True, None]

    #print(schedule_gdrive)
    #print(physio_gdrive)
    #print(video_gdrive)
    # Initialize variables that are the sets of files to be pulled
    #schedule_pull = None
    # See if the schedule file needs downloaded
    #if schedule_local == None:
    # Assumes schedule file always needs downloaded
    schedule_pull = schedule_gdrive #['title']
    # current schedule file 
    curr_sched = None
    # TODO: Add the local and md5 check here
    # Remove physio files that do not need pulled
    physio_pull = list()
    for physio_file in physio_gdrive:
        #print(updatePhysioName(old_name=physio_file['title']))
        #print('')
        # check against the json cache
        if updatePhysioName(old_name=physio_file['title']) not in physio_local:
            physio_pull.append(physio_file)
        # if checking for local existence
        if check_local == True:
            # check that the file exists locally
            if updatePhysioName(old_name=physio_file['title']) not in on_system_physio:
                physio_pull.append(physio_file)
            # check_local must be true to check md5
            if check_md5 == True and os.path.isfile(path + '/Subjects/' + id + '/physio/' + updatePhysioName(old_name=physio_file['title'])):
                # get the local checksum
                if os.path.isfile(path + '/Subjects/' + id + '/physio/' + updatePhysioName(old_name=physio_file['title'])):
                    local_checksum = hashlib.md5(open(path + '/Subjects/' + id + '/physio/' + updatePhysioName(old_name=physio_file['title']),'rb').read()).hexdigest()
                # get the GDrive checksum
                gdrive_checksum = physio_file["md5Checksum"]
                # check the the md5 checksums match
                if local_checksum != gdrive_checksum:
                    physio_pull.append(physio_file)
    # Remove video files that do not need pulled
    video_pull = list()
    for video_file in video_gdrive:
        if updateVideoName(old_name=video_file['title']) not in video_local:
            video_pull.append(video_file)
        # if checking for local existence
        if check_local == True:
            # check that the file exists locally
            if updateVideoName(old_name=video_file['title']) not in on_system_video:
                video_pull.append(video_file)
            # check_local must be true to check md5
            if check_md5 == True and os.path.isfile(clinical_path + '/' + id + '/' + updateVideoName(old_name=video_file['title'])):
                # get the local checksum
                if os.path.isfile(clinical_path + '/' + id + '/' + updateVideoName(old_name=video_file['title'])):
                    local_checksum = hashlib.md5(open(clinical_path + '/' + id + '/' + updateVideoName(old_name=video_file['title']),'rb').read()).hexdigest()
                # get the GDrive checksum
                gdrive_checksum = video_file["md5Checksum"]
                # check the the md5 checksums match
                if local_checksum != gdrive_checksum:
                    video_pull.append(video_file)
    #physio_pull = physio_gdrive - physio_local
    #video_pull = video_gdrive - video_local
    # Create the pull list
    pull_dict = {"schedule":schedule_pull, "physio":physio_pull, "video":video_pull}
    #print(str(len(pull_dict["video"])))

    # Boolean to pull an updated schedule file
    update_sched = True
    # if there are no physio or video files
    if len(pull_dict["physio"]) == len(pull_dict["video"]) == 0:
        # then do not pull the schedule file
        update_sched = False
    # if there are no physio and video files are not being pulled
    if len(pull_dict["physio"]) == 0 and not pull_video:
        #print("here")
        # then do not pull the schedule file
        update_sched = False

    # if checking to repull previous failed pulls
    if include_failed == True:
        # initialize failed sets
        physio_failed = []
        video_failed = []
        # try to determine if the schedule file failed to be pulled
        try:
            # if the sched file download failed
            if file_dict["schedule"]["pull_log"] == "Download Failed":
                # record that the schedule file failed to be pulled
                update_sched = True
                # update the current schedule file to be the one cached
                #curr_sched = file_dict["schedule"]["file_name"]
                curr_sched = get_schedule_name(id=id, path=path)
        except:
            pass
        # try to determine if the schedule file matches the one in the json cache
        try:
            # or if the system schedule file_name does not match that of the json cache
            if get_schedule_name_system(id=id, path=path) != get_schedule_name(id=id, path=path):
                # record that the schedule file failed to be pulled
                update_sched = True
                # update the current schedule file to be the one cached
                #curr_sched = file_dict["schedule"]["file_name"]
                # update the current schedule file to be the one on the system
                curr_sched = get_schedule_name_system(id=id, path=path)
        except:
            pass
        # try to determine if the physio files failed to be pulled
        try:
            # determine if any of the physio files failed to be pulled, NOTE: old_json_dict has been updated to include new files at this point
            physio_failed_timestamps = [x["file_name"].split("_")[-1].replace(".db", "") for x in file_dict["physio"] if x["pull_log"] == "Download Failed"]
            #print(physio_failed_timestamps)
            physio_failed = [x for x in physio_gdrive if ts_physio(x["title"]) in physio_failed_timestamps]
        except:
            pass
        # try to determine if the video files failed to be pulled
        try:
            # determine if any of the video files failed to be pulled, NOTE: old_json_dict has been updated to include new files at this point
            video_failed_timestamps = [x["file_name"].split("_")[-1].replace(".mp4", "") for x in file_dict["video"] if x["pull_log"] == "Download Failed"]
            #print(video_failed_timestamps)
            video_failed = [x for x in video_gdrive if ts_video(x["title"]) in video_failed_timestamps]
        except:
            pass
        # add these files to the pull_dict
        missed_physio = pull_dict["physio"]
        missed_physio.extend(physio_failed)
        #missed_physio = list(set(missed_physio))
        # get the current list of video to pull
        missed_video = pull_dict["video"]
        # add the files that failed to download
        missed_video.extend(video_failed)
        #print("here")
        #print(len(physio_failed))
        # drop redundant entries
        #missed_video = [x for x in missed_video if x["title"] not in [y["title"] for y in missed_video]] 
        # update the pull_dict
        pull_dict = {"schedule":schedule_pull, "physio":missed_physio, "video":missed_video}
        # if any physio or video files need pulled
        if (len(pull_dict["physio"]) > 0) or (len(pull_dict["video"]) > 0):
            # then update the schedule file
            update_sched = True
        #print("0: " + str(pull_dict["physio"]))

    # if set to explicitly check the local system
    if check_local == True:
        # name of the local schedule file
        local_sched = None
        # try to get the local sched file
        try:
            # check the schedule file in the local system
            local_sched = get_schedule_name_system(id=id, path=path)
            # update the current schedule file to be the one saved locally
            curr_sched = local_sched
        except:
            pass
        # if the local sched is None
        if local_sched == None:
            # then set to pull the schedule file
            update_sched = True

        # set of the local physio files
        local_physio = set()
        # try to get the local physio files
        try:
            # check the physio files in the local system
            local_physio = set(files_on_local(id=id, path=path, file_type="physio"))
        except:
            pass
        # get a set of the physio files not 
        not_local_physio = set()
        # try to determine if the physio files failed to be pulled
        try:
            # determine if any of the physio files failed to be pulled, NOTE: old_json_dict has been updated to include new files at this point
            physio_local_timestamps = [x.split("_")[-1].replace(".db", "") for x in local_physio]
            #print(physio_local_timestamps)
            not_local_physio = [x for x in physio_gdrive if ts_physio(x["title"]) not in physio_local_timestamps]
        except:
            pass

        #not_local_physio = set([x["title"] for x in physio_pull]) - set(local_physio)
        #not_local_physio = [x for x in physio_pull if x["title"] in not_local_physio]

        # set of the local video files
        local_video = set()
        # try to get the local video files
        try:
            # check the video files in the local system
            local_video = set(files_on_local(id=id, path=clinical_path, file_type="video"))
        except:
            pass

        #print("here")
        #print(local_video)

        # get a set of the video files not 
        not_local_video = set()
        # try to determine if the video files failed to be pulled
        try:
            # determine if any of the video files failed to be pulled, NOTE: old_json_dict has been updated to include new files at this point
            video_local_timestamps = [x.split("_")[-1].replace(".mp4", "") for x in local_video]
            #print(video_local_timestamps)
            not_local_video = [x for x in video_gdrive if ts_video(x["title"]) not in video_local_timestamps]
        except:
            pass

        #not_local_video = set([x["title"] for x in video_pull]) - set(local_video)
        #not_local_video = [x for x in video_pull if x["title"] in not_local_video]

        # get the current list of physio to pull
        missed_physio = pull_dict["physio"]
        # add the files that failed to download
        missed_physio.extend(not_local_physio)
        # drop redundant entries
        #missed_physio = [x for x in missed_physio if x["title"] not in [y["title"] for y in missed_physio]]

        # get the current list of video to pull
        missed_video = pull_dict["video"]
        #print("here")
        # add the files that failed to download
        missed_video.extend(not_local_video)
        # drop redundant entries
        #missed_video = [x for x in missed_video if x["title"] not in [y["title"] for y in missed_video]] 

        # update the pull_dict
        pull_dict = {"schedule":schedule_pull, "physio":missed_physio, "video":missed_video}
        
        # if not pulling videos
        if not pull_video:
            pull_dict["video"] = list()

        # if any physio or video files need pulled
        if (len(pull_dict["physio"]) > 0) or (len(pull_dict["video"]) > 0):
            # then update the schedule file
            update_sched = True
            curr_sched = get_schedule_name_system(id=id, path=path)
        #print("1: " + str(len(pull_dict["physio"])))

    # if not pulling videos
    if not pull_video:
        pull_dict["video"] = list()

    # if nothing needs pulled
    if (len(pull_dict["physio"]) == len(pull_dict["video"]) == 0) and (update_sched == False):
        pull_dict = None
    #print("2: " + str(len(pull_dict["video"])))
    # Return the list of files to pull
    return [pull_dict, update_sched, curr_sched]

def download_files(id, path, drive, fileDict, pull_schedule, clinical_path, clinical_url, run_checksum=True): # scheduleLatest, 
    '''
    Method for downloading the files.
    '''
    # if fileDict 
    # initialize the dictionary of logged results for each file
    logDict = dict()
    # initiate a dictionary for storing the video url paths
    videoDict =dict()
    # there are no files to download
    if fileDict == None:
        # Note this to console
        print('No new files to download.')
        # return the dictionary of logs
        return None
    # create a timestamp
    now = datetime.now()
    timestamp = datetime.timestamp(now)
    # Attempt to Download the schedule file NOTE: this may need updated => is the schedule file constantly appended to?
    print('Downloading the updated schedule file from GDrive:')
    # if set to pull the schedule file
    if pull_schedule == True:
        # Move old schedule file to archive if it exists
        #if scheduleLatest != None and os.path.isfile(path + '/' + '/Subjects/' + id + '/schedule/' + scheduleLatest):
        #    shutil.move(path + '/Subjects/' + id + '/schedule/' + scheduleLatest, path + '/' + '/Subjects/' + id + '/schedule/archive/' + scheduleLatest)
        # Move any other residual files in the schedule folder to the schedule file archive
        for file in [x for x in os.listdir(path + '/Subjects/' + id + '/schedule/') if os.path.isfile(path + '/Subjects/' + id + '/schedule/' + x) == True]:
            shutil.move(path + '/Subjects/' + id + '/schedule/' + file, path + '/Subjects/' + id + '/schedule/archive/' + file)
        
        # Download the updated schedule file and add a time stamp and add a time-stamp
        try:
            # get the name and path of the new sched file 
            new_sched = path + '/Subjects/' + id + '/schedule/' + str(timestamp) + '_' + fileDict["schedule"]["title"]
            # Reconnect to Google Drive
            drive = google_drive_connect(id=id, path=path)
            # attempt the download
            fileDict["schedule"].GetContentFile(new_sched)
            # get the downloaded file's checksum
            local_checksum = hashlib.md5(open(new_sched,'rb').read()).hexdigest()
            # get the downloaded GDrive checksum
            gdrive_checksum = fileDict["schedule"]["md5Checksum"]
            # if the checksums match
            if local_checksum == gdrive_checksum:
                # log that the download occured successfully
                logDict.update({new_sched.split("/")[-1]:"Download Successful"}) # {fileDict["schedule"]["title"]
            # otherwise
            else:
                # print to console that the download attempt failed
                print("Warning: download of " + str(fileDict["schedule"]["title"]) + " was unsuccessfull!")
                # log that the download occured unsuccessfully
                logDict.update({new_sched.split("/")[-1]:"Download Failed"}) #{ fileDict["schedule"]["title"]
            
        # if the attempt failed
        except:
            #print(str(fileDict["schedule"]))
            # print to console that the download attempt failed
            print("Warning: download of " + str(fileDict["schedule"]["title"]) + " was unsuccessfull!")
            # log that the download occured unsuccessfully
            logDict.update({new_sched.split("/")[-1]:"Download Failed"}) #{ fileDict["schedule"]["title"]
    
    #print(fileDict["schedule"])
    print('Downloading {} from GDrive'.format(fileDict["schedule"]["title"]))
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
            # Reconnect to Google Drive
            drive = google_drive_connect(id=id, path=path)
            # attempt the download
            file.GetContentFile(path + '/Subjects/' + id + '/physio/' + physio_title)
            # get the downloaded file's checksum
            local_checksum = hashlib.md5(open(path + '/Subjects/' + id + '/physio/' + physio_title,'rb').read()).hexdigest()
            # get the downloaded GDrive checksum
            gdrive_checksum = file["md5Checksum"]
            # if the checksums match
            if local_checksum == gdrive_checksum:
                # log that the download occured successfully
                logDict.update({physio_title: "Download Successful"})
            # otherwise
            else:
                # print to console that the download attempt failed
                print("Warning: download of " + physio_title + " was unsuccessfull!")
                # log that the download occured unsuccessfully
                logDict.update({physio_title: "Download Failed"})
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
        # This will download the files to the local machine.
        # Remove the if/else once decided where videos will be stored.
        if False:
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
        # For now, just create an empty video file with the correct name
        elif False:
            with open(path + '/Subjects/' + id + '/video/' + video_title, 'w') as fp:
                pass
            logDict.update({video_title: "Download Successful"})
        # Test for download of files to OneDrive, create empty files with names
        elif False:
            # Make the directory if it does not exist
            try:
                os.mkdir(clinical_path + '/' + id)
            except:
                pass
            # Make the test file
            with open(clinical_path + '/' + id + '/' + video_title, 'w') as fp:
                pass
            logDict.update({video_title: "Download Successful"})
        # Pull actual video from GDrive to OneDrive
        else:
            # Make the directory if it does not exist
            try:
                os.mkdir(clinical_path + '/' + id)
            except:
                pass
            # attempt to download the file
            try:
                #print(file)
                # Reconnect to Google Drive
                drive = google_drive_connect(id=id, path=path)
                # attempt the download
                file.GetContentFile(clinical_path + '/' + id + '/' + video_title)
                # get the downloaded file's checksum
                local_checksum = hashlib.md5(open(clinical_path + '/' + id + '/' + video_title,'rb').read()).hexdigest()
                # get the downloaded GDrive checksum
                gdrive_checksum = file["md5Checksum"]
                # if the checksums match
                if local_checksum == gdrive_checksum:
                    # log that the download occured successfully
                    logDict.update({video_title: "Download Successful"})
                    videoDict.update({video_title: clinical_url + '/' + id + '/' + video_title})
                # otherwise
                else:
                    # print to console that the download attempt failed
                    print("Warning: download of " + video_title + " was unsuccessfull!")
                    # log that the download occured unsuccessfully
                    logDict.update({video_title: "Download Failed"})
                    videoDict.update({video_title: "NA"})
            # if the attempt failed
            except:
                # print to console that the download attempt failed
                print("Warning: download of " + video_title + " was unsuccessfull!")
                # log that the download occured unsuccessfully
                logDict.update({video_title: "Download Failed"})
                videoDict.update({video_title: "NA"})
    # return the dictionary of logs
    #print(logDict)
    return [logDict, videoDict, get_schedule_name_system(id=id, path=path)]

def files_on_local(id, path, file_type):
    '''
    Method for getting the physio or video files that exist on the local system.
    '''
    db_files = None
    if file_type == "physio":
        # check the system
        try:
            # get all db files in the schedule directory standard to physio
            db_files = [f for f in os.listdir(path + '/Subjects/' + id + '/' + file_type) if f.endswith('.db') and f.startswith(id + '_' + file_type)]
        except:
            pass
    if file_type == "video":
        # check the system
        try:
            # get all mp4 files in the schedule directory standard to video
            db_files = [f for f in os.listdir(path + '/' + id ) if f.endswith('.mp4') and f.startswith(id + '_' + file_type)]
        except:
            pass
    # otherwise, return the file name
    return db_files

def update_json(id, path, clinical_path, fileDict, scheduleLatest, logDict, videoPathDict, check_local=True):
    '''
    Method for updating a subject's json file.
    '''
    #print(fileDict)
    # if there are no new  files were downloaded
    if logDict == {}:
        return
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
        #print(sched_name)
        # use the new schedule file
        old_json_dict["subject"]["files"]["schedule"] = {"file_name": scheduleLatest, "datetime_of_pull": now, "pull_log": logDict[scheduleLatest]} #sched_name
        # get a list of the titles of the physio files
        physioList = [{"file_name": updatePhysioName(x['title']), "datetime_of_pull": now, "pull_log": logDict[updatePhysioName(x['title'])]} for x in fileDict["physio"]]
        # update the physio files
        for item_new in physioList:
            for item_old in old_json_dict["subject"]["files"]["physio"]:
                if item_new["file_name"] == item_old["file_name"]:
                    old_json_dict["subject"]["files"]["physio"].remove(item_old)
            old_json_dict["subject"]["files"]["physio"].append(item_new)
        #print(new_json_dict == old_json_dict)
        # get a list of the titles of the video files
        videoList = [{"file_name": updateVideoName(x['title']), "datetime_of_pull": now, "pull_log": logDict[updateVideoName(x['title'])], "saved_to": videoPathDict[updateVideoName(x['title'])]} for x in fileDict["video"] if(updateVideoName(x['title']) in logDict.keys() and updateVideoName(x['title']) in videoPathDict.keys())]
        # update the video files
        for item_new in videoList:
            for item_old in old_json_dict["subject"]["files"]["video"]:
                if item_new["file_name"] == item_old["file_name"]:
                    old_json_dict["subject"]["files"]["video"].remove(item_old)
            old_json_dict["subject"]["files"]["video"].append(item_new)
        # set the variable out of this scope to feed in for writing
        new_json_dict = old_json_dict
        #print()

    # write to the json file
    with open(path + '/Subjects/' + id + '/subject.json', 'w') as json_file:
        # overwite the file with the new files recorded
        json.dump(new_json_dict, json_file, indent=4)
    #print("here3")

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
    db_files = None
    # check the system
    try:
        # get all db files in the schedule directory
        db_files = [f for f in os.listdir(path + '/Subjects/' + id + '/schedule') if f.endswith('_' + id + '_schedule.db')]
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
    # if unsuccessful
    except:
        # set db_files to something compatible with the return statement
        db_files = [None]
    # if no files exist: usual case on initial pull
    if db_files == []:
        # set db_files to something compatible with the return statement
        db_files = [None]
    # otherwise, return the file name
    return db_files[0]

def pull_files(id, path, clinical_path, clinical_url, tries=5, include_failed=True, check_local=True, pull_all=False, pull_video=True):
    '''
    Method for pulling data from the given Google Drive and updates the subjects json file.
    '''
    #####################################################
    # quick patch to not pull videos for cerrain subjects
    #####################################################
    # get the list of subjects to not pull videos for
    with open(path + "/dont_pull_videos.json", 'r') as f:
        NO_VIDEOS = json.load(f)
    # if this is a subject to not pull videos for
    if id in NO_VIDEOS:
        pull_these_videos = False
        print("Not pulling videos for: " + id)
    else:
        pull_these_videos = pull_video
    print("Pulling Videos for " + str(id) + " : " + str(pull_these_videos))
    #####################################################
    files2pull = True
    pull_schedule = True
    #print(files2pull)
    # counter for number of ties
    try_num = 1
    # if there are files to be downloaded
    while files2pull != None or pull_schedule == True:
        # if the max number of tries have been reached
        if try_num > tries:
            # raise an exception an exit
            raise RuntimeError("Not all files pulled after " + str(tries) + " attempts.")
            # end the program
            exit()
        # print attempt number to console
        print("Attempt #" + str(try_num) + ":")
        # Reconnect to Google Drive
        drive = google_drive_connect(id=id, path=path)
        # Update the files2pull
        query_results = get_pull_files(id=id, path=path, clinical_path=clinical_path, drive=drive, include_failed=include_failed, check_local=check_local, pull_video=pull_these_videos, pull_all=pull_all)
        files2pull = query_results[0]
        #print(files2pull)
        pull_schedule = query_results[1]
        current_schedule = query_results[2]
        #print("scheduleLatest: " + current_schedule)
        # Pull the files
        #print(query_results)
        pull_results = download_files(id=id, path=path, clinical_path=clinical_path, drive=drive, fileDict=files2pull, pull_schedule=pull_schedule, clinical_url=clinical_url)
        print("Pull Overview:")
        print(pull_results)
        if pull_results != None:
            logs = pull_results[0]
            video_paths = pull_results[1]
            current_schedule = pull_results[2]
            # Update the subject's json file
            update_json(id=id, path=path, fileDict=files2pull, scheduleLatest=current_schedule, logDict=logs, clinical_path=clinical_path, videoPathDict=video_paths)
            # get a set of the download results
            dl_results = set(logs.values())
            # if everything was downloaded successfully
            if dl_results == {"Download Successful"}:
                # print to console that the download is complete
                print("Data pull finished.")
                # break from the while loop
                break
            #if all_success:
        #print("Here2")
        # increment counter
        try_num = try_num + 1
