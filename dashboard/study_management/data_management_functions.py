import fnmatch
import json
import yaml
import os
import pandas as pd

# method to extract the path of a file from a json file setup as -> file:path
def getPathFromCfg_p(root_dir, cfg_name='cfg.json', sourced_file=None, keywords=None, exclusion=None, pattern=False, findRootDir=True):
	'''
	sourced_file will be used as a pattern or as an exact name based on the pattern boolean
	keywords will drop an item if the files found do not include them
	exclusion will drop an item if the files found do include them
	The goal of this function is to grab all of the paths or use the exclusion and keywords arguments to
	  reduce our output size. 
	'''
	# if findRootDir is true, then we look for the root directory mathcing the argument for "root_dir"
	root_path = None
	if(findRootDir == True):
		# get the root directory path
		root_path = findRoot_p(root_dir=root_dir)
	else: # otherwise, treat the root dir given as an absolute path
		root_path = root_dir
	# get the whole path to cfg.json
	cfg_path = root_path + '/' + cfg_name
	# variable to assign the path to
	file_data = None
	# open the cfg.json file
	with open(cfg_path, "r") as file:
		# read in the file
		file_content = file.read()
		# convert to a python object
		file_data = json.loads(file_content)
	# initialize variable for file path
	f_path = []
	keys = None
	# if the file name is not a pattern (should be used as the file name)
	if (pattern == False):
		# get the item the python dict
		#f_path = file_data[sourced_file]
		keys = [sourced_file]
		# if the string is a pattern
	elif (pattern == True):
		# get the list of keys from the json file
		keys = file_data.keys()
		# convert the list of keys to a list
		keys = list(keys)
		# reduce the list of keys (file names) that contain the sourced_file argument as a substring
		keys = fnmatch.filter(keys, "*" + sourced_file + "*")
		'''
		# initialize the final result of this inner method
		f_path = []
		# use the remaining keys to select the paths from the json, iterate through the keys
		for key in keys:
			# append the key to the character string
			f_path.append(file_data[key] + '/' + key)
		'''
	# get the list of file paths from the supplied keys
	for key in keys:
		# if the value is a single path (unique file)
		if type(file_data[key]) is str:
			# append the key to the character string
			f_path.append(file_data[key] + '/' + key)
		# if the value is a list of paths (not unique file)
		elif type(file_data[key]) is list:
			# get the list of file paths
			key_paths = [x + '/' + key for x in file_data[key]]
			# extend the list
			f_path.extend(key_paths)
	# if the file_path returned is a single string
	if (type(f_path) is str):
		# set the file_path to this single returned string
		f_path = [f_path]
	# if the file_path returned is a list -> should be forced into a list at this point
	if (type(f_path) is list):
		# if there are keyword selectors
		if (keywords != None):
			# iterate through the keywords given
			for kword in keywords:
				# drop the paths that do not contain the keyword
				f_path = fnmatch.filter(f_path, "*" + kword + "*")
		# if there are exclusion substrings
		if (exclusion != None):
			# iterate through the keywords given
			for exclude in exclusion:
				# get the items to drop
				to_drop = fnmatch.filter(f_path, "*" + exclude + "*")
				# run a set different
				set_diff = set(f_path) - set(to_drop)
				# cast back to a list
				f_path = list(set_diff)
	# ensure there is only a string returned
	#if ((len(f_path) > 1) or (len(f_path) < 1) or (f_path == None)):
	if ((len(f_path) < 1) or (f_path == None)):
		print("Error: From the filename given, keywords selected, and exclusion substrings; either no paths or multiple paths were returned.")
		if f_path == None:
			f_path = "None"
		print(str(f_path))
		return None
	# return the resultant file path
	if ((len(f_path) == 1) and type(f_path) is list):
		return f_path[0] #file_path
	# otherwise
	return f_path

# method to climb up the file hierarchy until the designated root input to this function is found
def findRoot_p(root_dir, start_from=os.getcwd()):
	# get the current working directory
	curr_path = start_from
	# split the cwd into a list of strings
	path_list = curr_path.split('/')
	# check that the root_dir is in that list
	if root_dir in path_list:
		# get the index of the root_dir
		root_idx = path_list.index(root_dir) + 1 # incremented by 1 to be inclusive of itself during list slice
		# remove anything after the root_dir
		path_list = path_list[0:root_idx]
		# concatenate back into a string
		root_path = '/'.join(path_list)
		# return the string
		return root_path
	# otherwise, log that the root_dir is not a parent
	else:
		print("Root directory given '{root_dir}' is not a parent of the current directory '{curr_path}'.")

# method that scans upwards to find a specified anchor file, by default, searches up from current directory
def findAnchor_p(anchor_file="cfg.yaml", start_from=os.getcwd()):
	# split the directory path into a list
	split_str = start_from.split('/')
	# iterate up through the file directory
	while (len(split_str) > 0):
		# get the current path
		curr_dir = '/'.join(split_str)
		# get the list of files at the current path
		curr_dir_files = [file for file in os.listdir(curr_dir) if os.path.isfile(os.path.join(curr_dir, file))]
		# if the anchor file was found, return it's full path
		if anchor_file in curr_dir_files:
	  		return curr_dir + '/' + anchor_file
		# otherwise, pop the last element of the directory list
		split_str.pop(-1)
	# if the anchor file was not found, return NULL
	return None

def get_cfg_var_p(cfg="cfg.yaml", start_from=os.getcwd(), var=None):
	# get the path to the cfg.yaml file
	yaml_path = findAnchor_p(anchor_file=cfg, start_from=start_from)
	#print(yaml_path)
	# read the cfg.yaml data into python
	yaml_data = yaml.load(open(yaml_path, 'r'))
	# get the specific variable
	ret_val = yaml_data[var]
	# return the selected variable
	return ret_val

def get_data_info(sid, data_type, keywords=[], exclusion=[], data_cfg="subject.json", cfg_name="data.json", pattern=True, as_str=False):
	# get the root from the cfg file
	root = os.path.basename(get_cfg_var_p(var="root"))
	#print(root)
	# add sid to keywords
	kwords = keywords
	kwords.append(sid)
	# add "archive" to exclusion
	exclude = exclusion
	exclude.append("archive")
	#print(str(kwords))
	#print(str(exclude))
	# get the path to the subject.json file
	json_path = getPathFromCfg_p(root_dir=root, cfg_name=cfg_name, sourced_file=data_cfg, keywords=kwords, exclusion=exclude, pattern=pattern)
	#print(str(json_path))
	# load in the json file
	json_file = json.load(open(json_path, 'r'))
	# get the files for the data
	files = json_file["subject"]["files"][data_type]
	# create a 1 row dataframe
	column_names = ["subject_id", "file_name", "file_path", "pull_time", "pull_log", "status", "cache_log"]
	data_df = pd.DataFrame(columns = column_names)
	#print(str(type(files)))
	if type(files) is dict:
		# get specific json information
		file_name = files["file_name"]
		pull_time = files["datetime_of_pull"]
		pull_log = files["pull_log"]
		status = json_file["subject"]["status"]
		# get the file path
		file_path = getPathFromCfg_p(root_dir=root, cfg_name=cfg_name, sourced_file=file_name, keywords=kwords, exclusion=exclude, pattern=False)
		# log that the file exists as a boolean
		cache_log = os.path.isfile(file_path)
		# create a 1 row dataframe
		data_df = pd.DataFrame({"subject_id": [str(sid)], "file_name": [file_name], "file_path": [file_path], "pull_time": [pull_time], "pull_log": [pull_log], "status": [status], "cache_log": [cache_log]}) #, last_cached=format(mtime, "%d%b%Y-%H%M%S")
	# if the returned item is a list
	elif type(files) is list:
		# while more than 1 file exists
		for file in files:
			# get specific json information
			file_name = file["file_name"]
			pull_time = file["datetime_of_pull"]
			pull_log = file["pull_log"]
			status = json_file["subject"]["status"]
			# get the file path
			try:
				file_path = getPathFromCfg_p(root_dir=root, cfg_name=cfg_name, sourced_file=file_name, keywords=kwords, exclusion=exclude, pattern=False)
			except:
				file_path = ""
			# log that the file exists as a boolean
			if file_path != "": 
				cache_log = os.path.isfile(file_path)
			else:
				cache_log = False
			# create a 1 row dataframe
			data_df_temp = pd.DataFrame({"subject_id": [str(sid)], "file_name": [file_name], "file_path": [file_path], "pull_time": [pull_time], "pull_log": [pull_log], "status": [status], "cache_log": [cache_log]}) #, last_cached=format(mtime, "%d%b%Y-%H%M%S")
			# merge the new row into the whole dataframe
			data_df = data_df.append(data_df_temp)
	# if set to return as csv string
	if as_str == True:
		return data_df.to_csv(index=False)
	# return the data as a raw pandas dataframe
	return data_df

# method to get a subject's status
def get_subject_status():
	pass

# method to get a get the list of active subjects
def getActiveList_p():
	pass

'''
# method to get the ema metadata across subjects
get_ema_subject_metadata_p <- function(root_dir=NULL, subject_list=NULL, data_dir=NULL, trigger_refresh=FALSE)  {
#checkmate::assert_directory_exists(data_dir)
checkmate::assert_logical(trigger_refresh)

if (isTRUE(trigger_refresh)) { refresh_ema_cache(data_dir) }

#folders <- list.dirs(path=data_dir, full.names=TRUE, recursive=FALSE)

#something along the lines of ...
sched_list <- list()
physio_list <- list()
video_list <- list()

# get a list of all participants
if(is.null(subject_list)){
subject_list <- getActiveList(root_dir=root_dir)
}

#for (ff in folders) {
for (sid in subject_list) {
print(sid)
#sid <- basename(ff)
#figure out schedule stuff
print("running schedule...")
sched_list[[sid]] <- get_schedule_info(sid=sid) #return a one-row data.frame summarizing status of schedule # , data_dir
print("running physio...")
physio_list[[sid]] <- get_physio_info(sid=sid) #return multi-row data.frame, one row per subject physio file # , data_dir
print("running video...")
video_list[[sid]] <- get_video_info(sid=sid) #return multi-row data.frame, one row per subject video file # , data_dir
}

sched_df <- bind_rows(sched_list)
# something like
# subject_id  subject_folder                                                      last_cached   active   cache_failure
# 9001        /projects/rl_ema_monitoring/Subjects/9001/schedule/9001_schedule.db    2Feb2021     TRUE           FALSE
# 9002        /projects/rl_ema_monitoring/Subjects/9002/schedule/9002_schedule.db    2Feb2021    FALSE           FALSE

physio_df <- bind_rows(physio_list)
#something like (if forget: do we get one file per recording? If so, we'd have multiple rows per sub)
# subject_id                                        physio_file    last_cached  active
#       9001   /abspath/Subjects/9001/physio/somethingphysio.db        2Feb2021   TRUE
#       9001   /abspath/Subjects/9001/physio/somethingphysio2.db       2Feb2021   TRUE
#       9002   /abspath/Subjects/9002/physio/somethingphysio.db        2Feb2021   FALSE

video_df <- bind_rows(video_list)
#something like
# subject_id                             video_file     last_cached  active
#       9001   /abspath/Subjects/9001/video/day1.mp4       2Feb2021   TRUE
#       9001   /abspath/Subjects/9001/video/day2.mp4       2Feb2021   TRUE
#       9002   /abspath/Subjects/9002/video/day1.mp4       2Feb2021  FALSE

return(list(schedule=sched_df, physio=physio_df, video=video_df))
}
'''
