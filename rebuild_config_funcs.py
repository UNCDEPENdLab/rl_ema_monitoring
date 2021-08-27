import argparse
import os
import sys
import json
import yaml
import pathlib

# method to separate a file/folder from it's overall path, returns {file/folder name:path} or [file/folder name, path]
def splitPathName(path, exclude_str=None, list_out=False):
    '''
    Method used to split a path. It will remove the "exclude_str" argument 
    '''
    # initialize pathList variable
    pathList = None
    # windows system hanlde
    if sys.platform == "win32":
        path = path.replace('\\','/')
    # split the path by '/'
    pathList = path.split('/')
    # pop the last element, it will be the file/folder since it is the last item
    fname = pathList.pop()
    # recombine the list to get the path to the file/folder
    pathStr = ""
    # loop through the items in pathList and recombine
    for item in pathList:
        pathStr = pathStr + '/' + item
    # handle for running on a windows operaing system (removes leading '/')
    if sys.platform == "win32":
        pathStr = pathStr[1:]
    # if there is a substring to exclude
    if exclude_str != None:
        # remove the substring
        pathStr = pathStr.remove(exclude_str)
    # if default or False
    if list_out == False:
		# instantiate a dictionary
        result = dict()
		# add the parsed item to the dict
        result[fname] = pathStr
        # return a dict
        return result
    # if set to True 
    elif list_out == True:
        # instantiate a list
        result = list()
        # add the file/folder name
        result.append(fname)
        # add the path
        result.append(pathStr)
        # return a list
        return result

# method that adds a path to the json file
def add_path_to_json(json_dict, file_name, file_path):
    '''
    Adds a single variable if no variable already exists.
    If a filename already exists, creates a list of all paths that contain a file with that name.
    '''
    #print(json_dict.keys())
    # if the json_dict does not contain that file_name
    if file_name not in json_dict.keys():
        #os.system('echo here 0')
        # set the filename as an element of the json with the value being the path
        json_dict[file_name] = file_path
    # if the json_dict already contains that file_name
    else:
        #os.system('echo here 1')
        # if the element is a string
        if isinstance(json_dict[file_name], str):
            #os.system('echo here a')
            # create a list with the old element and new element
            init_list = [json_dict[file_name], file_path]
            # replace the single element with the two-item list
            json_dict[file_name] = init_list
        # if the element is a list
        elif isinstance(json_dict[file_name], list):
            #os.system('echo here b')
            # append to the list
            json_dict[file_name].append(file_path)
    # return the new json dict
    return json_dict

# method to find get the root path by matching file hierarchy with user-based setting
def get_root_from_settings():
    # get the current working directory as a list of strings
    path_str = os.getcwd()
    # get the home directory for the user
    user_home = os.getenv("HOME")
    # get the list of ema projects
    proj_list = [user_home + '/.ema_management/' + proj for proj in os.listdir(user_home + '/.ema_management') if os.path.isdir(user_home + '/.ema_management/' + proj)]
    # get the list of potential projects roots from the project list
    root_list = [json.load(open(proj + '/root.json', 'r'))[0] for proj in proj_list]
    # the correct root path will be a substring of the current working directory
    correct_root = [x for x in root_list if x in path_str]
    # if more than one root is found, or no root is found, then note the error
    if len(correct_root) != 1:
        print("Error: {0} root objects found".format(len(correct_root)))
    else:
        return correct_root[0]

# method that builds a config file
def build_config(rootName=None, rootDir=None, file_name='cfg'):
    '''
    This method builds a config file with the following format
        -> each file will have its path from a given root
        -> this will be a dictionary relationship of 'file_name':'path_from_designated_root'
        -> there will also be a variable for 'root':'designated_root_directory'
        -> there will also be a variable for 'path_to_root':'path_to_designated_root_directory'
    Ultimately, you should be able to get the path to any file in the given directory by the following concatenation:
        path_to_root + root + path_from_root + file_name
    If root_name is None, then the config will be rebuild with the given previously set root, saved as the parent directory's name
    If root_name is not None, then the config will build a config by first renaming the parent directory
    There should be a text file that contains a list of keywords for skipping certain directores:
        These can be as general as 'this_string' or include paths like 'this_dir/this_subdir'
        Similar but not identifcal functionality to a .gitignore file
        Call this file .cfgignore -> each entry should be its own line
    '''
    # get the 
    # TODO: modify this to pull the root dir from the home directory config
    root_dir = get_root_from_settings()
    repo_dir = root_dir
    # if a root dir is not given
    if rootDir != None:
        # then set the root dir to current working directory
        root_dir = rootDir
    # if the config file already exists
    #if os.path.isfile(root_dir + '/' + file_name + '.json'):
    #    # read the json file
    #    old_json = json.load(open(root_dir + '/' + file_name + '.json', 'r'))
    #    # use its set directory at the root_dir
    #    root_dir = old_json["path_to_root"]
    # if the "cfg.yaml" files exists
    if os.path.isfile(repo_dir + '/' + "cfg.yaml"):
        # load the yaml file
        yaml_data = yaml.load(open(repo_dir + '/' + "cfg.yaml", 'r'))
        # if the file name exists in the cfg.yaml
        if file_name in yaml_data.keys():
            #use what it has listed for a root directory
            root_dir = yaml_data[file_name]
    # if the root name does not equal None
    if rootName != None:
        # get the new root name
        root_name = os.path.basename(root_dir)[0] + '/' + rootName
        # rename the current directory
        os.rename(root_dir, root_name)
        # reset root_dir
        root_dir = root_name
        # reset root_name
        root_name = os.path.basename(root_dir)
        # TODO: also make this update the user-specific project settings file
    else:
        # just asign root_name based on root_dir
        root_name = os.path.basename(root_dir)
    # dictionary to be converted to a simple json file
    json_data = {}
    # try to delete the old config
    #try:
    #    os.remove(root_dir + '/' + 'cfg.json')
    #except:
    #    pass
    # get info for the current working directory, this function should only be called in the designated_root_directory
    #root_info = splitPathName(root_dir, list_out=True)
    #root_name = root_info[0]
    #root_path = root_info[1]
    # set the root name
    json_data['root'] = root_name
    # set the path to root
    json_data['path_to_root'] = root_dir
    # load a list of strings used to exclude some files from being saved from .cfgignore
    # initialize the excludeList variable
    excludeList = None
    # if there is an ignore file
    if os.path.isfile(repo_dir + '/' + '.cfgignore'):
        # open .cfgignore
        with open(repo_dir + '/' + '.cfgignore') as exc: # root_dir
            # read the lines into the list
            excludeList = [x.strip() for x in exc.readlines()]
    # iterate through all of the potential dirpaths
    for (dirpath, dirnames, filenames) in os.walk(root_dir):
        # exclusion boolean
        skip_dir = False
        # if there is exclusion criteria
        if excludeList != None:
            # check for exclusion criteria
            for exclude in excludeList:
                # if an exlusion string is found
                if exclude in dirpath:
                    # set the skip boolean to true
                    skip_dir = True
                    break
        # if an exclusion string was not found
        if skip_dir == False:
            # for each filename
            for fname in filenames:
                # handle for windows systems
                if sys.platform == "win32":
                    # replce the '\\' with '/'
                    path = dirpath.replace('\\','/').replace('//','/')
                # otherwise the path is fine
                else:
                    path = dirpath
                # add that file to the json_data dict
                json_data = add_path_to_json(json_data, fname, path)
    # create the file if it does not exist
    #if os.path.isfile(root_dir + '/' +) == False:
    #    with open(root_dir + '/' + 'cfg.json', 'w') as fp:
    #        pass
    # open and dump the json_data dict into the new cfg.json file
    #if file_name == None:
    #    # if a filename is not given, will be set the to base cfg.json file (for inheriting functions)
    #    with open(root_dir + '/' + 'cfg.json', 'w+') as json_file:
    #        json.dump(json_data, json_file, indent=4)
    #else:
    # if a filename is not given, will be set the to base cfg.json file (for inheriting functions)
    with open(repo_dir + '/' + file_name + '.json', 'w+') as json_file:
        json.dump(json_data, json_file, indent=4)

