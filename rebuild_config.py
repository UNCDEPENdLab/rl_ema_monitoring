import argparse
import os
import sys
import json

# method to separate a file/folder from it's overall path, returns {file/folder name:path} or [file/folder name, path]
def splitPathName(path, exclude_str=None, list_out=False):
    '''
    Method used to split a path. It will remove the "exclude_str" argument 
    '''
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

# method that builds a config file
def build_config(root_name=None):
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
    # if the root name does not equal 
    if root_name != None:
        # rename the current directory
        os.rename(os.getcwd(), root_name)
    # dictionary to be converted to a simple json file
    json_data = {}
    # try to delete the old config
    try:
        os.remove('cfg.json')
    except:
        pass
    # get info for the current working directory, this function should only be called in the designated_root_directory
    root_info = splitPathName(os.getcwd(), list_out=True)
    root_name = root_info[0]
    root_path = root_info[1]
    # set the root name
    json_data['root'] = root_name
    # set the path to root
    json_data['path_to_root'] = root_path
    # load a list of strings used to exclude some files from being saved from .cfgignore
    # initialize the excludeList variable
    excludeList = None
    # open .cfgignore
    with open('.cfgignore') as exc:
        # read the lines into the list
        excludeList = [x.strip() for x in exc.readlines()]
    # iterate through all of the potential dirpaths
    for (dirpath, dirnames, filenames) in os.walk(os.getcwd()):
        # exclusion boolean
        skip_dir = False
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
                    # add that file to the json_data dict
                    json_data[fname] = dirpath
                # otherwise, the dirpath is already fine
                else:
                    # add that file to the json_data dict
                    json_data[fname] = dirpath
    # open and dump the json_data dict into the new cfg.json file
    with open('cfg.json', 'w') as json_file:
        json.dump(json_data, json_file, indent=4)

def create_arg_parser():
    '''
    Method for creating a parser for input arguments.
    '''
    # Initialize the argparser
    parser = argparse.ArgumentParser(description="Tool for setting cfg.json, should be called from the system directly.")
    # root_name argument
    parser.add_argument('--root_name', help='name of the designated root directory of the project', default = None)
    return parser

def main():
    '''
    Main method for this program.
    '''
    # Create the argparser
    arg_parser = create_arg_parser()
    # Parse the args from terminal input
    parsed_args = arg_parser.parse_args(sys.argv[1:])
    # run the build_config function
    build_config(root_name=parsed_args.root_name)

# Execution of main method
if __name__ == '__main__':
    main()
