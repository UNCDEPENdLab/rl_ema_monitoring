import os
import sys
import pathlib
import json

def build_system():
	# get the home directory 
	home_dir = os.getenv("HOME")
	# get the name of the directory above this one as the project name
	proj_path = pathlib.Path(__file__).parent.resolve()
	proj_name = os.path.basename(proj_path)
	#print(proj_name)
	env_path = home_dir + '/' + '.ema_management' + '/' + proj_name
	# create the hidden directory for this project
	os.makedirs(env_path, exist_ok=True)
	# create a json file that only contains the root dir in a list
	json_data = [str(proj_path)]
	# create the root file
	with open(env_path + '/root.json', "w+") as json_file:
		json.dump(json_data, json_file, indent=4)
