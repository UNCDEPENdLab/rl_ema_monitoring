import os
import subprocess
import sys
import yaml

# convenience function for updating dictionaries via a keyword arguments
def update_via_kwargs(my_dict, **kwargs):
	# drop any items whose value is None
	no_nones = {x: kwargs[x] for x in kwargs.keys() if kwargs[x] != None}
	# run the update
	my_dict.update(no_nones)
	# return the new dict
	return my_dict

def build_yaml(yaml_name="cfg.yaml", **kwargs):
	'''
	Method to build the yaml file.
	Creates a new files if one does not exist, otherwise, copies over old settings and overwrites if new ones are given.
	This function runs at the root of the repo where it expects the yaml to be read to exist.
	Kwargs is used to dynamically set any extra setting beyond the default options.
	'''
	# instantiate the yaml dict
	yaml_dict = dict()
	# if the yaml file already exists
	if os.path.isfile(yaml_name):
		# load the yaml
		yaml_dict = yaml.load(open(yaml_name, 'r'))
	# dict that containd the default yaml settings
	# NOTE: This places the data in the repo, and it is not recommended to be used this way
	default_yaml = {"root": os.getcwd(), "data": os.getcwd(), "videos": os.getcwd(), "video_url": None, "host": None, "fmri": None, "bids": None}
	# update the default yaml to include the file settings given from the file
	default_yaml.update(yaml_dict)
	yaml_dict = default_yaml
	#print(str(yaml_dict))
	# drop any items whose value is None
	no_nones = {x: kwargs[x] for x in kwargs.keys() if kwargs[x] != None}
	# update the yaml to overwrite if new arguments were given, will not overwrite of the "new setting" is None
	yaml_dict.update(no_nones)
	# convert Nones to empty strings
	#yaml_dict = {x: to_emptystr(yaml_dict[x]) for x in yaml_dict.keys()}
	# convert the dict to yaml
	yaml_dict = yaml.dump(yaml_dict)
	# export to cfg.yaml
	with open(yaml_name, 'w') as f:
		f.write(yaml_dict)