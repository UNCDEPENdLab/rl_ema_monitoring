import os

def rebuild_from_yaml():
    os.system("conda env create -n conda-env -f /path/to/environment.yml")
'''
def main():
    '''
    #Main method for this program.
    '''
    # Create the argparser
    arg_parser = create_arg_parser()
    # Parse the args from terminal input
    parsed_args = arg_parser.parse_args(sys.argv[1:])
    # run the build_config function
    build_config(root_name=parsed_args.root_name)
'''

# Execution of main method
if __name__ == '__main__':
    #main()
    def rebuild_from_yaml()