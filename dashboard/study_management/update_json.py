import json
import os
import sys

'''
General method for updating JSON files within the project
Can be fed into R using the reticulate R package
Reliant on the built-in json python package
Argument of 'elementPath' should be a simply parsed string containing elements names, '[' for lists, and '{' for objects.
All elements are to be comma separated with no white-space.
Should set/get the final element on the 'elementPath'.
Examples: 
    "['thing1'][0]['that3']"
'''

# element setter
def setElement(elementPath, newValue):
    '''
    Writes a line for replacing a varibles.
    elementPath should be a string.
    newValue should be a standard JSON datatype
    '''
    # variable to be converted to a string
    newValueStr = None
    # if the variable is a string
    if isinstance(newValue, str):
        newValueStr = '"{string}"'.format(string=newValue)
    # if the value is the boolean value of True
    elif newValue == True:
        newValueStr = 'true'
    # if the value is the boolean value of False
    elif newValue == False:
        newValueStr = 'false'
    # if the value is an int, float, array(list), or object(dict)
    elif isinstance(newValue,int) or isinstance(newValue,float) or isinstance(newValue,list), or isinstance(newValue,dict):
        newValueStr = str(newValue)
    # if the value is None
    elif newValue == None:
        newValueStr = 'null'
    # return an error if not a standard JSON datatype
    else:
        raise Exception("Error: this value given is not a standard JSON datatype.")
	# create the line of code to add the element
    return 'data{elemPath} = {value}'.format(elemPath=elementPath, value=newValueStr)

# list element append
def appendArrayElement(elementPath, appendValue):
    '''
    Writes a line for appending to a list.
    '''
	# create the line of code to parse to the element
    return 'data{elemPath} = {value}'.format(elemPath=elementPath, value=newValue)

# list element remove
def removeArrayElement(elementPath, removeValue)
    '''
    Writes a line for removing a list element.
    '''
	# create the line of code to parse to the element
    parseLine = 'data'

#class JSON_Query():

class JSON_Mutation():
    '''
    Class for defining and running mutations on a json file.
    '''
    # initializer
    def __init__(self):
        self.mutations = []
    # method for adding mutations
    def addMutation(self, mutateFunc, elemPath, value):
        self.mutations.append((mutateFunc, elemPath, value))
    # creates the expressions into python code based on the list of tuples in mutations
    def createJSONExpressions():
        pass
    # runs the mutations
	def createRunner(self,filePath):
        # create the header text
        header = """import json

def main():
    data = None
    with open({path}) as f:
        data = json.load(f)
""".format(path=filePath)
        # create the expressions for modifying the JSON
        jsonExpressions = createJSONExpressions()
        # dump the new json file
        dumper = """with open({path}, 'w') as json_file:
        json.dump(data, json_file, indent=4)
""".format(path=filePath)
        # add the main method for the script to be run
        main_meth = """if __name__ == '__main__':
    main()
"""
        # create the string containing the text for the code
        code_str = header + jsonExpressions + dumper + main_meth
        # create a random integer for creating a unique runner
        rand_int = 0
        # create the runner file name
        runner_name = "mutation_runner_" + str(rand_int) + ".py"
		# return the name of the runner script
        return runner_name

    # method that actually runs the mutation on the json file.
    def mutate(self, filePath):
        # create the script
        json_runner = createRunner(filePath)
        # execute the script
        os.system("python {runner}".format(runner=json_runner))
        # delete the script
        os.system("rm -rf {runner}".format(runner=json_runner))

# getter for multiple elements (return as a 1X1 wide pandas df with paths being labels)
# NOTE: wait for this method, may be time implement graphene
#def getJsonElements(jsonPath, returnValues):