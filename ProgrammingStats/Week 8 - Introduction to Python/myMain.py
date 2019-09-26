# file name: myMain.py

import myModule

print ("This is myMain.py")

import os
filename = os.path.join(os.getcwd(),'datafile.txt') 
 
myModule.get_input(filename)    
myModule.myMean = getMean(myList)
myModule.mySD = getSD(myList)
myModule.print_output(myList, myMean, mySD)