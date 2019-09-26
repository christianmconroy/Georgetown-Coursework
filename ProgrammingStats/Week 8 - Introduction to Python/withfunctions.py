# -*- coding: utf-8 -*-


# The 'main' part of the script starts here  
import os

import myfunctions as fns


filename = os.path.join(os.getcwd(),'datafile.txt') 
 
myList = fns.get_input(filename)    
myMean = fns.myfunctions.getMean(myList)
mySD = fns.myfunctions.getSD(myList)
fns.print_output(myList, myMean, mySD)