# file name: myModule.py

def get_input(filename):
    """Reads a file and returns the file contents.

    Args:
        filename: the name of the file including the path
    Returns:
        a list with the file contents
    """
    myList = []
    with open(filename) as f:
        for line in f:
            myList.append(int(line))
    return myList


def getMean(myList):
    """returns the mean of numbers in a list

    Args:
        myList (list): contains numbers for which mean is calculated
    Returns:
        (float) the mean of numbers in myList
    """    
    mysum = 0.0
    for i in range(len(myList)):
        mysum = mysum + myList[i]
    mymean = mysum/len(myList)
    return mymean


def getSD(myList):
    """returns the standard deviation of numbers in a list

    Args:
        myList (list): contains numbers for which standard deviation is calculated
    Returns:
        (float) the standard deviation of numbers in myList
    """    
    import math    
    n = len(myList) 
    mean = sum(myList) / n 
    dev = [x - mean for x in myList] 
    dev2 = [x*x for x in dev] 
    mysd = math.sqrt( sum(dev2) / (n-1)) 
    return mysd


def print_output(myList, mymean, mysd):
    """prints the output

    Args:
        myList (list): contains numbers to be counted
        mymean (float): contains value of mean for numbers in myList
        mysd (list): contains value of standard deviation for numbers in myList       
    """    
    print ("The size of the sample is {:d}".format(len(myList)))
    print ("The sample mean is {:10.2f} ".format(mymean))
    print ("The sample standard deviation is {:<16.2f}".format(mysd))
    
import os

if __name__ = "__main__":
    filename = os.path.join(os.getcwd(),'datafile.txt') 
    myModule.get_input(filename)    
    myModule.myMean = getMean(myList)
    myModule.mySD = getSD(myList)
    myModule.print_output(myList, myMean, mySD)
