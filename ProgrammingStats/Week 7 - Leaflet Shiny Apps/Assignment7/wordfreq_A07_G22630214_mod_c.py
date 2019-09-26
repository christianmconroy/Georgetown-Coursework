# -*- coding: utf-8 -*-
"""
Created on Tue Mar 20 22:19:02 2018

@author: chris
"""

def read_data():
    filename = os.path.join(os.getcwd(),'datafile.txt') 
    myList = []
    with open(filename) as f:
    for line in f:
        myList.append(line)
        print(myList)
        
def process_data(list_of_words):
    myStr = ''
    with open(list_of_words) as f: 
        myStr = f.read()
        print(myStr)
        
def print_count_histogram():
    with open(some_data_structure) as f:
        data = f.read()
        counts = create_counter(data)
        proportions = normalise_counts(counts) # if required
        draw_histogram(proportions)