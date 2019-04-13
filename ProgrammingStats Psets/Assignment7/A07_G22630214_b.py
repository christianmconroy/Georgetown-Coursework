# -*- coding: utf-8 -*-
"""
Created on Thu Mar 15 16:28:54 2018

@author: chris
"""

def read_data():
    filename = os.path.join(os.getcwd(), 'ScientificAmerican.txt')
    myList = []
    with open(filename) as f:
        for line in f:
            myList.append(line)
            return myList
        
def process_data(list_of_words):
    myStr = ''
    with open(list_of_words) as f: 
        myStr = f.read()
        print(myStr)
        
def print_count_histogram(some_data_structure):
    with open("ScientificAmerican.txt") as f:
        counts = Counter(myStr.split())
        # Making the actual visual
        width = 120
        longest_key = max(len(key) for key in counts)
        graph_width = width - longest_key - 2
        widest = counts.most_common(1)[0][1]
        scale = graph_width / float(widest)
        for key, size in sorted(counts.items()):
            print('{}: {}'.format(key, int(size * scale) * '*'))   
        
read_data()

process_data(myList)

print_count_histogram(myStr)