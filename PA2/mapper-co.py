#!/usr/bin/env python
"""mapper.py"""

import sys

# input comes from STDIN (standard input)
for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    # split the line into words
    words = line.split()
    # increase counters
    for i in range(1,len(words)):
        # write the results to STDOUT (standard output);
        # what we output here will be the input for the
        # Reduce step, i.e. the input for reducer.py
        #
        # tab-delimited; the trivial word count is 1
	word_ls = []
	if(len(words[i-1].strip())==0 or len(words[i].strip())==0):
		continue
	else:
		word_ls.append(words[i-1])
		word_ls.append(words[i])
		word_ls.sort()
		word = word_ls[0] + " " + word_ls[1]
		print '%s\t%s' % (word, 1)
        
#https://www.quora.com/How-do-I-execute-Python-mapper-and-reducer-programs-from-a-command-line
