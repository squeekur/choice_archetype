#---------------------------------------------------------
# April Dawn Kester
# akester@ischool.berkeley.edu
# INFO 232
# August 28, 2014
# research.kester.py
# Final research project
#---------------------------------------------------------

import csv
import sys
import pandas
import math
import numpy
import string

def strip_string(s1):
    s1 = s1.translate(string.maketrans("",""), string.punctuation)
    s1 = s1.replace(" ", "")
    return s1

def damerau_levenshtein_distance(s1, s2):
    s1 = strip_string(s1)
    d = {}
    lenstr1 = len(s1)
    lenstr2 = len(s2)
    for i in xrange(-1,lenstr1+1):
        d[(i,-1)] = i+1
    for j in xrange(-1,lenstr2+1):
        d[(-1,j)] = j+1
    
    for i in xrange(lenstr1):
        for j in xrange(lenstr2):
            if s1[i] == s2[j]:
                cost = 0
            else:
                cost = 1
            d[(i,j)] = min(
                           d[(i-1,j)] + 1, # deletion
                           d[(i,j-1)] + 1, # insertion
                           d[(i-1,j-1)] + cost, # substitution
                           )
            if i and j and s1[i]==s2[j-1] and s1[i-1] == s2[j]:
                d[(i,j)] = min (d[(i,j)], d[i-2,j-2] + cost) # transposition
    
    return d[lenstr1-1,lenstr2-1]

s1 = "1487642"
s2 = "1943248"
s3 = "9728713"

f = pandas.read_csv('beta_clean.csv', usecols=['guess_1'])

f['score_1'] = f.applymap(lambda x: damerau_levenshtein_distance(str(x), s1))

f.to_csv('scores1.csv')




