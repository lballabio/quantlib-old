#!/usr/bin/python

from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import os # for `diff`

version_tag = ""
version_filelist = ""
version_number_list = [0,0,0,'a',0] # major, minor, minor-minor, greeks, ordinal number, respectively

version_string = []

def initialize(tag, numlist):
    ver = "%d.%d.%d%c%d" % tuple(numlist)
    rver = "%d.%d" % (numlist[0], numlist[1])
    hver = "0x"+tag[1:-1]
    lver = "%d_%d" % (numlist[0], numlist[1])

    varStr = ['$RELEASE_TAG_VERSION', '$RELEASE_VERSION', '$HEX_VERSION', '$RELEASE_LIB_VERSION']
    verStr = [tag[:-1], rver, hver, lver, ver]

    return [varStr, verStr]

def updateVersion(f, lines):
    nlines = len(lines)
    vidx = [[] for i in range(nlines)]
    prefix = [[] for i in range(nlines)]
    suffix = [[] for i in range(nlines)]

    for l in range(nlines):
        for v in range(4):
            var = version_string[0][v]
            pos = lines[l].find(var)
            if pos >= 0:
                vidx[l].append(v)
                prefix[l].append(lines[l][0:pos])
                suffix[l].append(lines[l][pos + len(var):])

    #print(vidx)
    #print(prefix)
    #print(suffix)

    for i in range(len(f)):
        updated = [False, False, False, False]
        for l in range(nlines):
            for k in range(len(vidx[l])):
                if updated[vidx[l][k]]: break
                pidx = f[i].find(prefix[l][k])
                sidx = f[i].find(suffix[l][k], pidx + 1)
                if pidx >= 0 and sidx >= 0:
                    # print("YES %d" % (vidx[l][k]))
                    # print(">> %d %d" % (pidx, sidx))
                    f[i] = f[i][0:pidx]+ prefix[l][k] + version_string[1][vidx[l][k]] + suffix[l][k]
                    updated[vidx[l][k]] = True
    return f

def file_check(fname):
    # e.g. diff ../QuantLib-SWIG/Guile/setup.scm ../test/QuantLib-SWIG/Guile/setup.scm
    os.system('diff ../' + fname + ' ../test/'+ fname)

with open("version_tag.txt","r") as f:
    version_tag = f.read()

with open("version_filelist.txt","r") as f:
    version_filelist = f.read().split('\n')

#parsing
parsing_index = [1,3,5,7,8]
parsing_length = [2,2,2,1,1]
isnum = [True, True, True, False, True];

for i in range(5):
    idx, plen = parsing_index[i], parsing_length[i]
    version_number_list[i] = version_tag[idx:(idx+plen)]
    if isnum[i]:
        version_number_list[i] = int(version_number_list[i])

#print(version_tag)                      # R010300a2
#print(version_number_list)              # [1, 3, 0, 'a', 2]
#print(version_filelist)     # file lists described in version_number.txt

version_string = initialize(version_tag, version_number_list)
print("The current version is %s - %s" % (version_string[1][4], version_tag))

i = 0
while i < len(version_filelist):
    fname = version_filelist[i]
    i = i + 1
    print (fname)
    if len(fname) > 0:
        num_lines = int(version_filelist[i])
        i = i + 1
        fdata = []
        with open('../'+fname,'r') as f:
            fdata = f.read().split('\n')
            updateVersion(fdata, version_filelist[i:i+num_lines])
            i = i + num_lines
        with open('../test/'+fname,'w') as fw:  # generated for test
            for d in fdata:
                fw.write(d+'\n')
        file_check(fname)
