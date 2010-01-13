
##################################################################################
#
# find_replace.py - perform a recursive find/replace on a directory tree
#
# To use this script, first edit it as required (see below) then invoke
# as follows:
#
# find_replace.py -[mode]
# Where [mode] is either of:
#        d - display proposed substitutions
#        s - perform the substitutions
# Plus optionally
#        v - verbose
#
# At a DOS command prompt this script may be invoked as follows:
#
# "C:\Program Files\Python24\python.exe" find_replace.py -s
#
# Settings within this script:
#
# ROOT_DIR
# The root folder of the source tree from which you want the find/replace
# to begin.
#
# SUBSTITUTIONS
# A list of one or more regexes to be performed on each file.
#
# INCLUDE_FILES
# Regexes to indicate names of files to be processed by the find/replace.
#
# IGNORE_FILES
# Regexes to indicate names of files to be ignored by the find/replace.
# NB the script tests first whether the file is to be ignored, then whether it
# is to be included.
#
# IGNORE_DIRS
# Regexes to indicate directories to be ignored by the find/replace.
#
##################################################################################

import sys
import os
import re
import getopt
import shutil

ROOT_DIR = '/usr/local/erik/projects/trunk/QuantLibXL'

# callback functions - called from regexes which require multiple passes

# convert case
def toLower(m): return m.group(0).lower()

# replace pre increment/decrement with post increment/decrement
regex2 = re.compile(r'(\w+?)\+\+')
repl2 = r'++\1'
regex3 = re.compile(r'(\w+?)--')
repl3 = r'--\1'
def sub1(m):
    x = regex2.sub(repl2, m.group(2))
    x = regex3.sub(repl3, x)
    return m.group(1) + x + ')'

# SUBSTITUTIONS - a list of regexes to be performed

SUBSTITUTIONS = (

#   place the active regexes here,
#   using the commented examples below

#    (re.compile(r'aaa'), r'bbb'),

#   simple
#    (re.compile(r'include <Addins/C++'), r'include <Addins/Cpp'),

#   group (\1 requires r'' not '')
#    (re.compile(r"<Rule (.*)>"), r"<Rule \1><SubRules>"),

#   newline flag re.S
#    (re.compile(r"details\.\s.*?'(.*?)'", re.S), ''),

#   multiline (^$) and newline (.)
#    (re.compile(r'xxx^xxx.*xxx$xxx', re.M | re.S), 'yyy'),

#   call a conversion function
#    (re.compile(r'^import .*?$', re.M), toLower),

#   frequently used
    (re.compile(r'1_0_0'), '1_0_1'),
    (re.compile(r'1\.0\.0'), '1.0.1'),
    (re.compile(r'0x010000'), '0x010001'),
    (re.compile(r'R010000f0'), 'R010001f0'),
#    (re.compile(r'Major=1\nMinor=0\nRelease=0'), 'Major=1\nMinor=0\nRelease=1'),
    (re.compile(r'0\.10\.0c'), '0.10.0d'),

)

# INCLUDE_FILES
# Regexes to indicate names of files to be processed by the find/replace.

INCLUDE_FILES = (
#    re.compile(r'^.+\.[ch]pp$'),
)

# IGNORE_FILES
# Regexes to indicate names of files to be ignored by the find/replace.

IGNORE_FILES = (

    re.compile(r'^\.'),
    re.compile(r'^.+\.bmp$'),
    re.compile(r'^.+\.exe$'),
    re.compile(r'^.+\.exp$'),
    re.compile(r'^.+\.ico$'),
    re.compile(r'^.+\.jpg$'),
    re.compile(r'^.+\.la$'),
    re.compile(r'^.+\.lib$'),
    re.compile(r'^.+\.log$'),
    re.compile(r'^.+\.ncb$'),
    re.compile(r'^.+\.o$'),
    re.compile(r'^.+\.pdf$'),
    re.compile(r'^.+\.plg$'),
    re.compile(r'^.+\.png$'),
    re.compile(r'^.+\.pyc$'),
    re.compile(r'^.+\.xls$'),
    re.compile(r'^.+~$'),

    re.compile(r'^Announce\.txt$'),
    re.compile(r'^ChangeLog\.txt$'),
    re.compile(r'^changes\..+$'),
    re.compile(r'^config\.status$'),
    re.compile(r'^configure$'),
    re.compile(r'^design\.docs$'),
    re.compile(r'^history\.docs$'),
    re.compile(r'^libtool$'),
    re.compile(r'^Makefile$'),
    re.compile(r'^Makefile\.in$'),
    re.compile(r'^News\.txt$'),
    re.compile(r'^NEWS\.txt$'),
    re.compile(r'^objecthandler\.cpp$'),
    re.compile(r'^ohfunctions\.cpp$'),
    re.compile(r'^todonando\.txt$'),

)

# IGNORE_DIRS
# Regexes to indicate directories to be ignored by the find/replace.

IGNORE_DIRS = (
    re.compile(r'^\.'),
    re.compile(r'^autom4te\.cache$'),
    #re.compile(r'^bak.*$'),
    re.compile(r'^build$'),
    re.compile(r'^configure$'),
    re.compile(r'^dev_tools$'),
    re.compile(r'^framework$'),
    re.compile(r'^html$'),
    re.compile(r'^Launcher$'),
    re.compile(r'^lib$'),
    re.compile(r'^log4cxx$'),
    re.compile(r'^QuantLib-FpML$'),
    re.compile(r'^QuantLib-site$'),
    re.compile(r'^QuantLib-SWIG$'),
    re.compile(r'^SensitivityAnalysis$'),
    re.compile(r'^\.svn$'),
    re.compile(r'^Workbooks$'),
)

def prompt_exit(msg='', status=0):
    if msg:
        print msg
    #if sys.platform == 'win32':
    #    raw_input('press any key to exit')
    sys.exit(status)

def usage():
    prompt_exit('usage: ' + sys.argv[0] + ' -[mode]' + '''
where [mode] is either of:
        d - display proposed substitutions
        s - perform the substitutions
plus optionally
        v - verbose
''')

def logMessage(msg, priority = 1):
    global logLevel
    if priority <= logLevel:
        print msg

def ignoreItem(item, ignoreList):
    for r in ignoreList:
        if r.match(item):
            return True

def includeItem(item, includeList):
    if len(includeList) == 0: return True
    for r in includeList:
        if r.match(item):
            return True

def processFile(fullPath):
    global execSub
    f = open(fullPath, 'r')
    buf = f.read()
    bufNew = buf
    for sub in SUBSTITUTIONS:
        r, repl = sub
        bufNew = r.sub(repl, bufNew)
    if bufNew == buf:
        logMessage('no sub required in file ' + fullPath)
    else:
        if execSub:
            logMessage('*** overwriting file ' + fullPath, 0)
            f = open(fullPath, 'w')
            f.write(bufNew)
        else:
            logMessage('*** sub required in file ' + fullPath, 0)

def processDir(ignore, dirPath, nameList):
    i = len(nameList) - 1
    while i > -1:
        name = nameList[i]
        fullPath = os.path.join(dirPath, name).replace('\\', '/')
        logMessage('processing path ' + fullPath)
        if os.path.isdir(fullPath):
            logMessage('dir')
            if ignoreItem(name, IGNORE_DIRS):
                logMessage('ignoring directory ' + fullPath)
                del nameList[i]
        elif os.path.isfile(fullPath):
            if ignoreItem(name, IGNORE_FILES):
                logMessage('ignoring file ' + fullPath)
                del nameList[i]
            else:
                logMessage('testing filename ' + name)
                if includeItem(name, INCLUDE_FILES):
                    processFile(fullPath)
        else:
            prompt_exit('unknown file type: ' + fullPath)
        i -= 1

if not os.path.isdir(ROOT_DIR):
    prompt_exit('invalid directory: ' + ROOT_DIR)

try:
    opts, args = getopt.getopt(sys.argv[1:], 'dsvh', 'help' )
except getopt.GetoptError:
    usage()

logLevel = 0
execSub = -1
for o, a in opts:
    if o in ('-h', '--help'):
        usage()
    elif o == '-d':
        execSub = 0
    elif o == '-s':
        execSub = 1
    elif o == '-v':
        logLevel = 1
if execSub == -1:
    usage()

os.path.walk(ROOT_DIR, processDir, None)
prompt_exit()

