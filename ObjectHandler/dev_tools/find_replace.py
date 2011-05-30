
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

# ROOT_DIR - The root folder of the source tree from which you want the
# find/replace to begin.
#ROOT_DIR = 'C:/erik/projects/trunk/QuantLibXL'
ROOT_DIR = '/usr/local/erik/projects/trunk/QuantLibXL'

# CALLBACK FUNCTIONS - Called from regexes which require multiple passes

# Convert case
def toLower(m): return m.group(0).lower()

# Replace pre increment/decrement with post increment/decrement
regex1 = re.compile(r'(\w+?)\+\+')
regex2 = re.compile(r'(\w+?)--')
def callback_example(m):
    x = regex1.sub('++\1', m.group(2))
    x = regex2.sub('--\1', x)
    return m.group(1) + x + ')'

# SUBSTITUTIONS - A list of regexes to be performed.
# Each substitution is in the format
#   (re.compile('find text'), 'replace text'),

SUBSTITUTIONS = (

## Uncomment and modify the examples as required.

##  1) Simple
##  Straight find/replace.
#   (re.compile('aaa'), 'bbb'),

##  2) Group
##  Use parentheses to indicate group(s) in the find text.
##  Use \x in the replace text to refer to a group, where x = group number.
##  Replace text must be a raw string r'' instead of normal string ''.
#   (re.compile('ccc(.*)ccc'), r'ddd\1ddd'),

##  3) Newline flag
##  Use re.S to indicate that . matches newline.
##  This allows you to perform substitutions that span lines.
#   (re.compile('eee.*eee', re.S), 'fff'),

##  4) Multiline flag
##  Use re.M to anchor ^ and $ to begin/end of lines within buffer.
#   (re.compile('^ggg.*ggg$', re.M | re.S), 'hhh'),

##  5) Conversion function
##  Instead of replacement text, provide name of conversion function.
#   (re.compile('abcDEFghi'), toLower),

##  Frequently used
    (re.compile('1_0_1'), '1_1_0'),
    (re.compile('1\.0\.1'), '1.1.0'),
    (re.compile('0x010001'), '0x010100'),
    (re.compile('R010001f0'), 'R010100f0'),
#   (re.compile('0\.10\.0c'), '0.10.0d'),
)

# INCLUDE_FILES
# Regexes to indicate names of files to be processed by the find/replace.
# Leave this list empty to process all files in the directory tree
# except for those excluded by IGNORE_FILES.

INCLUDE_FILES = (

#    re.compile(r'^.+\.[ch]pp$'),

)

# IGNORE_FILES
# Regexes to indicate names of files to be ignored by the find/replace.

IGNORE_FILES = (

    re.compile('^.+\.bmp$'),
    re.compile('^.+\.exe$'),
    re.compile('^.+\.exp$'),
    re.compile('^.+\.ico$'),
    re.compile('^.+\.jpg$'),
    re.compile('^.+\.la$'),
    re.compile('^.+\.lib$'),
    re.compile('^.+\.log$'),
    re.compile('^.+\.ncb$'),
    re.compile('^.+\.o$'),
    re.compile('^.+\.pdf$'),
    re.compile('^.+\.plg$'),
    re.compile('^.+\.png$'),
    re.compile('^.+\.pyc$'),
    re.compile('^.+\.xls$'),
    re.compile('^.+~$'),
    re.compile('^\.'),

    re.compile('^Announce\.txt$'),
    re.compile('^ChangeLog\.txt$'),
    re.compile('^changes\..+$'),
    re.compile('^config\.status$'),
    re.compile('^configure$'),
    re.compile('^design\.docs$'),
    re.compile('^history\.docs$'),
    re.compile('^libtool$'),
    re.compile('^Makefile$'),
    re.compile('^Makefile\.in$'),
    re.compile('^NEWS\.txt$'),
    re.compile('^News\.txt$'),
    re.compile('^objecthandler\.cpp$'),
    re.compile('^ohfunctions\.cpp$'),
    re.compile('^todonando\.txt$'),
)

# IGNORE_DIRS
# Regexes to indicate directories to be ignored by the find/replace.

IGNORE_DIRS = (
    re.compile('^\.'),
    re.compile('^\.svn$'),
    re.compile('^autom4te\.cache$'),
    re.compile('^build$'),
    re.compile('^configure$'),
    re.compile('^dev_tools$'),
    re.compile('^framework$'),
    re.compile('^html$'),
    re.compile('^Launcher$'),
    re.compile('^lib$'),
    re.compile('^log4cxx$'),
    re.compile('^QuantLib$'),
    re.compile('^QuantLib-site$'),
    re.compile('^QuantLib-SWIG$'),
    re.compile('^Workbooks$'),
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

