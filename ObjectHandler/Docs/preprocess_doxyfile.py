
# Preprocess Doxygen configuration file
# Modify the Doxygen configuration depending on the platform,
# and whether the doxygen output is to be used online:
#  preprocess_doxyfile.py [-o] infile outfile
#        -o is for online html
#        infile = doxygen config file to be preprocessed
#        outfile = modified file

import sys
import os
import getopt
import re
import traceback

USAGE_ERROR = '''
Usage: %(scriptName)s [-o] infile outfile
    Where
        -o is for online html
        infile = doxygen config file to be preprocessed
        outfile = modified file'''

ERROR_HEADER='''
> 
> Fatal error:
>
> >>>>>>>>>> BEGIN STACK TRACE >>>>>>>>>>
'''

ERROR_FOOTER='''
> <<<<<<<<<<  END STACK TRACE  <<<<<<<<<< 
>
> Error message:
%s
>
'''

class PpdException(Exception):

    def __init__(self, text):
        self.text_ = text

    def __str__(self):
        '''Stringify this exception.  This application runs in a makefile
        project under visual studio, which truncates empty lines, we prevent
        this by prefixing a > to each line.'''
        return re.sub('(?m)^', '> ', self.text_)

def usage():
    print USAGE_ERROR % { 'scriptName' : sys.argv[0] }
    sys.exit(1)

def ppd_excepthook(type, value, tb):
    sys.stderr.write(ERROR_HEADER)
    traceback.print_tb(tb, None, sys.stderr)
    sys.stderr.write(ERROR_FOOTER % value)

sys.excepthook = ppd_excepthook

try:
    opts, args = getopt.getopt(sys.argv[1:], 'o')
except getopt.GetoptError:
    usage()

if len(args) != 2:
    usage()

root_dir, ignore = os.path.split(os.getcwd().replace('\\', '/'))

SUBSTITUTIONS = [
    (re.compile(r"(STRIP_FROM_PATH\s*=).*"), r"\1 %s" % root_dir),
    (re.compile(r"(STRIP_FROM_INC_PATH\s*=).*"), r"\1 %s" % root_dir)
]

for o, a in opts:
    if o == '-o':
        SUBSTITUTIONS.extend([
            (re.compile(r"(HTML_HEADER\s*=.*?)header(.*)"), r"\1headeronline\2"),
            (re.compile(r"(HTML_OUTPUT\s*=.*?)html(.*)"), r"\1html-online\2")
        ])

if os.name == 'posix':
    SUBSTITUTIONS.extend([
        (re.compile(r"(HAVE_DOT\s*=).*"), r"\1 YES"),
        (re.compile(r"(GENERATE_HTMLHELP\s*=).*"), r"\1 NO")
    ])
#elif os.name == 'nt':
#    SUBSTITUTIONS.extend([
#    ])

inFileName = args[0]
outFileName = args[1]

if not os.path.exists(inFileName):
    raise PpdException('The input file %s could not be found' % inFileName)

f = open(inFileName, 'r')
buf = f.read()

for sub in SUBSTITUTIONS:
    r, repl = sub
    buf = r.sub(repl, buf)

f = open(outFileName, 'w')
f.write(buf)

