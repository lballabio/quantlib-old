
############################################################
# setenv.py - script to set all of the environment variables
# required to build QuantLibXL
############################################################

import _winreg
import getopt
import sys
import os

PROJ_DIR='C:\\erik\\projects\\'

BRANCH_VALUES = [
    ('LOG4CXX_DIR', 'trunk\\log4cxx'),
    ('GENSRC_DIR', 'branch\\gensrc'),
    ('OBJECT_HANDLER_DIR', 'branch\\ObjectHandler'),
    ('QUANTLIBADDIN_DIR', 'branch\\QuantLibAddin'),
    ('QL_DIR', 'branch\\QuantLib'),
    ]

RELEASE_VALUES = [
    ('LOG4CXX_DIR', 'release\\log4cxx-0.9.7b'),
    ('GENSRC_DIR', 'release\\gensrc-0.1.5'),
    ('OBJECT_HANDLER_DIR', 'release\\ObjectHandler-0.1.5'),
    ('QUANTLIBADDIN_DIR', 'release\\QuantLibAddin-0.3.14'),
    ('QL_DIR', 'release\\QuantLib-0.3.14'),
    ]

TRUNK_VALUES = [
    ('LOG4CXX_DIR', 'trunk\\log4cxx'),
    ('GENSRC_DIR', 'trunk\\gensrc'),
    ('OBJECT_HANDLER_DIR', 'trunk\\ObjectHandler'),
    ('QUANTLIBADDIN_DIR', 'trunk\\QuantLibAddin'),
    ('QL_DIR', 'trunk\\QuantLib'),
    ]

def setKeys(values):
    k = _winreg.OpenKey(_winreg.HKEY_CURRENT_USER, "Environment",
                         0, _winreg.KEY_READ | _winreg.KEY_WRITE)
    for val in values:
        varName, varVal = val
        path = PROJ_DIR + varVal
        if not os.path.exists(path):
            raise Exception, 'nonexistent path : ' + path
        print "key=%s val=%s" % (varName, path)
        _winreg.SetValueEx(k, varName, 0, _winreg.REG_SZ, path)

def usage():
    raise Exception, 'usage: setenv.py -t(trunk)|b(branch)|r(release)'

def main():
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'tbr' )
    except getopt.GetoptError:
        usage()
    for o, a in opts:
        if o == '-t':
            setKeys(TRUNK_VALUES)
        elif o == '-b':
            setKeys(BRANCH_VALUES)
        elif o == '-r':
            setKeys(RELEASE_VALUES)
        else:
            usage()

try:
    main()
except Exception, e:
    print e
    raw_input('press any key to exit')
    sys.exit()

raw_input('press any key to exit')

