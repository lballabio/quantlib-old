
# import pdb
# import debug
# pdb.run('debug.debug1()')
# b gensrc/Dir/filename.py:##
# c

import sys
import os

sys.path.append(os.getcwd() + '/code')

from gensrc.addins import addinlist

def debug1():
    addinList = addinlist.AddinList(['x'])
    addinList.generate()

