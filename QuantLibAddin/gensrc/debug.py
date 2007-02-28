
# import pdb
# import debug
# pdb.run('debug.debug1()')
# b gensrc/Dir/filename.py:##
# c

from gensrc.Addins import addinlist

def debug1():
    addinList = addinlist.AddinList(['e'])
    addinList.generate()

