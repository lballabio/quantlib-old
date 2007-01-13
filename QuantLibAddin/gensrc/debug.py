import config
import addinexcel
import addincalc
import valueobjects
import utilities
import config
import pdb

# import pdb
# import debug
# pdb.run('debug.debug1()')
# b filename.py:##
# c

def debug1():
    pdb.set_trace()
    config.Config.getInstance().initialize()
    addins = []
    addins.append(utilities.serializeObject(addinexcel.AddinExcel))
    #addins.append(utilities.serializeObject(addincalc.AddinCalc))
    #addins.append(utilities.serializeObject(valueobjects.ValueObjects))
    utilities.generate(addins)

