#!/usr/bin/python

import common
import utils
import parse
import excel
import c
#import pprint

utils.init()
functionDefs = parse.getFunctionDefs()
#pprint.pprint(functionDefs)
excel.generate(functionDefs)
c.generate(functionDefs[common.FUNCGROUPS])
