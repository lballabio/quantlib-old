#!/usr/bin/python

#import pprint
import common
import utils
import parse
import c
import calc
import excel

utils.init()
functionDefs = parse.getFunctionDefs()
#pprint.pprint(functionDefs)
c.generate(functionDefs)
calc.generate(functionDefs)
excel.generate(functionDefs)
