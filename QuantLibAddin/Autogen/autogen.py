#!/usr/bin/python

import common
import utils
import parse
import qla
import c
import calc
import excel

utils.logMessage("begin ...")
utils.init()
functionDefs = parse.getFunctionDefs()
qla.generate(functionDefs)
c.generate(functionDefs)
calc.generate(functionDefs)
excel.generate(functionDefs)
utils.logMessage("end")

