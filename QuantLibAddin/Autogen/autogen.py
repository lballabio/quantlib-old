#!/usr/bin/python

import common
import utils
import parse
import excel
import pprint

utils.init()
functionLists = parse.getFunctionLists()
pprint.pprint(functionLists)
excel.generate(functionLists)
