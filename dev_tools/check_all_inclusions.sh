#!/bin/bash

# execute this script from the root QuantLib directory

find ./ql -name *.[ch]pp \
| grep -v '/qldefines\.hpp$' | grep -v '/auto_link\.hpp$' | grep -v 'config' \
| grep -v '/mathconstants\.hpp$' | grep -v '/quantlib\.hpp$' \
| xargs -n 1 ../dev_tools/check_inclusions.py

