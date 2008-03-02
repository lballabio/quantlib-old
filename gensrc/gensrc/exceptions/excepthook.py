
"""
 Copyright (C) 2007, 2008 Eric Ehlers

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it
 under the terms of the QuantLib license.  You should have received a
 copy of the license along with this program; if not, please email
 <quantlib-dev@lists.sf.net>. The license is also available online at
 <http://quantlib.org/license.shtml>.

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

import sys
import traceback
import re

ERROR_HEADER='''
> 
> gensrc has encountered a fatal error.
>
> >>>>>>>>>> BEGIN STACK TRACE >>>>>>>>>>
'''

ERROR_FOOTER='''
> <<<<<<<<<<  END STACK TRACE  <<<<<<<<<< 
>
> gensrc error:
%s
>
'''

def gensrc_excepthook(type, value, tb):
    sys.stderr.write(ERROR_HEADER)
    traceback.print_tb(tb, None, sys.stderr)
    # This application runs in a makefile project under visual studio, which
    # truncates empty lines, we prevent this by prefixing a > to each line.
    valueFormatted = re.sub('(?m)^', '> ', str(value))
    sys.stderr.write(ERROR_FOOTER % valueFormatted)

