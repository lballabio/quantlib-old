
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

ERROR_WRAPPER='''
> 
> gensrc has encountered a fatal error.
>
> >>>>>>>>>> BEGIN STACK TRACE >>>>>>>>>>
%(stack)s
> <<<<<<<<<<  END STACK TRACE  <<<<<<<<<< 
>
> gensrc error:
%(message)s
>
'''

def gensrc_excepthook(type, value, tb):

    # This application runs in a makefile project under Visual Studio, which
    # truncates empty lines, we prevent this by prefixing a > to each line.
    message = re.sub('(?m)^', '> ', str(value))

    # Format the stack trace
    stack = '>'.join(traceback.format_tb(tb))

    sys.stderr.write(ERROR_WRAPPER % {
        'stack' : stack,
        'message' : message })


