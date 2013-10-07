# -*- coding: iso-8859-1 -*-
"""
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

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
if sys.version_info.major >= 3:
    from .QuantLib import *
    from .QuantLib import _QuantLib
else:
    from QuantLib import *
    from QuantLib import _QuantLib
del sys

__author__ = 'The QuantLib Group'
__email__ = 'quantlib-users@lists.sourceforge.net'

if hasattr(_QuantLib,'__version__'):
    __version__ = _QuantLib.__version__
elif hasattr(_QuantLib.cvar,'__version__'):
    __version__ = _QuantLib.cvar.__version__
else:
    print('Could not find __version__ attribute')

if hasattr(_QuantLib,'__hexversion__'):
    __hexversion__ = _QuantLib.__hexversion__
elif hasattr(_QuantLib.cvar,'__hexversion__'):
    __hexversion__ = _QuantLib.cvar.__hexversion__
else:
    print('Could not find __hexversion__ attribute')

__license__ = """
COPYRIGHT AND PERMISSION NOTICE

Copyright (c) 2002, 2003 Ferdinando Ametrano
Copyright (c) 2001, 2002, 2003 Nicolas Di Césaré
Copyright (c) 2001, 2002, 2003 Sadruddin Rejeb
Copyright (c) 2000, 2001, 2002, 2003 RiskMap srl
All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, and/or sell copies of the
Software, and to permit persons to whom the Software is furnished to do so,
provided that the above copyright notice(s) and this permission notice appear
in all copies of the Software and that both the above copyright notice(s) and
this permission notice appear in supporting documentation.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT OF THIRD PARTY RIGHTS.
IN NO EVENT SHALL THE COPYRIGHT HOLDER OR HOLDERS INCLUDED IN THIS NOTICE BE
LIABLE FOR ANY CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Except as contained in this notice, the name of a copyright holder shall not
be used in advertising or otherwise to promote the sale, use or other
dealings in this Software without prior written authorization of the
copyright holder.
"""
