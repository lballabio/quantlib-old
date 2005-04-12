# -*- coding: iso-8859-1 -*-
"""
 Copyright (C) 2000-2004 StatPro Italia srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

import os, sys, string
from distutils.cmd import Command
from distutils.command.install_data import install_data
from distutils.command.install import install
from distutils.command.sdist import sdist
from distutils.file_util import copy_file
from distutils.core import setup, Extension

docs = ['LICENSE.TXT',
        'Authors.txt',
        'Contributors.txt',
        'News.txt',
        'README.txt']

swig_files = ['quantlib.i',
              'ql.i',
              'common.i',
              'blackmodel.i',
              'bonds.i',
              'calendars.i',
              'capfloor.i',
              'cashflows.i',
              'compoundforward.i',
              'currencies.i',
              'date.i',
              'daycounters.i',
              'stochasticprocess.i',
              'discountcurve.i',
              'distributions.i',
              'exchangerates.i',
              'exercise.i',
              'functions.i',
              'grid.i',
              'history.i',
              'indexes.i',
              'instruments.i',
              'integrals.i',
              'interestrate.i',
              'interpolation.i',
              'linearalgebra.i',
              'marketelements.i',
              'money.i',
              'montecarlo.i',
              'null.i',
              'observer.i',
              'operators.i',
              'optimizers.i',
              'options.i',
              'payoffs.i',
              'piecewiseflatforward.i',
              'randomnumbers.i',
              'rounding.i',
              'scheduler.i',
              'settings.i',
              'shortratemodels.i',
              'statistics.i',
              'swap.i',
              'swaption.i',
              'termstructures.i',
              'timebasket.i',
              'types.i',
              'vectors.i',
              'volatilities.i',
              # to be removed
              'old_pricers.i',
              'old_volatility.i']

test_files = ['QuantLibTestSuite.py',
              'date.py',
              'instruments.py',
              'integrals.py',
              'marketelements.py',
              'solvers1d.py',
              'termstructures.py']

class test(Command):
    # Original version of this class posted
    # by Berthold Höllmann to distutils-sig@python.org
    description = "test the distribution prior to install"

    user_options = [
        ('test-dir=', None,
         "directory that contains the test definitions"),
        ]

    def initialize_options(self):
        self.build_base = 'build'
        self.test_dir = 'test'

    def finalize_options(self):
        build = self.get_finalized_command('build')
        self.build_purelib = build.build_purelib
        self.build_platlib = build.build_platlib

    def run(self):
        import sys
        # Testing depends on the module having been built
        self.run_command('build')

        # extend sys.path
        old_path = sys.path[:]
        sys.path.insert(0, self.build_purelib)
        sys.path.insert(0, self.build_platlib)
        sys.path.insert(0, self.test_dir)

        # import and run test-suite
        module = __import__('QuantLibTestSuite', globals(), locals(), [''])
        module.test()

        # restore sys.path
        sys.path = old_path[:]


# This gathers the SWIG interface files before running sdist
class my_sdist(sdist):
    description = "build source distribution including SWIG interfaces"
    def run(self):
        swig_dir = os.path.join(".","SWIG")
        cleanup = 0
        if not os.path.exists(swig_dir):
            os.makedirs(swig_dir)
            for f in swig_files:
                copy_file(os.path.join("..","SWIG",f),swig_dir)
            cleanup = 1
        # now do what you do
        sdist.run(self)
        # clean up
        if cleanup:
            for f in os.listdir(swig_dir):
                os.remove(os.path.join(swig_dir,f))
            os.rmdir(swig_dir)

class my_wrap(Command):
    description = "generate Python wrappers"
    user_options = []
    def initialize_options(self): pass
    def finalize_options(self): pass
    def run(self):
        print 'Generating Python bindings for QuantLib...'
        swig_dir = os.path.join(".","SWIG")
        if not os.path.exists(swig_dir):
            swig_dir = os.path.join("..","SWIG")
        os.system('swig -python -c++ -modern ' +
                  '-I%s ' % swig_dir +
                  '-outdir QuantLib -o QuantLib/quantlib_wrap.cpp ' +
                  'quantlib.i')


if sys.platform == 'win32':
    try:
        QL_INSTALL_DIR = os.environ['QL_DIR']
    except Exception, e:
        raise 'unable to detect QuantLib installation'
    include_dirs = [QL_INSTALL_DIR]
    library_dirs = [os.path.join(QL_INSTALL_DIR, 'lib')]
    libraries = None

    if '--compiler=bcpp' in sys.argv:
        extra_compile_args = ['-vi-','-w-8057']
        define_macros = [('__WIN32__', None), ('MSC_CORE_BC_EXT', None)]
        extra_link_args = None
    else:
        extra_compile_args = ['/GR', '/FD', '/Zm250']
        define_macros = [('__WIN32__', None), ('WIN32', None),
                         ('NDEBUG', None), ('_WINDOWS', None),
                         ('NOMINMAX', None)]
        extra_link_args = ['/subsystem:windows',
                           '/machine:I386']

        if "--dll" in sys.argv:
            use_dll = 1
            for i in range(len(sys.argv)):
                if sys.argv[i] == "--dll":
                    del sys.argv[i]
                    break
        else:
            use_dll = 0

        if '--debug' in sys.argv:
            if use_dll:
                extra_compile_args.append('/MDd')
            else:
                extra_compile_args.append('/MTd')
        else:
            if use_dll:
                extra_compile_args.append('/MD')
            else:
                extra_compile_args.append('/MT')

else:
    from distutils import sysconfig
    ql_compile_args = os.popen('quantlib-config --cflags').read()[:-1].split()
    ql_link_args = os.popen('quantlib-config --libs').read()[:-1].split()
    define_macros = [ (arg[2:],None) for arg in ql_compile_args
                                     if arg.startswith('-D') ]
    include_dirs = [ arg[2:] for arg in ql_compile_args
                             if arg.startswith('-I') ]
    library_dirs = [ arg[2:] for arg in ql_link_args
                             if arg.startswith('-L') ]
    libraries = [ arg[2:] for arg in ql_link_args
                          if arg.startswith('-l') ]
    extra_compile_args = [ arg for arg in ql_compile_args
                               if not arg.startswith('-D')
                               if not arg.startswith('-I') ] \
                       + [ '-Wno-unused' ]
    if os.environ.has_key('CXXFLAGS'):
        extra_compile_args.extend(string.split(os.environ['CXXFLAGS']))
    extra_link_args = [ arg for arg in ql_link_args
                            if not arg.startswith('-L')
                            if not arg.startswith('-l') ]
    # changes the compiler from gcc to g++
    save_init_posix = sysconfig._init_posix
    def my_init_posix():
        save_init_posix()
        g = sysconfig._config_vars
        if os.environ.has_key('CXX'):
            g['CC'] = os.environ['CXX']
        else:
            g['CC'] = 'g++'
        if sys.platform.startswith("darwin"):
            g['LDSHARED'] = g['CC'] + \
                            ' -bundle -flat_namespace -undefined suppress'
        else:
            g['LDSHARED'] = g['CC'] + ' -shared'
    sysconfig._init_posix = my_init_posix

datafiles  = []

# patch distutils if it can't cope with the "classifiers" or
# "download_url" keywords
if sys.version < '2.2.3':
    from distutils.dist import DistributionMetadata
    DistributionMetadata.classifiers = None
    DistributionMetadata.download_url = None

classifiers = [
    'Development Status :: 4 - Beta',
    'Environment :: Console',
    'Intended Audience :: Developers',
    'Intended Audience :: End Users/Desktop',
    'License :: OSI Approved :: BSD License',
    'Natural Language :: English',
    'Operating System :: OS Independent',
    'Programming Language :: Python',
    'Topic :: Scientific/Engineering',
]

setup(name             = "QuantLib-Python",
      version          = "0.3.10",
      description      = "Python bindings for the QuantLib library",
      long_description = """
QuantLib (http://quantlib.org/) is a C++ library for financial quantitative
analysts and developers, aimed at providing a comprehensive software
framework for quantitative finance.
      """,
      author           = "QuantLib Team",
      author_email     = "quantlib-users@lists.sourceforge.net",
      url              = "http://quantlib.org",
      license          = open('LICENSE.TXT','r+').read(),
      classifiers      = classifiers,
      py_modules       = ['QuantLib.__init__','QuantLib.QuantLib'],
      ext_modules      = [Extension("QuantLib._QuantLib",
                                    ["QuantLib/quantlib_wrap.cpp"],
                                    libraries = libraries,
                                    define_macros = define_macros,
                                    include_dirs = include_dirs,
                                    library_dirs = library_dirs,
                                    extra_compile_args = extra_compile_args,
                                    extra_link_args = extra_link_args)
                         ],
      data_files       = datafiles,
      cmdclass         = {'test': test,
                          'sdist': my_sdist,
                          'wrap': my_wrap}
      )
