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
        'History.txt',
        'README.txt']

swig_files = ['quantlib.i',
              'ql.i',
              'common.i',
              'blackmodel.i',
              'calendars.i',
              'capfloor.i',
              'cashflows.i',
              'compoundforward.i',
              'currencies.i',
              'date.i',
              'daycounters.i',
              'diffusionprocess.i',
              'discountcurve.i',
              'distributions.i',
              'exercise.i',
              'functions.i',
              'history.i',
              'indexes.i',
              'instruments.i',
              'integrals.i',
              'interpolation.i',
              'linearalgebra.i',
              'marketelements.i',
              'montecarlo.i',
              'null.i',
              'observer.i',
              'operators.i',
              'optimizers.i',
              'options.i',
              'payoffs.i',
              'piecewiseflatforward.i',
              'randomnumbers.i',
              'scheduler.i',
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
        ('test-prefix=', None,
         "prefix to the testcase filename"),
        ('test-suffixes=', None,
         "a list of suffixes used to generate names of the testcases")
        ]

    def initialize_options(self):
        self.build_base = 'build'
        # these are decided only after 'build_base' has its final value
        # (unless overridden by the user or client)
        self.test_dir = 'test'
        self.test_prefix = 'QuantLibTestSuite'
        self.test_suffixes = None

    # initialize_options()

    def finalize_options(self):
        import os
        if self.test_suffixes is None:
            self.test_suffixes = []
            pref_len = len(self.test_prefix)
            for file in os.listdir(self.test_dir):
                if (file[-3:] == ".py" and
                    file[:pref_len]==self.test_prefix):
                    self.test_suffixes.append(file[pref_len:-3])

        build = self.get_finalized_command('build')
        self.build_purelib = build.build_purelib
        self.build_platlib = build.build_platlib

    # finalize_options()


    def run(self):
        import sys
        # Invoke the 'build' command to "build" pure Python modules
        # (ie. copy 'em into the build tree)
        self.run_command('build')

        # remember old sys.path to restore it afterwards
        old_path = sys.path[:]

        # extend sys.path
        sys.path.insert(0, self.build_purelib)
        sys.path.insert(0, self.build_platlib)
        sys.path.insert(0, self.test_dir)

        # build include path for test

        for case in self.test_suffixes:
            TEST = __import__(self.test_prefix+case,
                              globals(), locals(),
                              [''])
            try:
                tested_modules = TEST.tested_modules
            except AttributeError:
                tested_modules = None
            else:
                from code_coverage import Coverage
                coverage = Coverage(modules=tested_modules)
                sys.settrace(coverage.trace)

            TEST.test()

            if tested_modules is not None:
                # reload tested modules to get coverage of imports, etc.
                for name in tested_modules:
                    module = sys.modules.get(name)
                    if module:
                        reload(module)

                sys.settrace(None)
                sys.stdout.write("code coverage:\n")
                coverage.write_results(sys.stdout)

        # restore sys.path
        sys.path = old_path[:]

    # run()

# this sets installation paths for win32/unixes
def get_paths(distro):
    if sys.platform == 'win32':
        if sys.hexversion >= 0x02020000:
            predir = 'Lib\site-packages'
        else:
            predir = ''
    else:
        predir = 'include'
    return predir

# this is to have a separate installation folder for SWIG files
class install_swigfiles(install_data):
    description = "install SWIG files"
    def finalize_options(self):
        global predir
        install_data.finalize_options(self)
        predir = get_paths(self.distribution)
        if sys.platform == 'win32':
            swig_install_dir = os.path.join(
                string.split(self.distribution.get_name(), '-')[0],
                'SWIG')
        else:
            swig_install_dir = self.distribution.get_name()
        swig_dir = os.path.join(".","SWIG")
        if not os.path.exists(swig_dir):
            swig_dir = os.path.join("..","SWIG")
        self.data_files = [
            [os.path.join(predir, swig_install_dir),
             [os.path.join(swig_dir,f) for f in swig_files]]]

class my_install(install):
    description = "install everything"
    def run(self):
        install.run(self)
        self.run_command('install_swigfiles')

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
        os.system('swig -python -c++ ' +
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
        extra_compile_args = ['/GR', '/FD', '/Zm150']
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
    define_macros = [ arg[2:] for arg in ql_compile_args
                              if arg.startswith('-D') ]
    include_dirs = [ arg[2:] for arg in ql_compile_args
                             if arg.startswith('-I') ]
    library_dirs = [ arg[2:] for arg in ql_link_args
                             if arg.startswith('-L') ]
    libraries = [ arg[2:] for arg in ql_link_args
                          if arg.startswith('-l') ]
    extra_compile_args = [ arg for arg in ql_compile_args
                               if not arg.startswith('-D')
                               if not arg.startswith('-I') ]
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
      version          = "0.3.7",
      description      = "Python bindings for the QuantLib library",
      long_description = """
QuantLib (http://quantlib.org/) is a C++ library for financial quantitative
analysts and developers, aimed at providing a comprehensive software framework
for quantitative finance.
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
                          'install_swigfiles': install_swigfiles,
                          'install': my_install,
                          'sdist': my_sdist,
                          'wrap': my_wrap}
      )
