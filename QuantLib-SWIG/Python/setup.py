#!/usr/bin/env python

"""
 Copyright (C) 2000, 2001, 2002 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
"""

# $Id$

import os, sys, string
from distutils.cmd import Command
from distutils.command.install_data import install_data
from distutils.command.install import install
from distutils.core import setup, Extension

docs = ['LICENSE.TXT',
        'Authors.txt',
        'Contributors.txt',
        'History.txt',
        'README.txt']

swig_files = ['../SWIG/common.i',
              '../SWIG/calendars.i',
              '../SWIG/currencies.i',
              '../SWIG/date.i',
              '../SWIG/daycounters.i',
              '../SWIG/distributions.i',
              '../SWIG/null.i',
              '../SWIG/observer.i',
              '../SWIG/ql.i',
              '../SWIG/qlarray.i',
              '../SWIG/randomnumbers.i',
              '../SWIG/string.i',
              '../SWIG/types.i',
              '../SWIG/vector.i']

test_files = ['QuantLib/test/QuantLibTestSuite.py',
              'QuantLib/test/date.py',
              'QuantLib/test/daycounters.py',
              'QuantLib/test/distributions.py']

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
         "a list of suffixes used to generate names the of the testcases")
        ]

    def initialize_options(self):
        self.build_base = 'build'
        # these are decided only after 'build_base' has its final value
        # (unless overridden by the user or client)
        self.test_dir = 'QuantLib/test'
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
        docdir = os.path.join(
            string.split(distro.get_name(), '-')[0], 'doc')
    else:
        predir = 'share'
        docdir = os.path.join('doc', distro.get_name())
    return predir, docdir

# this is to have separate installation folders for SWIG files,
# docs and tests.
class install_swigfiles(install_data):
    description = "installs SWIG files"
    def finalize_options(self):
        global swig_files, predir
        install_data.finalize_options(self)
        predir, _ = get_paths(self.distribution)
        if sys.platform == 'win32':
            moduledir = string.split(self.distribution.get_name(), '-')[0]
        else:
            moduledir = self.distribution.get_name()
        self.data_files = [
            [os.path.join(predir,
                          moduledir,
                          'SWIG'), swig_files],]

class install_testfiles(install_data):
    description = "installs test programs"
    def finalize_options(self):
        global test_files, predir
        install_data.finalize_options(self)
        predir, docdir = get_paths(self.distribution)
        self.data_files = [
            [os.path.join(predir, docdir, 'tests'), test_files],]

class install_docs(install_data):
    description = "installs docs"
    def finalize_options(self):
        global docs, predir
        install_data.finalize_options(self)
        predir, docdir = get_paths(self.distribution)
        self.data_files = [[os.path.join(predir, docdir), docs],]

class my_install(install):
    description = "installs everything"
    def run(self):
        install.run(self)
        self.run_command('install_swigfiles')
        self.run_command('install_testfiles')
        self.run_command('install_docs')

if sys.platform == 'win32':
    try:
        QL_INSTALL_DIR = os.environ['QL_DIR']
    except Exception, e:
        raise 'unable to detect QuantLib installation'
    include_dirs = [QL_INSTALL_DIR]

    if '--compiler=bcpp' in sys.argv:
        library_dirs = [os.path.join(QL_INSTALL_DIR,
                                     'lib', 'win32', 'Borland')]
        libraries = ['QuantLib.lib']
        extra_compile_args = ['-vi-','-w-8057']
        define_macros = [('__WIN32__', None), ('MSC_CORE_BC_EXT', None)]
        extra_link_args = None
    else:
        library_dirs = [os.path.join(QL_INSTALL_DIR,
                                     'lib', 'win32', 'VisualStudio')]
        libraries = None
        extra_compile_args = ['/GR', '/FD']
        define_macros = [('__WIN32__', None), ('WIN32', None),
                         ('NDEBUG', None), ('_WINDOWS', None),
                         ('NOMINMAX', None)]
        extra_link_args = ['/subsystem:windows',
                           '/machine:I386']

    if '--debug' in sys.argv:
        define_macros.append(('QL_DEBUG', None))
    else:
        extra_compile_args.append('/Od')

else:
    from distutils import sysconfig
    ql_prefix = os.popen('quantlib-config --prefix').read()[:-1]
    ql_include_dir = ql_prefix+'/include'
    ql_library_dir = ql_prefix+'/lib'
    ql_compile_args = os.popen('quantlib-config --cflags').read()[:-1]
    ql_link_args = os.popen('quantlib-config --libs').read()[:-1]
    include_dirs = [ql_include_dir,"/usr/local/include","/usr/include"]
    library_dirs = [ql_library_dir]
    libraries = ["QuantLib"]
    extra_compile_args = [ql_compile_args]
    extra_link_args = [ql_link_args]
    define_macros = None
    # changes the compiler from gcc to g++
    save_init_posix = sysconfig._init_posix
    def my_init_posix():
        print 'my_init_posix: changing gcc to g++'
        save_init_posix()
        g = sysconfig._config_vars
        g['CC'] = 'g++'
        g['LDSHARED'] = 'g++ -shared'
    sysconfig._init_posix = my_init_posix

datafiles  = []

setup(name             = "QuantLib-Python",
      description      = "QuantLib Python extension",
      maintainer       = "QuantLib Team",
      maintainer_email = "quantlib-users@lists.sourceforge.net",
      url              = "http://quantlib.org",
      licence          = open('LICENSE.TXT','r+').read(),
      py_modules       = ['QuantLib.QuantLib','QuantLib.defaults'],
      ext_modules      = [Extension("QuantLib.QuantLibc",
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
                          'install_testfiles': install_testfiles,
                          'install_docs': install_docs,
                          'install': my_install},
      version          = "0.3.0c0-cvs")
