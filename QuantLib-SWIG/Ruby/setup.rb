=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email quantlib-dev@lists.sf.net
 The license is also available online at http://quantlib.org/html/license.html

 This program is distributed in the hope that it will be useful, but WITHOUT
 ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 FOR A PARTICULAR PURPOSE.  See the license for more details.
=end

require 'rbconfig'
require 'mkmf'
require 'ftools'

def usage
    puts <<EOU
Usage: ruby setup.rb command [options]
    Commands:
    wrap                                 generate wrappers from SWIG interfaces
    build                                build QuantLib-Ruby
    install [--prefix=path] [--debian]   install QuantLib-Ruby in path
    sdist                                create source distribution
    bdist                                create binary distribution
    clean                                clean up
EOU
    exit
end

# parse command line
cmd = ARGV.shift or usage()
cmd = cmd.downcase
usage() unless ['wrap','build','test','install',
                'sdist','bdist','clean'].member? cmd
if cmd != "install"
    usage() if ARGV.shift
else
    opt = ARGV.shift
    while opt
        if opt[0...9] == "--prefix="
            Prefix = opt[9..-1]
        elsif opt[0...8] == "--debian"
            Debian = true
        else
            usage()
        end
      opt = ARGV.shift
    end
end


# Current QuantLib version
Version = "0.3.7"

# Files
cfg = Config::MAKEFILE_CONFIG
Info     =    [ 'Authors.txt', 'ChangeLog.txt', 'Contributors.txt',
                'LICENSE.TXT', 'README.txt', 'History.txt', 'News.txt',
                'QuantLib-Ruby.spec' ]
Sources  =    [ 'QuantLib.rb', 'quantlib_wrap.cpp' ]
case cfg['host_os']
  when 'darwin'
    Binary = 'QuantLibc.bundle'
  else
    Binary = 'QuantLibc.so'
end

Scripts  =    [ 'setup.rb' ]

Interfaces =  [ 'quantlib.i',
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
                'stochasticprocess.i',
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
                'old_volatility.i' ]

Tests =       [ 'QuantLibTestSuite.rb',
                'dates.rb',
                'instruments.rb',
                'integrals.rb',
                'marketelements.rb',
                'solvers1d.rb',
                'termstructures.rb' ]

Examples =    [ 'american-option.rb',
                'bermudan-swaption.rb',
                'european-option.rb',
                'swap.rb' ]

# commands
class Command
    def initialize(&block)
        @block = block
    end
    def execute
        @block.call
    end
end

Wrap = Command.new {
    swigDir = "./SWIG"
    swigDir = "../SWIG" if not File.exists? swigDir
    puts "Generating Ruby bindings for QuantLib..."
    system "swig -ruby -c++ -I#{swigDir} -o ./quantlib_wrap.cpp quantlib.i"
}

SDist = Command.new {
    puts "Packing source distribution..."
    distDir = "QuantLib-Ruby-#{Version}"
    raise "Directory #{distDir} already exist" if File.exists? distDir
    swigDir = distDir+"/SWIG"
    testDir = distDir+"/test"
    exampleDir = distDir+"/examples"
    [distDir,swigDir,testDir,exampleDir].each { |path| File.makedirs path }
    Info.each       { |file| File.syscopy file, distDir }
    Sources.each    { |file| File.syscopy file, distDir }
    Scripts.each    { |file| File.syscopy file, distDir }
    Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
    Tests.each      { |file| File.syscopy 'test/'+file, testDir }
    Examples.each   { |file| File.syscopy 'examples/'+file, exampleDir }
    cfg = Config::MAKEFILE_CONFIG
    case cfg['host_os']
      when 'mswin32'
        system "zip -q -r #{distDir}.zip #{distDir}/"
      when 'linux','linux-gnu','darwin'
        system "tar cfz #{distDir}.tar.gz #{distDir}/"
      else
        puts "Unknown host: " + cfg['host_os']
    end
}

Build = Command.new {
    puts "Building extension..."
    cfg = Config::MAKEFILE_CONFIG
    case cfg['host_os']
      when 'mswin32'
        QL_DIR = ENV['QL_DIR']
        $CPPFLAGS += " /MD"
        $CPPFLAGS += " /GR"
        $CPPFLAGS += " /GX"
        $CPPFLAGS += " /DNOMINMAX"
        $CPPFLAGS += " /I#{QL_DIR}"
        $LIBPATH  += ["#{QL_DIR}\\lib"]
      when 'linux','linux-gnu','darwin'
        $CFLAGS   += " " + (ENV['CFLAGS'] || "")
        $CPPFLAGS += " " + IO.popen("quantlib-config --cflags").gets.strip
        $CPPFLAGS += " -Wno-uninitialized -Wno-unused-function"
        $CPPFLAGS += " " + (ENV['CXXFLAGS'] || "")
        $libs     += " " + IO.popen("quantlib-config --libs").gets.strip
        old_cc = cfg['CC']
        cfg['CC'] = ENV['CXX'] || "g++"
        cfg['CPP'].sub!(old_cc,cfg['CC'])
        cfg['LDSHARED'].sub!(old_cc,cfg['CC'])
        if cfg['host_os']=='darwin'
          cfg['LDSHARED'].sub!('cc',cfg['CC'])
          cfg['LDSHARED'] += " -flat_namespace -undefined suppress"
        end
      else
        puts "Unknown host: " + cfg['host_os']
    end
    create_makefile("QuantLibc")
    case cfg['host_os']
      when 'mswin32'
        system("nmake")
      else
        system("make")
    end
    # File.safe_unlink "./Makefile"
}

RunTests = Command.new {
    Build.execute
    puts "Testing QuantLib-Ruby..."
    $LOAD_PATH.unshift Dir.pwd
    Dir.chdir 'test'
    load 'QuantLibTestSuite.rb'
    Dir.chdir '..'
    $LOAD_PATH.shift
}

BDist = Command.new {
    Build.execute
    puts "Packing binary distribution..."
    distDir = "QuantLib-Ruby-#{Version}"
    raise "Directory #{distDir} already exist" if File.exists? distDir
    swigDir = distDir+"/SWIG"
    testDir = distDir+"/test"
    [distDir,swigDir,testDir].each { |path| File.makedirs path }
    File.syscopy Binary, distDir
    Info.each       { |file| File.syscopy file, distDir }
    Sources.each    { |file| File.syscopy file, distDir }
    Scripts.each    { |file| File.syscopy file, distDir }
    Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
    Tests.each      { |file| File.syscopy 'test/'+file, testDir }
    system "tar cfz #{distDir}.#{Config::CONFIG['arch']}.tar.gz #{distDir}/"
}

Install = Command.new {
    Build.execute
    puts "Installing QuantLib-Ruby..."
    if defined? Prefix
        # strip old prefix and add the new one
        oldPrefix = Config::CONFIG["prefix"]
        if defined? Debian
          archDir = Config::CONFIG["archdir"]
          libDir = Config::CONFIG["rubylibdir"]
        else
          archDir = Config::CONFIG["sitearchdir"]
          libDir = Config::CONFIG["sitelibdir"]
        end
        archDir    = Prefix + archDir.gsub(/^#{oldPrefix}/,"")
        libDir     = Prefix + libDir.gsub(/^#{oldPrefix}/,"")
    else
        archDir    = Config::CONFIG["sitearchdir"]
        libDir     = Config::CONFIG["sitelibdir"]
    end
    [archDir,libDir].each { |path| File.makedirs path }
    File.install "./"+Binary, archDir+"/"+Binary, 0555, true 
    File.install "./QuantLib.rb", libDir+"/QuantLib.rb", 0555, true
}

Clean = Command.new {
    [Binary,'quantlib_wrap.cpp','quantlib_wrap.o',
     'Makefile','mkmf.log'].each { |file|
      File.safe_unlink file if File.exists? file
    }
}

availableCommands = {
    "wrap"    => Wrap,
    "build"   => Build,
    "test"    => RunTests,
    "install" => Install,
    "sdist"   => SDist,
    "bdist"   => BDist,
    "clean"   => Clean }

availableCommands[cmd].execute

