=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl

 This file is part of QuantLib, a free-software/open-source library
 for financial quantitative analysts and developers - http://quantlib.org/

 QuantLib is free software: you can redistribute it and/or modify it under the
 terms of the QuantLib license.  You should have received a copy of the
 license along with this program; if not, please email ferdinando@ametrano.net
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
    wrap                      generate wrappers from SWIG interfaces
    build                     build QuantLib-Ruby
    install [--prefix=path]   install QuantLib-Ruby in path
    sdist                     create source distribution
    bdist                     create binary distribution
EOU
    exit
end

# parse command line
cmd = ARGV.shift or usage()
cmd.downcase!
usage() unless ['wrap','build','test','install','sdist','bdist'].member? cmd
if cmd != "install"
	usage() if ARGV.shift
else
	opt = ARGV.shift
	if opt
		if opt[0...9] == "--prefix="
			Prefix = opt[9..-1]
		else
			usage()
		end
    else
        Prefix = nil
	end
end


# Current QuantLib version
Version = "0.3.2a2-cvs"

# Files
Info     =    [ 'Authors.txt', 'ChangeLog.txt', 'Contributors.txt',
                'LICENSE.TXT', 'README.txt', 'History.txt', 'News.txt' ]
Sources  =    [ 'QuantLib.rb', 'quantlib_wrap.cpp' ]
Binaries =    [ 'QuantLibc.so' ]
Scripts  =    [ 'setup.rb' ]

Interfaces =  [ 'quantlib.i',
                'ql.i',
                'common.i',
                'blackmodel.i',
                'calendars.i',
                'capfloor.i',
                'currencies.i',
                'date.i',
                'daycounters.i',
                'discountcurve.i',
                'distributions.i',
                'functions.i',
                'history.i',
                'indexes.i',
                'instruments.i',
                'interpolation.i',
                'linearalgebra.i',
                'marketelements.i',
                'montecarlo.i',
                'null.i',
                'observer.i',
                'operators.i',
                'optimizers.i',
                'options.i',
                'piecewiseflatforward.i',
                'randomnumbers.i',
                'riskstatistics.i',
                'scheduler.i',
                'segmentintegral.i',
                'gaussianstatistics.i',
                'swap.i',
                'swaption.i',
                'termstructures.i',
                'types.i',
                'vectors.i',
                'volatilities.i',
                # to be removed
                'old_pricers.i',
                'old_volatility.i']

Tests =       [ 'QuantLibTestSuite.rb',
                'calendars.rb',
                'capfloor.rb',
                'covariance.rb',
                'dates.rb',
                'daycounters.rb',
                'distributions.rb',
                'europeanoption.rb',
                'instruments.rb',
                'marketelements.rb',
                'operators.rb',
                'piecewiseflatforward.rb',
                'riskstatistics.rb',
                'segmentintegral.rb',
                'simpleswap.rb',
                'solvers1d.rb',
                'gaussianstatistics.rb',
                'swaption.rb',
                'termstructures.rb',
                # to be removed
                'old_pricers.rb']

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
	[distDir,swigDir,testDir].each { |path| File.makedirs path }
	Info.each       { |file| File.syscopy file, distDir }
	Sources.each    { |file| File.syscopy file, distDir }
	Scripts.each    { |file| File.syscopy file, distDir }
	Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
	Tests.each      { |file| File.syscopy 'test/'+file, testDir }
	cfg = Config::MAKEFILE_CONFIG
	case cfg['host_os']
	  when 'mswin32'
    	system "zip -q -r #{distDir}.zip #{distDir}/"
	  when 'linux'
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
	    $LIBPATH  += ["#{QL_DIR}\\lib\\Win32\\VisualStudio"]
	  when 'linux'
        $CFLAGS   += ENV['CFLAGS'] || ""
    	$CFLAGS   += " -DHAVE_CONFIG_H"
        $CPPFLAGS += ENV['CXXFLAGS'] || ""
    	$CPPFLAGS += " -I/usr/local/include"
    	$libs     += " -lQuantLib"
        old_cc = cfg['CC']
        cfg['CC'] = ENV['CXX'] || "g++"
        cfg['CPP'].sub!(old_cc,cfg['CC'])
    	cfg['LDSHARED'].sub!(old_cc,cfg['CC'])
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
	Info.each       { |file| File.syscopy file, distDir }
	Sources.each    { |file| File.syscopy file, distDir }
	Binaries.each   { |file| File.syscopy file, distDir }
	Scripts.each    { |file| File.syscopy file, distDir }
	Interfaces.each { |file| File.syscopy '../SWIG/'+file, swigDir }
	Tests.each      { |file| File.syscopy 'test/'+file, testDir }
	system "tar cfz #{distDir}.#{Config::CONFIG['arch']}.tar.gz #{distDir}/"
}

Install = Command.new {
	Build.execute
	puts "Installing QuantLib-Ruby..."
	if Prefix.nil?
		archDir    = Config::CONFIG["sitearchdir"]
		libDir     = Config::CONFIG["sitelibdir"]
	else
		# strip old prefix and add the new one
		oldPrefix = Config::CONFIG["prefix"]
		archDir    = Prefix + \
                     Config::CONFIG["archdir"].gsub("^#{oldPrefix}","")
		libDir     = Prefix + \
                     Config::CONFIG["rubylibdir"].gsub("^#{oldPrefix}","")
	end
	[archDir,libDir].each { |path| File.makedirs path }
	File.install "./QuantLibc.so", archDir+"/QuantLibc.so", 0555, true
	File.install "./QuantLib.rb", libDir+"/QuantLib.rb", 0555, true
}


availableCommands = {
    "wrap"    => Wrap,
	"build"   => Build,
	"test"    => RunTests,
  	"install" => Install,
  	"sdist"   => SDist,
  	"bdist"   => BDist }

availableCommands[cmd].execute

