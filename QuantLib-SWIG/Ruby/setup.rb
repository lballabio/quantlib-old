=begin
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
=end

# $Id$

require 'rbconfig'
require 'mkmf'
require 'ftools'

def usage
    puts <<EOU
Usage: ruby setup.rb command [options]
    Commands:
    build                                   build QuantLib-Ruby
    install [--prefix=/path/to/install/dir] install QuantLib-Ruby
    sdist                                   create source distribution
    bdist                                   create binary distribution
EOU
    exit
end

# parse command line
cmd = ARGV.shift or usage()
cmd.downcase!
usage() unless ['build','test','install','sdist','bdist'].member? cmd
Prefix = nil
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
	end
end


# Current QuantLib version
Version = "0.3.1a0-cvs"

# Files
Info     =    [ 'Authors.txt', 'ChangeLog.txt', 'Contributors.txt',
                'LICENSE.TXT', 'README.txt', 'History.txt', 'News.txt' ]
Sources  =    [ 'QuantLib.rb', 'quantlib_wrap.cpp' ]
Binaries =    [ 'QuantLibc.so' ]
Scripts  =    [ 'setup.rb' ]

Interfaces =  [ 'common.i',
                'calendars.i',
                'currencies.i',
                'date.i',
                'daycounters.i',
                'distributions.i',
                'functions.i',
                'history.i',
                'marketelements.i',
                'null.i',
                'observer.i',
                'quantlib.i',
                'ql.i',
                'qlarray.i',
                'randomnumbers.i',
                'riskstatistics.i',
                'solvers1d.i',
                'types.i',
                'vectors.i']

Tests =       [ 'dates.rb',
                'daycounters.rb',
                'distributions.rb',
                'marketelements.rb',
                'riskstatistics.rb',
                'solvers1d.rb',
                'QuantLibTestSuite.rb' ]

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
	# check dependencies
	if File.exists? "quantlib_wrap.cpp"
		genTime = File.mtime("quantlib_wrap.cpp")
		needsWrapping = false
		Interfaces.each { |file|
			if File.mtime("../SWIG/#{file}") > genTime
				needsWrapping = true
			end
		}
	else
		needsWrapping = true
	end
	if needsWrapping
		puts "Generating wrappers for QuantLib-Ruby..."
		system "swig -ruby -c++ -I../SWIG -o ./quantlib_wrap.cpp quantlib.i"
	end
}

SDist = Command.new {
	Wrap.execute
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
	Wrap.execute
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
    	$CFLAGS   += " -DHAVE_CONFIG_H"
    	$CPPFLAGS += " -I/usr/local/include"
    	$libs     += " -lQuantLib"
    	cfg['LDSHARED'] = cfg['LDSHARED'].sub('gcc','g++')
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

Test = Command.new {
	Wrap.execute
	Build.execute
	puts "Testing QuantLib-Ruby..."
	$LOAD_PATH.unshift Dir.pwd
	Dir.chdir 'test'
	load 'QuantLibTestSuite.rb'
	Dir.chdir '..'
	$LOAD_PATH.shift
}

BDist = Command.new {
	Wrap.execute
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
	Wrap.execute
	Build.execute
	puts "Installing QuantLib-Ruby..."
	if Prefix.nil?
		archDir = Config::CONFIG["sitearchdir"]
		libDir  = Config::CONFIG["sitelibdir"]
		dataDir = Config::CONFIG["datadir"]
	else
		# strip old prefix and add the new one
		oldPrefix = Config::CONFIG["prefix"]
		archDir = Prefix + Config::CONFIG["archdir"].gsub("^#{oldPrefix}","")
		libDir  = Prefix + Config::CONFIG["rubylibdir"].gsub("^#{oldPrefix}","")
		dataDir = Prefix + Config::CONFIG["datadir"].gsub("^#{oldPrefix}","")
	end
	swigDir   = dataDir + "/QuantLib-Ruby/SWIG"
	docDir    = dataDir + "/doc/QuantLib-Ruby"
	testDir   = docDir + "/test"
	[archDir,libDir,swigDir,testDir].each { |path| File.makedirs path }
	File.install "./QuantLibc.so", archDir+"/QuantLibc.so", 0555, true
	File.install "./QuantLib.rb", libDir+"/QuantLib.rb", 0555, true
	Info.each { |file| File.install "./#{file}",docDir+"/#{file}",nil,true }
	Interfaces.each { |file| File.install "../SWIG/"+file,swigDir+"/#{file}",nil,true }
	Tests.each { |file| File.install "./test/"+file,testDir+"/#{file}",nil,true }
}


availableCommands = {
	"build"   => Build,
	"test"    => Test,
  	"install" => Install,
  	"sdist"   => SDist,
  	"bdist"   => BDist }

availableCommands[cmd].execute

