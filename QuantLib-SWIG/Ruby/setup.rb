=begin
 Copyright (C) 2000, 2001, 2002, 2003 RiskMap srl
 Copyright (C) 2003, 2004, 2005, 2006, 2007 StatPro Italia srl

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
=end

require 'rbconfig'
require 'mkmf'
require 'fileutils'

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
Version = "1.7"

cfg = RbConfig::MAKEFILE_CONFIG

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
    swigDir = "../SWIG"
    cmd = "swig -ruby -c++ -I#{swigDir} -o ./quantlib_wrap.cpp quantlib.i"
    puts cmd
    system cmd
}

Build = Command.new {
    cfg = RbConfig::MAKEFILE_CONFIG
    if cfg['host_os'] == 'mswin32'
      QL_DIR = ENV['QL_DIR']
      if QL_DIR
        $CPPFLAGS += " /I#{QL_DIR}"
        $LIBPATH  += ["#{QL_DIR}\\lib"]
      else
        puts 'warning: unable to detect QuantLib installation'
        puts 'I will assume that it was added to the default compiler paths'
      end
      $CPPFLAGS += " /MD"
      $CPPFLAGS += " /GR"
      $CPPFLAGS += " /GX"
      $CPPFLAGS += " /Zm250"
      $CPPFLAGS += " /DNOMINMAX"
    else
      $CFLAGS   += " " + (ENV['CFLAGS'] || "")
      $CPPFLAGS += " " + IO.popen("quantlib-config --cflags").gets.strip
      $CPPFLAGS += " -Wno-uninitialized -Wno-unused"
      $CPPFLAGS += " " + (ENV['CXXFLAGS'] || "")
      $CPPFLAGS += " -DBOOST_DISABLE_THREADS"
      $libs     += " " + IO.popen("quantlib-config --libs").gets.strip
      old_cc = cfg['CC']
      cfg['CC'] = ENV['CXX'] || "g++"
      cfg['CPP'].sub!(old_cc,cfg['CC'])
      cfg['LDSHARED'].sub!(old_cc,cfg['CC'])
      if cfg['host_os'][0..5] =='darwin'
        cfg['LDSHARED'].sub!('cc',cfg['CC'])
        cfg['LDSHARED'] += " -flat_namespace -undefined suppress"
      end
    end
    # we have to juggle files as create_makefile is stubborn about file names
    if File.exists?("Makefile")
      File.rename("Makefile","Makefile.old")
    end
    create_makefile("QuantLibc")
    File.rename("Makefile","extension.mak")
    if File.exists?("Makefile.old")
      File.rename("Makefile.old", "Makefile")
    end
    if cfg['host_os'] == 'mswin32'
      system("nmake /f extension.mak")
    else
      system("make -f extension.mak")
    end
}

RunTests = Command.new {
    Build.execute
    puts "Testing QuantLib-Ruby #{Version}..."
    $LOAD_PATH.unshift Dir.pwd
    Dir.chdir 'test'
    load 'QuantLibTestSuite.rb'
    Dir.chdir '..'
    $LOAD_PATH.shift
}

Install = Command.new {
    Build.execute
    if defined? Prefix
        # strip old prefix and add the new one
        oldPrefix = RbConfig::CONFIG["prefix"]
        if defined? Debian
          archDir = RbConfig::CONFIG["archdir"]
          libDir = RbConfig::CONFIG["rubylibdir"]
        else
          archDir = RbConfig::CONFIG["sitearchdir"]
          libDir = RbConfig::CONFIG["sitelibdir"]
        end
        archDir    = Prefix + archDir.gsub(/^#{oldPrefix}/,"")
        libDir     = Prefix + libDir.gsub(/^#{oldPrefix}/,"")
    else
        archDir    = RbConfig::CONFIG["sitearchdir"]
        libDir     = RbConfig::CONFIG["sitelibdir"]
    end
    [archDir,libDir].each { |path| FileUtils.makedirs path }
    if cfg['host_os'][0..5] == 'darwin'
      binary = 'QuantLibc.bundle'
    else
      binary = 'QuantLibc.so'
    end
    FileUtils.install "./"+binary, archDir+"/"+binary, :mode => 0555, :verbose => true 
    FileUtils.install "./QuantLib.rb", libDir+"/QuantLib.rb", :mode => 0555, :verbose => true
}

availableCommands = {
    "wrap"    => Wrap,
    "build"   => Build,
    "test"    => RunTests,
    "install" => Install }

availableCommands[cmd].execute

