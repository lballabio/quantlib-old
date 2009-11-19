##########################################################################
#
# Makefile for a simple Calc Addin in C++
#
# Modified version of 
# $OO_SDK_HOME/examples/DevelopersGuide/Components/Makefile

# Prerequisites:
# --------------
# 1) OOo SDK installed and environment variables set, in particular
#    OO_SDK_HOME and OO_SDK_OUT
# 2) Environment variables set:
#    QL_PATH: Full path to the QuantLib directory 
#    OH_PATH: Full path to the ObjectHandler directory
#    QA_PATH: Full path to the QuantLibAddin directory
#    LG_PATH: Full path to the log4cxx directory
#
# Changes compared to the original: 
# ---------------------------------
# - removed APP1
# - added full path to the SDK root directory
# - added source files
# - replaced cxx with cpp
# - additional include paths
# - additional library paths and various libraries
#
# Todo:
# -----
# 1) Fix RL_LIB: link dylibs rather than archives 
#                restrict to QuantLib, ObjectHandler and QuantLibAddin library
#                avoid linking log4cxx, apr, boost, expat etc.
# 2) Turn this file into a Makefile.am, use configure to set QuantLib related 
#    paths 
#
##########################################################################

#*************************************************************************
#
#  $RCSfile: Makefile,v $
#
#  $Revision: 1.11 $
#
#  last change: $Author: rt $ $Date: 2008-07-11 14:17:49 $
#
#  The Contents of this file are made available subject to the terms of
#  the BSD license.
#  
#  Copyright (c) 2003 by Sun Microsystems, Inc.
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#  3. Neither the name of Sun Microsystems, Inc. nor the names of its
#     contributors may be used to endorse or promote products derived
#     from this software without specific prior written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
#  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
#  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
#  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
#  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
#  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
#  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
#  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#     
#**************************************************************************

# Builds the C++ component example of the Developers Guide.

# RL added: Absolute path to OOo SDK
PRJ=$(OO_SDK_HOME)

# RL: added RL_INC and RL_LIB for QuantLib
RL_INC= -I$(QL_PATH) -I$(OH_PATH) -I$(QA_PATH)
RL_LIB= $(QL_PATH)/ql/.libs/libQuantLib.a \
	$(OH_PATH)/oh/.libs/libObjectHandler.a \
	$(QA_PATH)/qlo/.libs/libQuantLibAddin.a \
	$(LG_PATH)/src/main/cpp/.libs/liblog4cxx.a \
	$(LG_PATH)/src/apr/.libs/libapr-1.a \
	$(LG_PATH)/src/apr-util/.libs/libaprutil-1.a \
	/usr/local/lib/libboost_serialization-xgcc40-mt-1_39.a \
	/usr/local/lib/libboost_filesystem-xgcc40-mt-1_39.a \
	/usr/local/lib/libboost_regex-xgcc40-mt-1_39.a \
	/usr/local/lib/libboost_system-xgcc40-mt-1_39.a \
	-L/usr/lib -lexpat \
	-L/usr/lib -liconv

#RL_LIB= -L$(QLDIR) -lQuantLib \
	-L$(OHDIR)/oh/.libs -lObjectHandler \
	-L$(QADIR)/qlo/.libs -lQuantLibAddin
#RL_LIB= -L$(QLDIR) -lQuantLib \
	$(QADIR)/qlo/.libs/libQuantLibAddin.a \
	$(OHDIR)/oh/.libs/libObjectHandler.a


SETTINGS=$(PRJ)/settings

include $(SETTINGS)/settings.mk
include $(SETTINGS)/std.mk
include $(SETTINGS)/dk.mk

# Define non-platform/compiler specific settings
SAMPLE_NAME=CalcAddinCpp
SAMPLE_INC_OUT=$(OUT_INC)/$(SAMPLE_NAME)
SAMPLE_GEN_OUT=$(OUT_MISC)/$(SAMPLE_NAME)
SAMPLE_SLO_OUT=$(OUT_SLO)/$(SAMPLE_NAME)
SAMPLE_OBJ_OUT=$(OUT_OBJ)/$(SAMPLE_NAME)

COMP_NAME=CalcAddins
COMP_IMPL_NAME=$(COMP_NAME).uno.$(SHAREDLIB_EXT) 

COMP_RDB_NAME = $(COMP_NAME).uno.rdb
COMP_RDB = $(SAMPLE_GEN_OUT)/$(COMP_RDB_NAME)
COMP_PACKAGE = $(OUT_BIN)/$(COMP_NAME).$(UNOOXT_EXT)
COMP_PACKAGE_URL = $(subst \\,\,"$(COMP_PACKAGE_DIR)$(PS)$(COMP_NAME).$(UNOOXT_EXT)")
COMP_UNOPKG_MANIFEST = $(SAMPLE_GEN_OUT)/$(COMP_NAME)/META-INF/manifest.xml
COMP_MAPFILE = $(SAMPLE_GEN_OUT)/$(COMP_NAME).uno.map

COMP_REGISTERFLAG = $(SAMPLE_GEN_OUT)/devguide_$(COMP_NAME)_register_component.flag
COMP_TYPEFLAG = $(SAMPLE_GEN_OUT)/devguide_$(COMP_NAME)_types.flag

# RL: enter the IDL file name here
IDLFILES = calcaddins.idl

# RL: enter all sources here
CPPFILES = calcaddins.cpp \
	calcutils.cpp \
	conversions.cpp \
	init.cpp \
	funcdef.cpp \
	logging.cpp \
	utilities.cpp \
	exercise.cpp \
	instruments.cpp \
	options.cpp \
	payoffs.cpp \
	pricingengines.cpp \
	processes.cpp \
	settings.cpp \
	volatilities.cpp

SLOFILES = $(patsubst %.cpp,$(SAMPLE_SLO_OUT)/%.$(OBJ_EXT),$(CPPFILES))

GENURDFILES = $(patsubst %.idl,$(SAMPLE_GEN_OUT)/%.urd,$(IDLFILES))

# RL: these need to be concsistent with the names/namespaces in the IDL file 
TYPELIST=-Tquantlib.XCalcAddins \
	-Tquantlib.CalcAddins


# Targets
.PHONY: ALL
ALL : \
	$(SAMPLE_NAME)

include $(SETTINGS)/stdtarget.mk

$(SAMPLE_GEN_OUT)/%.urd : %.idl
	-$(MKDIR) $(subst /,$(PS),$(@D))
	$(IDLC) -I. -I$(IDL_DIR) -O$(SAMPLE_GEN_OUT) $<

$(SAMPLE_GEN_OUT)/%.rdb : $(GENURDFILES)
	-$(DEL) $(subst \\,\,$(subst /,$(PS),$@))
	-$(MKDIR) $(subst /,$(PS),$(@D))
	$(REGMERGE) $@ /UCR $(GENURDFILES)

$(COMP_TYPEFLAG) : $(COMP_RDB) $(SDKTYPEFLAG)
	-$(DEL) $(subst \\,\,$(subst /,$(PS),$@))
	-$(MKDIR) $(subst /,$(PS),$(@D))
	$(CPPUMAKER) -Gc -BUCR -O$(SAMPLE_INC_OUT) $(TYPESLIST) $(COMP_RDB) -X$(URE_TYPES) -X$(OFFICE_TYPES)
	echo flagged > $@

# modified: added RL_INC
$(SAMPLE_SLO_OUT)/%.$(OBJ_EXT) : %.cpp $(COMP_TYPEFLAG)
	-$(MKDIR) $(subst /,$(PS),$(@D))
	$(CC) $(CC_FLAGS) $(CC_INCLUDES) -I$(SAMPLE_INC_OUT) $(RL_INC) $(CC_DEFINES) $(CC_OUTPUT_SWITCH)$(subst /,$(PS),$@) $<

$(COMP_MAPFILE) : $(SLOFILES)
	-$(MKDIR) $(subst /,$(PS),$(@D))
	cat $(PRJ)/settings/component.uno.map > $(COMP_MAPFILE) 
ifeq "$(OS)" "MACOSX"
	nm -gx $(SLOFILES) | $(ADDSYMBOLS) >> $(COMP_MAPFILE)
endif

# modified: added RL_LIB
ifeq "$(OS)" "WIN"
$(SHAREDLIB_OUT)/%.$(SHAREDLIB_EXT) : $(SLOFILES)
	-$(MKDIR) $(subst /,$(PS),$(@D))
	-$(MKDIR) $(subst /,$(PS),$(SAMPLE_GEN_OUT))
	$(LINK) $(COMP_LINK_FLAGS) /OUT:$@ \
	/MAP:$(SAMPLE_GEN_OUT)/$(subst $(SHAREDLIB_EXT),map,$(@F)) $(SLOFILES) \
	$(CPPUHELPERLIB) $(CPPULIB) $(SALLIB) $(STLPORTLIB) msvcrt.lib kernel32.lib
	$(LINK_MANIFEST_VC8_ONLY)
else
$(SHAREDLIB_OUT)/%.$(SHAREDLIB_EXT) : $(SLOFILES) $(COMP_MAPFILE)
	-$(MKDIR) $(subst /,$(PS),$(@D)) && $(DEL) $(subst \\,\,$(subst /,$(PS),$@))
	$(LINK) $(COMP_LINK_FLAGS) $(LINK_LIBS) -o $@ $(SLOFILES) \
	$(CPPUHELPERLIB) $(CPPULIB) $(SALLIB) $(STLPORTLIB) $(STC++LIB) $(CPPUHELPERDYLIB) $(CPPUDYLIB) $(SALDYLIB) $(RL_LIB)
ifeq "$(OS)" "MACOSX"
	$(INSTALL_NAME_URELIBS)  $@
endif
endif	

# rule for component package manifest
$(SAMPLE_GEN_OUT)/%/manifest.xml :
	-$(MKDIR) $(subst /,$(PS),$(@D))
	@echo $(OSEP)?xml version="$(QM)1.0$(QM)" encoding="$(QM)UTF-8$(QM)"?$(CSEP) > $@
	@echo $(OSEP)!DOCTYPE manifest:manifest PUBLIC "$(QM)-//OpenOffice.org//DTD Manifest 1.0//EN$(QM)" "$(QM)Manifest.dtd$(QM)"$(CSEP) >> $@
	@echo $(OSEP)manifest:manifest xmlns:manifest="$(QM)http://openoffice.org/2001/manifest$(QM)"$(CSEP) >> $@
	@echo $(SQM)  $(SQM)$(OSEP)manifest:file-entry manifest:media-type="$(QM)application/vnd.sun.star.uno-typelibrary;type=RDB$(QM)" >> $@
	@echo $(SQM)                       $(SQM)manifest:full-path="$(QM)$(subst /META-INF,,$(subst $(SAMPLE_GEN_OUT)/,,$(@D))).uno.rdb$(QM)"/$(CSEP) >> $@
	@echo $(SQM)  $(SQM)$(OSEP)manifest:file-entry manifest:media-type="$(QM)application/vnd.sun.star.uno-component;type=native;platform=$(UNOPKG_PLATFORM)$(QM)" >> $@
	@echo $(SQM)                       $(SQM)manifest:full-path="$(QM)$(subst /META-INF,,$(subst $(SAMPLE_GEN_OUT)/,,$(UNOPKG_PLATFORM)/$(@D))).uno.$(SHAREDLIB_EXT)$(QM)"/$(CSEP) >> $@
	@echo $(OSEP)/manifest:manifest$(CSEP) >> $@

$(COMP_PACKAGE) : $(SHAREDLIB_OUT)/$(COMP_IMPL_NAME) $(COMP_RDB) $(COMP_UNOPKG_MANIFEST)
	-$(DEL) $(subst \\,\,$(subst /,$(PS),$@))
	-$(MKDIR) $(subst /,$(PS),$(@D))
	-$(MKDIR) $(subst /,$(PS),$(SAMPLE_GEN_OUT)/$(UNOPKG_PLATFORM))	 
	$(COPY) $(subst /,$(PS),$<) $(subst /,$(PS),$(SAMPLE_GEN_OUT)/$(UNOPKG_PLATFORM))
	cd $(subst /,$(PS),$(SAMPLE_GEN_OUT)) && $(SDK_ZIP) ../../bin/$(@F) $(COMP_RDB_NAME) $(UNOPKG_PLATFORM)/$(<F)
	cd $(subst /,$(PS),$(SAMPLE_GEN_OUT)/$(subst .$(UNOOXT_EXT),,$(@F))) && $(SDK_ZIP) -u ../../../bin/$(@F) META-INF/manifest.xml

$(COMP_REGISTERFLAG) : $(COMP_PACKAGE)
ifeq "$(SDK_AUTO_DEPLOYMENT)" "YES"
	-$(DEL) $(subst \\,\,$(subst /,$(PS),$@))
	-$(MKDIR) $(subst /,$(PS),$(@D))
	$(DEPLOYTOOL) $(COMP_PACKAGE_URL)
	@echo flagged > $(subst /,$(PS),$@)
else
	@echo --------------------------------------------------------------------------------
	@echo  If you want to install your component automatically, please set the environment
	@echo  variable SDK_AUTO_DEPLOYMENT = YES. But note that auto deployment is only 
	@echo  possible if no office instance is running. 
	@echo --------------------------------------------------------------------------------
endif

$(SAMPLE_NAME) : $(COMP_REGISTERFLAG)

.PHONY: clean
clean :
	-$(DELRECURSIVE) $(subst /,$(PS),$(SAMPLE_INC_OUT))
	-$(DELRECURSIVE) $(subst /,$(PS),$(SAMPLE_GEN_OUT))
	-$(DELRECURSIVE) $(subst /,$(PS),$(SAMPLE_SLO_OUT))
	-$(DELRECURSIVE) $(subst /,$(PS),$(SAMPLE_OBJ_OUT))
	-$(DEL) $(subst \\,\,$(subst /,$(PS),$(OUT_BIN)/$(COMP_NAME)*))