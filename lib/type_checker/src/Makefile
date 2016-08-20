#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1996-2013. All Rights Reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# %CopyrightEnd%
#

ifdef BOOTSTRAP
EGEN=$(BOOTSTRAP_TOP)/lib/type_checker/egen
EBIN=$(BOOTSTRAP_TOP)/lib/type_checker/ebin
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(TYPE_CHECKER_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/type_checker-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
MODULES =  \
	terl_any \
	terl_boolean \
	terl_binary \
	terl_float \
	terl_integer \
	terl_list \
	terl_pid \
	terl_string \
	terl_union \
	type_check \
	type_check_compiler_opts \
	type_internal

BEAM_H = $(wildcard ../priv/beam_h/*.h)

HRL_FILES= \
	type_macros.hrl

ERLANG_TYPES = ../priv/erlang_types.eterm

ERL_FILES= $(MODULES:%=%.erl)
INSTALL_FILES= $(MODULES:%=$(EBIN)/%.$(EMULATOR)) $(APP_TARGET) \
	$(APPUP_TARGET) $(ERLANG_TYPES)
TARGET_FILES= $(INSTALL_FILES)
 
APP_FILE= type_checker.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= type_checker.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
endif
ERL_COMPILE_FLAGS += +inline +warn_unused_import \
 -Werror \
 -I../../stdlib/include -I$(EGEN) -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(VSN);' $< > $@

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/src"
	$(INSTALL_DATA) $(ERL_FILES) $(HRL_FILES) $(EXTRA_FILES) \
		$(YRL_FILE) "$(RELSYSDIR)/src"
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(INSTALL_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

# ----------------------------------------------------
# Dependencies -- alphabetically, please
# ----------------------------------------------------

$(EBIN)/terl_any.beam: type_macros.hrl
$(EBIN)/terl_boolean.beam: type_macros.hrl
$(EBIN)/terl_binary.beam: type_macros.hrl
$(EBIN)/terl_float.beam: type_macros.hrl
$(EBIN)/terl_integer.beam: type_macros.hrl
$(EBIN)/terl_list.beam: type_macros.hrl
$(EBIN)/terl_string.beam: type_macros.hrl
$(EBIN)/terl_pid.beam: type_macros.hrl
$(EBIN)/terl_union.beam: type_macros.hrl
$(EBIN)/type_internal.beam: type_macros.hrl
