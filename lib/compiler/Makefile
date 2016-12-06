#! /bin/bash
# Copyright (c) 2016 Amir Moulavi
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

include $(TERL_ROOT)/make/common_targets.mk

MODULES =  \
  erl_compile	\
  terl_anno \
	terl_bifs \
  terl_bits \
	terl_compile \
  terl_core \
	terl_expand_records \
  terl_eval \
  terl_epp \
  terl_internal \
	terl_lint \
  terl_pp \
  terl_parse \
	terl_pre_expand \
  terl_scan

BEAM_H = $(wildcard ../priv/beam_h/*.h)

YRL_FILE = $(SRC)/terl_parse.yrl

ERL_FILES= $(MODULES:%=$(SRC)/%.erl)
BEAM_FILES = $(MODULES:%=$(EBIN)/%.beam)

INSTALL_FILES= $(BEAM_FILES) $(APP_TARGET) $(APPUP_TARGET)
TARGET_FILES= $(BEAM_FILES)

APP_FILE= compiler.app
APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= compiler.appup
APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
endif
ERL_COMPILE_FLAGS += +inline +warn_unused_import \
 -Werror  -I$(EBIN) -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

compile: $(TARGET_FILES)

docs:

clean:
	rm -f $(TARGET_FILES)

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EBIN)/terl_parse.beam: $(SRC)/terl_parse.yrl
	$(ERLC) -o $(EBIN) $<
	$(ERLC) -o $(EBIN) $(EBIN)/terl_parse.erl

# Inlining erl_lint is slow and has no benefit.
$(EBIN)/terl_lint.beam: $(SRC)/terl_lint.erl
	$(ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -I$(SRC) -o$(EBIN) $<
