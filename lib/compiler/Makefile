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
	beam_a \
	beam_asm \
	beam_block \
	beam_bool \
	beam_bsm \
	beam_clean \
	beam_dead \
	beam_dict \
	beam_disasm \
	beam_except \
	beam_flatten \
	beam_jump \
	beam_listing \
	beam_opcodes \
	beam_peep \
	beam_receive \
	beam_split \
	beam_trim \
	beam_type \
	beam_utils \
	beam_validator \
	beam_z \
	cerl \
	cerl_clauses \
	cerl_inline \
	cerl_sets \
	cerl_trees \
	compile \
	core_lib \
	core_lint \
	core_parse \
	core_pp \
	core_scan \
  erl_anno \
	erl_bifs \
  erl_compile	\
	erl_expand_records \
	erl_lint \
  erl_pp \
  erl_tar \
  erl_bits \
  erl_eval \
  erl_internal \
  erl_parse \
  erl_posix_msg \
  erl_scan \
	rec_env \
	sys_core_dsetel \
	sys_core_fold \
	sys_core_fold_lists \
	sys_core_inline \
	sys_pre_attributes \
	sys_pre_expand \
	v3_codegen \
	v3_core \
	v3_kernel \
	v3_kernel_pp \
	v3_life

BEAM_H = $(wildcard ../priv/beam_h/*.h)

HRL_FILES= \
	beam_disasm.hrl \
	core_parse.hrl \
	v3_kernel.hrl \
	v3_life.hrl

YRL_FILE = $(SRC)/core_parse.yrl \
         $(SRC)/erl_parse.yrl

EXTRA_FILES= $(EGEN)/beam_opcodes.hrl

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
	rm -f $(EGEN)/beam_opcodes.erl $(EGEN)/beam_opcodes.hrl
	rm -f $(EGEN)/core_parse.erl
	rm -f core

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

$(EGEN)/beam_opcodes.erl $(EGEN)/beam_opcodes.hrl: $(SRC)/genop.tab
	perl $(TERL_ROOT)/erts/emulator/utils/beam_makeops -compiler -outdir $(EGEN) $<

$(EBIN)/beam_asm.beam: $(SRC)/beam_asm.erl $(EGEN)/beam_opcodes.hrl
	$(ERLC) $(ERL_COMPILE_FLAGS) -DCOMPILER_VSN='"6.0.3"' -o$(EBIN) $<

$(EBIN)/erl_compile.beam: $(SRC)/erl_compile.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -I../kernel/include -o$(EBIN) $(SRC)/erl_compile.erl

$(EBIN)/cerl_inline.beam: $(SRC)/cerl_inline.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) +nowarn_shadow_vars -o$(EBIN) $<

# Inlining core_parse is slow and has no benefit.
$(EBIN)/core_parse.beam: $(EGEN)/core_parse.erl
	$(ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -I$(SRC) -o$(EBIN) $<

# Inlining erl_parse is slow and has no benefit.
$(EBIN)/erl_parse.beam: $(EGEN)/erl_parse.erl
	$(ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -I$(SRC) -o$(EBIN) $<

# Inlining erl_lint is slow and has no benefit.
$(EBIN)/erl_lint.beam: $(SRC)/erl_lint.erl
	$(ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -I$(SRC) -o$(EBIN) $<
