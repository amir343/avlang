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
	state_dl \
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
	type_err_msg \
	type_internal

HRL_FILES= \
	type_macros.hrl \
	type_checker_state.hrl

ERLANG_TYPES = priv/erlang_types.eterm

ERL_FILES= $(MODULES:%=$(SRC)/%.erl)
HEADER_FILES = $(HRL_FILES:%=$(SRC)/%)
BEAM_FILES = $(MODULES:%=$(EBIN)/%.beam)

INSTALL_FILES= $(BEAM_FILES) $(ERLANG_TYPES)
TARGET_FILES= $(INSTALL_FILES)


# ------------------------------------------------------------------------------
# Flags
# ------------------------------------------------------------------------------

ERL_COMPILE_FLAGS += +inline +warn_unused_import -Werror -W

# ------------------------------------------------------------------------------
# Targets
# ------------------------------------------------------------------------------

compile: $(TARGET_FILES)

clean:
	rm -fr $(BEAM_FILES)

