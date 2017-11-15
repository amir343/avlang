# Copyright (c) 2016-2017 Amir Moulavi
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

SRC     = src
EBIN    = ebin
INCLUDE = include
ERLC    = erlc

APP_DEF = avl.app

ERLS  = $(notdir $(wildcard $(SRC)/*.erl))
YRLS  = $(notdir $(wildcard $(SRC)/*.yrl))
EBINS = $(ERLS:.erl=.beam) $(YRLS:.yrl=.beam)
TARGET_FILES = $(addprefix $(EBIN)/, $(EBINS)) $(EBIN)/$(APP_DEF)

ERLANG_TYPES = priv/erlang_types.eterm

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(NATIVE_LIBS_ENABLED),yes)
	ERL_COMPILE_FLAGS += +native
endif
ERL_COMPILE_FLAGS += +inline +warn_unused_import -Werror -W -pa $(EBIN)

# ----------------------------------------------------
# Build Targets
# ----------------------------------------------------

$(EBIN)/%.beam: $(SRC)/%.erl
	@mkdir -p $(EBIN)
	$(ERLC) -I $(INCLUDE) -o $(EBIN) $(ERL_COMPILE_FLAGS) $<

$(EBIN)/$(APP_DEF): $(SRC)/$(APP_DEF).src
	@mkdir -p $(EBIN)
	cp $(SRC)/$(APP_DEF).src $(EBIN)/$(APP_DEF)

%.erl: %.yrl
	$(ERLC) -o $(SRC) $<

.NOTPARALLEL:

.PHONY: all test compile clean

compile: $(TARGET_FILES)

clean:
	rm -f $(TARGET_FILES)

test:
	./avl test

all: compile test

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# Inlining erl_lint is slow and has no benefit.
$(EBIN)/avl_lint.beam: $(SRC)/avl_lint.erl
	$(ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -I$(SRC) -o$(EBIN) $<

# ----------------------------------------------------
# Dependancy
# ----------------------------------------------------

$(EBIN)/avl_any.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_atom.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_binary.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_boolean.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_char.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_float.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_integer.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_none.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_number.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_pid.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_port.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_reference.beam: $(EBIN)/type_interface.beam
$(EBIN)/avl_string.beam: $(EBIN)/type_interface.beam
