include $(TERL_ROOT)/make/common_targets.mk

SRC = src
EBIN = ebin
EGEN = ebin

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
	erl_bifs \
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

YRL_FILE = $(SRC)/core_parse.yrl

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
 -Werror \
 -I../stdlib/include -I$(EBIN) -W

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

$(EBIN)/%.erl: $(SRC)/%.yrl
	$(ERLC) $(YRL_FLAGS) -o$(EBIN) $<

$(EBIN)/%.erl: $(SRC)/%.xrl
	$(ERLC) $(XRL_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(SRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

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

$(EBIN)/cerl_inline.beam: $(SRC)/cerl_inline.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) +nowarn_shadow_vars -o$(EBIN) $<

# Inlining core_parse is slow and has no benefit.
$(EBIN)/core_parse.beam: $(EGEN)/core_parse.erl
	$(ERLC) $(subst +inline,,$(ERL_COMPILE_FLAGS)) -I$(SRC) -o$(EBIN) $<
