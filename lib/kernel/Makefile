include $(TERL_ROOT)/make/common_targets.mk

MODULES = \
	application \
	application_controller \
	application_master \
	application_starter \
	auth \
	code \
	code_server \
	disk_log \
	disk_log_1 \
	disk_log_server \
	disk_log_sup \
	dist_ac \
	dist_util \
	erl_boot_server \
	erl_ddll \
	erl_distribution \
	erl_epmd \
	erl_reply \
	erts_debug \
	error_handler \
	error_logger \
	file \
	file_io_server \
	file_server \
	gen_tcp \
	gen_udp \
	gen_sctp \
	global \
	global_group \
	global_search \
	group \
	heart \
	hipe_unified_loader \
	inet \
	inet6_tcp \
	inet6_tcp_dist \
	inet6_udp \
	inet6_sctp \
	inet_config \
	inet_db \
	inet_dns \
	inet_gethost_native \
	inet_hosts \
	inet_parse \
	inet_res \
	inet_tcp \
	inet_tcp_dist \
	inet_udp \
	inet_sctp \
	kernel \
	kernel_config \
	net \
	net_adm \
	net_kernel \
	os \
	pg2 \
	ram_file \
	rpc \
	seq_trace \
	standard_error \
	user \
	user_drv \
	user_sup \
	wrap_log_reader

HRL_FILES= include/file.hrl include/inet.hrl include/inet_sctp.hrl \
	include/dist.hrl include/dist_util.hrl include/net_address.hrl

INTERNAL_HRL_FILES= application_master.hrl disk_log.hrl \
        erl_epmd.hrl hipe_ext_format.hrl \
        inet_dns.hrl inet_res.hrl \
        inet_boot.hrl inet_config.hrl inet_int.hrl \
	inet_dns_record_adts.hrl

ERL_FILES= $(MODULES:%=$(SRC)/%.erl)
BEAM_FILES = $(MODULES:%=$(EBIN)/%.beam)

TARGET_FILES = $(BEAM_FILES) \
		       $(APP_TARGET) $(APPUP_TARGET)

APP_FILE= kernel.app

APP_SRC= $(APP_FILE).src
APP_TARGET= $(EBIN)/$(APP_FILE)

APPUP_FILE= kernel.appup

APPUP_SRC= $(APPUP_FILE).src
APPUP_TARGET= $(EBIN)/$(APPUP_FILE)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

ifeq ($(NATIVE_LIBS_ENABLED),yes)
ERL_COMPILE_FLAGS += +native
endif
ERL_COMPILE_FLAGS += -I../include -Werror

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

compile: $(BEAM_FILES)

# Note: In the open-source build clean must not destroyed the preloaded
# beam files.
clean:
	rm -f $(TARGET_FILES)
	rm -f core


docs:

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

../../hipe/main/hipe.hrl: ../../hipe/vsn.mk ../../hipe/main/hipe.hrl.src
	$(vsn_verbose)sed -e "s;%VSN%;$(HIPE_VSN);" ../../hipe/main/hipe.hrl.src > ../../hipe/main/hipe.hrl

$(APP_TARGET): $(APP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(KERNEL_VSN);' $< > $@

$(APPUP_TARGET): $(APPUP_SRC) ../vsn.mk
	$(vsn_verbose)sed -e 's;%VSN%;$(KERNEL_VSN);' $< > $@


EPMD_FLAGS = -Depmd_port_no=$(EPMD_PORT_NO)	\
             -Depmd_node_type=$(EPMD_NODE_TYPE)	\
             -Depmd_dist_low=$(EPMD_DIST_LOW)	\
             -Depmd_dist_high=$(EPMD_DIST_HIGH)	\
             -Derlang_daemon_port=$(EPMD_PORT_NO)

$(ESRC)/inet_dns_record_adts.hrl: $(ESRC)/inet_dns_record_adts.pl
	$(gen_verbose)LANG=C $(PERL) $< > $@

$(EBIN)/erl_epmd.beam: $(ESRC)/erl_epmd.erl
	$(V_ERLC) $(ERL_COMPILE_FLAGS) $(EPMD_FLAGS) -o$(EBIN) $<

