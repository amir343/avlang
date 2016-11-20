
ERLC = $(TERL_ROOT)/bootstrap/bin/erlc
SRC = src
EBIN = ebin
EGEN = ebin

$(EBIN)/%.erl: $(SRC)/%.yrl
	$(ERLC) $(YRL_FLAGS) -o$(EBIN) $<

$(EBIN)/%.erl: $(SRC)/%.xrl
	$(ERLC) $(XRL_FLAGS) -o$(EBIN) $<

$(EBIN)/%.beam: $(SRC)/%.erl
	$(ERLC) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
