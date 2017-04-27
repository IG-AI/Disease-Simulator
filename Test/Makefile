EC =erl -compile
ERUN =erl -noshell -run
ESTOP =-s init stop -extra

adj_map.beam: adj_map.erl
	$(EC) $^

lmerge.beam: lmerge.erl
	$(EC) $^

erun: adj_map.beam
	$(ERUN) adj_map adj_map  $(ESTOP)

e: lmerge.beam
	$(ERUN) lmerge lmerge  $(ESTOP)
