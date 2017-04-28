EC =erl -compile
ERUN =erl -noshell -run
ESTOP =-s init stop -extra

adj_map.beam: adj_map.erl
	$(EC) $^

lmerge.beam: lmerge.erl
	$(EC) $^

a_star_demo.beam: a_star_demo.erl
	$(EC) $^

a_star.beam: a_star.erl
	$(EC) $^

arun: a_star_demo.beam a_star.beam
	$(ERUN) a_star_demo b1 $(ESTOP)

erun: adj_map.beam
	$(ERUN) adj_map start  $(ESTOP)

e: lmerge.beam
	$(ERUN) lmerge lmerge  $(ESTOP)
