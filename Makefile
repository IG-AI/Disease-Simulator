
# Shamelessly stolen from Karl Marklund
ESTOP = -s init stop
ERUN = erl -noshell -pa ebin -run
EEXTRA = -extra
ERLC_FLAGS=-Wall +debug_info
SOURCES=$(wildcard src/*.erl)
HEADERS=$(wildcard src/*.hrl)
OBJECTS:=$(SOURCES:src/%.erl=ebin/%.beam)

.PHONY: doc doc_url skeleton coverage clean
### SPECIAL VARS ###

ESMALLRUN = map_zero.bmp 5 5 # ARGS FOR ERLANG



### COMPILATION ###

all: $(OBJECTS)

ebin/%.beam: src/%.erl 
	erlc $(ERLC_FLAGS) -o ebin/ $<

### RUNNING ###
erun:
	$(ERUN) main start $(ESTOP) $(EEXTRA) $(ESMALLRUN)


### CLEANUP ###
clean:
	rm -Rf ebin/*
	rm -Rf src/*.beam
	rm -Rf doc/*.html


### DOCUMENTATION ###
doc: 
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"."' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

doc_url:
	@echo 
	@echo "EDoc index page available at file://$(PWD)/doc/index.html"
	@echo












### EUnit ###

# Make a comma separated list:
# http://ftp.gnu.org/old-gnu/Manuals/make-3.79.1/html_chapter/make_8.html

comma:= ,
empty:=
space:= $(empty) $(empty)

OBJECTS_LIST:= $(subst $(space),$(comma),$(OBJECTS:ebin/%.beam=%))

test: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [])' -s init stop

testv: $(OBJECTS)
	erl -noshell -pa ebin -eval 'eunit:test([$(OBJECTS_LIST)], [verbose])' -s init stop

test_%: ebin/%.beam
	erl -noshell -pa ebin -eval "eunit:test($(subst test_,, $@), [])" -s init stop

testv_%: ebin/%.beam
	erl -noshell -pa ebin -eval "eunit:test($(subst testv_,, $@), [verbose])" -s init stop

