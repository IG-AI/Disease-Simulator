
# Shamelessly stolen from Karl Marklund
ESTOP = -s init stop
ERUN = erl -noshell -pa Erlang/ebin -run
EEXTRA = -extra
ERLC_FLAGS=-Wall +debug_info
SOURCES=$(wildcard Erlang/src/*.erl)
HEADERS=$(wildcard Erlang/src/*.hrl)
OBJECTS:=$(SOURCES:Erlang/src/%.erl=Erlang/ebin/%.beam)

### SPECIAL FLAGS ###

.PHONY: doc doc_url skeleton coverage clean

mkdir:
	mkdir -p Erlang/ebin Erlang/doc

epmd_run:
	Bash/epmd_startup.sh

### SPECIAL VARS ###
MAP?=map_one.bmp #Default value for map
IND?=5 #Default number of individuals
INF?=2 #Default number of infected individuals
INFC?=1 #Default chanse of infection
TICKS?=20 #Default length of the simulation


ESMALLRUN = $(MAP) $(IND) $(TICKS) $(INF) $(INFC) # ARGS FOR ERLANG
ETINY = map_zero.bmp 3 20 0 0

### SPECIAL RUNS ###
erun_tiny: epmd_run all
	$(ERUN) main start $(ESTOP) $(EEXTRA) $(ETINY)

### COMPILATION ###

all: mkdir $(OBJECTS)

Erlang/ebin/%.beam: Erlang/src/%.erl 
	erlc $(ERLC_FLAGS) -o Erlang/ebin/ $<

### RUNNING ###
erun: epmd_run all 
	$(ERUN) main start $(ESTOP) $(EEXTRA) $(ESMALLRUN)


### CLEANUP ###
clean:
	rm -Rf Erlang/ebin/*
	rm -Rf Erlang/src/*.beam
	rm -Rf Erlang/doc/*.html


### DOCUMENTATION ###
doc: 
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"./Erlang/"' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

doc_url:
	@echo 
	@echo "EDoc index page available at file://$(PWD)/Erlang/doc/index.html"
	@echo



### EUnit ###

# Make a comma separated list:
# http://ftp.gnu.org/old-gnu/Manuals/make-3.79.1/html_chapter/make_8.html

comma:= ,
empty:=
space:= $(empty) $(empty)

OBJECTS_LIST:= $(subst $(space),$(comma),$(OBJECTS:Erlang/ebin/%.beam=%))

test: mkdir $(OBJECTS)
	erl -noshell -pa Erlang/ebin -eval 'eunit:test([$(OBJECTS_LIST)], [])' -s init stop

testv: mkdir $(OBJECTS)
	erl -noshell -pa Erlang/ebin -eval 'eunit:test([$(OBJECTS_LIST)], [verbose])' -s init stop

test_%: mkdir Erlang/ebin/%.beam
	erl -noshell -pa Erlang/ebin -eval "eunit:test($(subst test_,, $@), [])" -s init stop

testv_%: mkdir Erlang/ebin/%.beam
	erl -noshell -pa Erlang/ebin -eval "eunit:test($(subst testv_,, $@), [verbose])" -s init stop

