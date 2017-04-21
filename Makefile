
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

### SPECIAL VARS ###

ESMALLRUN = map_one.bmp 5 5 # ARGS FOR ERLANG



### COMPILATION ###

all: mkdir $(OBJECTS)

Erlang/ebin/%.beam: Erlang/src/%.erl 
	erlc $(ERLC_FLAGS) -o Erlang/ebin/ $<

### RUNNING ###
erun: all
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

