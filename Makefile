# Shamelessly stolen from Karl Marklund

### ERLANG ###
ESTOP = -s init stop
ERUN = erl -noshell -pa Erlang/ebin -run
EEXTRA = -extra
ERLC_FLAGS=-Wall +debug_info
ERLANG_SOURCES=$(wildcard Erlang/src/*.erl)
ERLANG_HEADERS=$(wildcard Erlang/src/*.hrl)
ERLANG_OBJECTS:=$(ERLANG_SOURCES:Erlang/src/%.erl=Erlang/ebin/%.beam)

### JAVA ###
JPACK = -cp .:Java/lib/OtpErlang.jar:Java/bin:Java/src
JAVAPACKAGE = -cp .:Java/lib/OtpErlang.jar:Java/bin
JAVATESTPACKAGES = -cp .:Java/lib/junit-4.12.jar:Java/lib/OtpErlang.jar
#TEST#
TESTSRC = $(shell find src/test -name "*.java" -printf "test.%f ")
TESTS = $(subst .java,,$(TESTSRC))
TESTCMD = java -cp ./Java:Java/bin:./Java/lib/junit-4.12.jar:./Java/lib/hamcrest-core-1.3.jar org.junit.runner.JUnitCore
###
JAVAC = javac
JAVA_SOURCES = $(wildcard Java/src/*/*.java)
JAVA_CLASSES = $(JAVA_SOURCES:Java/src/%.java=Java/bin/%.class)

### SPECIAL FLAGS ###

.PHONY: doc doc_url skeleton coverage clean

mkdir:
	@mkdir -p Erlang/ebin Erlang/doc
	@mkdir -p Java/bin Java/doc

epmd_run:
	Bash/epmd_startup.sh


### JAVA ###
Java/bin/%.class : Java/src/%.java
	$(JAVAC) $(JPACK) -d Java/bin/ $<

jrun: all
	java $(JAVAPACKAGE) Main.GUIsim

jrun_test: all
	$(TESTCMD) $(TESTS)



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

all: mkdir $(ERLANG_OBJECTS) $(JAVA_CLASSES)

Erlang/ebin/%.beam: Erlang/src/%.erl 
	erlc $(ERLC_FLAGS) -o Erlang/ebin/ $<

### RUNNING ###
erun: epmd_run all 
	$(ERUN) main start $(ESTOP) $(EEXTRA) $(ESMALLRUN)


### CLEANUP ###
clean:
	rm -Rf Erlang/ebin/*
	rm -Rf Erlang/src/*.beam
	rm -rf Java/bin/*

clean_doc:
	rm -rf Java/doc/*
	rm -rf Erlang/doc/*

### DOCUMENTATION ###
edoc: 
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"./Erlang/"' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

edoc_url:
	@echo 
	@echo "EDoc index page available at file://$(PWD)/Erlang/doc/index.html"
	@echo

jdoc:
	find ./Java/src/ -name "*.java" | xargs javadoc -d Java/doc -classpath Java/lib/OtpErlang.jar -sourcepath Java/src/ 


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

