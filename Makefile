# Erlang parts shamelessly stolen from Karl Marklund

# SPECIAL VARS #
MAP?=map_one #Default value for map
IND?=1000 #Default number of individuals
TICKS?=5000 #Default length of the simulation
INF?=100 #Default number of infected individuals
VAC?=300 #Default number of vaccinated individuals
RANGE?=3 #Default radius for the range in which processes can be infected
PROB?=1.0 #Default chanse of infection
LIFE?=100 #Default nummber of ticks
MOVE?=bounce #Default movement behaviour, "bounce", "path" or "bounce_random"
END?=dead #Default end condition
TVAC?=on #Default vaccine setting
REC?=play #Default record behaviour
RECFIL?=none #Default recording file
RAND?=auto #Default random behaviour

### ERLANG ###
ERLANG_STOP = -s init stop
ERLANG_RUN = erl -noshell -pa Erlang/ebin -run
ERLANG_EXTRA = -extra
ERLANGC=erlc
ERLANGC_FLAGS=-Wall +debug_info
ERLANG_SOURCES=$(wildcard Erlang/src/*.erl)
ERLANG_HEADERS=$(wildcard Erlang/src/*.hrl)
ERLANG_OBJECTS:=$(ERLANG_SOURCES:Erlang/src/%.erl=Erlang/ebin/%.beam)

### JAVA ###
JAVA_PACKAGE_SRC = -cp .:Java/lib/OtpErlang.jar:Java/bin:Java/src
JAVA_PACKAGE = -cp .:Java/lib/OtpErlang.jar:Java/bin
JAVAC = javac
JAVA_SOURCES = $(wildcard Java/src/*/*.java)
JAVA_CLASSES = $(JAVA_SOURCES:Java/src/%.java=Java/bin/%.class)
JAVA_MAIN = Main.Main
#TEST#
JAVA_TESTSRC = $(shell find src/test -name "*.java" -printf "test.%f ")
JAVA_TESTS = $(subst .java,,$(JAVA_TESTSRC))
JAVA_TESTCMD = java -cp ./Java:Java/bin:./Java/lib/junit-4.12.jar:./Java/lib/hamcrest-core-1.3.jar org.junit.runner.JUnitCore

### SPECIAL FLAGS ###

.PHONY: clean clean_exec clean_doc mkdir empd_run all ecompile jcompile run erun erun_tiny jrun test testv jrun_test edoc edoc_url jdoc

# MAKE DIRECTORIES # 
mkdir:	#Needed because subdirs seem not to get created during compilation of Java and Erlang.
	@mkdir -p Erlang/ebin Erlang/doc
	@mkdir -p Java/bin Java/doc
	@mkdir -p recordings
	@mkdir -p logs

# EPMD # 
epmd_run:	#Startup epmd -daemon, or communication will fail.
	@Bash/epmd_startup.sh



# COMPILATION #
all: ecompile jcompile

run: epmd_run all
	java $(JAVA_PACKAGE) $(JAVA_MAIN) $(JAVA_DEFAULT_PARAMS) &
	$(ERLANG_RUN) main start $(ERLANG_STOP) $(ERLANG_EXTRA) $(ERLANG_DEFAULT_PARAMS)

### JAVA ###

JAVA_DEFAULT_PARAMS = $(REC) $(RECFIL)

jcompile: mkdir $(JAVA_CLASSES)

Java/bin/%.class : Java/src/%.java
	$(JAVAC) $(JAVA_PACKAGE_SRC) -d Java/bin/ $<

jrun: epmd_run jcompile
	java $(JAVA_PACKAGE) $(JAVA_MAIN) $(JAVA_DEFAULT_PARAMS)

jrun_test: jcompile
	$(JAVA_TESTCMD) $(JAVA_TESTS)

# To run the GUI
gui: all epmd_run
	java -jar Project-snowfox.jar

### ERLANG ###

ERLANG_DEFAULT_PARAMS = $(MAP) $(IND) $(TICKS) $(INF) $(VAC) $(RANGE) $(PROB) $(LIFE) $(MOVE) $(END) $(TVAC) $(REC) $(RAND)# ARGS FOR ERLANG

# SPECIAL RUNS #
erun_tiny: epmd_run all
	$(ERLANG_RUN) main start $(ERLANG_STOP) $(ERLANG_EXTRA) $(ETINY)

# COMPILATION #
ecompile: mkdir $(ERLANG_OBJECTS)

Erlang/ebin/%.beam: Erlang/src/%.erl 
	$(ERLANGC) $(ERLANGC_FLAGS) -o Erlang/ebin/ $<

# RUNNING #
erun: epmd_run ecompile
	$(ERLANG_RUN) main start $(ERLANG_STOP) $(ERLANG_EXTRA) $(ERLANG_DEFAULT_PARAMS)


### CLEANUP ###
clean:
	rm -rf Erlang/ebin/*
	rm -rf Erlang/src/*.beam
	rm -rf Java/bin/*
	rm -rf Java/doc/*
	rm -rf Erlang/doc/*
	rm -rf logs/*
	rm -rf recordings/*

clean_exec:
	rm -rf Erlang/ebin/*
	rm -rf Erlang/src/*.beam
	rm -rf Java/bin/*

clean_doc:
	rm -rf Java/doc/*
	rm -rf Erlang/doc/*

clean_rec:
	rm -rf recordings/*

clean_log:
	rm -rf logs/*

### DOCUMENTATION ###
edoc: 
	erl -noshell -run edoc_run application "'$(APPNAME)'"  '"./Erlang/"' '[{def,{vsn,"$(VSN)"}}, {stylesheet, "my_style.css"}]'

edoc_url:
	@echo 
	@echo "EDoc index page available at file://$(PWD)/Erlang/doc/index.html"
	@echo

jdoc:
	find ./Java/src/ -name "*.java" | xargs javadoc -d Java/doc -classpath Java/lib/OtpErlang.jar -sourcepath Java/src/ 


### ERLANG TEST ###
# EUnit #

# Make a comma separated list:
# http://ftp.gnu.org/old-gnu/Manuals/make-3.79.1/html_chapter/make_8.html

comma:= ,
empty:=
space:= $(empty) $(empty)

ERLANG_OBJECTS_LIST:= $(subst $(space),$(comma),$(ERLANG_OBJECTS:Erlang/ebin/%.beam=%))

test: mkdir $(ERLANG_OBJECTS)
	erl -noshell -pa Erlang/ebin -eval 'eunit:test([$(ERLANG_OBJECTS_LIST)], [])' -s init stop

testv: mkdir $(ERLANG_OBJECTS)
	erl -noshell -pa Erlang/ebin -eval 'eunit:test([$(ERLANG_OBJECTS_LIST)], [verbose])' -s init stop

test_%: mkdir Erlang/ebin/%.beam
	erl -noshell -pa Erlang/ebin -eval "eunit:test($(subst test_,, $@), [])" -s init stop

testv_%: mkdir Erlang/ebin/%.beam
	erl -noshell -pa Erlang/ebin -eval "eunit:test($(subst testv_,, $@), [verbose])" -s init stop
