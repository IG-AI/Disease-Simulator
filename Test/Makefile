EC =erl -compile
ERUN =erl -noshell -run
ESTOP =-s init stop -extra


all: Main.class starter.beam MapParser.class

Main.class: Main.java
	$(JC) $(JINTERJARS) $^

TestCommunication.class: TestCommunication.java
	$(JC) $(JINTERJARS) $^

JavaErlangCommunication.class: JavaErlangCommunication.java
	$(JC) $(JINTERJARS) $^

MapParser.class: MapParser.java
	$(JC) $(JINTERJARS) $^

adj_map.beam: adj_map.erl
	$(EC) $^

jtest: TestCommunication.class JavaErlangCommunication.class MapParser.class
	java $(JINTERJARS) TestCommunication

jrun: Main.class MapParser.class
	java $(JINTERJARS) Main

erun: adj_map.beam
	$(ERUN) adj_map adj_map  $(ESTOP)

erun_one: starter.beam
	$(ERUN) starter start  $(ESTOP) map_one.bmp

run_all: starter.beam Main.class MapParser.class
	gedit &
	java $(JINTERJARS) Main &
	$(ERUN) starter start  $(ESTOP) map_zero.bmp
