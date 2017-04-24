sources = $(wildcard *.java)
classes = $(sources:%.java=%.class)

all: $(classes)

%.class : %.java
	javac $<


jrun: all
	java Main

.PHONY: clean

clean:
	rm -f *.class
	rm -f *~
