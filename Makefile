SPACE :=
SPACE +=
JPACK = -cp .:Java/lib/OtpErlang.jar:Java/bin:Java/src
JAVAPACKAGE = -cp .:Java/lib/OtpErlang.jar:Java/bin
JAVATESTPACKAGES = -cp .:Java/lib/junit-4.12.jar:Java/lib/OtpErlang.jar

TESTSRC = $(shell find src/test -name "*.java" -printf "test.%f ")
TESTS = $(subst .java,,$(TESTSRC))
TESTCMD = java -cp ./Java:Java/bin:./Java/lib/junit-4.12.jar:./Java/lib/hamcrest-core-1.3.jar org.junit.runner.JUnitCore
JAVAC = javac
sources = $(wildcard Java/src/*/*.java)
classes = $(sources:Java/src/%.java=Java/bin/%.class)

### SPECIAL RULES ###
mkdir:
	@mkdir -p Java/bin Java/doc

.PHONY: run compile clean sources.txt sources_test.txt


### SLOW BUT DO NOT RECOMPILE UNNECESSARY ###
all : mkdir $(classes)

Java/bin/%.class : Java/src/%.java
	$(JAVAC) $(JPACK) -d Java/bin/ $<

jhr: all
	java $(JAVAPACKAGE) Main.GUIsim

jhrt: all
	$(TESTCMD) $(TESTS)

### FASTER BUT WILL ALWAYS RECOMPILE ###
jcompile: mkdir sources.txt
	javac $(JAVAPACKAGE) @Java/sources.txt -d Java/bin
jcompile_test: sources_test.txt
	javac $(JAVATESTPACKAGES) @Java/sources_test.txt -d Java/bin

jrun: jcompile
	java $(JAVAPACKAGE) Main.GUIsim

jrun_test: jcompile_test
	$(TESTCMD) $(TESTS)

sources.txt:
	find Java -name "*.java" > Java/sources.txt

sources_test.txt:
	find Java -name "*.java" > Java/sources_test.txt

### DOCUMENTATION ###
doc:
	find ./Java/src/ -name "*.java" | xargs javadoc -d Java/doc -classpath Java/lib/OtpErlang.jar -sourcepath Java/src/ 

### CLEANUP ###
clean:
	rm -rf Java/bin/*
	rm -rf Java/doc/*


