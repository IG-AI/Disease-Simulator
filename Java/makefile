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

jrun: all
	java $(JAVAPACKAGE) Main.GUIsim

jrun_test: all
	$(TESTCMD) $(TESTS)

### DOCUMENTATION ###
doc:
	find ./Java/src/ -name "*.java" | xargs javadoc -d Java/doc -classpath Java/lib/OtpErlang.jar -sourcepath Java/src/ 

### CLEANUP ###
clean:
	rm -rf bin/*
	rm -rf doc/*


