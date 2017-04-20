SPACE :=
SPACE +=
JAVAPACKAGE = .:Java/lib/OtpErlang.jar:Java/bin
JAVATESTPACKAGES = .:Java/lib/junit-4.12.jar:Java/lib/OtpErlang.jar

TESTSRC = $(shell find src/test -name "*.java" -printf "test.%f ")
TESTS = $(subst .java,,$(TESTSRC))
TESTCMD = java -cp ./Java:Java/bin:./Java/lib/junit-4.12.jar:./Java/lib/hamcrest-core-1.3.jar org.junit.runner.JUnitCore

.PHONY: run compile clean sources.txt sources_test.txt

jcompile: sources.txt
	javac -cp $(JAVAPACKAGE) @Java/sources.txt -d Java/bin

jcompile_test: sources_test.txt
	javac -cp $(JAVATESTPACKAGES) @Java/sources_test.txt -d Java/bin

jrun: jcompile
	java -cp $(JAVAPACKAGE) Main.GUIsim

jrun_test: jcompile_test
	$(TESTCMD) $(TESTS)

sources.txt:
	find Java -name "*.java" > Java/sources.txt

sources_test.txt:
	find Java -name "*.java" > Java/sources_test.txt

clean:
	rm -rf Java/bin/*
