

all: Ergoline*.class

Ergoline*.java: Ergoline.g4
	java org.antlr.v4.Tool Ergoline.g4

Ergoline*.class: Ergoline*.java
	javac Ergoline*.java

test: all
	java org.antlr.v4.gui.TestRig Ergoline program -gui < ../examples/hello.erg

clean:
	rm -f *.class *.java *.tokens *.interp