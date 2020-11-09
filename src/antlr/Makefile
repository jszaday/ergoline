OUTPUT_PATH=../main/java/edu/illinois/cs/ergoline
OUTPUT_JAVA=$(OUTPUT_PATH)/Ergoline*.java
OUTPUT_CLASSES=$(OUTPUT_PATH)/Ergoline*.class

all: $(OUTPUT_CLASSES)

$(OUTPUT_JAVA): Ergoline.g4
	java org.antlr.v4.Tool -o $(OUTPUT_PATH) -package edu.illinois.cs.ergoline Ergoline.g4

$(OUTPUT_CLASSES): $(OUTPUT_JAVA)
	javac $(OUTPUT_JAVA)

test: all
	cd $(OUTPUT_PATH)
	java org.antlr.v4.gui.TestRig Ergoline program -gui < ../../examples/hello.erg

clean:
	rm -f $(OUTPUT_CLASSES) $(OUTPUT_JAVA) $(OUTPUT_PATH)/*.tokens $(OUTPUT_PATH)/*.interp
