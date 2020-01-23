all: jar

SRC_FILES := $(shell find src/ -type f -name '*.scala')

test: src/test/resources/Test.class

src/test/resources/Test.class: src/test/resources/Test.java
	javac $^

JAR := target/scala-2.13/liquid-assembly-0.1.jar

$(JAR): $(SRC_FILES)
	sbt assembly

jar: $(JAR)
