all:
	sbt "run --apk examples/example-1.apk --spec examples/example-1.txt --out examples/example-1.json"

test: src/test/resources/Test.class

src/test/resources/Test.class: src/test/resources/Test.java
	javac $^
