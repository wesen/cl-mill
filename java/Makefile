all: LispRecorder.jar


LIBDIR=~/siff-svn/processing/sketches/libraries
CLASSPATH=/Applications/Processing.app/Contents/Resources/Java/core.jar

LispRecorder.jar: ruinwesen/lisp/LispRecorder.java
	javac -cp "$(CLASSPATH)" ruinwesen/lisp/LispRecorder.java
	jar cvf LispRecorder.jar ruinwesen

install: LispRecorder.jar
	mkdir -p $(LIBDIR)/LispRecorder/library
	cp LispRecorder.jar $(LIBDIR)/LispRecorder/library 