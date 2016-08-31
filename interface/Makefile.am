AUTOMAKE_OPTIONS = nostdinc subdir-objects

noinst_PROGRAMS = extract_interface
lib_LTLIBRARIES =
if WANT_LIBISL_JNI
lib_LTLIBRARIES += libisl_jni.la
endif
if WANT_LIBISL_JNI_PORTABLE
lib_LTLIBRARIES += libisl_jni_portable.la
endif

AM_CXXFLAGS = $(CLANG_CXXFLAGS) -fno-rtti
AM_LDFLAGS = $(CLANG_LDFLAGS)

includes = -I$(top_builddir) -I$(top_srcdir) \
	-I$(top_builddir)/include -I$(top_srcdir)/include

extract_interface_CPPFLAGS = $(includes)
extract_interface_SOURCES = \
	generator.h \
	generator.cc \
	python.h \
	python.cc \
	java.h \
	java.cc \
	extract_interface.h \
	extract_interface.cc
extract_interface_LDADD = \
	-lclangFrontend -lclangSerialization -lclangParse -lclangSema \
	$(LIB_CLANG_EDIT) \
	-lclangAnalysis -lclangAST -lclangLex -lclangBasic -lclangDriver \
	$(CLANG_LIBS) $(CLANG_LDFLAGS)

CLEANFILES = isl.py

libisl_jni_la_SOURCES  = java/gen/isl_jni.c
libisl_jni_la_CPPFLAGS = $(includes) $(addprefix -I,$(JNI_INCLUDE)) $(addprefix -I,$(JNIMD_INCLUDE))
libisl_jni_la_LIBADD   = $(top_builddir)/libisl.la
libisl_jni_la_LDFLAGS  = -version-info @versioninfo@

libisl_jni_portable_la_SOURCES  = $(libisl_jni_la_SOURCES)
libisl_jni_portable_la_CPPFLAGS = $(libisl_jni_la_CPPFLAGS)
libisl_jni_portable_la_LIBADD   = $(top_builddir)/libisl_conv.la
libisl_jni_portable_la_LDFLAGS  = $(libisl_jni_la_LDFLAGS)

JAVAC_FLAGS=
SCALAC_FLAGS=-cp isl.jar

.PHONY: test
test: extract_interface$(EXEEXT)
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h

isl.py: extract_interface$(EXEEXT) isl.py.top
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h \
		--output-dir=python --language=python
	cat isl.py.top python/isl.py > isl.py


isl.jar java/gen/isl_jni.c: extract_interface$(EXEEXT) java/Manifest.txt
	./extract_interface$(EXEEXT) $(includes) $(srcdir)/all.h --language=java --output-dir=java/gen
	$(MKDIR_P) java/target/src
	$(CP) -r java/src/isl java/target/src/
	$(CP) -r java/gen/isl java/target/src/
	$(JAVAC) $(JAVAC_FLAGS) -d java/target `find java/target/src -name \*.java`
	$(JAR) -cfm isl.jar java/Manifest.txt -C java/target .

java/Manifest.txt:
	$(MKDIR_P) java
	echo "" >$@

isl-scala.jar: isl.jar scala/src/isl/*.scala
	$(CP) isl.jar isl-scala.jar
	$(MKDIR_P) scala/target/src
	$(CP) -r scala/src scala/target/
	$(SCALAC) $(SCALAC_FLAGS) -d scala/target `find scala/target/src -name \*.scala`
	$(JAR) -uf $@ -C scala/target .

# Do not delete then java/gen directory; make needs
# the java/gen/.deps directory to work correctly.
.PHONY: clean
clean:
	$(RM) -rf python java/gen/* java/target scala/target
	$(RM) -f isl.py java/Manifest.txt isl.jar isl-scala.jar *.o

dist-hook: isl.py
	cp isl.py $(distdir)/
