all: pot-glue.dylib

pot-glue.dylib: pot-glue.c
	gcc -dynamiclib -flat_namespace -o pot-glue.dylib pot-glue.c -I/sw/include -L/sw/lib -lpotrace

#	gcc -dynamiclib -flat_namespace -undefined suppress -o libpotrace.dylib curve.o trace.o decompose.o potracelib.o