
all: libmhashtest.so MHashTest.exe

MHash.dll: MHash.cs
	mcs -debug -unsafe -target:library -out:MHash.dll MHash.cs

MHashTest.exe: MHashTest.cs MHash.dll
	mcs -debug -unsafe -target:exe -out:MHashTest.exe MHashTest.cs -r:MHash.dll

libmhashtest.so: mhash.c mhashtest.c
	gcc -g `pkg-config --cflags glib-2.0` -shared -o libmhashtest.so mhash.c mhashtest.c `pkg-config --libs glib-2.0`