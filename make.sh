#!/bin/bash
if [ ! -d ebin ] ; then
	mkdir ebin
fi
erl -make
make -C c_src/posix
