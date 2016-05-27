all:
	mkdir -p ebin
	erl -make
	make -C c_src/posix

clean:
	rm -r ebin
	make -C c_src/posix clean
