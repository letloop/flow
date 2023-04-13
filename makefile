LETLOOP=letloop

example: liburing libloop
	LD_LIBRARY_PATH=$(PWD)/local/lib $(LETLOOP) exec $(PWD) example.scm -- 127.0.0.1 9999

uxample: liburing libloop
	LD_LIBRARY_PATH=$(PWD)/local/lib $(LETLOOP) exec $(PWD) uxample.scm -- 127.0.0.1 9999

liburing: local/include/liburing.h

local/include/liburing.h:
	mkdir -p local/src/
	rm -rf local/src/liburing/
	cd local/src/ && git clone https://github.com/axboe/liburing/
	cd local/src/liburing && git checkout liburing-2.3
	cd local/src/liburing && ./configure --prefix=$(PWD)/local/
	cd local/src/liburing && make -j $(shell nproc)
	cd local/src/liburing && make install

libloop: libloop.c local/include/liburing.h
	mkdir -p local/lib/
	cc -fPIC -shared -I$(PWD)/include/ libloop.c -o $(PWD)/local/lib/libloop.so
