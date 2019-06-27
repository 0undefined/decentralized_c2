CC=erlc
CFLAGS=+debug_info
RUNFLAGS=-connect_all false -proto_dist_inet_tls
TMP_DIR=tmp
SOURCE:=$(wildcard src/*.erl)
BEAM:=$(notdir $(SOURCE:.erl=.beam))


compile: $(BEAM)

%.beam: src/%.erl
	$(CC) $(CFLAGS) $<

run: compile
	epmd -daemon
	erl $(RUNFLAGS) -s main start

debug: compile
	bash src/spawn_nodes.sh
	@touch src/__init__.py
	erl $(RUNFLAGS) -env ERL_LIBS lib -s debug start loop

clean:
	rm -f *.dump *.beam idfile* src/*.pyc
	rm -rf src/__pycache__
	rm -f src/__init__.py
	rm -rf $(TMP_DIR)/*
	rm -f *.pdf *.gv *.zip

tmp:
	mkdir -p $(TMP_DIR)
	sudo mount -t tmpfs -o size=512M tmpfs $(TMP_DIR)

setup: tmp
	git submodule init
	git submodule update
	make -kC lib/erlport/ release

zip: clean
	make -kC lib/erlport clean
	mkdir -p source
	cp -r src/ lib/ Makefile source
	zip -r source.zip source
	rm -r source
