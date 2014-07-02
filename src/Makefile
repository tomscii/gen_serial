BEAM_OUT=../ebin
HTML_OUT=../doc

MV=mv
ERLC=erlc -W

EDOC_SRC=$(HOME)/Erlang/edoc-0.1/
EDOC_LIB=$(HOME)/Erlang
EDOC=erl \
	-noinput \
	-pa $(EDOC_LIB)/edoc-0.1/ebin \
	-pa $(EDOC_LIB)/syntax_tools-1.2/ebin \
	-pa $(EDOC_LIB)/xmerl-0.17/ebin

OBJ_EBIN:=\
	gen_serial.beam \
	gen_serial_test.beam \

OBJ_DOC:=\
	stylesheet.css \
	gen_serial.html \
	gen_serial_test.html \

all: 		_all
clean:		_clean

doc:		_doc
clean_doc:	_clean_doc

######################################################################

.SUFFIXES: .erl .beam .erl .html
VPATH:=$(BEAM_OUT):$(HTML_OUT)

.PHONY: _all all
.PHONY: _clean clean

.PHONY: _doc doc
.PHONY: _clean_doc clean_doc

_all: $(OBJ_EBIN)
_doc: $(OBJ_DOC)

_clean:
	-rm -f $(BEAM_OUT)/*.beam
	-rm -f ../priv/bin/*

_clean_doc:
	-rm -f $(HTML_OUT)/*.html

$(BEAM_OUT):
	mkdir $@

.erl.beam: $(BEAM_OUT)
	$(ERLC) -o $(BEAM_OUT) $<

.erl.html:
	$(EDOC) -run edoc file $< -run init stop
	$(MV) $*.html $(HTML_OUT)

gen_serial.beam: gen_serial.erl
gen_serial_test.beam: gen_serial_test.erl

gen_serial.html: gen_serial.erl
gen_serial_test.html: gen_serial_test.erl
stylesheet.css: $(EDOC_SRC)/stylesheet.css
	cp $< $(HTML_OUT)
