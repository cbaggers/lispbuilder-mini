#### -*- Mode: Makefile -*-
#### Author: Matthew Danish <mrd@debian.org>
#### C glue GNU makefile

#### Modified by: Luke Crook <luke@balooga.com>


include ../Makefile.opts
include ../Makefile.common

all: $(SHLIB_NAME).$(SHLIB_SUFFIX)
	cp $(SHLIB_NAME).$(SHLIB_SUFFIX) ../bin
	mv $(SHLIB_NAME).$(SHLIB_SUFFIX) ../build

$(LIB_NAME).$(OBJ_SUFFIX): $(LIB_NAME).c
	$(CC) $(CPPFLAGS) $(SDL_GLUE_CFLAGS) -c -o $@ $<

$(SHLIB_NAME).$(SHLIB_SUFFIX): $(LIB_NAME).$(OBJ_SUFFIX)
	$(CC) $(SDL_GLUE_LDFLAGS) -o $@ $<

clean:
	rm -f $(STUBS)
