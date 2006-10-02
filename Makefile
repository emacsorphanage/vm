########### all user servicable parts are in Makefile.User ##################
include Makefile.User

############## no user servicable parts beyond this point ###################

# VM version
VMV =	$(shell sed -n -e 's/^.defconst vm-version "\([0-9]*\.[0-9]*\).*/\1/p' vm-version.el)

# no csh please
SHELL = /bin/sh

# byte compiler options
BYTEOPTS = ./vm-byteopts.el

# have to preload the files that contain macro definitions or the
# byte compiler will compile everything that references them
# incorrectly.  also preload a file that sets byte compiler options.
PRELOADS = -l $(BYTEOPTS) -l ./vm-version.el -l ./vm-message.el -l ./vm-macro.el -l ./vm-vars.el  

# compile with noninteractive and relatively clean environment
BATCHFLAGS = -batch -q -no-site-file -eval '(setq buffer-file-coding-system nil)'

# files that contain key macro definitions.  almost everything
# depends on them because the byte-compiler inlines macro
# expansions.  everything also depends on the byte compiler
# options file since this might do odd things like turn off
# certain compiler optimizations.
CORE = vm-message.el vm-macro.el vm-byteopts.el

# vm-version.elc needs to be first in this list, because load time
# code needs the Emacs/XEmacs MULE/no-MULE feature stuff.
SOURCES = vm-version.el $(wildcard *.el)
OBJECTS = $(SOURCES:.el=.elc)

UTILS = qp-decode qp-encode base64-decode base64-encode

.el.elc:
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-compile $<

all: vm.elc $(OBJECTS) $(UTILS) vm.info vm-pcrisis.info

recompile:
	$(EMACS) $(BATCHFLAGS) $(PRELOADS) -f batch-byte-recompile-directory .

noautoload:	$(OBJECTS) tapestry.elc
	@echo "building vm.elc (with all modules included)..."
	cat $(OBJECTS) tapestry.elc > vm.elc

debug:	$(SOURCES) tapestry.el
	@echo "building vm.elc (uncompiled, no autoloads)..."
	cat $(SOURCES) tapestry.el > vm.elc

install: all
	mkdirhier $(INFODIR) $(LISPDIR) $(PIXMAPDIR) $(BINDIR)
	cp vm.info vm.info-* vm-pcrisis.info $(INFODIR)
	cp *.elc $(LISPDIR)
	cp pixmaps/*.xpm $(PIXMAPDIR)
	cp $(UTILS) $(BINDIR)

vm.info: vm.texinfo
	@echo "making vm.info..."
	$(EMACS) $(BATCHFLAGS) -insert vm.texinfo -l texinfmt -f texinfo-format-buffer -f save-buffer
	@echo "(fmakunbound 'vm-its-such-a-cruel-world)" >> vm.el

vm-pcrisis.info: vm-pcrisis.texi
	@echo "making $@..."
	$(EMACS) $(BATCHFLAGS) -insert vm-pcrisis.texi -l texinfmt -f texinfo-format-buffer -f save-buffer

clean:
	rm -f vm-autoload.el vm.el *.elc \
	base64-decode base64-encode qp-decode qp-encode

vm.el: vm-autoload.elc tapestry.elc
	@echo "building $@ (with all modules set to autoload)..."
	@echo "(defun vm-its-such-a-cruel-world ()" > vm.el
	@echo "   (require 'vm-version)" >> vm.el
	@echo "   (require 'vm-startup)" >> vm.el
	@echo "   (require 'vm-vars)" >> vm.el
	@echo "   (require 'vm-autoload))" >> vm.el
	@echo "(vm-its-such-a-cruel-world)" >> vm.el
	@echo "(fmakunbound 'vm-its-such-a-cruel-world)" >> vm.el

noautoloads=vm.el vm-autoload.el
vm-autoload.el: $(filter-out $(noautoloads),$(SOURCES))
	@echo scanning sources to build autoload definitions...
	@echo "(provide 'vm-autoload)" > vm-autoload.el
	$(EMACS) $(BATCHFLAGS) -l ./make-autoloads -f print-autoloads $(filter-out $(noautoloads),$(SOURCES)) >> vm-autoload.el

utils: $(UTILS)

qp-encode: qp-encode.c
qp-decode: qp-decode.c
base64-encode: base64-encode.c
base64-decode: base64-decode.c

##############################################################################
snapshot: push patch ball single-files

VMPATCH=vm-$(VMV).patch
ELISPDIR=$(HOME)/html-data/www.robf.de/Hacking/elisp

push:
	bzr push

patch:
	-rm -f *.orig *.rej
	bzr log --verbose > ChangeLog
	echo 'Version: $$Id: '`bzr nick`-`bzr revno`'$$' > $(VMPATCH)
	echo "" >> $(VMPATCH)
	echo '*******************************************************************************' >> $(VMPATCH)
	cat patchdoc.txt >> $(VMPATCH); diff --ignore-all-space -u -P -x qp-encode -x qp-decode -x patchdoc.txt -x vm-autoload.el -x vm.el -x '*.elc' -x '#*' -x '*.gz' -x '*.patch' -x '*info*' -x ',*' $(HOME)/.hacking/vm-$(VMV) . | grep -v '^Only in'  | grep -v '^Binary files' >> $(VMPATCH); echo patch $(VMPATCH) written ...
	gzip -f $(VMPATCH)
	cp $(VMPATCH).gz $(ELISPDIR)
	touch $(ELISPDIR)/index.rml

ball:
	echo 'Version: $$Id: '`bzr nick`-`bzr revno`'$$' > ,id
	tar chfvz vmrf.tgz ,id *.c *.texi* *ChangeLog patchdoc.txt Makefile* make-autoloads *.el
	cp vmrf.tgz $(ELISPDIR)
	touch $(ELISPDIR)/index.rml

vm-pcrisis.tgz: vm-pcrisis.info
	dest="vm-pcrisis-"`grep vmpc-version vm-pcrisis.el | cut -d '"' -f 2`; \
	rm -rf $$dest; \
	mkdir $$dest; \
	updateWithId vm-pcrisis.el $$dest/vm-pcrisis.el; \
	updateWithId vm-pcrisis.texi $$dest/vm-pcrisis.texi; \
	cp vm-pcrisis.info $$dest/vm-pcrisis.info; \
	tar chfvz $$dest.tgz $$dest; \
	cp $$dest.tgz $(ELISPDIR); \
	touch $(ELISPDIR)/index.rml

# As long as I am maintaining tla and CVS at the same time 
single-files: $(ELISPDIR)/vm-mime.el \
            $(ELISPDIR)/vm-serial.el \
            $(ELISPDIR)/vm-summary-faces.el \
            $(ELISPDIR)/vm-avirtual.el \
            $(ELISPDIR)/vm-biff.el \
            $(ELISPDIR)/vm-grepmail.el \
            $(ELISPDIR)/vm-pine.el \
            $(ELISPDIR)/vm-ps-print.el \
            $(ELISPDIR)/vm-rfaddons.el

$(ELISPDIR)/%.el: %.el
	@echo Updating $<
	@updateWithId $< $@ 
	@touch $(ELISPDIR)/index.rml

##############################################################################
update:
	if test -e '.bzr'; then echo ERROR: No updates in checkouts; exit -1; fi;
	wget -N http://www.robf.de/Hacking/elisp/vmrf.tgz
	if test vmrf.tgz -nt vmrf-newer.tgz; then cp vmrf.tgz vmrf-newer.tgz; tar --exclude=Makefile.User -x -v -f -z vmrf.tgz; fi;
	rm -f vm-autoload.el*
	make

get-other-extensions:
	wget -N http://www.splode.com/~friedman/software/emacs-lisp/src/vcard.el
	wget -N http://www.splode.com/~friedman/software/emacs-lisp/src/vm-vcard.el
	wget -N http://de.geocities.com/ulf_jasper/lisp/u-vm-color.el
	wget -N http://www.cis.upenn.edu/~bjornk/bogofilter/vm-bogofilter.el
	wget -N http://groups-beta.google.com/group/gnu.emacs.vm.info/attach/f37a5e0ce73a4687/vm-message-history.el.%7E1.2%7E?part=2&hl=en

