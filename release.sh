#!/bin/sh

make
dir="release/`bzr nick`"
rm -rf $dir
bzr export $dir
cp configure $dir
(cd lisp; make vm-revno.el)
cp lisp/vm-revno.el $dir/lisp
tar cvfz $dir.tgz $dir
