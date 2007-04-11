#!/bin/sh

make
dir="release/`bzr nick`"
rm -rf $dir
bzr export $dir
cp configure $dir
(cd lisp; make vm-revno.el)
cp lisp/vm-revno.el $dir/lisp
echo 'Version: $$Id: = '`bzr nick`-`bzr revno ` > $dir/id
tar cvfz $dir.tgz $dir
if [ -e $1 ]; then
  ./$1 $dir.tgz;
fi
