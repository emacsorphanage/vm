#!/bin/sh

make
bzr=bzr --no-plugins --no-aliases
rdir=`$bzr nick`
dir="release/$rdir"
rm -rf $dir
$bzr export $dir
cp configure $dir
(cd lisp; rm vm-revno.el; make vm-revno.el)
cp lisp/vm-revno.el $dir/lisp
echo 'Version: $Id: '`$bzr nick`-`$bzr revno`'$' > $dir/id
cd release
tar cvfz $rdir.tgz $rdir
cd ..
if [ -n "$1" -a -e "$1" ]; then
  ./$1 $dir.tgz;
fi
