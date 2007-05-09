#!/bin/sh

make
rdir=`bzr --no-plugins nick`
dir="release/$rdir"
rm -rf $dir
bzr --no-plugins export $dir
cp configure $dir
(cd lisp; rm vm-revno.el; make vm-revno.el)
cp lisp/vm-revno.el $dir/lisp
echo 'Version: $Id: '`bzr --no-plugins nick`-`bzr --no-plugins revno`'$' > $dir/id
cd release
tar cvfz $rdir.tgz $rdir
cd ..
if [ -n "$1" -a -e "$1" ]; then
  ./$1 $dir.tgz;
fi
