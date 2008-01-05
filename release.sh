#!/bin/sh

bzr diff || exit 1
make || exit 1
bzr="bzr --no-plugins --no-aliases"
nick=(`$bzr tags | tail -1`)
nickrevno=${nick[1]}
nick=${nick[0]}
revno=`$bzr revno`
if [ "$nickrevno" != "$revno" ]; then
  echo "ERROR: No tag present at the head revision."
  echo "ERROR: First you must create a release tag!"
  exit -1
fi
rdir=$nick-$revno
dir="release/$rdir"
rm -rf $dir
mkdir -p release
$bzr export $dir
cp configure $dir
(cd lisp; rm vm-revno.el; make vm-revno.el)
cp lisp/vm-revno.el $dir/lisp
echo 'Version: $Id: '$rdir'$' > $dir/id
cd release
tar cvfz $rdir.tgz $rdir
cd ..
if [ -n "$1" -a -e "$1" ]; then
  ./$1 $dir.tgz $nick $revno;
fi
