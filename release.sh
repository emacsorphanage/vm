#!/usr/bin/env bash -x
# -*- shell-script -*-

. ./getversion.sh


# now check for uncommitted changes
if [ "$1" != "test" ]; then 
  bzr diff || exit 1
fi

# just create the version-info, no bundle 
if [ "$1" != "version-info" ]; then 
  exit 0
fi

# check for an error less build
if [ "$1" != "test" ]; then 
  make || exit 1
  # make sure we delete the existing file
  rm -f lisp/revno.el
  make lisp/revno.el || exit 1
fi

dir="release/$rdir"
rm -rf $dir
mkdir -p release
$bzr export $dir

cp configure $dir
mv lisp/revno.el $dir/lisp

cd release
tar cvfz $rdir.tgz $rdir
cd ..

if [ -n "$1" -a -e "$1" ]; then
  ./$1 $dir.tgz $nick $revno
fi

if [ "$1" != "test" ]; then 
  $bzr push
fi
