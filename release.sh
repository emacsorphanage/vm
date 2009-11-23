#!/usr/bin/env bash
# -*- shell-script -*-

. ./getversion.sh


# now check for uncommitted changes
if [ "$1" != "test" ]; then 
  bzr diff || exit 1
fi

# check for an error less build
if [ "$1" != "test" ]; then 
  # make sure we delete the existing files containing version info
  rm -f lisp/vm-revno.el info/version.texi
  make all lisp/vm-revno.el info/version.texi || exit 1
fi

# just create the version-info, no bundle 
if [ "$1" == "version-info" ]; then 
  exit 0
fi

dir="release/$rdir"
rm -rf $dir
mkdir -p release
$bzr export $dir

cp configure $dir
mv lisp/vm-revno.el $dir/lisp
mv info/version.texi $dir/info
rm $dir/getversion.sh $dir/release.sh $dir/lisp/vm-revno.sh
cd release
tar cvfz $rdir.tgz $rdir
cd ..

if [ -n "$1" -a -e "$1" ]; then
  ./$1 $dir.tgz $nick $revno
fi

if [ "$1" != "test" ]; then 
  $bzr push --overwrite
fi
