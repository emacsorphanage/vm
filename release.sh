#!/bin/sh -x

bzr="bzr --no-plugins --no-aliases"
nick=`$bzr nick`
devo=`echo $nick | fgrep -q devo && echo devo`
revno=`$bzr revno`
echo $devo
if [ "$devo" = "devo" ] ; then
  tag=$nick
  rdir=$tag-$revno
  version=$rdir
else
  tag=(`$bzr tags | tail -1`)
  if [ "${tag[1]}" != "$revno" ]; then
    echo "ERROR: No tag present at the head revision."
    echo "ERROR: First you must create a release tag!"
    exit -1
  fi
  tag=${tag[0]}
  rdir=$tag-$revno
  version=$tag
fi

cat > lisp/vm-revno.el <<EOFREVNO
;;; This is a generated file, do not edit it!
(setq vm-version "$version")
(setq vm-version-info '(
`bzr version-info --custom --template='  (revdate "{date}")\n  (revno {revno})\n  (revid "{revision_id}")\n  (branch_nick "{branch_nick}")'`
  (author "`$bzr whoami`")
))
EOFREVNO

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
fi

dir="release/$rdir"
rm -rf $dir
mkdir -p release
$bzr export $dir
cp configure $dir
cp lisp/vm-revno.el $dir/lisp

cd release
tar cvfz $rdir.tgz $rdir
cd ..

if [ -n "$1" -a -e "$1" ]; then
  ./$1 $dir.tgz $nick $revno
fi

if [ "$1" != "test" ]; then 
  $bzr push
fi
