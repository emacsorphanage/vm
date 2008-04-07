#!/bin/sh
# -*- shell-script -*-

bzr="bzr --no-plugins --no-aliases"
$bzr rocks > /dev/null || (echo "ERROR: cannot run bzr." && exit 1)
nick=`$bzr nick`
devo=`echo $nick | fgrep -q devo && echo devo`
revno=`$bzr revno`

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
