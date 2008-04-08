#!/usr/bin/env bash
# -*- shell-script -*-

. ../getversion.sh

cat > vm-revno.el <<EOFREVNO
;;; This is a generated file, do not edit it!
(setq vm-version "$version")
(setq vm-version-info '(
`bzr version-info --custom --template='  (revdate "{date}")\n  (revno {revno})\n  (revid "{revision_id}")\n  (branch_nick "{branch_nick}")'`
  (author "`$bzr whoami`")
))
EOFREVNO
