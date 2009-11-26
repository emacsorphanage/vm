#!/usr/bin/env bash
# -*- shell-script -*-

bzr log $1 | head -5 | egrep '^timestamp: ' | cut -c12-
