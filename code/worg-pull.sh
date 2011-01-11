#!/usr/bin/env bash
# File: worg-pull.sh 
# Author: Jeffrey Horn <jrhorn424@gmail.com> 
# Description: This script updates the Worg repo in a more robust way,
# avoiding issues like the one mentioned at
# http://notes.envato.com/developers/rebasing-merge-commits-in-git/

git fetch origin
git rebase -p origin/master
