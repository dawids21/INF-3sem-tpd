#!/bin/bash
git stash;
git pull;
git stash pop;
git add -A;
git commit -m "$(date)";
git push;
