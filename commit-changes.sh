#!/usr/bin/env bash

if [[ -z $(git status -s) ]]
then
  echo "No changes detected, not bothering"
else
  git config --local user.email "test@github.com"
  git config --local user.name "GitHub Action test"
  git add ./docs
  git commit -m "Add periodic changes"
fi
