#!/bin/bash

set -e

git clone git@github.com:dasudian/dsdin-api-docs.git /tmp/dsdin-api-docs
cp apps/aehttp/priv/swagger.json /tmp/dsdin-api-docs/
cd /tmp/dsdin-api-docs/

git add swagger.json;
STATUS=`git status --porcelain`

if [ -z "$STATUS" ]; then
    echo "Nothing to commit, docs did not change";
else
    git config user.name "CircleCI"
    git config user.email "circleci@dasudian.com"
    git commit -a -m "Update dsdin version: $CIRCLE_TAG docs CircleCI";
    git push origin master
fi
