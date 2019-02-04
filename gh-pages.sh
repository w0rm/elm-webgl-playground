#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir -p gh-pages

# compile JS using Elm
for i in animation2d circle copter3d css3d planet3d shadertoy tangram osloelmday; do
  elm make $i.elm --optimize --output gh-pages/$i.html
done

# copy the texture
cp animation2d.png gh-pages/

# init branch and commit
cd gh-pages
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-webgl-playground.git" master:gh-pages
