#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir -p gh-pages

# compile JS using Elm
for i in animation2d copter3d css3d planet3d shadertoy tangram; do
  elm make $i.elm --yes --output ../gh-pages/$i.html
done

# copy the textures
cp -R texture ../gh-pages/

# configure domain
cd ../gh-pages
echo "unsoundscapes.com" >> CNAME

# init branch and commit
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-webgl-playground.git" master:gh-pages
