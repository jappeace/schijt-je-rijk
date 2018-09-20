#! /bin/sh

mkdir release
cp -R img release/img
elm-make Main.elm --output=release/start.html
cp LICENSE release/LICENSE.txt
cp readme.org release/readme.org
# just to make sure windows people can open it
cp readme.org release/leesmij.txt

tar -czvf release.tar.gz release
