#!/bin/bash

for dir in ./sprite-projects/render/*/
do
    fileName=${dir%*/}
    fileName=${fileName##*/}
    tmpFile=/tmp/$fileName-spritesheet.png
    outFile=./assets/img/$fileName-spritesheet.png
    convert -append -background transparent $dir/*.png $tmpFile
    p2w=`convert $tmpFile -format "%[fx:2^(ceil(log(w)/log(2)))]" info:`
    p2h=`convert $tmpFile -format "%[fx:2^(ceil(log(h)/log(2)))]" info:`
    convert $tmpFile -background transparent -gravity NorthWest -extent ${p2w}x${p2h} $outFile
    rm $tmpFile
done