#! /bin/sh

cabal test

test -d dist/tests || mkdir dist/tests

echo '<html><body><ul>' > dist/tests/index.html

for f in $(find tests -name "*.alava")
do
    cabal run -v0 main -- html "$f" > "dist/${f}.html"
    echo '<li><a href="'../$f'.html">'$f'</a></li>' >> dist/tests/index.html
done

echo '</ul></body></html>' >> dist/tests/index.html
