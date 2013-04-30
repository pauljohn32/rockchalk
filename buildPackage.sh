PACKAGE="rockchalk"

VERSION=$(awk -F": +" '/^Version/ { print $2 }' ${PACKAGE}/DESCRIPTION)

rm -rf ${PACKAGE}.gitex;

mkdir ${PACKAGE}.gitex
cd ${PACKAGE}

cd vignettes
echo "$pwd"

lyx -f -e sweave Rstyle.lyx;
lyx -f -e sweave rockchalk.lyx;
lyx -f -e sweave Rchaeology.lyx;

cd ..

##git archive master | tar -x -C "../${PACKAGE}.gitex"
## copies UNCOMMITTED but TRACKED files.
git ls-files . | tar cT - | tar -x -C "../${PACKAGE}.gitex"
cd ..

## cd ${PACKAGE}.gitex/vignettes


## perl -pi.bak  -e 's/bibliography\{0.*rockchalk\}/bibliography{rockchalk}/' rockchalk.Rnw
## cp -f rockchalk.pdf ../inst/doc
## cp -f Rstyle.pdf ../inst/doc

## cp -f Rchaeology.pdf ../inst/doc
## cd ../..

R --vanilla -f runRoxygen2.R


R CMD build rockchalk.gitex

read -p "enter name of tarball: " result

R CMD check --as-cran $result

