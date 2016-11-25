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

## copies UNCOMMITTED but TRACKED files.
git ls-files . | tar cT - | tar -x -C "../${PACKAGE}.gitex"
cd ..

cd ${PACKAGE}.gitex/vignettes

## perl -pi.bak  -e 's/bibliography\{0.*rockchalk\}/bibliography{rockchalk}/' rockchalk.Rnw

lyx -f -e pdf2 Rstyle.lyx
lyx -f -e pdf2 rockchalk.lyx
lyx -f -e pdf2 Rchaeology.lyx
mkdir ../inst/doc
cp -f rockchalk.pdf ../inst/doc
cp -f Rstyle.pdf ../inst/doc
cp -f Rchaeology.pdf ../inst/doc
cd ../..

R --vanilla -f runRoxygen2.R


R CMD build rockchalk.gitex --resave-data 


read -p "Install: OK? (y or n)" result
if [ $result = "y" ]; then
R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz
fi

read -p "Run check: OK? (y or n)" result

if [ $result = "y" ];  then
R CMD check --as-cran ${PACKAGE}_${VERSION}.tar.gz
fi

read -p "Erase git temporary: OK? (y or n)" result
if [ $result = "y" ]; then
	rm -rf ${PACKAGE}.gitex
	rm -rf ${PACKAGE}.Rcheck
fi

echo "Consider upload to KRAN"

