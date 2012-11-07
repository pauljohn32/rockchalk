rm -rf rockchalk.svnex

svn export rockchalk rockchalk.svnex
cd rockchalk.svnex/vignettes
lyx -e pdf2 rockchalk.lyx
lyx -e sweave rockchalk.lyx
## perl -pi.bak  -e 's/bibliography\{0.*rockchalk\}/bibliography{rockchalk}/' rockchalk.Rnw
cp -f rockchalk.pdf ../inst/doc


lyx -e pdf2 Rchaeology.lyx
lyx -e sweave Rchaeology.lyx
##perl -pi.bak  -e 's/bibliography\{0.*rockchalk\}/bibliography{rockchalk}/' Rchaeology.Rnw

cp -f Rchaeology.pdf ../inst/doc
cd ../..

R --vanilla -f runRoxygen2.R


R CMD build rockchalk.svnex

read -p "enter name of tarball: " result

R CMD check --as-cran $result
