cd /home/pauljohn/SVN/rgroup/trunk/rockchalk/rockchalk/vignettes

lyx -e pdf2 Rchaeology.lyx
lyx -e sweave Rchaeology.lyx


lyx -e pdf2 rockchalk.lyx
lyx -e sweave rockchalk.lyx

#mkdir Rchaeology.html.LyXconv
#cp Rchaeology.Rnw Rchaeology.html.LyXconv
#cd Rchaeology.html.LyXconv
#R CMD Sweave Rchaeology.Rnw
#../sw


NOW=$(date +"%Y%m%d")
dir="backup/backup-$NOW"
budir="freefaculty.org/R/backup/backup-$NOW"

ssh freefaculty mkdir -p $budir

rsync -e ssh  -b --suffix="-bak2" --backup-dir=$dir Rchaeology.pdf  freefaculty:freefaculty.org/R

rsync -e ssh -b --suffix="-bak2" --backup-dir=$dir rockchalk.pdf  freefaculty:freefaculty.org/R

##rsync -e ssh -b --suffix="-bak2" --backup-dir=$dir  Rchaeology.html.LyXconv/Rchaeology.html  freefaculty:freefaculty.org/R
