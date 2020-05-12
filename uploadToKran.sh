read -p "enter name of tarball: " result

## rsync -e ssh  $result  rweb.crmda.ku.edu:/web/htdocs.rweb/kran/src/contrib

## Or if you are PJ

rsync -e "ssh -o PubkeyAuthentication=false" $result rweb.crmda.ku.edu:/web/htdocs.rweb/kran/src/contrib
 
