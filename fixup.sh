
for f in *; do
  if (test -r $f && grep -q Copyright $f); then
    echo $f
    sed -e "s+ + +" <$f >$f.tmp
    mv -f $f.tmp $f
  fi
done
