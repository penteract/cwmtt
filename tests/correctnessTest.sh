TIMEOUT=100

git log --oneline -1
git status -uno --short

stack build
CMD=`stack exec which cwmtt`

for test in tests/5dpgn/*.5dpgn
do
  echo $test
  if cat $test | timeout $TIMEOUT $CMD count
  then true
  else echo timeout
  fi
done
