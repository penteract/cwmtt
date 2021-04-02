TIMEOUT=20

git log --oneline -1
git status -uno --short

stack build
CMD=`stack exec which cwmtt`

function tstmate {
  T=`date +%s%N`
  cat $test | timeout $TIMEOUT $CMD checkmate > /dev/null
  T="$(($(date +%s%N)-T))"
  echo ${T:0:${#T}-6}ms
}
for test in tests/checkmate/*.5dpgn
do
  echo $test
  tstmate
done

function tstall {
  T=`date +%s%N`
  cat $test | timeout $(expr $TIMEOUT \* 5) $CMD perftest
  T="$(($(date +%s%N)-T))"
  echo ""
  echo ${T:0:${#T}-6}ms
}
for test in tests/checkmate/*.5dpgn
do
  echo $test
  tstall
done
