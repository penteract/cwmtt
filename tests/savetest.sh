F=`date +%Y-%m-%dT%H%M%S`@`git log -1 --format="%h"`.txt
mkdir -p speedtest
tests/performanceTest.sh > speedtest/$F
