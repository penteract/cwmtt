F=`date +%Y-%m-%dT%H%M%S`@`git log -1 --format="%h"`.txt
speedtest/runtest.sh > speedtest/$F
