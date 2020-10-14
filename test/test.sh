#!/bin/bash
ROO=$1
IN=$(echo $ROO | sed -e s/\.roo$/\.in/)
OUT=$(echo $ROO | sed -e s/\.roo$/\.out/)

./Roo $ROO > ./test/tmp.oz \
    && ([[ -e $IN ]] && ./oz/oz ./test/tmp.oz < $IN \
                     || ./oz/oz ./test/tmp.oz)

rm -f ./test/tmp.oz