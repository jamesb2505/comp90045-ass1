#!/bin/bash
make -f ./Makefile;
printf "\n";
printf "WARNING: Do not use unless you know what you're doing\n"

CORRECT=0; TESTS=0
for ROO in $(find ./testdata -name '*.roo'); do
    TESTS=$(($TESTS+1))

    echo "Compiling $ROO";
    if ! ./Roo $ROO > ./test/tmp.oz;
    then
        echo "Compilation failed.";
    else
        echo "Compilation successfull.";

        IN=$(echo $ROO | sed 's/.roo$/.in/')
        OUT=$(echo $ROO | sed 's/.roo$/.out/')
        if [[ ! -e $OUT ]]
        then
            if [[ -e $IN ]]
            then
                ./oz/oz ./test/tmp.oz < $IN > $OUT
            else
                ./oz/oz ./test/tmp.oz > $OUT
            fi
        fi
    fi
    
    printf "\n";
done

rm -f ./test/tmp.out ./test/tmp.oz