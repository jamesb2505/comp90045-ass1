#!/bin/bash
make -f ./Makefile;
printf "\n";

CORRECT=0; TESTS=0
for ROO in ./testdata/*.roo; do
    TESTS=$(($TESTS+1))

    echo "Compiling $ROO";
    if ! ./Roo $ROO > ./test/tmp.oz;
    then
        echo "Compilation failed.";
    else
        echo "Compilation successfull.";

        IN=$(echo $ROO | sed 's/.roo$/.in/')
        if [[ -e $IN ]] 
        then
            echo "Running with input $IN"
            ./oz/oz ./test/tmp.oz < $IN > ./test/tmp.out
        else
            echo "Running with no input"
            ./oz/oz ./test/tmp.oz > ./test/tmp.out
        fi

        OUT=$(echo $ROO | sed 's/.roo$/.out/')
        if [[ -e $OUT ]] 
        then
            DIFF=$(diff $OUT ./test/tmp.out)
            if [[ "$DIFF" ]]
            then
                echo "Difference:"
                echo $DIFF
            else
                echo "Correct output"
                CORRECT=$(($CORRECT+1))
            fi
        else
            echo "Output:"
            cat ./test/tmp.out
        fi
    fi
    
    printf "\n";
done

rm -f ./test/tmp.out ./test/tmp.oz

printf "%.2f%% correct outputs\n" $((100 * $CORRECT / $TESTS))