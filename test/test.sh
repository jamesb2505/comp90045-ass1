#!/bin/bash
make -f ./Makefile;
printf "\n";

COMPILATIONS=0; CORRECT=0; TESTS=0
for ROO in $(find ./testdata -name '*.roo'); do
    TESTS=$(($TESTS+1))

    echo "Compiling $ROO";
    if ! ./Roo $ROO > ./test/tmp.oz;
    then
        echo "Compilation failed.";
    else
        COMPILATIONS=$(($COMPILATIONS+1))

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

printf "%.2f%% successful compilations from %d attempts\n" \
    $((100 * $COMPILATIONS / $TESTS)) \
    $TESTS
printf "%.2f%% correct outputs from %d tests\n" \
    $((100 * $CORRECT / $TESTS)) \
    $COMPILATIONS