#!/bin/bash

function compile_app {
    [[ -z "$1" ]] && echo "No input given to compile function" && exit 1
    local rc=0
    make $1
}

function compile {
    for i in type_checker compiler stdlib kernel; do
        compile_app $i
        [[ $? -ne 0 ]] && echo "Failed to compile $i" && exit 1
    done
}

function shell {
    bootstrap/bin/erl -pa lib/type_checker/ebin \
                      -pa lib/compiler/ebin \
                      -pa lib/kernel/ebin \
                      -pa lib/stdlib/ebin
}

function evaluate {
    [[ -z "$1" ]] && echo "No input given to eval" && exit 1

    bootstrap/bin/erl -hidden -noshell -sname test -connect_all false \
                      -pa lib/type_checker/ebin \
                      -pa lib/compiler/ebin \
                      -pa lib/kernel/ebin \
                      -pa lib/stdlib/ebin \
                      -eval $1

}

while [[ $# -gt 0 ]]; do
    key="$1"

    case $key in
        compile)
            compile
            shift
            ;;
        shell)
            shell
            shift
            ;;
        -eval)
            shift
            evaluate $1
            shift
            ;;
        *)
            # unknown option
            ;;
    esac
done
