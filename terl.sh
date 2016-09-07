#!/bin/bash
make type_checker || exit 1
make compiler || exit 1
make stdlib || exit 1
make kernel || exit 1
bootstrap/bin/erl -pa lib/type_checker/ebin \
                  -pa lib/compiler/ebin \
                  -pa lib/kernel/ebin \
                  -pa lib/stdlib/ebin
