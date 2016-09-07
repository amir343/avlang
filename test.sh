#!/bin/bash

TYPE_CHECK=lib/type_checker
EBIN=${TYPE_CHECK}/ebin

bootstrap/bin/erlc -pa ${EBIN} -o ${EBIN} \
                   ${TYPE_CHECK}/test/type_check_SUITE.erl \
                   ${TYPE_CHECK}/test/run_test.erl

bootstrap/bin/erl \
          -pa ${EBIN} \
          -pa lib/stdlib/ebin \
          -pa lib/compiler/ebin \
          -pa lib/kernel/ebin \
          -noshell \
          -s run_test run

