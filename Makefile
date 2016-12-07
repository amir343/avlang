# Copyright (c) 2016 Amir Moulavi
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.NOTPARALLEL:

.PHONY: type_checker compiler all test

TERL_ROOT = $(shell pwd)

TYPE_CHECK=lib/type_checker
COMPILER=lib/compiler

compiler:
	cd lib/compiler && \
	  TERL_ROOT=$(TERL_ROOT) \
		$(MAKE) compile BUILD_ALL=true

type_checker:
	cd lib/type_checker && \
	  TERL_ROOT=$(TERL_ROOT) \
		$(MAKE) compile BUILD_ALL=true

clean-type_checker:
	cd lib/type_checker && \
	  TERL_ROOT=$(TERL_ROOT) \
		$(MAKE) clean BUILD_ALL=true

clean-compiler:
	cd lib/compiler && \
	  TERL_ROOT=$(TERL_ROOT) \
		$(MAKE) clean BUILD_ALL=true

clean: clean-type_checker clean-compiler

test:
	@bootstrap/bin/erlc \
		-pa $(TYPE_CHECK)/ebin -o $(TYPE_CHECK)/ebin \
		$(TYPE_CHECK)/test/type_check_SUITE.erl \
		$(TYPE_CHECK)/test/run_test.erl

	@bootstrap/bin/erl \
		-pa $(TYPE_CHECK)/ebin \
		-pa $(COMPILER)/ebin -noshell \
		-s run_test run

all: compiler type_checker test
