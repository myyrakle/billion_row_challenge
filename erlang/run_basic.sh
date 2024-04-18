#!/bin/sh
erlc ./erlang/basic/main.erl
erl -noshell -s main start -s init stop