#!/bin/bash

cd /tmp
# git clone git@github.com:erlang/rebar3.git
mv /root/rebar3-master.zip .
unzip rebar3-master.zip
cd rebar3-master
./bootstrap
mv rebar3 /usr/local/bin/
chmod +x /usr/local/bin/rebar3

rm -rf /tmp/*
