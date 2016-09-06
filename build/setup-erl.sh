#!/bin/bash

cd /tmp
# wget http://erlang.org/download/otp_src_19.0.tar.gz

mv /root/otp_src_19.0.tar.gz .
tar -xvzf otp_src_19.0.tar.gz
cd otp_src_19.0
./configure
make
make install

rm -rf /tmp/*
