FROM debian:jessie

RUN apt-get clean &&\
    apt-get -y update && \
    apt-get clean && \
    apt-get -y dist-upgrade && \
    apt-get clean &&\
    apt-get install --fix-missing -y build-essential \
    libncurses5-dev \
    openssl \
    libssl-dev \
    fop \
    wget \
    git \
    mysql-client \
    monit

COPY build/setup-erl.sh /root/.
COPY build/otp_src_19.0.tar.gz /root/.
RUN /root/setup-erl.sh
COPY build/setup-reb.sh /root/.
COPY build/rebar3-master.zip /root/.
RUN /root/setup-reb.sh

RUN mkdir /myapp
WORKDIR /myapp
COPY . /myapp

RUN rebar3 release
