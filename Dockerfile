FROM debian:jessie

RUN apt-get clean &&\
    apt-get -y update && \
    apt-get clean && \
    apt-get -y dist-upgrade && \
    apt-get clean &&\
    apt-get install --fix-missing -y build-essential \
    libwxbase3.0-0 \
    libwxgtk3.0-0 \
    libncurses5-dev \
    openssl \
    libssl-dev \
    fop \
    wget \
    git \
    vim \
    mysql-client \
    monit

COPY build/esl-erlang_19.0.3-1-debian-jessie_amd64.deb /root/.
RUN dpkg -i /root/esl-erlang_19.0.3-1-debian-jessie_amd64.deb
COPY build/setup-reb.sh /root/.
COPY build/rebar3-master.zip /root/.
RUN /root/setup-reb.sh

RUN mkdir /myapp
WORKDIR /myapp
COPY . /myapp

RUN rebar3 release
COPY build/sys.config _rel/elvis_server/releases/0.1.0/sys.config
COPY build/vm.args _build/default/rel/spellingci/releases/0.0.1/vm.args
COPY build/spellingci.monit.conf /etc/monit/conf.d/spellingci.conf
