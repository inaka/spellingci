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
