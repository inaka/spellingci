FROM debian:jessie

RUN echo \
   'deb ftp://ftp.us.debian.org/debian/ jessie main\n \
    deb ftp://ftp.us.debian.org/debian/ jessie-updates main\n \
    deb http://security.debian.org jessie/updates main\n' \
    > /etc/apt/sources.list

ENV DEPENDENCY_PACKAGES=""
ENV BUILD_PACKAGES="build-essential libwxbase3.0-0 libwxgtk3.0-0 libncurses5-dev openssl libssl-dev fop wget git vim mysql-client monit"

RUN apt-get clean &&\
    apt-get -y update && \
    apt-get -y dist-upgrade && \
    apt-get install -y $DEPENDENCY_PACKAGES $BUILD_PACKAGES && \
    apt-get autoremove -y && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY build/esl-erlang_19.0.3-1-debian-jessie_amd64.deb /root/.
RUN dpkg -i /root/esl-erlang_19.0.3-1-debian-jessie_amd64.deb
COPY build/rebar3 /usr/local/bin/.
COPY build/covertool /usr/local/bin/.

RUN mkdir /myapp
WORKDIR /myapp
COPY . /myapp

COPY build/sys.config /myapp/config/app.config
RUN rebar3 release -n spellingci

COPY build/vm.args _build/default/rel/spellingci/releases/0.0.1/vm.args
COPY build/spellingci.monit.conf /etc/monit/conf.d/spellingci.conf
COPY build/monitrc /etc/monit/monitrc
RUN chmod 700 /etc/monit/monitrc
