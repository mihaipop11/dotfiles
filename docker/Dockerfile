FROM ubuntu:18.04

RUN apt-get update && apt-get -y upgrade

RUN apt-get -y install \
    build-essential \
    libtool \
    autoconf \
    automake \
    pkg-config \
    unzip \
    libkrb5-dev \
    locales \
    emacs \
    git

RUN locale-gen en_US.UTF-8

# Disable password and not ask for finger info
#RUN adduser --disabled-password --gecos '' ubuntu
#RUN adduser ubuntu sudo
#RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

#USER ubuntu

# Hush login messages
# RUN touch ~/.hushlogin

#WORKDIR /home/ubuntu

RUN cd /tmp && git clone git://github.com/jedisct1/libsodium.git && cd libsodium && git checkout e2a30a && ./autogen.sh && ./configure && make check && make install && ldconfig
RUN cd /tmp && git clone --depth 1 git://github.com/zeromq/libzmq.git -b v4.2.5 && cd libzmq && ./autogen.sh && ./configure && make
RUN cd /tmp/libzmq && make check
RUN cd /tmp/libzmq && make install && ldconfig
RUN rm /tmp/* -rf