# DockerFile for the HYPE model.
# The input data root directory should be mounted as /data
# The filedir.txt in the input directory should only contain the single line: /data/
# The output folder should exist within the mounted volume
FROM ubuntu:bionic
MAINTAINER Andrew Bennett <bennett.andr@gmail.com>
RUN apt-get update

# Prerequisite packages
RUN apt-get update && \
    apt-get install -y wget git build-essential \
    g++ make cmake curl automake libtool \
    pkg-config software-properties-common python-software-properties \
    ca-certificates libnetcdff-dev liblapack-dev vim

# install gfortran-6
RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y \
    && apt-get update \
    && apt-get install -y --no-install-recommends gfortran-6 \
    && apt-get clean

# set environment variables for docker build
ENV F_MASTER /code
ENV FC gfortran
ENV FC_EXE gfortran
ENV FC_ENV gfortran-6-docker


# Build grpc from source
RUN git clone -b $(curl -L https://grpc.io/release) --depth=1 https://github.com/grpc/grpc /opt/grpc
WORKDIR /opt/grpc
RUN git submodule update --init --recursive
RUN make install
WORKDIR third_party/protobuf
RUN make install

# Build bmi-c from source
RUN git clone --depth=1 https://github.com/eWaterCycle/grpc4bmi.git /opt/grpc4bmi
WORKDIR /opt/grpc4bmi
RUN git submodule update --init --recursive
RUN mkdir -p /opt/grpc4bmi/cpp/bmi-c/build
WORKDIR /opt/grpc4bmi/cpp/bmi-c/build
RUN cmake ..
RUN make install

# Build grpc4bmi from source
RUN mkdir -p /opt/grpc4bmi/cpp/build
WORKDIR /opt/grpc4bmi/cpp/build
RUN cmake ..
RUN make install

VOLUME /data
# WORKDIR /data
ENV LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/usr/local/lib"
ENV PATH="${PATH}:/usr/local/bin"

# add code directory
WORKDIR /code
ADD . /code

# fetch tags and build summa
RUN git fetch --tags && make -C build/ -f Makefile summa_bmi

# run summa when running the docker image
WORKDIR bin
ENTRYPOINT ["./summa_bmi.exe"]

