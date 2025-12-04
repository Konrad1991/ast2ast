FROM debian:latest

RUN apt-get update && \
  apt-get install -y \
  clang \
  build-essential

ENV CC=/usr/bin/clang
ENV CXX=/usr/bin/clang++

WORKDIR /usr/src/app

COPY ~/Documents/ast2ast/ .

CMD ["/bin/bash"]
