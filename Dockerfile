FROM ocaml/opam:debian-11

LABEL mantainer="Leonardo Amaral <dev@leonardoamaral.com.br>"

USER root:root
ENV HOME=/root
WORKDIR /root

RUN DEBIAN_FRONTEND=noninteractive apt -y update \
	&& DEBIAN_FRONTEND=noninteractive apt -y install libstring-shellquote-perl libipc-system-simple-perl pkg-config libgsl-dev libncurses-dev \
	&& DEBIAN_FRONTEND=noninteractive apt clean \
	&& rm -rf /var/cache/apt/lists/*

USER opam:opam
ENV HOME=/home/opam
WORKDIR /home/opam

RUN opam install dune camlp5 ocamlfind curses gsl num

RUN git clone https://github.com/pelzlpj/orpie.git \
	&& cd orpie \
	&& eval $(opam config env) \
	&& make install \
	&& cd .. \
	&& rm -rf orpie

USER root:root
ENV HOME=/root
WORKDIR /root

RUN apt -y purge pkg-config libgsl-dev libncurses-dev

USER opam:opam
ENV HOME=/home/opam
WORKDIR /home/opam

COPY --chown=opam:opam --chmod=0755 ./docker-entrypoint.sh /docker-entrypoint.sh

ENTRYPOINT /docker-entrypoint.sh orpie
