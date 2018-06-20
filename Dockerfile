FROM ocaml/opam2:alpine-3.7

ENV OPAMYES=1

RUN opam pin add crowbar --dev -n
RUN opam install opam-depext
RUN opam depext -i jbuilder fmt logs lwt menhir higher ppx_deriving \
      crowbar ocaml-protoc

COPY . /src
RUN sudo chown -R opam /src
WORKDIR /src

RUN opam exec -- make
