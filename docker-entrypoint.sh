#!/bin/sh

eval $(opam config env)
exec "$@"
