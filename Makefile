
_build/install/default/bin/orpie:
	dune build

_build/install/default/bin/orpie-curses-keys:
	dune build

install: _build/install/default/bin/orpie _build/install/default/bin/orpie-curses-keys
	ORPIE_PREFIX=`scripts/compute_prefix eval` && \
	mkdir -p "$(DESTDIR)/$$ORPIE_PREFIX" && \
	dune install --prefix="$(DESTDIR)/$$ORPIE_PREFIX"

docker:
	docker build -t orpie:latest .

clean:
	dune clean

.PHONY: install clean

