OPAM_PACKAGES='ocamlfind lablgtk oasis'

export OPAMYES=1

# $HOME/.opam is cached, hence always present.
if [ -f "$HOME/.opam/config" ]; then
    opam update
    opam upgrade --yes
else
    opam init
fi

if [ -n "${OPAM_SWITCH}" ]; then
    opam switch ${OPAM_SWITCH}
fi
eval `opam config env`
opam install -q -y ${OPAM_PACKAGES}

opam pin add cairo2 .
opam remove cairo2

# Compile the tests
OCAML_VERSION_MAJOR=`ocamlc -version | cut -d . -f 1`
if [ $OCAML_VERSION_MAJOR -gt 3 ]; then
    oasis setup
    make
fi
