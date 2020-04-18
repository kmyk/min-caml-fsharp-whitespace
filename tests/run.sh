#!/bin/bash
set -e

tempdir=`mktemp -d`
trap 'rm -rf "$tempdir"' EXIT

for f in examples/*.ml ; do
    echo test "$f" ...
    {
        echo 'print_int('
        cat "$f"
        echo '); print_newline()'
    } > "$tempdir"/expected.ml
    ocaml "$tempdir"/expected.ml > "$tempdir"/expected.txt
    dotnet run < "$f" > "$tempdir"/actual.ws
    perl tests/whitespace.pl "$tempdir"/actual.ws > "$tempdir"/actual.txt
    diff "$tempdir"/actual.txt "$tempdir"/expected.txt
done
