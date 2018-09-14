#!/usr/bin/env bash

LIBS=(
    -l jagger-util.el
    -l jagger-swap.el
    -l jagger-move.el
    -l jagger-sort.el
    -l test-util.el
    -l test-jagger-util.el
    -l test-jagger-swap.el
    -l test-jagger-move.el
)

if [[ -n $1 ]]; then
    emacs-nightly --batch -l ert ${LIBS[@]} --eval "(ert-run-tests-batch-and-exit '$1)"
else
    emacs-nightly --batch -l ert ${LIBS[@]} -f ert-run-tests-batch-and-exit
fi
