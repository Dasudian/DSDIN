#!/bin/bash
set -e

# Use TestNet example config as configuration template
cp docs/examples/dsdin_testnet.yaml dsdin.yaml

# Using console with extra arguments because "foreground" does not handle SIGTERM/SIGQUIT
exec ./bin/dsdin console -noshell -noinput $@
