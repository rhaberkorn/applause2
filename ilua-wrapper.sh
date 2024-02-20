#!/bin/sh
# This wrapper exists, so you can pass additional arguments to Applause when
# using it as an ILua interpreter.
# Also, it's useful as you do not have to change the directory before invoking ILua.
# Thirdly, if you create a symbolic link to lua in a Python environment of ILua, this invoke
# Applause by default and it will even work with Jupyter Notebooks without further tweaks.
cd $(dirname $(readlink "$0"))
exec ./applause $APPLAUSE_OPTS "$@"
