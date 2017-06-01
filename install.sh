#!/bin/bash
# first install rebar package manager

if [ -e "rebar" ]
then
    echo "rebar already installed"
else
    uname=$(uname);
    case "$uname" in
	(*Linux*) wget https://raw.githubusercontent.com/wiki/rebar/rebar/rebar && chmod u+x rebar; ;;
	(*Darwin*) curl https://raw.githubusercontent.com/wiki/rebar/rebar/rebar -o rebar ;  chmod u+x rebar; ;;
	(*) echo 'error: unsupported platform.'; exit 2; ;;
    esac;
#use rebar to install other dependencies, explained in rebar.config
    ./rebar get
    ./rebar compile
fi



