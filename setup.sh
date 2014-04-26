#!/usr/bin/env bash
HOME=/home/vagrant

## Erlang requirements
echo "** Installing erlang requirements **"
apt-get update
apt-get fix-missing
apt-get install -y build-essential libncurses5-dev openssl libssl-dev fop xsltproc unixodbc-dev

## Erlang 17.0
echo "** Installing erlang **"
wget -q -O $HOME/kerl https://raw.github.com/spawngrid/kerl/master/kerl
chmod a+x $HOME/kerl
mkdir -p $HOME/erlang
$HOME/kerl update releases
$HOME/kerl build 17.0 erlang17
$HOME/kerl install erlang17 $HOME/erlang
echo ". ~/erlang/activate" >> $HOME/.bashrc
