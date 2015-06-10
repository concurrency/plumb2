#!/bin/bash

DATE=`date +%Y%m%d`
PKG=server-bundle.tgz

pushd /usr/local/kroc-avr
  tar cvzf ~/${PKG} .
popd

if [[ -f ~/.ssh/small-imac-berea ]]; then
  KEY=~/.ssh/small-imac-berea
fi
if [[ -f ~/.ssh/big-mac-berea ]]; then
  KEY=~/.ssh/big-mac-berea
fi
if [[ -f ~/.ssh/serafina_rsa ]]; then
  KEY=~/.ssh/serafina_rsa
fi

scp -i "$KEY" ~/${PKG} jadudm@jadud.com:~/jadud.com/downloads/plumb/   
