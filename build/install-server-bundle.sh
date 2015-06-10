#!/bin/bash

pushd /tmp
  curl -o bundle.tgz http://jadud.com/downloads/plumb/server-bundle.tgz
popd

pushd /usr/local/kroc-avr
  tar xvf /tmp/bundle.tgz
popd

rm /tmp/bundle.tgz
