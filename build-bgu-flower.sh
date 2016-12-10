#!/bin/bash
#
clear
echo Start Building

cd ./flower
../tetrapak/bin/tetrapak build
cd ..

cd ./gen_listener_tcp
../tetrapak/bin/tetrapak build
cd ..

cd ./gen_socket
../tetrapak/bin/tetrapak build
cd ..

cd ./regine
../tetrapak/bin/tetrapak build
cd ..

cd ./vrrpd_mon
../tetrapak/bin/tetrapak build
cd ..


