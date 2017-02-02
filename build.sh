# !/bin/sh
stack install --local-bin-path bin

if [ -d "bin/lib" ]; then
    rm -r bin/lib
fi
cp -r lib bin
