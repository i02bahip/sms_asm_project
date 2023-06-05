#!/bin/sh
clear
case $1 in
	0)
		folder="0-completeExample"
		;;
	1)
		folder="1-helloWorld"
		break
		;;
	2)
		folder="2-background"
		break
		;;
	*)
        echo "Wrong param: $1"
        echo "The correct format is: ./build param"
        echo "Possible params:"
        echo "----------------------------"
        echo "0: complete example"
        echo "1: hello world"
        echo "2: background"
        echo "----------------------------"
        echo "Example for build example 0:" 
        echo "./build.sh 0"
		;;
  esac

cd $folder

../environment/compiler/wla-dx/bin/linux/wla-z80 -v -o ../out/main.o example-$1.asm
../environment/compiler/wla-dx/bin/linux/wlalink -d -v -s ../environment/compiler/wla-dx/link/link.lk ../out/example-$1.sms

rm ../out/*.o

echo "Build finished."
cd ..