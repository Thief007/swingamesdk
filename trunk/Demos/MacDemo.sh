#!/bin/sh

Usage()
{
	echo "Usage: $0 [-h] DemoName"
	exit 1
}

while getopts h o
do
	case "$o" in
	h)   Usage;;
	esac
done

shift $((${OPTIND}-1))

if [ -n "$1" ] 
then
	DEMO_NAME="$1"
else
	Usage
fi

