#!/bin/bash

# remove all .git directories from our systems
find systems/ -name ".git" -type d -exec rm -rf {} \;

# link all our .asd files into our central registry
mkdir -p registry
pushd registry > /dev/null
SYSTEM_PATH=../systems
rm -f *.asd
for f in `find $SYSTEM_PATH -name "*.asd"`; do
	LOAD="(load #P\"$f\")"
	echo $LOAD > `basename ./$f`
done
popd > /dev/null


