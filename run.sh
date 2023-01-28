#! /bin/sh
set -ex
if test -e result.txt; then
	rm result.txt
fi

basedir=$(dirname $0)
args="-extra-arg -Wno-gnu-folding-constant -extra-arg -Wno-dangling-else"
for case in $(find $basedir/minic-test-cases-2022s/ -name '*.c'); do
	ret=$($basedir/build/koopa-UBcheck $args $case 2>&1)
	con=$(echo $ret | grep "\[END WARNING\]" || true)
	if test ! \( -z "$con" \); then
		echo "$case" >> result.txt
#break
	else
		echo "$case PASS"
	fi
done

	

