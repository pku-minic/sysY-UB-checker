#! /bin/sh
basedir=$(dirname $0)
args="-extra-arg -Wno-gnu-folding-constant -extra-arg -Wno-dangling-else"
for case in $(find $basedir/minic-test-cases-2022s/ -name '*.c'); do
	ret=$($basedir/build/koopa-UBcheck $args $case 2>&1)
	con=$(echo $ret | grep "\[WARNING\]")
	if test ! \( -z "$con" \); then
		echo "$case FAILED, output:"
		echo $ret
		break
	else
		echo "$case PASS"
	fi
done

	

