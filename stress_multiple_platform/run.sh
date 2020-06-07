if [ "$#" -lt 4 ]; then
	echo "Illegal number of parameters (PLATFORM, VERSION_TEST, TOTAL_TIME, ARRAY_TESTS)";
	exit 2;
fi

PLATFORM=$1; shift;
VERSION=$1; shift;
TOTAL_TIME=$1; shift;
ARRAY=("$@");

TOTAL_CPUS=`grep -c ^processor /proc/cpuinfo`

mkdir $VERSION;

echo "VERSION $VERSION";
echo "RUNNING OPTIONS ${ARRAY[@]}";

for CPU in "${ARRAY[@]}"; do	
	INIT=`date -u`;	
	echo "RUNNING STRESS ($CPU/$TOTAL_CPUS)CPUs";
	stress-ng --cpu $CPU --timeout $TOTAL_TIME;
	END=`date -u`;
	echo "STRESS;$PLATFORM;$CPU;$INIT;$END" >> $VERSION/benchmarks.time;
	sleep 60;
done;

INIT=`date -u`;
sleep 600;
END=`date -u`;
echo "STRESS;$PLATFORM;0;$INIT;$END" >> $VERSION/benchmarks.time;