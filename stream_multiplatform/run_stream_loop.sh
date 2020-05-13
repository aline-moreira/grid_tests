if [ "$#" -lt 3 ]; then
	echo "Illegal number of parameters (PLATFORM, VERSION_TEST, ARRAY_TESTS)";
	exit 2;
fi

PLATFORM=$1; shift;
VERSION=$1; shift;
ARRAY=("$@");

mkdir $VERSION;

echo "VERSION $VERSION";
echo "RUNNING OPTIONS $ARRAY";

for RAM in "${ARRAY[@]}"; do	
	INIT=`date -u`;
	TAM=$(echo "44743611*$RAM" | bc);
	TAM=${TAM/\.*}
	echo "RUNNING STREAM WITH $RAM GB OF RAM (TOTAL $TAM)";
	rm -f stream;
	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
	./stream & echo "STREAM PROCESS PID $!" && BENCH_PROC=$!;
	END=`date -u`;
	echo "Finish the test in time $END";
	echo "STREAM;$PLATFORM;$RAM;$INIT;$END" >> $VERSION/benchmarks.time;
	rm stream;
	sleep 60;
done;
