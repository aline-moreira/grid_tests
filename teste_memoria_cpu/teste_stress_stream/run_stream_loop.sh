if [ "$#" -lt 3 ]; then
	echo "Illegal number of parameters (CPU, VERSION_TEST, ARRAY_TESTS)";
	exit 2;
fi

CPU=$1; shift;
#RAM=$1; shift;
VERSION=$1; shift;
ARRAY=("$@");

echo "CPU USAGE $CPU";
#echo "RAM USAGE $RAM";
echo "VERSION $VERSION";
echo "RUNNING OPTIONS $ARRAY";

for RAM in "${ARRAY[@]}"; do	
	INIT=`date -u`;
	TAM=$((44743611*$RAM));
	echo "RUNNING STREAM WITH $RAM GB OF RAM (TOTAL $TAM)";
	rm -f stream;
	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
	export OMP_NUM_THREADS=$CPU;
	./stream & echo "STREAM PROCESS PID $!" && BENCH_PROC=$!;
	bash top_loop.sh $VERSION/stream_"$CPU"cpu_"$RAM"gb.log & echo "Top Loop running PID $!" && CPU_PROC=$!;
	wait $BENCH_PROC && kill $CPU_PROC;
	END=`date -u`;
	echo "Finish the test in time $END";
	echo "STREAM;$CPU;$RAM;$INIT;$END" >> $VERSION/benchmarks.time;
	sleep 60;
done;
