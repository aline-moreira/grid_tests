if [ "$#" -ne 4 ]; then
	echo "Illegal number of parameters (CPU, MEMORY, TOTAL_TIME, VERSION_TEST)";
	exit 2;
fi
CPU=$1;
if [ $(echo "$CPU < 1" | bc ) -ne 0 ]; then
	CPU_SET=1;
else
	CPU_SET=$CPU
fi
RAM=$2;
TOTAL_TIME=$3;
VERSION=$4;
INIT=`date -u`;
echo "Start the test in time $INIT";
stress-ng --cpu $CPU_SET --timeout $TOTAL_TIME  & echo "Stress NG running in PID $!" && BENCH_PROC=$!;
bash top_loop.sh  $VERSION/stress_"$CPU"cpu_"$RAM"gb.log & echo "Top Loop running in PID $!" && CPU_PROC=$!;
wait $BENCH_PROC && kill $CPU_PROC;
END=`date -u`;
echo "FINISH the test in time $END";
echo "STRESS;$1;$2;$INIT;$END" >> $VERSION/benchmarks.time;
sleep 60;
