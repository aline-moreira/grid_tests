if [ "$#" -ne 4 ]; then
	echo "Illegal number of parameters (CPU, MEMORY, TOTAL_TIME, VERSION_TEST)";
	exit 2;
fi
CPU=$1;
RAM=$2;
TOTAL_TIME=$3;
VERSION=$4;
INIT=`date -u`;
echo "Start the test in time $INIT";
stress-ng --cpu $CPU --timeout $TOTAL_TIME  & echo "Stress NG running in PID $!" && PROC=$!;
bash top_loop.sh  $VERSION/stress_"$CPU"cpu_"$RAM"gb.log & echo "Top Loop running in PID $!" && CPU=$!;
wait $PROC && kill $CPU;
END=`date -u`;
echo "FINISH the test in time $END";
echo "STRESS;$1;$2;$INIT;$END" >> $VERSION/benchmarks.time;
sleep 60;
