if [ "$#" -ne 3 ]; then
	echo "Illegal number of parameters (CPU, MEMORY, TOTAL_TIME)"
	exit 2
fi
INIT=`date -u`;
echo "Start the test in time $INIT";
stress-ng --cpu $1 --timeout $3  & echo "Stress NG running in PID $!" && PROC=$!;
bash top_loop.sh  stress_$1cpu_$2gb.log & echo "Top Loop running in PID $!" && CPU=$!;
wait $PROC && kill $CPU;
END=`date -u`;
echo "FINISH the test in time $END";
echo "STRESS;$1;$2;$INIT;$END" >> times_n_pinado.log;
