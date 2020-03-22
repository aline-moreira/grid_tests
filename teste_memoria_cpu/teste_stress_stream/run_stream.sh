if [ "$#" -ne 2 ]; then
	echo "Illegal number of parameters (CPU, MEMORY)";
	exit 2;
fi
if [ $2 -eq 32 ]; then
	echo "TAM SIZE 32GB";
	TAM=1346000000;

else 
	if [ $2 -eq 754 ]; then
		TAM=33466190671;
		echo "TAM SIZE 754GB";
	else
		echo "Illegal number of memory requested";
		exit 3;
	fi
fi
INIT=`date -u`;
#TAM=1346000000;
#TAM=33466190671;
echo "RUNNING STREAM WITH $TAM SIZE";
rm -f stream;
gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
export OMP_NUM_THREADS=$1;
./stream & echo "STREAM PROCESS PID $!" && PROC=$!;
bash top_loop.sh stream_$1cpu_$2gb.log & echo "Top Loop running PID $!" && CPU=$!;
wait $PROC && kill $CPU;
END=`date -u`;
echo "Finish the test in time $END";
echo "STREAM;$1;$2;$INIT;$END" >> times_n_pinado.log;
