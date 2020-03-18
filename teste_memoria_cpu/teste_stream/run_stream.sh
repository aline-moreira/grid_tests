#!/bin/bash
for (( TAM = 356000000; TAM <= 1436000000; TAM = TAM + 90000000)); do 
	INIT=`date -u`;
	echo "Start the test in time $INIT";
	echo "\tRUNNING STREAM WITH $TAM SIZE";
	rm -f stream;
	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
	export OMP_NUM_THREADS=4
        ./stream & 
	echo "\tSTREAM PROCESS PID $!" && PROC=$!;
	echo "\tRUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log pinado_logs/stream_memory_$TAM.log --include-children;
	END=`date -u`;
	echo "Finish the test in time $END";
	echo "$TAM;$LIMIT;$INIT;$END" >> pinado_logs/times.log;
done;

for (( LIMIT = 25; LIMIT <= 100; LIMIT = LIMIT + 25)); do
	for (( TAM = 356000000; TAM <= 1436000000; TAM = TAM + 90000000)); do
		INIT=`date -u`;
		echo "Start the test in time $INIT";
		echo "\tRUNNING STREAM WITH $TAM SIZE AND CPU LIMIT WITH $LIMIT";
		rm -f stream;
	       	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
		export OMP_NUM_THREADS=4
	        ./stream &
	       	echo "\tSTREAM PROCESS PID $!" && PROC=$!;
		echo "\tRUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log pinado_logs/stream-memory-$TAM-limit-$LIMIT.log --include-children &
	       	echo "\tRUNNING CPULIMIT ON $PROC PROCESS" && cpulimit -l $LIMIT -p $PROC;
		END=`date -u`;
		echo "Finish the test in time $END";
		echo "$TAM;$LIMIT;$INIT;$END" >> pinado_logs/times.log;
	done;
done;

rm -f stream;

