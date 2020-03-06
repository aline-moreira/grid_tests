#!/bin/bash
for (( TAM = 356000000; TAM <= 1436000000; TAM = TAM + 90000000)); do 
	INIT=`date -u`;
	echo "Start the test in time $INIT";
	echo "RUNNING STREAM WITH $TAM SIZE";
	rm -f stream;
	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
        ./stream & 
	echo "STREAM PROCESS PID $!" && PROC=$!;
	echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log stream_memory_$TAM.png --include-children;
	END=`date -u`;
	echo "Finish the test in time $END";
	echo "$TAM;0;$INIT;$END" >> times.log;
done;

for (( TAM = 356000000; TAM <= 1436000000; TAM = TAM + 90000000)); do
	for (( LIMIT = 10; LIMIT <= 100; LIMIT = LIMIT + 10)); do
		INIT=`date -u`;
		echo "Start the test in time $INIT";
		echo "RUNNING STREAM WITH $TAM SIZE AND CPU LIMIT WITH $LIMIT";
		rm -f stream;
	       	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
	        ./stream &
	       	echo "STREAM PROCESS PID $!" && PROC=$!;
		echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log stream-memory-$TAM-limit-$LIMIT.log --include-children &
	       	echo "RUNNING CPULIMIT ON $PROC PROCESS" && cpulimit -l $LIMIT -p $PROC;
		END=`date -u`;
		echo "Finish the test in time $END";
		echo "$TAM;$LIMIT;$INIT;$END" >> times.log;
	done;
done;

rm -f stream;
