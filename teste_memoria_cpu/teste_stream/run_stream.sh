#!/bin/bash
for (( TAM = 356000000; TAM <= 1436000000; TAM = TAM + 90000000)); do 
	echo "RUNNING STREAM WITH $TAM SIZE";
	rm -f stream;
	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
        ./stream & 
	echo "STREAM PROCESS PID $!" && PROC=$!;
	echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log logs/stream_memory_$TAM.log --include-children;
done;

for (( LIMIT = 25; LIMIT <= 100; LIMIT = LIMIT + 25)); do
	for (( TAM = 356000000; TAM <= 1436000000; TAM = TAM + 90000000)); do
		echo "RUNNING STREAM WITH $TAM SIZE AND CPU LIMIT WITH $LIMIT";
		rm -f stream;
	       	gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
	        ./stream &
	       	echo "STREAM PROCESS PID $!" && PROC=$!;
		echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log logs/stream-memory-$TAM-limit-$LIMIT.log --include-children &
	       	echo "RUNNING CPULIMIT ON $PROC PROCESS" && cpulimit -l $LIMIT -p $PROC;
	done;
done;

rm -f stream;
