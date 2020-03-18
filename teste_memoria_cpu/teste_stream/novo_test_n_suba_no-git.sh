#!/bin/bash
for (( LIMIT = 100; LIMIT <= 100; LIMIT = LIMIT + 25)); do
	for (( TAM = 1166000000; TAM <= 1436000000; TAM = TAM + 90000000)); do
		INIT=`date -u`;
		echo "RUNNING STREAM WITH $TAM SIZE AND CPU LIMIT WITH $LIMIT";
		rm -f stream;
		gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream;
		./stream &
		echo "STREAM PROCESS PID $!" && PROC=$!;
		echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log nao_pinado/stream-memory-$TAM-limit-$LIMIT.log --include-children &
	       	echo "RUNNING CPULIMIT ON $PROC PROCESS" && cpulimit -l $LIMIT -p $PROC;
		END=`date -u`;
		echo "Finish the test in time $END";
		echo "$TAM;$LIMIT;$INIT;$END" >> times_n_pinado.log;
	done;
done;

rm -f stream;
