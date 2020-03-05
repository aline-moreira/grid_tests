for TAM in {3560000..999999999..90000000}
	do echo $TAM
	rm stream && gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream && (./stream & ( echo "RUNNING PSRECORD ON $! PROCESS" && psrecord $! --interval 0.1 --log stream_memory_$TAM.png --include-children))
done
for TAM in {3560000..999999999..90000000} do
	for LIMIT in {10..100..10} do
		echo "RUNNING STREAM WITH $TAM SIZE AND CPU LIMIT WITH $LIMIT"
		rm stream && gcc -O2 -fopenmp -DNTIMES=20 -DSTREAM_ARRAY_SIZE=$TAM -mcmodel=medium stream.c -o stream && (./stream & (PROC=$! && (echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $! --interval 0.1 --log stream_memory_$TAM_limit_$LIMIT.png --include-children) & cpulimit -l $LIMIT -p $PROC))
	done
done
