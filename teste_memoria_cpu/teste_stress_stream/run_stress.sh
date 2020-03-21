INIT=`date -u`;
echo "Start the test in time $INIT";
stress-ng --cpu 4 --timeout 600  &
echo "Stress NG running in PID $!" && PROC=$!;
echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log stress_4cpu_32g.log --include-children;
END=`date -u`;
echo "FINISH the test in time $END";
echo "STRESS;4;32;$INIT;$END" >> times_n_pinado.log;
