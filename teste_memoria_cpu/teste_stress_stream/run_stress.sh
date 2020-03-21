INIT=`date -u`;
echo "Start the test in time $INIT";
stress-ng --cpu 128 --timeout 600  &
echo "Stress NG running in PID $!" && PROC=$!;
echo "RUNNING PSRECORD ON $PROC PROCESS" && psrecord $PROC --interval 0.1 --log nao_pinado_logs_stress.log --include-children;
END=`date -u`;
echo "FINISH the test in time $END";
echo "STRESS;$INIT;$END" >> times_n_pinado.log;
