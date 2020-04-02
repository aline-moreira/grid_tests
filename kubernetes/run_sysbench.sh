#for cpus in 1 2 4 8 16 32 64 128; do
for cpus in 256; do
#Iniciar o deploy, dorme por 10 minutos e depois mata os pods
	START=`date -u` 
	kubectl apply --namespace testes -f sysbench/sysbench-$cpus.yaml 
	sleep 600; 
	kubectl delete --namespace testes deployment/sysbench-$cpus ;
	END=`date -u`;
	echo "$cpus;$START;$END" >> sysbench/kubernetes.times;
	sleep 90;
done

