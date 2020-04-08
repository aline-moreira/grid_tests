if [ "$#" -ne 3 ]; then
	echo "The number of parameters are incorrect (number of pods, number of containers in each pod, version)";
	echo "$#";
	exit 2;
fi;

START=`date -u`;

bash create_multiple_pods_files.sh $1 $2;

while : ; do
	echo "inside while"
	kubectl get pods -l jobgroup=lifecycle > lifecycle_adm.completion;
	PODS_COMPLETED=`awk '/Completed/ {count++} END {print count++}' lifecycle_adm.completion`;
	
	if [ $PODS_COMPLETED -eq 1 ];then
		break;
	fi
done

END=`date -u`;

rm lifecycle_adm.completion;

for i in $(seq 1 $1); do
	kubectl delete jobs.batch lifecycle-$i;
done;

echo "$1;$2;$START;$END" >> $3/lifecycle.times;
