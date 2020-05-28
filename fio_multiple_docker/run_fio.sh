if [ "$#" -ne 3 ]; then
	echo "Illegal number of parameters (VERSION_TEST, IO, DOCKERS)";
	exit 2;
fi


VERSION=$1;
IO=$2;
DOCKERS=$3;

ls $VERSION || mkdir $VERSION;
ls $VERSION/logs || mkdir $VERSION/logs;
ls fio-data || mkdir fio-data;

for i in $IO; do
	for ((j=1;j<=$DOCKERS;j++)); do
		echo "
		[global]
		randrepeat=1
		loops=30
		direct=1
		gtod_reduce=1
		name=test
		filename=test
		bs=32k
		readwrite=randread

		[teste_fio_${i}G_docker]
		size=${i}G
		ioengine=libaio
		iodepth=64
			" > ./fio-data/${i}G-$j.fio;
	done;

	echo "Running test with ${i}GB";

	START=`date -u`;
	
	for ((j=1;j<$DOCKERS;j++)); do
		docker run --rm -v `pwd`/fio-data:/tmp/fio-data -e JOBFILES="${i}"G-$j.fio -d clusterhq/fio-tool > $VERSION/logs/teste_fio_"${i}"G_"${DOCKERS}docker".log;
	done;	
	docker run --rm -v `pwd`/fio-data:/tmp/fio-data -e JOBFILES="${i}"G-$DOCKERS.fio  clusterhq/fio-tool > $VERSION/logs/teste_fio_"${i}"G_"${DOCKERS}docker".log;.

	END=`date -u`;
	echo "docker;$i;$DOCKERS;$START;$END" >> $VERSION/fio.times;
done;

rm -rf fio-data
#START=`date -u`;
#sleep 600;
#END=`date -u`;
#echo "docker;0;$DOCKERS;$START;$END" >> $VERSION/fio.times;

#curl -kn https://api.grid5000.fr/stable/sites/grenoble/metrics/power/timeseries/?job_id="${OAR_JOB_ID}" > ./$1/energy_"${OAR_JOB_ID}".json;
