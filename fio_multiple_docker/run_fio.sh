if [ "$#" -ne 4 ]; then
	echo "Illegal number of parameters (PLATFORM, VERSION_TEST, IO, DOCKERS)";
	exit 2;
fi

PLATFORM=$1;
VERSION=$2;
IO=$3;
DOCKERS=$4;

ls $VERSION || mkdir $VERSION;
ls $VERSION/logs || mkdir $VERSION/logs;
ls fio-data || mkdir fio-data;

#if [ "$PLATFORM" = "host" ] || [ "$PLATFORM" = "vm" ]; then
#	sudo-g5k apt-get install fio -y
#fi

for i in $IO; do
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

[teste_fio_${i}G_${PLATFORM}]
size=${i}G
ioengine=libaio
iodepth=64
	" > ./fio-data/${i}G.fio;

	sleep 60;

	echo "Running test with ${i}GB";

	START=`date -u`;
	
	if [ "$PLATFORM" = "host" ] || [ "$PLATFORM" = "vm" ]; then
		fio fio-data/${i}G.fio --output=./$VERSION/logs/teste_fio_"${i}"G_"${PLATFORM}".log;
		rm -f test
	fi
	if [ "$PLATFORM" = "docker" ]  || [ "$PLATFORM" = "vm_docker" ]; then
		docker run --rm -v `pwd`/fio-data:/tmp/fio-data -e JOBFILES="${i}"G.fio  clusterhq/fio-tool > $VERSION/logs/teste_fio_"${i}"G_"${PLATFORM}".log;
	fi

	END=`date -u`;
	echo "$PLATFORM;$i;$DOCKERS;$START;$END" >> $VERSION/fio.times;
	sleep 60;
done;

rm -r fio-data
START=`date -u`;
sleep 600;
END=`date -u`;
echo "$PLATFORM;0;$DOCKERS;$START;$END" >> $VERSION/fio.times;

#curl -kn https://api.grid5000.fr/stable/sites/grenoble/metrics/power/timeseries/?job_id="${OAR_JOB_ID}" > ./$1/energy_"${OAR_JOB_ID}".json;
