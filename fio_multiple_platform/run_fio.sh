if [ "$#" -ne 2 ]; then
	echo "Illegal number of parameters (PLATFORM, VERSION_TEST)";
	exit 2;
fi

PLATFORM=$1;
VERSION=$2;

ls $VERSION | mkdir $VERSION;
ls $VERSION/logs | mkdir $VERSION/logs;

#if [ "$PLATFORM" = "host" ] || [ "$PLATFORM" = "vm" ]; then
#	sudo-g5k apt-get install fio -y
#fi

for i in 1 5 10 50; do
	START=`date -u`;
	if [ "$PLATFORM" = "host" ] || [ "$PLATFORM" = "vm" ]; then
		fio --randrepeat=1 --ioengine=libaio --direct=1 --gtod_reduce=1 --name=test --filename=test --bs=4k --iodepth=64 --readwrite=randread --directory=./ --output=./$VERSION/logs/teste_fio_"${i}"G_"${PLATFORM}".log --size="${i}"G;
	fi
	if [ "$PLATFORM" = "docker" ]  || [ "$PLATFORM" = "vm_docker" ]; then
		docker run --rm -v `pwd`/:/root/fio ljishen/fio  --randrepeat=1 --ioengine=libaio --direct=1 --gtod_reduce=1 --name=test --filename=test --bs=4k --iodepth=64 --readwrite=randread --directory=/root/fio --output=/root/fio/$VERSION/logs/teste_fio_"${i}"G_"${PLATFORM}".log --size="${i}"G;
	fi
	END=`date -u`;
	echo "$PLATFORM;$i;$START;$END" >> $VERSION/fio.times;
	sleep 60;
done;

START=`date -u`;
sleep 600;
END=`date -u`;
echo "$PLATFORM;0;$START;$END" >> $VERSION/fio.times;

#curl -kn https://api.grid5000.fr/stable/sites/grenoble/metrics/power/timeseries/?job_id="${OAR_JOB_ID}" > ./$1/energy_"${OAR_JOB_ID}".json;
