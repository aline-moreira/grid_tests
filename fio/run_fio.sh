if [ "$#" -ne 1 ]; then
	echo "Illegal number of parameters (VERSION_TEST)";
	exit 2;
fi

ls $1 | mkdir $1
ls $1/logs | mkdir $1/logs

for i in 1 5 10 50 100 500 1000; do
	START=`date -u`;
	docker run --rm -v `pwd`/:/root/fio ljishen/fio  --randrepeat=1 --ioengine=libaio --direct=1 --gtod_reduce=1 --name=test --filename=test --bs=4k --iodepth=64 --readwrite=randread --directory=/root/fio --output=/root/fio/$1/logs/teste_fio_"${i}"G.log --size="${i}"G;
	END=`date -u`;
	echo "$i;$START;$END" >> $1/fio.times;
	sleep 60;
done;

START=`date -u`;
sleep 600;
END=`date -u`;
echo "0;$START;$END" >> $1/fio.times;

curl -kn https://api.grid5000.fr/stable/sites/grenoble/metrics/power/timeseries/?job_id="${OAR_JOB_ID}" > ./$1/energy_"${OAR_JOB_ID}".json;
