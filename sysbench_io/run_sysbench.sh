if [ "$#" -ne 1 ]; then
	echo "Illegal number of parameters (VERSION)";
	exit 2;
fi

ls $1 | mkdir $1;
ls $1/results | mkdir $1/results
ls $1/workdir | mkdir $1/workdir

filesize=(1G); #(1M 10M 100M 1G 10G);
totalfiles=(31250); #(32 313 3125 31250 312500);

for i in 1 5 10 50 100 500 1000; do
#for i in ${!filesize[@]}; do
	#Prepare data
	docker run --rm \
		-v `pwd`/$1/workdir:/root/workdir \
		-v `pwd`/$1/results:/root/results \
		ljishen/sysbench \
		/root/results/output_file_prepare_"${filesize[$i]}".prof \
		--test=fileio \
		--file-total-size="${i}G" \
		prepare;

	#Run test
	START=`date -u`;
	docker run --rm \
		-v `pwd`/$1/workdir:/root/workdir \
		-v `pwd`/$1/results:/root/results \
		ljishen/sysbench \
		/root/output_fileio_run_"${filesize[$i]}".prof \
		--test=fileio \
		--file-total-size="${i}G" \
		--init-rng=on \
		--file-test-mode=rndwr \
		run;
	END=`date -u`;
	echo "$i;$START;$END" >> $1/sysbench.times;

	#Clean data
	docker run --rm \
		-v `pwd`/$1/workdir:/root/workdir \
		-v `pwd`/$1/results:/root/results \
		ljishen/sysbench \
		/root/results/output_fileio_clean_"${filesize[$i]}".prof \
		--test=fileio \
		--file-total-size="${i}G" \
		cleanup;
done;

