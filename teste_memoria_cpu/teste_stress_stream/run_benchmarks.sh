if [ "$#" -ne 3 ]; then
	echo "Illegal number of parameters (CPU, RAM, VERSION_TEST)";
	exit 2;
fi

CPU=$1;
RAM=$2;
VERSION=$3;

echo "RUNNING STREAM WITH $CPU CPU AND $RAM MEMORY";
bash run_stream_loop.sh $CPU $VERSION $RAM;
echo "STREAM [OK]";
sleep 60;

echo "RUNNING STRESS WITH $CPU CPU AND $RAM MEMORY";
bash run_stress.sh $CPU $RAM 600 $VERSION;
echo "STRESS [OK]";
sleep 60;
