if [ "$#" -ne 2 ]; then 
	echo "Illegal number of parameters (version, platform)";
	exit 2;
fi

VERSION=$1;
PLATFORM=$2;

echo "$PLATFORM;client;normal;`hostname`" >> $VERSION/nodes-client.types

TIME=600;
ip_iperf=10.132.7.253;
echo $ip_iperf;

for i in 1; do 
	for band in 1M 10M 100M 1000M 10000M; do
		START=`date -u`;
		docker run -it --network host --rm networkstatic/iperf3 -c $ip_iperf -b $band -t $TIME;
		END=`date -u`;
		echo "$PLATFORM;normal;$i;$band;$START;$END" >> $VERSION/iperf-client.times;
		sleep 60;
	done;
done;

START=`date -u` && sleep $TIME && END=`date -u` && echo "$PLATFORM;normal;1;0;$START;$END" >> $VERSION/iperf-client.times;
