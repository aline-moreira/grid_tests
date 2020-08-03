if [ "$#" -ne 1 ]; then 
	echo "Illegal number of parameters (version)";
	exit 2;
fi

VERSION=$1;

echo "client;normal;`hostname`" >> $VERSION/nodes.types

TIME=600;
ip_iperf=`cat ip_server`;
echo $ip_iperf;

for i in 1; do 
	for band in 1M 10M 100M 1000M 10000M; do
		START=`date -u`;
		docker run -it --network host --rm networkstatic/iperf3 -c $ip_iperf -b $band -t $TIME;
		END=`date -u`;
		echo "host;normal;$i;$band;$START;$END" >> $VERSION/iperf.times;
		sleep 60;
	done;
done;

START=`date -u` && sleep $TIME && END=`date -u` && echo "host;normal;1;0;$START;$END" >> $VERSION/iperf.times;