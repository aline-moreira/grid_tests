if [ "$#" -ne 1 ]; then
	echo "Illegal number of paramers (version)";
	exit 2;
fi

VERSION=$1;

echo "client;`hostname`" >> $VERSION/nodes.types

TIME=600;
ip_iperf=`cat ip_server`;
echo $ip_iperf;

for clients in 1 10 50 100; do
	for band in 1M 10M 100M 1000M; do
		echo "RUNNING $clients CLIENTS WITH $band"
		START=`date -u`;
		docker run --name iperf3-client -itd --network host networkstatic/iperf3 -c $ip_iperf -b $band -t $TIME -P $clients;
		echo "$clients-$band" > client-start;
		bash top_loop.sh $VERSION/client-$clients-$band.cpu_log & echo "Top Loop Running PID $!" && CPU_PROC=$!;
		sleep $TIME;
		touch client-done;
		kill $CPU_PROC;
		echo"Remove docker iperf3 client";
		docker rm -f iperf3-client;
		END=`date -u`;
		echo "host;$clients;$band;$START;$END" >> $VERSION/iperf.times;
		sleep 60;
	done;
done;

touch all-done;
rm ip_server;
sleep 60;

echo "RUNNING IN IDLE"

START=`date -u` && sleep 600 && END=`date -u` && echo "host;0;0;$START;$END" >> $VERSION/iperf.times;
