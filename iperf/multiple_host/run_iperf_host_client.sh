ip_iperf=`cat ip_server`;
echo $ip_iperf;

for clients in 1 10 50 100; do
	for band in 1M 10M 100M 1000M; do
		START=`date -u`;
		docker run -it --network host --rm networkstatic/iperf3 -c $ip_iperf -b $band -t 600 -P $clients;
		END=`date -u`;
		echo "host;$clients;$band;$START;$END" >> iperf.times;
		sleep 60;
	done;
done;

START=`date -u` && sleep 600 && END=`date -u` && echo "host;0;0;$START;$END" >> iperf.times;
