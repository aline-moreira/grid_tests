ip_iperf=`hostname -i`
echo $ip_iperf

for band in 1M 10M 100M 1000M; do
	START=`date -u`;
	docker run -it --network host --rm networkstatic/iperf3 -c $ip_iperf -b $band -t 600;
	END=`date -u`;
	echo "host;$band;$START;$END" >> iperf.times;
	sleep 60;
done;

START=`date -u` && sleep 600 && END=`date -u` && echo "host;0;$START;$END" >> iperf.times;
