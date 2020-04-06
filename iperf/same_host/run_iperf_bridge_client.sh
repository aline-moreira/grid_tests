ip_iperf=`docker inspect --format "{{ .NetworkSettings.IPAddress }}" iperf3-server`

for band in 1M 10M 100M 1000M; do
	START=`date -u`;
	docker run -it --rm networkstatic/iperf3 -c $ip_iperf -b $band -t 600;
	END=`date -u`;
	echo "bridge;$band;$START;$END" >> iperf.times;
	sleep 60;
done;
START=`date -u` && sleep 600 && END=`date -u` && echo "bridge;0;$START;$END" >> iperf.times;
