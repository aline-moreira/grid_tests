if [ "$#" -ne 1 ]; then
	echo "Illegal number of parameters (version)";
	exit 2;
fi

echo `hostname -i` > ip_server;

VERSION=$1;

echo "server;cpu;`hostname`" >> $VERSION/nodes.types

docker run --name iperf3-server --network=host -itd --name=iperf3-server -p 5201:5201 networkstatic/iperf3 -s 

FINISH=0;
CS=0;
CD=0;
ALL=0;
while : ; do
	cat client-start 2>/dev/null && CS=1;
	if [ $CS -eq 1 ]; then
		TEST=`cat client-start`;
	       	echo "SERVER RUNNING TEST $TEST";
	        bash top_loop.sh $VERSION/server-$TEST.cpu_log & echo "Top Loop Running PID $!" && CPU_PROC=$!;
		rm client-start;
		CS=0;
	fi

	cat client-done 2>/dev/null && CD=1;
        if [ $CD -eq 1 ]; then
		echo "TEST END KILLING TOP_LOOP $CPU_PROC";
	        kill $CPU_PROC;
	        rm client-done;
		CD=0;
	fi

	cat all-done 2>/dev/null && ALL=1; 
        if [ $ALL -eq 1 ]; then
		echo "ALL TESTS END";
	        FINISH=1;
		ALL=0;
		rm all-done;
	fi

	if [ $FINISH -eq 1 ]; then
		echo "FINISH SERVER";
 		break;
	fi
done;

docker rm -f iperf3-server
