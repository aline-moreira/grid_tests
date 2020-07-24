if [ "$#" -ne 1 ] ; then
	echo "Illegal number of parameters (version)";
	exit 2;
fi

echo `hostname -i` > ip_server

VERSION=$1;

echo "server;normal;`hostname`" >> $VERSION/nodes.types

docker run  --network=host -it --rm --name=iperf3-server -p 5201:5201 networkstatic/iperf3 -s
