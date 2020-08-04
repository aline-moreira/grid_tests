if [ "$#" -ne 1 ] ; then
	echo "Illegal number of parameters (version)";
	exit 2;
fi

echo `hostname -i`

VERSION=$1;
PLATFORM=$2;

echo "$PLATFORM;server;normal;`hostname`" >> $VERSION/nodes-server.types

docker run  --network=host -it --rm --name=iperf3-server -p 5201:5201 networkstatic/iperf3 -s
