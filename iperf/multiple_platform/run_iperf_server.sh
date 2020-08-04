if [ "$#" -ne 2 ] ; then
	echo "Illegal number of parameters (version, platform)";
	exit 2;
fi

echo `hostname -i` 

VERSION=$1;
PLATFORM=$2;

echo "$PLATFORM;server;normal;`hostname`" >> $VERSION/nodes-server.types

iperf3 -p 5201 -s
