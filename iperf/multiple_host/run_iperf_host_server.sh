echo `hostname -i` > ip_server

docker run  --network=host -it --rm --name=iperf3-server -p 5201:5201 networkstatic/iperf3 -s
