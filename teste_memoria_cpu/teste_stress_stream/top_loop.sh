if [[ $# -ne 1 ]]; then
	echo "Illegal number of parameters (FILE NAME)"
	exit 2
fi
while true; do
	top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9. │]*\)%* id.*/\1/" | awk '{print (100 - $1)}' >> $1
done;
