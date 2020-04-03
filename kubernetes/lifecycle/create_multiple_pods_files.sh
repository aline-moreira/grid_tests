if [ "$#" -ne 2 ]; then
	echo "Invalid number of parameters (Ammount of pods, ammount of containers in each pod)";
	exit 2;
fi;

rm -rf ./lifecycle-$1 && mkdir ./lifecycle-$1;

for i in `seq $1`; do
	cat lifecycle-$2pods.yaml | sed "s/\$ITEM/$i/" > ./lifecycle-$1/lifecycle-$i.yaml;
done;

kubectl create -f ./lifecycle-$1;
