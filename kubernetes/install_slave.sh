sudo-g5k swapoff -a
docker -v || sudo-g5k /grid5000/code/bin/g5k-setup-docker 

sudo-g5k apt-get update && sudo apt-get install -y apt-transport-https curl vim htop

sudo-g5k curl -s https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -

sudo-g5k cat <<EOF | sudo tee /etc/apt/sources.list.d/kubernetes.list
deb https://apt.kubernetes.io/ kubernetes-xenial main
EOF

sudo-g5k apt-get update

sudo-g5k apt-get install -y kubelet kubeadm kubectl

sudo-g5k apt-mark hold kubelet kubeadm kubectl

# Precisa reiniciar o kubelet
sudo-g5k systemctl daemon-reload
sudo-g5k systemctl restart kubelet

cd grid_tests/kubernetes/

IP=`cat adm_config/IP`
TOKEN=`cat adm_config/token`
CERT=`cat adm_config/certificado`

sudo-g5k kubeadm join --token $TOKEN $IP:6443 --discovery-token-ca-cert-hash sha256:$CERT

docker pull busybox
