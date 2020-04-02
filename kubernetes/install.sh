sudo-g5k swapoff -a
docker -v || sudo-g5k /grid5000/code/bin/g5k-setup-docker 

sudo-g5k apt-get update && sudo apt-get install -y apt-transport-https curl vim htop

#INSTALANDO O GO
sudo-g5k cp -r ~/go /usr/local
export PATH=$PATH:/usr/local/go/bin

cd ~/
#INSTALANDO COMPONENTES KUBERNETES
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

sudo-g5k kubeadm init --apiserver-advertise-address $(hostname -i)

#Configura as permissões do cluster no usuário
mkdir -p $HOME/.kube
sudo-g5k cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
sudo-g5k chown $(id -u):$(id -g) $HOME/.kube/config

#Configurando a rede
#kubectl apply -f https://docs.projectcalico.org/v3.11/manifests/calico.yaml
#!! PARA VERIFICAR SE A INSTALACAO DA REDE DOS PODS FOI FEITA:
#!! kubectl get pods --all-namespaces e ver se o CoreDNSPod está ativo!!

#copiar o kustomize para o /usr/bin
cd grid_tests/kubernetes/

sudo-g5k cp kustomize /usr/bin/
#curl -s "https://raw.githubusercontent.com/\
#kubernetes-sigs/kustomize/master/hack/install_kustomize.sh"  | bashls

kustomize build github.com/xridge/kubestone/config/default | kubectl create -f -

#Agora falta fazer os outros 2 nós virarem escravos do kubeadm master para o cluster estar completo.
#Para isso, é necessário resgatar 2 informações, o token e o certificado do master
#Para o token execute 
kubeadm token list | sed -n 2p | awk '{printf $1}' > adm_config/token
#Para o certificado execute
openssl x509 -pubkey -in /etc/kubernetes/pki/ca.crt | openssl rsa -pubin -outform der 2>/dev/null | openssl dgst -sha256 -hex | sed 's/^.* //' > adm_config/certificado
#Para salvar o IP do master
echo `hostname -i` > adm_config/IP

#PARA PEGAR A LISTA DOS NOS CONECTADOS EXECUTE
# kubectl get nodes
# Até o momento os nós ainda não estão prontos, pq falta configurar a rede dos pods
kubectl apply -f "https://cloud.weave.works/k8s/net?k8s-version=$(kubectl version | base64 | tr -d '\n')"
# se rodar kubectl get nodes agora os nodes tem que aparecer READY, porém se não der certo, instale o wavenet em todos os nodes...

source <(kubectl completion bash) # setup autocomplete in bash into the current shell, bash-completion package should be installed first.
echo "source <(kubectl completion bash)" >> ~/.bashrc # add autocomplete permanently to your bash shell.

kubectl create namespace testes

oarsh `uniq $OAR_NODE_FILE | sed -n 2p` 'bash ~/grid_tests/kubernetes/install_slave.sh'
oarsh `uniq $OAR_NODE_FILE | sed -n 3p` 'bash ~/grid_tests/kubernetes/install_slave.sh'
