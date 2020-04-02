text = """apiVersion: batch/v1
kind: Job
metadata:
\tname: lifecycle-$ITEM
\tlabels:
\t\tjobgroup: lifecycle
spec:
\ttemplate:
\t\tmetadata:
\t\t\tname: lifecycle
\t\t\tlabels:
\t\t\t\tjobgroup: lifecycle
\t\tspec:
\t\t\tcontainers:
"""

size = int(input("Digite a quantidade de containers no pod: "))

for i in range(size):
    text += '\t\t\t\t- name: lifecycle-container-'+str(i+1)+'\n'
    text += '\t\t\t\t  image: busybox\n'
    text += '\t\t\t\t  command: ["sh","-c","echo Processing item '+str(i+1)+' && sleep 10"]\n'

text += "\t\t\trestartPolicy: Never\n"

file = open("lifecycle-"+str(size)+'pods.yaml', "w")
file.write(text)
file.close()