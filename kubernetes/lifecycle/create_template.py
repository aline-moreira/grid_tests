text = """apiVersion: batch/v1
kind: Job
metadata:
    name: lifecycle-$ITEM
    labels:
        jobgroup: lifecycle
spec:
    template:
        metadata:
            name: lifecycle
            labels:
                jobgroup: lifecycle
        spec:
            containers:
"""

size = int(input("Digite a quantidade de containers no pod: "))

for i in range(size):
    text += '                - name: lifecycle-container-'+str(i+1)+'\n'
    text += '                  image: busybox\n'
    text += '                  command: ["sh","-c","echo Processing item '+str(i+1)+' && sleep 10"]\n'

text += "            restartPolicy: Never\n"

file = open("lifecycle-"+str(size)+'pods.yaml', "w")
file.write(text)
file.close()
