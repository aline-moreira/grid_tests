dt <- read.table("sysbench_memory_8G.log",header=TRUE,sep=";")

## Plot first set of data and draw its axis
plot(dt$Elapsed.time, dt$CPU...., pch=16, axes=FALSE, xlab="", ylab="", 
     type="b",col="black", main="Correlação consumo CPU e Memória")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("CPU (%)",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(dt$Elapsed.time, dt$Real..MB., pch=15,  xlab="", ylab="", ylim=c(0,7000), 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Memória (Mb)",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(dt$Elapsed.time),10))
mtext("Tempo (Segundos)",side=1,col="black",line=2.5)  
