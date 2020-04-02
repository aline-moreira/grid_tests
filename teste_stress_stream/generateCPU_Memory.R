files <- list.files(path = "./", pattern = "*.log")

for (file in files) {
    png(paste0("./",gsub(".log$",".png",file)), width=1400,height=900)
    dt <- read.table(gsub(" ", "", paste0("./",file)),header=FALSE,sep="")
    dt$`Elapsed time` <- seq.int(nrow(dt))
    names(dt) <- c("CPU (%)","Elapsed time")
    
    q <- quantile(dt$`CPU (%)`, c(0.1, 0.9))
    
    dt <- dt[dt$`CPU (%)` >= q[1] & dt$`CPU (%)` <= q[2], ]
    
    print(file)
    print(paste("Média:",mean(dt$`CPU (%)`)))
    
    cex <- 2
    par(cex.lab=cex, cex.axis=cex, cex.main=cex)
    par(mar=c(6,9,5,10))
    ## Plot first set of data and draw its axis
    plot(dt$`Elapsed time`, dt$`CPU (%)`, pch=16, axes=FALSE, xlab="", ylab="", 
         type="s",col="black", main="Correlação consumo CPU e Memória")
    axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
    mtext("CPU (%)",side=2,line=6.5, cex=cex)
    box()
    
    ## Draw the time axis
    axis(1,pretty(range(dt$`Elapsed time`),10))
    mtext("Tempo (Segundos)",side=1,col="black",line=4, cex=cex)  
    
    dev.off()
}
