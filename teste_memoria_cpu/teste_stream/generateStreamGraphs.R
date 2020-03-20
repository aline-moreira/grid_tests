path_pinado <- "pinado_logs_v2"
path_nao_pinado <- "./nao_pinado_logs_v2"

files <- list.files(path = path_nao_pinado, pattern = "*.log")

for (file in files) {

    png(paste0("./graficos_nao_pinados/",gsub(".log$",".png",file)), width=1400,height=900)
  dt <- read.table(gsub(" ", "", paste0("nao_pinado_logs_v2/",file)),header=FALSE,sep="")
  names(dt) <- c("Elapsed time","CPU (%)","Real (MB)","Virtual (MB)")
  
  cex <- 2
  par(cex.lab=cex, cex.axis=cex, cex.main=cex)
  par(mar=c(6,9,5,10))
  ## Plot first set of data and draw its axis
  plot(dt$`Elapsed time`, dt$`CPU (%)`, pch=16, axes=FALSE, xlab="", ylab="", 
       type="s",col="black", main="Correlação consumo CPU e Memória (Nao Pinado)")
  axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
  mtext("CPU (%)",side=2,line=6.5, cex=cex)
  box()
  
  ## Allow a second plot on the same graph
  par(new=TRUE)
  
  ## Plot the second plot and put axis scale on right
  plot(dt$`Elapsed time`, dt$`Real (MB)`, pch=15,  xlab="", ylab="", 
       axes=FALSE, type="s", col="red")
  ## a little farther out (line=4) to make room for labels
  mtext("Memória (Mb)",side=4,col="red",line=7, cex=cex) 
  axis(4, col="red",col.axis="red",las=1)
  
  ## Draw the time axis
  axis(1,pretty(range(dt$`Elapsed time`),10))
  mtext("Tempo (Segundos)",side=1,col="black",line=4, cex=cex)  
  
  dev.off()
}

files <- list.files(path = path_pinado, pattern = "*.log")

for (file in files) {
  
  png(paste0("./graficos_pinado/",gsub(".log$",".png",file)), width=1400,height=900)
  dt <- read.table(gsub(" ", "", paste0("nao_pinado_logs_v2/",file)),header=FALSE,sep="")
  names(dt) <- c("Elapsed time","CPU (%)","Real (MB)","Virtual (MB)")
  
  cex <- 2
  par(cex.lab=cex, cex.axis=cex, cex.main=cex)
  par(mar=c(6,9,5,10))
  ## Plot first set of data and draw its axis
  plot(dt$`Elapsed time`, dt$`CPU (%)`, pch=16, axes=FALSE, xlab="", ylab="", 
       type="s",col="black", main="Correlação consumo CPU e Memória (Pinado)")
  
  axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
  mtext("CPU (%)",side=2,line=6.5, cex=cex)
  box()
  
  ## Allow a second plot on the same graph
  par(new=TRUE)
  
  ## Plot the second plot and put axis scale on right
  plot(dt$`Elapsed time`, dt$`Real (MB)`, pch=15,  xlab="", ylab="", 
       axes=FALSE, type="s", col="red")
  ## a little farther out (line=4) to make room for labels
  mtext("Memória (Mb)",side=4,col="red",line=7, cex=cex) 
  axis(4, col="red",col.axis="red",las=1)
  
  ## Draw the time axis
  axis(1,pretty(range(dt$`Elapsed time`),10))
  mtext("Tempo (Segundos)",side=1,col="black",line=4, cex=cex)  
  
  dev.off()
}
