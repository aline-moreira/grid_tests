packages <- c("ggplot2")

for(package in packages){
    if(!require(package, character.only = TRUE)){
        install.packages(package, dep=TRUE)
        if(!require(package, character.only=TRUE)) stop("Pacote indisponivel")
        
    }
    library(package, character.only=TRUE)
}
rm(packages)
rm(package)

df <- read.table("summary.log",header=TRUE,sep=";", stringsAsFactors=FALSE)

#ggplot(data=df, aes(x=Test, y=GFlops, fill=Test)) +
#    geom_bar(stat="identity")+
#    geom_hline(yintercept=df$GFlops[[1]], linetype="dashed", color = "red")+
#    geom_hline(yintercept=df$GFlops[[2]], linetype="dashed", color = "red")+
#    geom_hline(yintercept=df$GFlops[[3]], linetype="dashed", color = "red")+
#    geom_hline(yintercept=df$GFlops[[4]], linetype="dashed", color = "red")+
#    theme_classic()

host <- read.table("host_filter.log",header=TRUE,sep=";", stringsAsFactors=FALSE)
container <- read.table("container_filter.log",header=TRUE,sep=";", stringsAsFactors=FALSE)
vm <- read.table("vm_filter.log",header=TRUE,sep=";", stringsAsFactors=FALSE)
vm_container <- read.table("vm_container_filter.log",header=TRUE,sep=";", stringsAsFactors=FALSE)

quart_host <-quantile(host$GFlops, c(.1, .9))
quart_vm <-quantile(vm$GFlops, c(.1, .9))
quart_container <-quantile(container$GFlops, c(.1, .9))
quart_vm_container <-quantile(vm_container$GFlops, c(.1, .9))


df$Norm <- c(
    mean(host[host$GFlops > quart_host[[1]] & host$GFlops < quart_host[[2]],]$GFlops),
    mean(container[container$GFlops > quart_container[[1]] & container$GFlops < quart_container[[2]],]$GFlops),
    mean(vm[vm$GFlops > quart_vm[[1]] & vm$GFlops < quart_vm[[2]],]$GFlops),
    mean(vm_container[vm_container$GFlops > quart_vm_container[[1]] & vm_container$GFlops < quart_vm_container[[2]],]$GFlops)
)

p <- ggplot(data=df, aes(x=Test, y=Norm, fill=Test)) +
    geom_bar(stat="identity")+
    geom_hline(yintercept=df$Norm[[1]], linetype="dashed", color = "red")+
    geom_hline(yintercept=df$Norm[[2]], linetype="dashed", color = "red")+
    geom_hline(yintercept=df$Norm[[3]], linetype="dashed", color = "red")+
    geom_hline(yintercept=df$Norm[[4]], linetype="dashed", color = "red")+
    theme_classic()+
    labs(x = "Ambiente", y = "GFlops")+
    scale_fill_discrete(name = "Ambiente", labels = c("Contêiner", "Host", "MV","Contêiner sobre MV"))+
    scale_x_discrete(limits=c("Host", "Contêiner", "MV","Contêiner sobre MV"))+
    theme(
        legend.position="top"
    )+ 
    scale_colour_brewer("Colors in Spectral", palette="Spectral")

tiff("result.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
plot(p)
dev.off()
system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")