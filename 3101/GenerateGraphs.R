packages <- c("ggplot2","dplyr","scales","ggsci","rjson")

for(package in packages){
    if(!require(package, character.only = TRUE)){
        install.packages(package, dep=TRUE)
        if(!require(package, character.only=TRUE)) stop("Pacote indisponivel")
        
    }
    library(package, character.only=TRUE)
}
rm(packages)
rm(package)

library(dplyr)

pallet_colors <- c("#00AFBB", "#E7B800", "#FC4E07","#52854C","#FFDB6D","#4E84C4")

times<- read.table("sysbench_docker.log",header=TRUE,sep=";")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%a, %d %b %Y %H:%M:%S %z")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%a, %d %b %Y %H:%M:%S %z")

scaleFUN <- function(x) round(as.numeric(x), digits=0)

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1085913.json")

energy <- as.data.frame(energy)

energy$items.timestamps = as.POSIXct(energy$items.timestamps, origin="1970-01-01",tz="UTC")

energy <- energy %>% select(items.timestamps,items.values)

names(energy) <- c("tempo","servidor")

###################################################
# CPU
###################################################

h_0 <- energy %>% filter(
    energy$tempo >= times$start[times$type=="Host" & times$vcpu == 0] & energy$tempo <= times$end[times$type=="Host" & times$vcpu == 0] 
)
h_0$value <- -1


d_0 <- energy %>% filter(
    energy$tempo >= times$start[times$type=="Docker" & times$vcpu == 0] & energy$tempo <= times$end[times$type=="Docker" & times$vcpu == 0] 
)
d_0$value <- 0

d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$type=="Docker" & times$vcpu == 1] & energy$tempo <= times$end[times$type=="Docker" & times$vcpu == 1] 
)
d_1$value <- 1

d_2 <- energy %>% filter(
    energy$tempo >= times$start[times$type=="Docker" & times$vcpu == 2] & energy$tempo <= times$end[times$type=="Docker" & times$vcpu == 2] 
)
d_2$value <- 2

d_3 <- energy %>% filter(
    energy$tempo >= times$start[times$type=="Docker" & times$vcpu == 3] & energy$tempo <= times$end[times$type=="Docker" & times$vcpu == 3] 
)
d_3$value <- 3

d_4 <- energy %>% filter(
    energy$tempo >= times$start[times$type=="Docker" & times$vcpu == 4] & energy$tempo <= times$end[times$type=="Docker" & times$vcpu == 4] 
)
d_4$value <- 4


cpu <- rbind(h_0, d_0)
cpu <- rbind(cpu, d_1)
cpu <- rbind(cpu, d_2)
cpu <- rbind(cpu, d_3)
cpu <- rbind(cpu, d_4)

cpu_mean <- as.data.frame(t(data.frame(
    #c(scaleFUN(mean(cpu$servidor[cpu$value==-1])),-1),
    c(scaleFUN(mean(cpu$servidor[cpu$value==0])),0),
    c(scaleFUN(mean(cpu$servidor[cpu$value==1])),25),
    c(scaleFUN(mean(cpu$servidor[cpu$value==2])),50),
    c(scaleFUN(mean(cpu$servidor[cpu$value==3])),75),
    c(scaleFUN(mean(cpu$servidor[cpu$value==4])),100)
)))
colnames(cpu_mean)<- c("power","vcpu")

gprice <- function(x) {
    price_cpu = (cpu_mean$power[cpu_mean$vcpu==x] - cpu_mean$power[cpu_mean$vcpu==0]) / 80
    return(price_cpu * 0.006072 * (x / 25))
}

cpu_mean$price <- c(
    gprice(0),
    gprice(25),
    gprice(50),
    gprice(75),
    gprice(100)
    )

amazon_pricing <- function(x){
    energy_consumed_cpu <- (cpu_mean$power[cpu_mean$vcpu==100] - cpu_mean$power[cpu_mean$vcpu==0]) / 80
        price <-  0.04048 * x * 4
        
    return(price)
}

amazon <- c(
    amazon_pricing(0.05),
    amazon_pricing(0.10),
    amazon_pricing(0.15)
)

tiff("cpu.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
cpu_plot <- ggplot(data=cpu_mean, aes(x=vcpu, y=price))+
    geom_line(aes(linetype="Proposta"))+
    geom_hline(aes(yintercept = amazon[1], linetype=" 5% AWS"), 
               color = "blue", size=1)+
    geom_hline(aes(yintercept = amazon[2], linetype="10% AWS"), 
               color = "red", size=1)+
    geom_hline(aes(yintercept = amazon[3], linetype="15% AWS"),
               color = "green", size=1)+
    theme_classic()+
    scale_linetype_manual(name = "Modelo de custo", values = c(3, 4, 5, 1),
                          guide = guide_legend(override.aes = list(color = c("blue", "red", "green", "black"))))+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 0,
            hjust = 0.7,
            size=12
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.key = element_blank(),
        legend.box = "vertical"
    )+
    labs(
        x="Carga de trabalho (%)",
        y="Preço (US$/hora)"
    )
# +
# scale_x_discrete(limits=c("0%","25%","50%","75%","100%"))
# scale_fill_d3(labels = c("Docker", "Docker sobre MV", "Host", "MV"))

plot(cpu_plot)
dev.off()

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("mv *.png graphs")
system("rm *.tiff")
