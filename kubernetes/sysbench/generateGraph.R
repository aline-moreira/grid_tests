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

times<- read.table("kubernetes.times",header=FALSE,sep=";")
names(times) <- c("cpus","start","end")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1924132.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values
host <- "slave1"

energy1 <- data.frame(time,values, host)

#energy$items[[number_of_host]]
energy$items[[2]]$timestamps = as.POSIXct(energy$items[[2]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[2]]$timestamps
values <- energy$items[[2]]$values
host <- "master"

energy2 <- data.frame(time,values, host)

#energy$items[[number_of_host]]
energy$items[[3]]$timestamps = as.POSIXct(energy$items[[3]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[3]]$timestamps
values <- energy$items[[3]]$values
host <- "slave 2"

energy3 <- data.frame(time,values, host)

energy <- rbind(energy1,energy2)
energy <- rbind(energy,energy3)

rm(time)
rm(values)
rm(host)
rm(energy1)
rm(energy2)
rm(energy3)

names(energy) <- c("tempo","consumo","node")

###################################################
# Filtrando a energia de acordo com os tempos
###################################################
sysbench0 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==0] &
        energy$tempo <= times$end[times$cpus==0]
)
sysbench0$cpus <- 0
q <- quantile(sysbench0$consumo, c(0.1, 0.9))
sysbench0 <- sysbench0[sysbench0$consumo >= q[1] & sysbench0$consumo <= q[2], ]


sysbench1 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==1] &
        energy$tempo <= times$end[times$cpus==1]
)
sysbench1$cpus <- 1
q <- quantile(sysbench1$consumo, c(0.1, 0.9))
sysbench1 <- sysbench1[sysbench1$consumo >= q[1] & sysbench1$consumo <= q[2], ]

sysbench2 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==2] &
        energy$tempo <= times$end[times$cpus==2]
)
sysbench2$cpus <- 2
q <- quantile(sysbench2$consumo, c(0.1, 0.9))
sysbench2 <- sysbench2[sysbench2$consumo >= q[1] & sysbench2$consumo <= q[2], ]

sysbench4 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==4] &
        energy$tempo <= times$end[times$cpus==4]
)
sysbench4$cpus <- 4
q <- quantile(sysbench4$consumo, c(0.1, 0.9))
sysbench4 <- sysbench4[sysbench4$consumo >= q[1] & sysbench4$consumo <= q[2], ]

sysbench8 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==8] &
        energy$tempo <= times$end[times$cpus==8]
)
sysbench8$cpus <- 8
q <- quantile(sysbench8$consumo, c(0.1, 0.9))
sysbench8 <- sysbench8[sysbench8$consumo >= q[1] & sysbench8$consumo <= q[2], ]

sysbench16 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==16] &
        energy$tempo <= times$end[times$cpus==16]
)
sysbench16$cpus <- 16
q <- quantile(sysbench16$consumo, c(0.1, 0.9))
sysbench16 <- sysbench16[sysbench16$consumo >= q[1] & sysbench16$consumo <= q[2], ]

sysbench32 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==32] &
        energy$tempo <= times$end[times$cpus==32]
)
sysbench32$cpus <- 32
q <- quantile(sysbench32$consumo, c(0.1, 0.9))
sysbench32 <- sysbench32[sysbench32$consumo >= q[1] & sysbench32$consumo <= q[2], ]

sysbench64 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==64] &
        energy$tempo <= times$end[times$cpus==64]
)
sysbench64$cpus <- 64
q <- quantile(sysbench64$consumo, c(0.1, 0.9))
sysbench64 <- sysbench64[sysbench64$consumo >= q[1] & sysbench64$consumo <= q[2], ]

sysbench128 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==128] &
        energy$tempo <= times$end[times$cpus==128]
)
sysbench128$cpus <- 128
q <- quantile(sysbench128$consumo, c(0.1, 0.9))
sysbench128 <- sysbench128[sysbench128$consumo >= q[1] & sysbench128$consumo <= q[2], ]

sysbench256 <- energy %>% filter(
    energy$tempo >= times$start[times$cpus==256] &
        energy$tempo <= times$end[times$cpus==256]
)
sysbench256$cpus <- 256
q <- quantile(sysbench256$consumo, c(0.1, 0.9))
sysbench256 <- sysbench256[sysbench256$consumo >= q[1] & sysbench256$consumo <= q[2], ]

dt_tests <- rbind(sysbench0, sysbench1)
dt_tests <- rbind(dt_tests,sysbench2)
dt_tests <- rbind(dt_tests,sysbench4)
dt_tests <- rbind(dt_tests,sysbench8)
dt_tests <- rbind(dt_tests,sysbench16)
dt_tests <- rbind(dt_tests,sysbench32)
dt_tests <- rbind(dt_tests,sysbench64)
dt_tests <- rbind(dt_tests,sysbench128)
dt_tests <- rbind(dt_tests,sysbench256)

rm(sysbench0)
rm(sysbench1)
rm(sysbench2)
rm(sysbench4)
rm(sysbench8)
rm(sysbench16)
rm(sysbench32)
rm(sysbench64)
rm(sysbench128)
rm(sysbench256)

tiff("sysbench_kubernetes_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=as.factor(cpus), y=consumo, color=as.factor(node)))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
    theme_classic()+
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
        x="CPUs",
        y="Consumo (W/s)",
        color= "Configuração de CPU do Contêiner"
    )+
    scale_x_discrete(
        limits=c(
            "0",
            "1",
            "2",
            "4",
            "8",
            "16",
            "32",
            "64",
            "128",
            "256"
        ),
        labels=c(
             "idle",
             "1 cpu",
             "2 cpu",
             "4 cpu",
             "8 cpu",
             "16 cpu",
             "32 cpu",
             "64 cpu",
             "128 cpu",
             "256 cpu"
    ))

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
