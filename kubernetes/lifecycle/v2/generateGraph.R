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

times<- read.table("lifecycle.times",header=FALSE,sep=";")
names(times) <- c("pods","containers","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

times$total_containers <- times$pods * times$containers

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1925045.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values
host <- "master"

energy1 <- data.frame(time,values, host)

#energy$items[[number_of_host]]
energy$items[[2]]$timestamps = as.POSIXct(energy$items[[2]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[2]]$timestamps
values <- energy$items[[2]]$values
host <- "slave 1"

energy2 <- data.frame(time,values, host)

#energy$items[[number_of_host]]
energy$items[[3]]$timestamps = as.POSIXct(energy$items[[3]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[3]]$timestamps
values <- energy$items[[3]]$values
host <- "slave 2"

energy3 <- data.frame(time,values, host)

#energy <- rbind(energy1,energy2)
#energy <- rbind(energy,energy3)
energy <- energy1

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
pod_1 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==1] &
        energy$tempo <= times$end[times$total_containers==1]
)
pod_1$total_containers <- 1
q <- quantile(pod_1$consumo, c(0.1, 0.9))
pod_1 <- pod_1[pod_1$consumo >= q[1] & pod_1$consumo <= q[2], ]

pod_256 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==256] &
        energy$tempo <= times$end[times$total_containers==256]
)
pod_256$total_containers <- 256
q <- quantile(pod_256$consumo, c(0.1, 0.9))
pod_256 <- pod_256[pod_256$consumo >= q[1] & pod_256$consumo <= q[2], ]

pod_512 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==512] &
        energy$tempo <= times$end[times$total_containers==512]
)
pod_512$total_containers <- 512
q <- quantile(pod_512$consumo, c(0.1, 0.9))
pod_512 <- pod_512[pod_512$consumo >= q[1] & pod_512$consumo <= q[2], ]

pod_1024 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==1024] &
        energy$tempo <= times$end[times$total_containers==1024]
)
pod_1024$total_containers <- 1024
q <- quantile(pod_1024$consumo, c(0.1, 0.9))
pod_1024 <- pod_1024[pod_1024$consumo >= q[1] & pod_1024$consumo <= q[2], ]

pod_2048 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==2048] &
        energy$tempo <= times$end[times$total_containers==2048]
)
pod_2048$total_containers <- 2048
q <- quantile(pod_2048$consumo, c(0.1, 0.9))
pod_2048 <- pod_2048[pod_2048$consumo >= q[1] & pod_2048$consumo <= q[2], ]

pod_4096 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==4096] &
        energy$tempo <= times$end[times$total_containers==4096]
)
pod_4096$total_containers <- 4096
q <- quantile(pod_4096$consumo, c(0.1, 0.9))
pod_4096 <- pod_4096[pod_4096$consumo >= q[1] & pod_4096$consumo <= q[2], ]

pod_8192 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==8192] &
        energy$tempo <= times$end[times$total_containers==8192]
)
pod_8192$total_containers <- 8192
q <- quantile(pod_8192$consumo, c(0.1, 0.9))
pod_8192 <- pod_8192[pod_8192$consumo >= q[1] & pod_8192$consumo <= q[2], ]

pod_16384 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==16384] &
        energy$tempo <= times$end[times$total_containers==16384]
)
pod_16384$total_containers <- 16384
q <- quantile(pod_16384$consumo, c(0.1, 0.9))
pod_16384 <- pod_16384[pod_16384$consumo >= q[1] & pod_16384$consumo <= q[2], ]

pod_32768 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==32768] &
        energy$tempo <= times$end[times$total_containers==32768]
)
pod_32768$total_containers <- 32768
q <- quantile(pod_32768$consumo, c(0.1, 0.9))
pod_32768 <- pod_32768[pod_32768$consumo >= q[1] & pod_32768$consumo <= q[2], ]

dt_tests <- rbind(pod_1, pod_256)
dt_tests <- rbind(dt_tests,pod_512)
dt_tests <- rbind(dt_tests,pod_1024)
dt_tests <- rbind(dt_tests,pod_2048)
dt_tests <- rbind(dt_tests,pod_4096)
dt_tests <- rbind(dt_tests,pod_8192)
dt_tests <- rbind(dt_tests,pod_16384)
dt_tests <- rbind(dt_tests,pod_32768)

rm(pod_1)
rm(pod_256)
rm(pod_512)
rm(pod_1024)
rm(pod_2048)
rm(pod_4096)
rm(pod_8192)
rm(pod_16384)
rm(pod_32768)

tiff("sysbench_kubernetes_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=as.factor(total_containers), y=consumo, color=as.factor(node)))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
    #geom_abline( mapping=aes(slope=0, intercept=186.6, 
    #                         colour="idle"), linetype="dashed")+
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
        x="Quantidade de Contêineres",
        y="Consumo (W/s)",
        color= "Tipo de hospedeiro"
    )#+
    # scale_x_discrete(
    #     limits=c(
    #         "0",
    #         "1",
    #         "2",
    #         "4",
    #         "8",
    #         "16",
    #         "32",
    #         "64",
    #         "128",
    #         "256"
    #     ),
    #     labels=c(
    #          "idle",
    #          "1 cpu",
    #          "2 cpu",
    #          "4 cpu",
    #          "8 cpu",
    #          "16 cpu",
    #          "32 cpu",
    #          "64 cpu",
    #          "128 cpu",
    #          "256 cpu"
    # ))

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
