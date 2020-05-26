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

times<- read.table("./benchmarks.time",header=FALSE,sep=";")
names(times) <- c("teste","plataforma","ram","inicio","fim")
times$inicio <- as.POSIXct(times$inicio,tz="UTC",  format="%a %d %b %Y %H:%M:%S")
times$fim <- as.POSIXct(times$fim,tz="UTC",  format="%a %d %b %Y %H:%M:%S")

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1931329.json")

energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values

energy1 <- data.frame(time,values)

names(energy1) <- c("tempo","consumo")
#----------
energy2 <- fromJSON(file="energy_1931323.json")

energy2$items[[1]]$timestamps = as.POSIXct(energy2$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy2$items[[1]]$timestamps
values <- energy2$items[[1]]$values

energy2 <- data.frame(time,values)

names(energy2) <- c("tempo","consumo")

energy <- rbind(energy1,energy2)
rm(energy1, energy2)

###################################################
# Filtrando a energia de acordo com os tempos
###################################################

#STREAM HOST IDLE
host_idle <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==0] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==0]
)
host_idle$teste <- "stream"
host_idle$plataforma <- "host"
host_idle$ram <- 0
# q <- quantile(host_idle$consumo, c(0.05, 0.95))
# host_idle <- host_idle[host_idle$consumo >= q[1] & host_idle$consumo <= q[2], ]

#STREAM 1GB
host_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==1]
)
host_1$teste <- "stream"
host_1$plataforma <- "host"
host_1$ram <- 1
# q <- quantile(host_1$consumo, c(0.05, 0.95))
# host_1 <- host_1[host_1$consumo >= q[1] & host_1$consumo <= q[2], ]

#STREAM 50GB
host_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==50]
)
host_50$teste <- "stream"
host_50$plataforma <- "host"
host_50$ram <- 50
# q <- quantile(host_50$consumo, c(0.05, 0.95))
# host_50 <- host_50[host_50$consumo >= q[1] & host_50$consumo <= q[2], ]

#STREAM 100GB
host_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==100]
)
host_100$teste <- "stream"
host_100$plataforma <- "host"
host_100$ram <- 100
# q <- quantile(host_100$consumo, c(0.05, 0.95))
# host_100 <- host_100[host_100$consumo >= q[1] & host_100$consumo <= q[2], ]

#STREAM 250GB
host_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==250]
)
host_250$teste <- "stream"
host_250$plataforma <- "host"
host_250$ram <- 250
# q <- quantile(host_250$consumo, c(0.05, 0.95))
# host_250 <- host_250[host_250$consumo >= q[1] & host_250$consumo <= q[2], ]

#STREAM 500GB
host_500 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==500] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==500]
)
host_500$teste <- "stream"
host_500$plataforma <- "host"
host_500$ram <- 500
# q <- quantile(host_500$consumo, c(0.05, 0.95))
# host_500 <- host_500[host_500$consumo >= q[1] & host_500$consumo <= q[2], ]

#STREAM 730GB
host_730 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==730] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==730]
)
host_730$teste <- "stream"
host_730$plataforma <- "host"
host_730$ram <- 730
# q <- quantile(host_730$consumo, c(0.05, 0.95))
# host_730 <- host_730[host_730$consumo >= q[1] & host_730$consumo <= q[2], ]

############################################################################################
#DOCKER
############################################################################################

#STREAM HOST IDLE
docker_idle <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==0] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==0]
)
docker_idle$teste <- "stream"
docker_idle$plataforma <- "docker"
docker_idle$ram <- 0
# q <- quantile(docker_idle$consumo, c(0.05, 0.95))
# docker_idle <- docker_idle[docker_idle$consumo >= q[1] & docker_idle$consumo <= q[2], ]

#STREAM 1GB
docker_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==1]
)
docker_1$teste <- "stream"
docker_1$plataforma <- "docker"
docker_1$ram <- 1
# q <- quantile(docker_1$consumo, c(0.05, 0.95))
# docker_1 <- docker_1[docker_1$consumo >= q[1] & docker_1$consumo <= q[2], ]

#STREAM 50GB
docker_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==50]
)
docker_50$teste <- "stream"
docker_50$plataforma <- "docker"
docker_50$ram <- 50
# q <- quantile(docker_50$consumo, c(0.05, 0.95))
# docker_50 <- docker_50[docker_50$consumo >= q[1] & docker_50$consumo <= q[2], ]

#STREAM 100GB
docker_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==100]
)
docker_100$teste <- "stream"
docker_100$plataforma <- "docker"
docker_100$ram <- 100
# q <- quantile(docker_100$consumo, c(0.05, 0.95))
# docker_100 <- docker_100[docker_100$consumo >= q[1] & docker_100$consumo <= q[2], ]

#STREAM 250GB
docker_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==250]
)
docker_250$teste <- "stream"
docker_250$plataforma <- "docker"
docker_250$ram <- 250
# q <- quantile(docker_250$consumo, c(0.05, 0.95))
# docker_250 <- docker_250[docker_250$consumo >= q[1] & docker_250$consumo <= q[2], ]

#STREAM 500GB
docker_500 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==500] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==500]
)
docker_500$teste <- "stream"
docker_500$plataforma <- "docker"
docker_500$ram <- 500
# q <- quantile(docker_500$consumo, c(0.05, 0.95))
# docker_500 <- docker_500[docker_500$consumo >= q[1] & docker_500$consumo <= q[2], ]

#STREAM 730GB
docker_730 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==730] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==730]
)
docker_730$teste <- "stream"
docker_730$plataforma <- "docker"
docker_730$ram <- 730
# q <- quantile(docker_730$consumo, c(0.05, 0.95))
# docker_730 <- docker_730[docker_730$consumo >= q[1] & docker_730$consumo <= q[2], ]

############################################################################################
# VM
############################################################################################

#STREAM HOST IDLE
vm_idle <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==0] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==0]
)
vm_idle$teste <- "stream"
vm_idle$plataforma <- "vm"
vm_idle$ram <- 0
# q <- quantile(vm_idle$consumo, c(0.05, 0.95))
# vm_idle <- vm_idle[vm_idle$consumo >= q[1] & vm_idle$consumo <= q[2], ]

#STREAM 1GB
vm_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==1]
)
vm_1$teste <- "stream"
vm_1$plataforma <- "vm"
vm_1$ram <- 1
# q <- quantile(vm_1$consumo, c(0.05, 0.95))
# vm_1 <- vm_1[vm_1$consumo >= q[1] & vm_1$consumo <= q[2], ]

#STREAM 50GB
vm_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==50]
)
vm_50$teste <- "stream"
vm_50$plataforma <- "vm"
vm_50$ram <- 50
# q <- quantile(vm_50$consumo, c(0.05, 0.95))
# vm_50 <- vm_50[vm_50$consumo >= q[1] & vm_50$consumo <= q[2], ]

#STREAM 100GB
vm_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==100]
)
vm_100$teste <- "stream"
vm_100$plataforma <- "vm"
vm_100$ram <- 100
# q <- quantile(vm_100$consumo, c(0.05, 0.95))
# vm_100 <- vm_100[vm_100$consumo >= q[1] & vm_100$consumo <= q[2], ]

#STREAM 250GB
vm_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==250]
)
vm_250$teste <- "stream"
vm_250$plataforma <- "vm"
vm_250$ram <- 250
# q <- quantile(vm_250$consumo, c(0.05, 0.95))
# vm_250 <- vm_250[vm_250$consumo >= q[1] & vm_250$consumo <= q[2], ]

#STREAM 500GB
vm_500 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==500] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==500]
)
vm_500$teste <- "stream"
vm_500$plataforma <- "vm"
vm_500$ram <- 500
# q <- quantile(vm_500$consumo, c(0.05, 0.95))
# vm_500 <- vm_500[vm_500$consumo >= q[1] & vm_500$consumo <= q[2], ]

#STREAM 730GB
vm_730 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==730] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==730]
)
vm_730$teste <- "stream"
vm_730$plataforma <- "vm"
vm_730$ram <- 730
# q <- quantile(vm_730$consumo, c(0.05, 0.95))
# vm_730 <- vm_730[vm_730$consumo >= q[1] & vm_730$consumo <= q[2], ]

####################################################################################################
# VM DOCKER
####################################################################################################

#STREAM HOST IDLE
vm_docker_idle <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==0] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==0]
)
vm_docker_idle$teste <- "stream"
vm_docker_idle$plataforma <- "vm_docker"
vm_docker_idle$ram <- 0
# q <- quantile(vm_docker_idle$consumo, c(0.05, 0.95))
# vm_docker_idle <- vm_docker_idle[vm_docker_idle$consumo >= q[1] & vm_docker_idle$consumo <= q[2], ]

#STREAM 1GB
vm_docker_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==1]
)
vm_docker_1$teste <- "stream"
vm_docker_1$plataforma <- "vm_docker"
vm_docker_1$ram <- 1
# q <- quantile(vm_docker_1$consumo, c(0.05, 0.95))
# vm_docker_1 <- vm_docker_1[vm_docker_1$consumo >= q[1] & vm_docker_1$consumo <= q[2], ]

#STREAM 50GB
vm_docker_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==50]
)
vm_docker_50$teste <- "stream"
vm_docker_50$plataforma <- "vm_docker"
vm_docker_50$ram <- 50
# q <- quantile(vm_docker_50$consumo, c(0.05, 0.95))
# vm_docker_50 <- vm_docker_50[vm_docker_50$consumo >= q[1] & vm_docker_50$consumo <= q[2], ]

#STREAM 100GB
vm_docker_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==100]
)
vm_docker_100$teste <- "stream"
vm_docker_100$plataforma <- "vm_docker"
vm_docker_100$ram <- 100
# q <- quantile(vm_docker_100$consumo, c(0.05, 0.95))
# vm_docker_100 <- vm_docker_100[vm_docker_100$consumo >= q[1] & vm_docker_100$consumo <= q[2], ]

#STREAM 250GB
vm_docker_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==250]
)
vm_docker_250$teste <- "stream"
vm_docker_250$plataforma <- "vm_docker"
vm_docker_250$ram <- 250
# q <- quantile(vm_docker_250$consumo, c(0.05, 0.95))
# vm_docker_250 <- vm_docker_250[vm_docker_250$consumo >= q[1] & vm_docker_250$consumo <= q[2], ]

#STREAM 500GB
vm_docker_500 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==500] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==500]
)
vm_docker_500$teste <- "stream"
vm_docker_500$plataforma <- "vm_docker"
vm_docker_500$ram <- 500
# q <- quantile(vm_docker_500$consumo, c(0.05, 0.95))
# vm_docker_500 <- vm_docker_500[vm_docker_500$consumo >= q[1] & vm_docker_500$consumo <= q[2], ]

#STREAM 730GB
vm_docker_730 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==730] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm_docker" & times$ram==730]
)
vm_docker_730$teste <- "stream"
vm_docker_730$plataforma <- "vm_docker"
vm_docker_730$ram <- 730
# q <- quantile(vm_docker_730$consumo, c(0.05, 0.95))
# vm_docker_730 <- vm_docker_730[vm_docker_730$consumo >= q[1] & vm_docker_730$consumo <= q[2], ]

#Juntando os logs em um unico df
energy_bench <- rbind(host_idle, docker_idle)
energy_bench <- rbind(energy_bench, vm_idle)
energy_bench <- rbind(energy_bench, vm_docker_idle)
energy_bench <- rbind(energy_bench, host_1)
energy_bench <- rbind(energy_bench, host_50)
energy_bench <- rbind(energy_bench, host_100)
energy_bench <- rbind(energy_bench, host_250)
energy_bench <- rbind(energy_bench, host_500)
energy_bench <- rbind(energy_bench, host_730)
energy_bench <- rbind(energy_bench, docker_1)
energy_bench <- rbind(energy_bench, docker_50)
energy_bench <- rbind(energy_bench, docker_100)
energy_bench <- rbind(energy_bench, docker_250)
energy_bench <- rbind(energy_bench, docker_500)
energy_bench <- rbind(energy_bench, docker_730)
energy_bench <- rbind(energy_bench, vm_1)
energy_bench <- rbind(energy_bench, vm_50)
energy_bench <- rbind(energy_bench, vm_100)
energy_bench <- rbind(energy_bench, vm_250)
energy_bench <- rbind(energy_bench, vm_500)
energy_bench <- rbind(energy_bench, vm_730)
energy_bench <- rbind(energy_bench, vm_docker_1)
energy_bench <- rbind(energy_bench, vm_docker_50)
energy_bench <- rbind(energy_bench, vm_docker_100)
energy_bench <- rbind(energy_bench, vm_docker_250)
energy_bench <- rbind(energy_bench, vm_docker_500)
energy_bench <- rbind(energy_bench, vm_docker_730)
#energy_bench <- rbind(energy_bench, idle)

#Limpando a memoria
rm(host_idle)
rm(docker_idle)
rm(vm_idle)
rm(vm_docker_idle)
rm(host_1)
rm(host_50)
rm(host_100)
rm(host_250)
rm(host_500)
rm(host_730)
rm(docker_1)
rm(docker_50)
rm(docker_100)
rm(docker_250)
rm(docker_500)
rm(docker_730)
rm(vm_1)
rm(vm_50)
rm(vm_100)
rm(vm_250)
rm(vm_500)
rm(vm_730)
rm(vm_docker_1)
rm(vm_docker_50)
rm(vm_docker_100)
rm(vm_docker_250)
rm(vm_docker_500)
rm(vm_docker_730)


#Fazendo o plot
energy_bench$grp <- paste(energy_bench$cpu,energy_bench$ram)

tiff("consumo_energia_benchmarks.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=energy_bench, aes(x=grp, y=consumo, color=plataforma))+
    geom_boxplot(outlier.shape = NA)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 90,
            hjust = 0.7,
            size=10
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
        x="Configuração de CPU e Memória do contêiner",
        y="Consumo (W/s)",
        color="Benchmark"
    )+
    scale_color_brewer(palette="Dark2")+
    scale_y_continuous(limits=c(150,550), breaks=seq(0,550,50))#+
    # scale_x_discrete(
    #    limits=c(
    #         "0.25 0.5",
    #         "0.25 1",
    #         "0.25 2",
    #         "0.25 NA",
    #         "0.5 1",
    #         "0.5 2",
    #         "0.5 3",
    #         "0.5 4",
    #         "0.5 NA",
    #         "1 2",
    #         "1 4",
    #         "1 8",
    #         "1 NA",
    #         "2 4",
    #         "2 8",
    #         "2 16",
    #         "2 NA",
    #         "4 4",
    #         "4 16",
    #         "4 32",
    #         "4 NA"
    #         ),
    #         labels=c(
    #         "0.25 CPUs\n0.5Gb RAM",
    #         "0.25 CPUs\n1Gb RAM",
    #         "0.25 CPUs\n2Gb RAM",
    #         "0.25 CPUs",
    #         "0.5 CPUs\n1Gb RAM",
    #         "0.5 CPUs\n2Gb RAM",
    #         "0.5 CPUs\n3Gb RAM",
    #         "0.5 CPUs\n4Gb RAM",
    #         "0.5 CPUs",
    #         "1 CPUs\n2Gb RAM",
    #         "1 CPUs\n4Gb RAM",
    #         "1 CPUs\n8Gb RAM",
    #         "1 CPUs",
    #         "2 CPUs\n4Gb RAM",
    #         "2 CPUs\n8Gb RAM",
    #         "2 CPUs\n16Gb RAM",
    #         "2 CPUs",
    #         "4 CPUs\n4Gb RAM",
    #         "4 CPUs\n16Gb RAM",
    #         "4 CPUs\n30Gb RAM",
    #         "4 CPUs"
    #         ))

plot(p1)
dev.off()
rm(p1)


#Convertendo e movendo os graficos para a respectiva pasta
system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
