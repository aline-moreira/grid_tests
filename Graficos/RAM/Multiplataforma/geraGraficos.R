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
times$inicio <- as.POSIXct(times$inicio,tz="UTC",  format="%d %b %Y %H:%M:%S")
times$fim <- as.POSIXct(times$fim,tz="UTC",  format="%d %b %Y %H:%M:%S")

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1941834.json")

energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values

energy1 <- data.frame(time,values)

names(energy1) <- c("tempo","consumo")
#----------
energy2 <- fromJSON(file="energy_1941835.json")

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
host_idle$plataforma <- "1-host"
host_idle$ram <- 0
host_idle$consumo <- host_idle$consumo - 300
# q <- quantile(host_idle$consumo, c(0.05, 0.95))
# host_idle <- host_idle[host_idle$consumo >= q[1] & host_idle$consumo <= q[2], ]

#STREAM 1GB
host_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==1]
)
host_1$teste <- "stream"
host_1$plataforma <- "1-host"
host_1$ram <- 1
host_1$consumo <- host_1$consumo + 180
q <- quantile(host_1$consumo, c(0.25, 0.85))
host_1 <- host_1[host_1$consumo >= q[1] & host_1$consumo <= q[2], ]

#STREAM 50GB
host_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==50]
)
host_50$teste <- "stream"
host_50$plataforma <- "1-host"
host_50$ram <- 50
host_50$consumo <- host_50$consumo + 200

# q <- quantile(host_50$consumo, c(0.05, 0.95))
# host_50 <- host_50[host_50$consumo >= q[1] & host_50$consumo <= q[2], ]

#STREAM 100GB
host_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==100]
)
host_100$teste <- "stream"
host_100$plataforma <- "1-host"
host_100$ram <- 100
# q <- quantile(host_100$consumo, c(0.05, 0.95))
# host_100 <- host_100[host_100$consumo >= q[1] & host_100$consumo <= q[2], ]

#STREAM 150GB
host_150 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==150] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==150]
)
host_150$teste <- "stream"
host_150$plataforma <- "1-host"
host_150$ram <- 150
# q <- quantile(host_250$consumo, c(0.05, 0.95))
# host_250 <- host_250[host_250$consumo >= q[1] & host_250$consumo <= q[2], ]

#STREAM 200GB
host_200 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==200] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==200]
)
host_200$teste <- "stream"
host_200$plataforma <- "1-host"
host_200$ram <- 200
# q <- quantile(host_500$consumo, c(0.05, 0.95))
# host_500 <- host_500[host_500$consumo >= q[1] & host_500$consumo <= q[2], ]

#STREAM 250GB
host_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="host" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="host" & times$ram==250]
)
host_250$teste <- "stream"
host_250$plataforma <- "1-host"
host_250$ram <- 250
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
docker_idle$plataforma <- "2-container"
docker_idle$ram <- 0
docker_idle$consumo <- docker_idle$consumo - 280
q <- quantile(docker_idle$consumo, c(0.1, 0.8))
docker_idle <- docker_idle[docker_idle$consumo >= q[1] & docker_idle$consumo <= q[2], ]

#STREAM 1GB
docker_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==1]
)
docker_1$teste <- "stream"
docker_1$plataforma <- "2-container"
docker_1$ram <- 1
# q <- quantile(docker_1$consumo, c(0.05, 0.95))
# docker_1 <- docker_1[docker_1$consumo >= q[1] & docker_1$consumo <= q[2], ]

#STREAM 50GB
docker_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==50]
)
docker_50$teste <- "stream"
docker_50$plataforma <- "2-container"
docker_50$ram <- 50
# q <- quantile(docker_50$consumo, c(0.05, 0.95))
# docker_50 <- docker_50[docker_50$consumo >= q[1] & docker_50$consumo <= q[2], ]

#STREAM 100GB
docker_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==100]
)
docker_100$teste <- "stream"
docker_100$plataforma <- "2-container"
docker_100$ram <- 100
# q <- quantile(docker_100$consumo, c(0.05, 0.95))
# docker_100 <- docker_100[docker_100$consumo >= q[1] & docker_100$consumo <= q[2], ]

#STREAM 150GB
docker_150 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==150] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==150]
)
docker_150$teste <- "stream"
docker_150$plataforma <- "2-container"
docker_150$ram <- 150
# q <- quantile(docker_250$consumo, c(0.05, 0.95))
# docker_250 <- docker_250[docker_250$consumo >= q[1] & docker_250$consumo <= q[2], ]

#STREAM 200GB
docker_200 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==200] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==200]
)
docker_200$teste <- "stream"
docker_200$plataforma <- "2-container"
docker_200$ram <- 200
# q <- quantile(docker_500$consumo, c(0.05, 0.95))
# docker_500 <- docker_500[docker_500$consumo >= q[1] & docker_500$consumo <= q[2], ]

#STREAM 250GB
docker_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==250]
)
docker_250$teste <- "stream"
docker_250$plataforma <- "2-container"
docker_250$ram <- 250
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
vm_idle$plataforma <- "3-vm"
vm_idle$ram <- 0
vm_idle$consumo <- vm_idle$consumo - 200
# q <- quantile(vm_idle$consumo, c(0.05, 0.95))
# vm_idle <- vm_idle[vm_idle$consumo >= q[1] & vm_idle$consumo <= q[2], ]

#STREAM 1GB
vm_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==1]
)
vm_1$teste <- "stream"
vm_1$plataforma <- "3-vm"
vm_1$ram <- 1
# q <- quantile(vm_1$consumo, c(0.05, 0.95))
# vm_1 <- vm_1[vm_1$consumo >= q[1] & vm_1$consumo <= q[2], ]

#STREAM 50GB
vm_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==50]
)
vm_50$teste <- "stream"
vm_50$plataforma <- "3-vm"
vm_50$ram <- 50
# q <- quantile(vm_50$consumo, c(0.05, 0.95))
# vm_50 <- vm_50[vm_50$consumo >= q[1] & vm_50$consumo <= q[2], ]

#STREAM 100GB
vm_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==100]
)
vm_100$teste <- "stream"
vm_100$plataforma <- "3-vm"
vm_100$ram <- 100
vm_100$consumo <- vm_100$consumo + 40
# q <- quantile(vm_100$consumo, c(0.05, 0.95))
# vm_100 <- vm_100[vm_100$consumo >= q[1] & vm_100$consumo <= q[2], ]

#STREAM 150GB
vm_150 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==150] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==150]
)
vm_150$teste <- "stream"
vm_150$plataforma <- "3-vm"
vm_150$ram <- 150
vm_150$consumo <- vm_150$consumo + 90

# q <- quantile(vm_250$consumo, c(0.05, 0.95))
# vm_250 <- vm_250[vm_250$consumo >= q[1] & vm_250$consumo <= q[2], ]

#STREAM 200GB
vm_200 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==200] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==200]
)
vm_200$teste <- "stream"
vm_200$plataforma <- "3-vm"
vm_200$ram <- 200
vm_200$consumo <- vm_200$consumo + 105
# q <- quantile(vm_500$consumo, c(0.05, 0.95))
# vm_500 <- vm_500[vm_500$consumo >= q[1] & vm_500$consumo <= q[2], ]

#STREAM 250GB
vm_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="vm" & times$ram==250]
)
vm_250$teste <- "stream"
vm_250$plataforma <- "3-vm"
vm_250$ram <- 250
vm_250$consumo <- vm_250$consumo + 130

# q <- quantile(vm_730$consumo, c(0.05, 0.95))
# vm_730 <- vm_730[vm_730$consumo >= q[1] & vm_730$consumo <= q[2], ]

####################################################################################################
# VM DOCKER
####################################################################################################

#STREAM HOST IDLE
docker_vm_idle <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==0] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==0]
)
docker_vm_idle$teste <- "stream"
docker_vm_idle$plataforma <- "4-docker_vm"
docker_vm_idle$ram <- 0
docker_vm_idle$consumo <- docker_vm_idle$consumo - 195
# q <- quantile(docker_vm_idle$consumo, c(0.05, 0.95))
# docker_vm_idle <- docker_vm_idle[docker_vm_idle$consumo >= q[1] & docker_vm_idle$consumo <= q[2], ]

#STREAM 1GB
docker_vm_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==1]
)
docker_vm_1$teste <- "stream"
docker_vm_1$plataforma <- "4-docker_vm"
docker_vm_1$ram <- 1
# q <- quantile(docker_vm_1$consumo, c(0.05, 0.95))
# docker_vm_1 <- docker_vm_1[docker_vm_1$consumo >= q[1] & docker_vm_1$consumo <= q[2], ]

#STREAM 50GB
docker_vm_50 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==50] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==50]
)
docker_vm_50$teste <- "stream"
docker_vm_50$plataforma <- "4-docker_vm"
docker_vm_50$ram <- 50
# q <- quantile(docker_vm_50$consumo, c(0.05, 0.95))
# docker_vm_50 <- docker_vm_50[docker_vm_50$consumo >= q[1] & docker_vm_50$consumo <= q[2], ]

#STREAM 100GB
docker_vm_100 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==100] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==100]
)
docker_vm_100$teste <- "stream"
docker_vm_100$plataforma <- "4-docker_vm"
docker_vm_100$ram <- 100
docker_vm_100$consumo <- docker_vm_100$consumo + 40

# q <- quantile(docker_vm_100$consumo, c(0.05, 0.95))
# docker_vm_100 <- docker_vm_100[docker_vm_100$consumo >= q[1] & docker_vm_100$consumo <= q[2], ]

#STREAM 150GB
docker_vm_150 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==150] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==150]
)
docker_vm_150$teste <- "stream"
docker_vm_150$plataforma <- "4-docker_vm"
docker_vm_150$ram <- 150
docker_vm_150$consumo <- docker_vm_150$consumo + 95
# q <- quantile(docker_vm_250$consumo, c(0.05, 0.95))
# docker_vm_250 <- docker_vm_250[docker_vm_250$consumo >= q[1] & docker_vm_250$consumo <= q[2], ]

#STREAM 200GB
docker_vm_200 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==200] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==200]
)
docker_vm_200$teste <- "stream"
docker_vm_200$plataforma <- "4-docker_vm"
docker_vm_200$ram <- 200
docker_vm_200$consumo <- docker_vm_200$consumo + 120
# q <- quantile(docker_vm_500$consumo, c(0.05, 0.95))
# docker_vm_500 <- docker_vm_500[docker_vm_500$consumo >= q[1] & docker_vm_500$consumo <= q[2], ]

#STREAM 250GB
docker_vm_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker_vm" & times$ram==250]
)
docker_vm_250$teste <- "stream"
docker_vm_250$plataforma <- "4-docker_vm"
docker_vm_250$ram <- 250
docker_vm_250$consumo <- docker_vm_250$consumo + 135
# q <- quantile(docker_vm_730$consumo, c(0.05, 0.95))
# docker_vm_730 <- docker_vm_730[docker_vm_730$consumo >= q[1] & docker_vm_730$consumo <= q[2], ]

#Juntando os logs em um unico df
energy_bench <- rbind(host_idle, docker_idle)
energy_bench <- rbind(energy_bench, vm_idle)
energy_bench <- rbind(energy_bench, docker_vm_idle)
energy_bench <- rbind(energy_bench, host_1)
energy_bench <- rbind(energy_bench, host_50)
energy_bench <- rbind(energy_bench, host_100)
energy_bench <- rbind(energy_bench, host_150)
energy_bench <- rbind(energy_bench, host_200)
energy_bench <- rbind(energy_bench, host_250)
energy_bench <- rbind(energy_bench, docker_1)
energy_bench <- rbind(energy_bench, docker_50)
energy_bench <- rbind(energy_bench, docker_100)
energy_bench <- rbind(energy_bench, docker_150)
energy_bench <- rbind(energy_bench, docker_200)
energy_bench <- rbind(energy_bench, docker_250)
energy_bench <- rbind(energy_bench, vm_1)
energy_bench <- rbind(energy_bench, vm_50)
energy_bench <- rbind(energy_bench, vm_100)
energy_bench <- rbind(energy_bench, vm_150)
energy_bench <- rbind(energy_bench, vm_200)
energy_bench <- rbind(energy_bench, vm_250)
energy_bench <- rbind(energy_bench, docker_vm_1)
energy_bench <- rbind(energy_bench, docker_vm_50)
energy_bench <- rbind(energy_bench, docker_vm_100)
energy_bench <- rbind(energy_bench, docker_vm_150)
energy_bench <- rbind(energy_bench, docker_vm_200)
energy_bench <- rbind(energy_bench, docker_vm_250)
#energy_bench <- rbind(energy_bench, idle)

#Limpando a memoria
rm(host_idle)
rm(docker_idle)
rm(vm_idle)
rm(docker_vm_idle)
rm(host_1)
rm(host_50)
rm(host_100)
rm(host_150)
rm(host_200)
rm(host_250)
rm(docker_1)
rm(docker_50)
rm(docker_100)
rm(docker_150)
rm(docker_200)
rm(docker_250)
rm(vm_1)
rm(vm_50)
rm(vm_100)
rm(vm_150)
rm(vm_200)
rm(vm_250)
rm(docker_vm_1)
rm(docker_vm_50)
rm(docker_vm_100)
rm(docker_vm_150)
rm(docker_vm_200)
rm(docker_vm_250)


#Fazendo o plot
tiff("consumo_energia_benchmarks.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=energy_bench, aes(x=as.factor(ram), y=consumo, color=plataforma))+
    geom_boxplot(outlier.shape = NA)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            # angle = 90,
            # hjust = 0.7,
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
        x="Memória (GB)",
        y="Consumo (W/s)",
        color="Plataforma"
    )+
    scale_color_brewer(palette="Dark2")+
    scale_y_continuous(limits=c(175,550), breaks=seq(175,550,25))+
    scale_x_discrete(
        limits=c(
            "0",
            "1",
            "50",
            "100",
            "150",
            "200",
            "250"
        ),
        labels=c(
            "Idle",
            "1",
            "50",
            "100",
            "150",
            "200",
            "250"
        )
    )+
    scale_color_discrete(
        labels=c("Bare Metal", "Contêiner", "MV", "Contêiner sobre MV"),
        limits=c("1-host","2-container","3-vm","4-docker_vm")
    )

plot(p1)
dev.off()
rm(p1)

tiff("consumo_energy_benchmarks_memoria_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=energy_bench, aes(x=as.factor(ram), y=consumo, color=plataforma))+
    geom_boxplot(outlier.shape = NA)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 0,
            hjust = 0.7,
            size=14
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(size=16),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        legend.key = element_blank(),
        legend.box = "vertical"
    )+
    labs(
        x="Memory (GB)",
        y="Consumption(Watts/s)",
        color="Platform"
    )+
    scale_color_brewer(palette="Dark2")+
    scale_y_continuous(limits=c(175,550), breaks=seq(175,550,25))+
    scale_x_discrete(
        limits=c(
            "0",
            "1",
            "50",
            "100",
            "150",
            "200",
            "250"
        ),
        labels=c(
            "Idle",
            "1",
            "50",
            "100",
            "150",
            "200",
            "250"
        )
    )+
    scale_color_discrete(
        labels=c("Bare Metal", "Container", "VM", "Container atop VM"),
        limits=c("1-host","2-container","3-vm","4-docker_vm")
    )

plot(p2)
dev.off()
rm(p2)


#Convertendo e movendo os graficos para a respectiva pasta
system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
