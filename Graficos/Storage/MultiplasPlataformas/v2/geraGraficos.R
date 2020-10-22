packages <- c("ggplot2","dplyr","scales","ggsci","rjson", "gridExtra")

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

times<- read.table("fio.times",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(times) <- c("plataforma","IO","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$execution_time <- times$end - times$start

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1929785.json")

#energy$items[[number_of_host]]
energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values

energy1 <- data.frame(time,values)

rm(time)
rm(values)

names(energy1) <- c("tempo","consumo")

energy2 <- fromJSON(file="energy_1929918.json")

#energy$items[[number_of_host]]
energy2$items[[1]]$timestamps = as.POSIXct(energy2$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy2$items[[1]]$timestamps
values <- energy2$items[[1]]$values

energy2 <- data.frame(time,values)

rm(time)
rm(values)

names(energy2) <- c("tempo","consumo")

energy <- rbind(energy1, energy2)
rm(energy1)
rm(energy2)

###################################################
# Filtrando a energia de acordo com os tempos
###################################################
idle_host <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO=="0"] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO=="0"]
)
idle_host$IO <- "idle"
idle_host$plataforma <- "Bare Metal"
q <- quantile(idle_host$consumo, c(0.1, 0.9))
idle_host <- idle_host[idle_host$consumo >= q[1] & idle_host$consumo <= q[2], ]
#80G
host_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==50]
)
host_50$IO <- "50G"
host_50$plataforma <- "Bare Metal"
q <- quantile(host_50$consumo, c(0.1, 0.9))
host_50 <- host_50[host_50$consumo >= q[1] & host_50$consumo <= q[2], ]
#60G
host_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==60]
)
host_60$IO <- "60G"
host_60$plataforma <- "Bare Metal"
q <- quantile(host_60$consumo, c(0.1, 0.9))
host_60 <- host_60[host_60$consumo >= q[1] & host_60$consumo <= q[2], ]
#70G
host_70 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==70] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==70]
)
host_70$IO <- "70G"
host_70$plataforma <- "Bare Metal"
q <- quantile(host_70$consumo, c(0.1, 0.9))
host_70 <- host_70[host_70$consumo >= q[1] & host_70$consumo <= q[2], ]
#80G
host_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==80]
)
host_80$IO <- "80G"
host_80$plataforma <- "Bare Metal"
q <- quantile(host_80$consumo, c(0.1, 0.9))
host_80 <- host_80[host_80$consumo >= q[1] & host_80$consumo <= q[2], ]
##############################
# DOCKER

idle_docker <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==0] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==0]
)
idle_docker$IO <- "idle"
idle_docker$plataforma <- "Contêiner"
q <- quantile(idle_docker$consumo, c(0.1, 0.9))
idle_docker <- idle_docker[idle_docker$consumo >= q[1] & idle_docker$consumo <= q[2], ]
#50G
docker_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==50]
)
docker_50$IO <- "50G"
docker_50$plataforma <- "Contêiner"
q <- quantile(docker_50$consumo, c(0.1, 0.9))
docker_50 <- docker_50[docker_50$consumo >= q[1] & docker_50$consumo <= q[2], ]
#60G
docker_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==60]
)
docker_60$IO <- "60G"
docker_60$plataforma <- "Contêiner"
q <- quantile(docker_60$consumo, c(0.1, 0.9))
docker_60 <- docker_60[docker_60$consumo >= q[1] & docker_60$consumo <= q[2], ]
#70G
docker_70 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==70] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==70]
)
docker_70$IO <- "70G"
docker_70$plataforma <- "Contêiner"
q <- quantile(docker_70$consumo, c(0.1, 0.9))
docker_70 <- docker_70[docker_70$consumo >= q[1] & docker_70$consumo <= q[2], ]
#80G
docker_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==80]
)
docker_80$IO <- "80G"
docker_80$plataforma <- "Contêiner"
q <- quantile(docker_80$consumo, c(0.1, 0.9))
docker_80 <- docker_80[docker_80$consumo >= q[1] & docker_80$consumo <= q[2], ]
##############################
# VM

idle_vm <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==0] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==0]
)
idle_vm$IO <- "idle"
idle_vm$plataforma <- "MV"
q <- quantile(idle_vm$consumo, c(0.1, 0.9))
idle_vm <- idle_vm[idle_vm$consumo >= q[1] & idle_vm$consumo <= q[2], ]
#50G
vm_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==50]
)
vm_50$IO <- "50G"
vm_50$plataforma <- "MV"
q <- quantile(vm_50$consumo, c(0.1, 0.9))
vm_50 <- vm_50[vm_50$consumo >= q[1] & vm_50$consumo <= q[2], ]
#60G
vm_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==60]
)
vm_60$IO <- "60G"
vm_60$plataforma <- "MV"
q <- quantile(vm_60$consumo, c(0.1, 0.9))
vm_60 <- vm_60[vm_60$consumo >= q[1] & vm_60$consumo <= q[2], ]
#70G
vm_70 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==70] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==70]
)
vm_70$IO <- "70G"
vm_70$plataforma <- "MV"
q <- quantile(vm_70$consumo, c(0.1, 0.9))
vm_70 <- vm_70[vm_70$consumo >= q[1] & vm_70$consumo <= q[2], ]
#80G
vm_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==80]
)
vm_80$IO <- "80G"
vm_80$plataforma <- "MV"
q <- quantile(vm_80$consumo, c(0.1, 0.9))
vm_80 <- vm_80[vm_80$consumo >= q[1] & vm_80$consumo <= q[2], ]
##############################
# VM DOCKER

idle_vm_docker <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==0] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==0]
)
idle_vm_docker$IO <- "idle"
idle_vm_docker$plataforma <- "Contêiner sobre MV"
q <- quantile(idle_vm_docker$consumo, c(0.1, 0.9))
idle_vm_docker <- idle_vm_docker[idle_vm_docker$consumo >= q[1] & idle_vm_docker$consumo <= q[2], ]
#50G
vm_docker_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==50]
)
vm_docker_50$IO <- "50G"
vm_docker_50$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_50$consumo, c(0.1, 0.9))
vm_docker_50 <- vm_docker_50[vm_docker_50$consumo >= q[1] & vm_docker_50$consumo <= q[2], ]
#60G
vm_docker_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==60]
)
vm_docker_60$IO <- "60G"
vm_docker_60$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_60$consumo, c(0.1, 0.9))
vm_docker_60 <- vm_docker_60[vm_docker_60$consumo >= q[1] & vm_docker_60$consumo <= q[2], ]
#70G
vm_docker_70 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==70] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==70]
)
vm_docker_70$IO <- "70G"
vm_docker_70$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_70$consumo, c(0.1, 0.9))
vm_docker_70 <- vm_docker_70[vm_docker_70$consumo >= q[1] & vm_docker_70$consumo <= q[2], ]
#80G
vm_docker_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==80]
)
vm_docker_80$IO <- "80G"
vm_docker_80$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_80$consumo, c(0.1, 0.9))
vm_docker_80 <- vm_docker_80[vm_docker_80$consumo >= q[1] & vm_docker_80$consumo <= q[2], ]
###############
# Combinando testes
dt_tests <- rbind(idle_host, idle_docker)
dt_tests <- rbind(dt_tests, idle_vm)
dt_tests <- rbind(dt_tests, idle_vm_docker)
dt_tests <- rbind(dt_tests, host_50)
dt_tests <- rbind(dt_tests, host_60)
dt_tests <- rbind(dt_tests, host_70)
dt_tests <- rbind(dt_tests, host_80)
dt_tests <- rbind(dt_tests, docker_50)
dt_tests <- rbind(dt_tests, docker_60)
dt_tests <- rbind(dt_tests, docker_70)
dt_tests <- rbind(dt_tests, docker_80)
dt_tests <- rbind(dt_tests, vm_50)
dt_tests <- rbind(dt_tests, vm_60)
dt_tests <- rbind(dt_tests, vm_70)
dt_tests <- rbind(dt_tests, vm_80)
dt_tests <- rbind(dt_tests, vm_docker_50)
dt_tests <- rbind(dt_tests, vm_docker_60)
dt_tests <- rbind(dt_tests, vm_docker_70)
dt_tests <- rbind(dt_tests, vm_docker_80)

#rm(idle)
rm(idle_host)
rm(idle_docker)
rm(host_50)
rm(host_60)
rm(host_70)
rm(host_80)
rm(docker_50)
rm(docker_60)
rm(docker_70)
rm(docker_80)
rm(vm_50)
rm(vm_60)
rm(vm_70)
rm(vm_80)
rm(vm_docker_50)
rm(vm_docker_60)
rm(vm_docker_70)
rm(vm_docker_80)

tiff("fio_multiplatform_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=IO, y=consumo, color=plataforma))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 0,
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
        x="Memória (GB)",
        y="Consumo (W/s)",
        color= "Plataforma"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,425,25))+
    scale_x_discrete(
        limits=c(
            "idle",
            "50G",
            "60G",
            "70G",
            "80G"
        ),
        labels=c(
            "Idle",
            "50GB",
            "60GB",
            "70GB",
            "80GB"
        ))

plot(p1)
dev.off()

rm(p1)

df <- subset(times, select = -c(start,end) )
tiff("execution_time.tiff", width=2200 , height= 3600, units="px", res=400,compression = 'lzw')
grid.table(df)
dev.off()

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
