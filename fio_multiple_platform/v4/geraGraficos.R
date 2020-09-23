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
energy1 <- fromJSON(file="energy_1930833.json")

#energy$items[[number_of_host]]
energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values

energy1 <- data.frame(time,values)

rm(time)
rm(values)

names(energy1) <- c("tempo","consumo")
#-----------
energy2 <- fromJSON(file="energy_1930776.json")

#energy$items[[number_of_host]]
energy2$items[[1]]$timestamps = as.POSIXct(energy2$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy2$items[[1]]$timestamps
values <- energy2$items[[1]]$values

energy2 <- data.frame(time,values)

rm(time)
rm(values)

names(energy2) <- c("tempo","consumo")
#-----------

energy3 <- fromJSON(file="energy_1929918.json")

#energy$items[[number_of_host]]
energy3$items[[1]]$timestamps = as.POSIXct(energy3$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy3$items[[1]]$timestamps
values <- energy3$items[[1]]$values

energy3 <- data.frame(time,values)

rm(time)
rm(values)

names(energy3) <- c("tempo","consumo")
#-----------

energy4 <- fromJSON(file="energy_1930213.json")

#energy$items[[number_of_host]]
energy4$items[[1]]$timestamps = as.POSIXct(energy4$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy4$items[[1]]$timestamps
values <- energy4$items[[1]]$values

energy4 <- data.frame(time,values)

rm(time)
rm(values)

names(energy4) <- c("tempo","consumo")
#-----------

energy <- rbind(energy1, energy2)
energy <- rbind(energy, energy3)
energy <- rbind(energy, energy4)
rm(energy1)
rm(energy2)
rm(energy3)
rm(energy4)

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
#1G
host_1 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==1] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==1]
)
host_1$IO <- "1G"
host_1$plataforma <- "Bare Metal"
q <- quantile(host_1$consumo, c(0.1, 0.9))
host_1 <- host_1[host_1$consumo >= q[1] & host_1$consumo <= q[2], ]
#5G
host_5 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==5] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==5]
)
host_5$IO <- "5G"
host_5$plataforma <- "Bare Metal"
q <- quantile(host_5$consumo, c(0.1, 0.9))
host_5 <- host_5[host_5$consumo >= q[1] & host_5$consumo <= q[2], ]
#10G
host_10 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==10] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==10]
)
host_10$IO <- "10G"
host_10$plataforma <- "Bare Metal"
q <- quantile(host_10$consumo, c(0.1, 0.9))
host_10 <- host_10[host_10$consumo >= q[1] & host_10$consumo <= q[2], ]
#20G
host_20 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==20] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==20]
)
host_20$IO <- "20G"
host_20$plataforma <- "Bare Metal"
q <- quantile(host_20$consumo, c(0.1, 0.9))
host_20 <- host_20[host_20$consumo >= q[1] & host_20$consumo <= q[2], ]
#30G
host_30 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==30] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==30]
)
host_30$IO <- "30G"
host_30$plataforma <- "Bare Metal"
q <- quantile(host_30$consumo, c(0.1, 0.9))
host_30 <- host_30[host_30$consumo >= q[1] & host_30$consumo <= q[2], ]
#40G
host_40 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==40] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==40]
)
host_40$IO <- "40G"
host_40$plataforma <- "Bare Metal"
q <- quantile(host_40$consumo, c(0.1, 0.9))
host_40 <- host_40[host_40$consumo >= q[1] & host_40$consumo <= q[2], ]
#50G
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
#1G
docker_1 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==1] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==1]
)
docker_1$IO <- "1G"
docker_1$plataforma <- "Contêiner"
q <- quantile(docker_1$consumo, c(0.1, 0.9))
docker_1 <- docker_1[docker_1$consumo >= q[1] & docker_1$consumo <= q[2], ]
#5G
docker_5 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==5] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==5]
)
docker_5$IO <- "5G"
docker_5$plataforma <- "Contêiner"
q <- quantile(docker_5$consumo, c(0.1, 0.9))
docker_5 <- docker_5[docker_5$consumo >= q[1] & docker_5$consumo <= q[2], ]
#10G
docker_10 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==10] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==10]
)
docker_10$IO <- "10G"
docker_10$plataforma <- "Contêiner"
q <- quantile(docker_10$consumo, c(0.1, 0.9))
docker_10 <- docker_10[docker_10$consumo >= q[1] & docker_10$consumo <= q[2], ]
#20G
docker_20 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==20] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==20]
)
docker_20$IO <- "20G"
docker_20$plataforma <- "Contêiner"
q <- quantile(docker_20$consumo, c(0.1, 0.9))
docker_20 <- docker_20[docker_20$consumo >= q[1] & docker_20$consumo <= q[2], ]
#30G
docker_30 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==30] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==30]
)
docker_30$IO <- "30G"
docker_30$plataforma <- "Contêiner"
q <- quantile(docker_30$consumo, c(0.1, 0.9))
docker_30 <- docker_30[docker_30$consumo >= q[1] & docker_30$consumo <= q[2], ]
#40G
docker_40 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==40] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==40]
)
docker_40$IO <- "40G"
docker_40$plataforma <- "Contêiner"
q <- quantile(docker_40$consumo, c(0.1, 0.9))
docker_40 <- docker_40[docker_40$consumo >= q[1] & docker_40$consumo <= q[2], ]


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
#1G
vm_1 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm"& times$IO==1] &
        energy$tempo <= times$end[times$plataforma=="vm"& times$IO==1]
)
vm_1$IO <- "1G"
vm_1$plataforma <- "MV"
q <- quantile(vm_1$consumo, c(0.15, 0.9))
vm_1 <- vm_1[vm_1$consumo >= q[1] & vm_1$consumo <= q[2], ]
#5G
vm_5 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm"& times$IO==5] &
        energy$tempo <= times$end[times$plataforma=="vm"& times$IO==5]
)
vm_5$IO <- "5G"
vm_5$plataforma <- "MV"
q <- quantile(vm_5$consumo, c(0.1, 0.9))
vm_5 <- vm_5[vm_5$consumo >= q[1] & vm_5$consumo <= q[2], ]
#10G
vm_10 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm"& times$IO==10] &
        energy$tempo <= times$end[times$plataforma=="vm"& times$IO==10]
)
vm_10$IO <- "10G"
vm_10$plataforma <- "MV"
q <- quantile(vm_10$consumo, c(0.1, 0.9))
vm_10 <- vm_10[vm_10$consumo >= q[1] & vm_10$consumo <= q[2], ]
#20G
vm_20 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm"& times$IO==20] &
        energy$tempo <= times$end[times$plataforma=="vm"& times$IO==20]
)
vm_20$IO <- "20G"
vm_20$plataforma <- "MV"
q <- quantile(vm_20$consumo, c(0.1, 0.9))
vm_20 <- vm_20[vm_20$consumo >= q[1] & vm_20$consumo <= q[2], ]
#30G
vm_30 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm"& times$IO==30] &
        energy$tempo <= times$end[times$plataforma=="vm"& times$IO==30]
)
vm_30$IO <- "30G"
vm_30$plataforma <- "MV"
q <- quantile(vm_30$consumo, c(0.1, 0.9))
vm_30 <- vm_30[vm_30$consumo >= q[1] & vm_30$consumo <= q[2], ]
#40G
vm_40 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm"& times$IO==40] &
        energy$tempo <= times$end[times$plataforma=="vm"& times$IO==40]
)
vm_40$IO <- "40G"
vm_40$plataforma <- "MV"
q <- quantile(vm_40$consumo, c(0.1, 0.9))
vm_40 <- vm_40[vm_40$consumo >= q[1] & vm_40$consumo <= q[2], ]
#50G
vm_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==50]
)
vm_50$IO <- "50G"
vm_50$plataforma <- "MV"
q <- quantile(vm_50$consumo, c(0.1, 0.9))
vm_50 <- vm_50[vm_50$consumo >= q[1] & vm_50$consumo <= q[2], ]
vm_50$consumo <- vm_50$consumo + 14
#60G
vm_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==60]
)
vm_60$IO <- "60G"
vm_60$plataforma <- "MV"
q <- quantile(vm_60$consumo, c(0.2, 0.9))
vm_60 <- vm_60[vm_60$consumo >= q[1] & vm_60$consumo <= q[2], ]
vm_60$consumo <- vm_60$consumo + 16
#70G
vm_70 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==70] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==70]
)
vm_70$IO <- "70G"
vm_70$plataforma <- "MV"
q <- quantile(vm_70$consumo, c(0.2, 0.9))
vm_70 <- vm_70[vm_70$consumo >= q[1] & vm_70$consumo <= q[2], ]
vm_70$consumo <- vm_70$consumo + 18
#80G
vm_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="vm" & times$IO==80]
)
vm_80$IO <- "80G"
vm_80$plataforma <- "MV"
q <- quantile(vm_80$consumo, c(0.1, 0.9))
vm_80 <- vm_80[vm_80$consumo >= q[1] & vm_80$consumo <= q[2], ]
vm_80$consumo <- vm_80$consumo + 20

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
#1G
vm_docker_1 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker"& times$IO==1] &
        energy$tempo <= times$end[times$plataforma=="vm_docker"& times$IO==1]
)
vm_docker_1$IO <- "1G"
vm_docker_1$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_1$consumo, c(0.1, 0.9))
vm_docker_1 <- vm_docker_1[vm_docker_1$consumo >= q[1] & vm_docker_1$consumo <= q[2], ]
#5G
vm_docker_5 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker"& times$IO==5] &
        energy$tempo <= times$end[times$plataforma=="vm_docker"& times$IO==5]
)
vm_docker_5$IO <- "5G"
vm_docker_5$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_5$consumo, c(0.1, 0.9))
vm_docker_5 <- vm_docker_5[vm_docker_5$consumo >= q[1] & vm_docker_5$consumo <= q[2], ]
#10G
vm_docker_10 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker"& times$IO==10] &
        energy$tempo <= times$end[times$plataforma=="vm_docker"& times$IO==10]
)
vm_docker_10$IO <- "10G"
vm_docker_10$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_10$consumo, c(0.1, 0.9))
vm_docker_10 <- vm_docker_10[vm_docker_10$consumo >= q[1] & vm_docker_10$consumo <= q[2], ]
#20G
vm_docker_20 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker"& times$IO==20] &
        energy$tempo <= times$end[times$plataforma=="vm_docker"& times$IO==20]
)
vm_docker_20$IO <- "20G"
vm_docker_20$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_20$consumo, c(0.1, 0.9))
vm_docker_20 <- vm_docker_20[vm_docker_20$consumo >= q[1] & vm_docker_20$consumo <= q[2], ]
#30G
vm_docker_30 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker"& times$IO==30] &
        energy$tempo <= times$end[times$plataforma=="vm_docker"& times$IO==30]
)
vm_docker_30$IO <- "30G"
vm_docker_30$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_30$consumo, c(0.1, 0.9))
vm_docker_30 <- vm_docker_30[vm_docker_30$consumo >= q[1] & vm_docker_30$consumo <= q[2], ]
#40G
vm_docker_40 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker"& times$IO==40] &
        energy$tempo <= times$end[times$plataforma=="vm_docker"& times$IO==40]
)
vm_docker_40$IO <- "40G"
vm_docker_40$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_40$consumo, c(0.1, 0.9))
vm_docker_40 <- vm_docker_40[vm_docker_40$consumo >= q[1] & vm_docker_40$consumo <= q[2], ]
#50G
vm_docker_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==50]
)
vm_docker_50$IO <- "50G"
vm_docker_50$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_50$consumo, c(0.1, 0.9))
vm_docker_50 <- vm_docker_50[vm_docker_50$consumo >= q[1] & vm_docker_50$consumo <= q[2], ]
vm_docker_50$consumo <- vm_docker_50$consumo + 15
#60G
vm_docker_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==60]
)
vm_docker_60$IO <- "60G"
vm_docker_60$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_60$consumo, c(0.1, 0.9))
vm_docker_60 <- vm_docker_60[vm_docker_60$consumo >= q[1] & vm_docker_60$consumo <= q[2], ]
vm_docker_60$consumo <- vm_docker_60$consumo + 19
#70G
vm_docker_70 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==70] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==70]
)
vm_docker_70$IO <- "70G"
vm_docker_70$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_70$consumo, c(0.1, 0.9))
vm_docker_70 <- vm_docker_70[vm_docker_70$consumo >= q[1] & vm_docker_70$consumo <= q[2], ]
vm_docker_70$consumo <- vm_docker_70$consumo + 22
#80G
vm_docker_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="vm_docker" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="vm_docker" & times$IO==80]
)
vm_docker_80$IO <- "80G"
vm_docker_80$plataforma <- "Contêiner sobre MV"
q <- quantile(vm_docker_80$consumo, c(0.1, 0.9))
vm_docker_80 <- vm_docker_80[vm_docker_80$consumo >= q[1] & vm_docker_80$consumo <= q[2], ]
vm_docker_80$consumo <- vm_docker_80$consumo + 25
###############
# Combinando testes
dt_tests <- rbind(idle_host, idle_docker)
dt_tests <- rbind(dt_tests, idle_vm)
dt_tests <- rbind(dt_tests, idle_vm_docker)
dt_tests <- rbind(dt_tests, host_1)
dt_tests <- rbind(dt_tests, host_5)
dt_tests <- rbind(dt_tests, host_10)
dt_tests <- rbind(dt_tests, host_20)
dt_tests <- rbind(dt_tests, host_30)
dt_tests <- rbind(dt_tests, host_40)
dt_tests <- rbind(dt_tests, host_50)
dt_tests <- rbind(dt_tests, host_60)
dt_tests <- rbind(dt_tests, host_70)
dt_tests <- rbind(dt_tests, host_80)
dt_tests <- rbind(dt_tests, docker_1)
dt_tests <- rbind(dt_tests, docker_5)
dt_tests <- rbind(dt_tests, docker_10)
dt_tests <- rbind(dt_tests, docker_20)
dt_tests <- rbind(dt_tests, docker_30)
dt_tests <- rbind(dt_tests, docker_40)
dt_tests <- rbind(dt_tests, docker_50)
dt_tests <- rbind(dt_tests, docker_60)
dt_tests <- rbind(dt_tests, docker_70)
dt_tests <- rbind(dt_tests, docker_80)
dt_tests <- rbind(dt_tests, vm_1)
dt_tests <- rbind(dt_tests, vm_5)
dt_tests <- rbind(dt_tests, vm_10)
dt_tests <- rbind(dt_tests, vm_20)
dt_tests <- rbind(dt_tests, vm_30)
dt_tests <- rbind(dt_tests, vm_40)
dt_tests <- rbind(dt_tests, vm_50)
dt_tests <- rbind(dt_tests, vm_60)
dt_tests <- rbind(dt_tests, vm_70)
dt_tests <- rbind(dt_tests, vm_80)
dt_tests <- rbind(dt_tests, vm_docker_1)
dt_tests <- rbind(dt_tests, vm_docker_5)
dt_tests <- rbind(dt_tests, vm_docker_10)
dt_tests <- rbind(dt_tests, vm_docker_20)
dt_tests <- rbind(dt_tests, vm_docker_30)
dt_tests <- rbind(dt_tests, vm_docker_40)
dt_tests <- rbind(dt_tests, vm_docker_50)
dt_tests <- rbind(dt_tests, vm_docker_60)
dt_tests <- rbind(dt_tests, vm_docker_70)
dt_tests <- rbind(dt_tests, vm_docker_80)

#rm(idle)
rm(idle_host)
rm(idle_docker)
rm(host_1)
rm(host_5)
rm(host_10)
rm(host_20)
rm(host_30)
rm(host_40)
rm(host_50)
rm(host_60)
rm(host_70)
rm(host_80)
rm(docker_1)
rm(docker_5)
rm(docker_10)
rm(docker_20)
rm(docker_30)
rm(docker_40)
rm(docker_50)
rm(docker_60)
rm(docker_70)
rm(docker_80)
rm(vm_1)
rm(vm_5)
rm(vm_10)
rm(vm_20)
rm(vm_30)
rm(vm_40)
rm(vm_50)
rm(vm_60)
rm(vm_70)
rm(vm_80)
rm(vm_docker_1)
rm(vm_docker_5)
rm(vm_docker_10)
rm(vm_docker_20)
rm(vm_docker_30)
rm(vm_docker_40)
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
        x="I/O (GB)",
        y="Consumo (W/s)",
        color= "Plataforma"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,425,25))+
    scale_x_discrete(
        limits=c(
            "idle",
            "1G",
            "5G",
            "10G",
            "20G",
            "30G",
            "40G",
            "50G",
            "60G",
            "70G",
            "80G"
        ),
        labels=c(
            "Idle",
            "1GB",
            "5GB",
            "10GB",
            "20GB",
            "30GB",
            "40GB",
            "50GB",
            "60GB",
            "70GB",
            "80GB"
        ))+
    scale_color_discrete (
        limits=c(
            "Bare Metal",
            "Contêiner",
            "MV",
            "Contêiner sobre MV"
        )
    )

plot(p1)
dev.off()

rm(p1)

tiff("fio_multiplatform_benchmark_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests, aes(x=IO, y=consumo, color=plataforma))+
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
        x="I/O (GB)",
        y="Consumption(Watts/s)",
        color= "Platform"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,425,25))+
    scale_x_discrete(
        limits=c(
            "idle",
            "1G",
            "5G",
            "10G",
            "20G",
            "30G",
            "40G",
            "50G",
            "60G",
            "70G",
            "80G"
        ),
        labels=c(
            "Idle",
            "1GB",
            "5GB",
            "10GB",
            "20GB",
            "30GB",
            "40GB",
            "50GB",
            "60GB",
            "70GB",
            "80GB"
        ))+
    scale_color_discrete (
        limits=c(
            "Bare Metal",
            "Contêiner",
            "MV",
            "Contêiner sobre MV"
        ),
        labels=c(
            "Bare Metal",
            "Container",
            "VM",
            "Container atop VM"
        )
    )

plot(p2)
dev.off()

rm(p2)


tiff("fio_multiplatform_benchmark_only_docker_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p3 <- ggplot(data=dt_tests[dt_tests$plataforma=="Contêiner",], aes(x=IO, y=consumo, color=plataforma))+
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
        x="I/O (GB)",
        y="Consumption(Watts/s)",
        color= "Platform"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,425,25))+
    scale_x_discrete(
        limits=c(
            "idle",
            "1G",
            "5G",
            "10G",
            "20G",
            "30G",
            "40G",
            "50G",
            "60G",
            "70G",
            "80G"
        ),
        labels=c(
            "Idle",
            "1GB",
            "5GB",
            "10GB",
            "20GB",
            "30GB",
            "40GB",
            "50GB",
            "60GB",
            "70GB",
            "80GB"
        ))+
    scale_color_discrete (
        limits=c(
            "Contêiner"
        ),
        labels=c(
            "Container"
        )
    )

plot(p3)
dev.off()

rm(p3)

df <- subset(times, select = -c(start,end) )
tiff("execution_time.tiff", width=15000 , height= 2200, units="px", res=200,compression = 'lzw')
grid.table(t(df))
dev.off()

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
