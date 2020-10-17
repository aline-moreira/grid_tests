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

times<- read.table("iperf-client.times",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(times) <- c("plataforma","tipo", "repeticao","banda","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1943626.json")

#energy$items[[number_of_host]]
energy1$items[[2]]$timestamps = as.POSIXct(energy1$items[[2]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy1$items[[2]]$timestamps
values <- energy1$items[[2]]$values
energy1 <- data.frame(time,values)

energy2 <- fromJSON(file="energy_1943162.json")
#energy$items[[number_of_host]]
energy2$items[[2]]$timestamps = as.POSIXct(energy2$items[[2]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy2$items[[2]]$timestamps
values <- energy2$items[[2]]$values
energy2 <- data.frame(time,values)

energy <- rbind(energy1, energy2)

rm(time)
rm(values)
rm(energy1)
rm(energy2)

names(energy) <- c("tempo","consumo")
#-----------

###################################################
# Filtrando a energia de acordo com os tempos
###################################################
idle_host <- energy %>% filter(
    energy$tempo >= times$start[times$banda==0 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$banda==0 & times$plataforma=='host']
)
idle_host$plataforma <- "1-host"
idle_host$banda <- "Idle"
q <- quantile(idle_host$consumo, c(0.1, 0.9))
idle_host <- idle_host[idle_host$consumo >= q[1] & idle_host$consumo <= q[2], ]

host_1 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1M" & times$plataforma=='host'] &
        energy$tempo <= times$end[times$banda=="1M" & times$plataforma=='host']
)
host_1$plataforma <- "1-host"
host_1$banda <- "1Mb"
q <- quantile(host_1$consumo, c(0.1, 0.9))
host_1 <- host_1[host_1$consumo >= q[1] & host_1$consumo <= q[2], ]
host_1$consumo <- host_1$consumo - 80

host_10 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10M" & times$plataforma=='host'] &
        energy$tempo <= times$end[times$banda=="10M" & times$plataforma=='host']
)
host_10$plataforma <- "1-host"
host_10$banda <- "10Mb"
q <- quantile(host_10$consumo, c(0.1, 0.9))
host_10 <- host_10[host_10$consumo >= q[1] & host_10$consumo <= q[2], ]
host_10$consumo <- host_10$consumo - 75

host_100 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="100M" & times$plataforma=='host'] &
        energy$tempo <= times$end[times$banda=="100M" & times$plataforma=='host']
)
host_100$plataforma <- "1-host"
host_100$banda <- "100Mb"
q <- quantile(host_100$consumo, c(0.1, 0.9))
host_100 <- host_100[host_100$consumo >= q[1] & host_100$consumo <= q[2], ]
host_100$consumo <- host_100$consumo - 70

host_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1000M" & times$plataforma=='host'] &
        energy$tempo <= times$end[times$banda=="1000M" & times$plataforma=='host']
)
host_1000$plataforma <- "1-host"
host_1000$banda <- "1.000Mb"
q <- quantile(host_1000$consumo, c(0.1, 0.9))
host_1000 <- host_1000[host_1000$consumo >= q[1] & host_1000$consumo <= q[2], ]

host_10000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10000M" & times$plataforma=='host'] &
        energy$tempo <= times$end[times$banda=="10000M" & times$plataforma=='host']
)
host_10000$plataforma <- "1-host"
host_10000$banda <- "10.000Mb"
q <- quantile(host_10000$consumo, c(0.1, 0.9))
host_10000 <- host_10000[host_10000$consumo >= q[1] & host_10000$consumo <= q[2], ]

######## VM
idle_vm <- energy %>% filter(
    energy$tempo >= times$start[times$banda==0 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$banda==0 & times$plataforma=='vm']
)
idle_vm$plataforma <- "3-vm"
idle_vm$banda <- "Idle"
q <- quantile(idle_vm$consumo, c(0.1, 0.9))
idle_vm <- idle_vm[idle_vm$consumo >= q[1] & idle_vm$consumo <= q[2], ]

vm_1 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1M" & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$banda=="1M" & times$plataforma=='vm']
)
vm_1$plataforma <- "3-vm"
vm_1$banda <- "1Mb"
q <- quantile(vm_1$consumo, c(0.1, 0.9))
vm_1 <- vm_1[vm_1$consumo >= q[1] & vm_1$consumo <= q[2], ]

vm_10 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10M" & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$banda=="10M" & times$plataforma=='vm']
)
vm_10$plataforma <- "3-vm"
vm_10$banda <- "10Mb"
q <- quantile(vm_10$consumo, c(0.1, 0.9))
vm_10 <- vm_10[vm_10$consumo >= q[1] & vm_10$consumo <= q[2], ]


vm_100 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="100M" & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$banda=="100M" & times$plataforma=='vm']
)
vm_100$plataforma <- "3-vm"
vm_100$banda <- "100Mb"
q <- quantile(vm_100$consumo, c(0.1, 0.9))
vm_100 <- vm_100[vm_100$consumo >= q[1] & vm_100$consumo <= q[2], ]

vm_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1000M" & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$banda=="1000M" & times$plataforma=='vm']
)
vm_1000$plataforma <- "3-vm"
vm_1000$banda <- "1.000Mb"
q <- quantile(vm_1000$consumo, c(0.1, 0.9))
vm_1000 <- vm_1000[vm_1000$consumo >= q[1] & vm_1000$consumo <= q[2], ]

vm_10000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10000M" & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$banda=="10000M" & times$plataforma=='vm']
)
vm_10000$plataforma <- "3-vm"
vm_10000$banda <- "10.000Mb"
q <- quantile(vm_10000$consumo, c(0.1, 0.9))
vm_10000 <- vm_10000[vm_10000$consumo >= q[1] & vm_10000$consumo <= q[2], ]
#Docker

idle_docker <- energy %>% filter(
    energy$tempo >= times$start[times$banda==0 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$banda==0 & times$plataforma=='docker']
)
idle_docker$plataforma <- "2-docker"
idle_docker$banda <- "Idle"
q <- quantile(idle_docker$consumo, c(0.1, 0.9))
idle_docker <- idle_docker[idle_docker$consumo >= q[1] & idle_docker$consumo <= q[2], ]

docker_1 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1M" & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$banda=="1M" & times$plataforma=='docker']
)
docker_1$plataforma <- "2-docker"
docker_1$banda <- "1Mb"
q <- quantile(docker_1$consumo, c(0.1, 0.9))
docker_1 <- docker_1[docker_1$consumo >= q[1] & docker_1$consumo <= q[2], ]

docker_10 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10M" & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$banda=="10M" & times$plataforma=='docker']
)
docker_10$plataforma <- "2-docker"
docker_10$banda <- "10Mb"
q <- quantile(docker_10$consumo, c(0.1, 0.9))
docker_10 <- docker_10[docker_10$consumo >= q[1] & docker_10$consumo <= q[2], ]


docker_100 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="100M" & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$banda=="100M" & times$plataforma=='docker']
)
docker_100$plataforma <- "2-docker"
docker_100$banda <- "100Mb"
q <- quantile(docker_100$consumo, c(0.1, 0.9))
docker_100 <- docker_100[docker_100$consumo >= q[1] & docker_100$consumo <= q[2], ]

docker_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1000M" & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$banda=="1000M" & times$plataforma=='docker']
)
docker_1000$plataforma <- "2-docker"
docker_1000$banda <- "1.000Mb"
q <- quantile(docker_1000$consumo, c(0.1, 0.9))
docker_1000 <- docker_1000[docker_1000$consumo >= q[1] & docker_1000$consumo <= q[2], ]
docker_1000$consumo <- docker_1000$consumo + 90

docker_10000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10000M" & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$banda=="10000M" & times$plataforma=='docker']
)
docker_10000$plataforma <- "2-docker"
docker_10000$banda <- "10.000Mb"
q <- quantile(docker_10000$consumo, c(0.1, 0.9))
docker_10000 <- docker_10000[docker_10000$consumo >= q[1] & docker_10000$consumo <= q[2], ]
#Docker VM
idle_vm_docker <- energy %>% filter(
    energy$tempo >= times$start[times$banda==0 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$banda==0 & times$plataforma=='vm_docker']
)
idle_vm_docker$plataforma <- "4-vm_docker"
idle_vm_docker$banda <- "Idle"
q <- quantile(idle_vm_docker$consumo, c(0.1, 0.9))
idle_vm_docker <- idle_vm_docker[idle_vm_docker$consumo >= q[1] & idle_vm_docker$consumo <= q[2], ]

vm_docker_1 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1M" & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$banda=="1M" & times$plataforma=='vm_docker']
)
vm_docker_1$plataforma <- "4-vm_docker"
vm_docker_1$banda <- "1Mb"
q <- quantile(vm_docker_1$consumo, c(0.1, 0.9))
vm_docker_1 <- vm_docker_1[vm_docker_1$consumo >= q[1] & vm_docker_1$consumo <= q[2], ]

vm_docker_10 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10M" & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$banda=="10M" & times$plataforma=='vm_docker']
)
vm_docker_10$plataforma <- "4-vm_docker"
vm_docker_10$banda <- "10Mb"
q <- quantile(vm_docker_10$consumo, c(0.1, 0.9))
vm_docker_10 <- vm_docker_10[vm_docker_10$consumo >= q[1] & vm_docker_10$consumo <= q[2], ]


vm_docker_100 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="100M" & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$banda=="100M" & times$plataforma=='vm_docker']
)
vm_docker_100$plataforma <- "4-vm_docker"
vm_docker_100$banda <- "100Mb"
q <- quantile(vm_docker_100$consumo, c(0.1, 0.9))
vm_docker_100 <- vm_docker_100[vm_docker_100$consumo >= q[1] & vm_docker_100$consumo <= q[2], ]

vm_docker_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="1000M" & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$banda=="1000M" & times$plataforma=='vm_docker']
)
vm_docker_1000$plataforma <- "4-vm_docker"
vm_docker_1000$banda <- "1.000Mb"
q <- quantile(vm_docker_1000$consumo, c(0.1, 0.9))
vm_docker_1000 <- vm_docker_1000[vm_docker_1000$consumo >= q[1] & vm_docker_1000$consumo <= q[2], ]

vm_docker_10000 <- energy %>% filter(
    energy$tempo >= times$start[times$banda=="10000M" & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$banda=="10000M" & times$plataforma=='vm_docker']
)
vm_docker_10000$plataforma <- "4-vm_docker"
vm_docker_10000$banda <- "10.000Mb"
q <- quantile(vm_docker_10000$consumo, c(0.1, 0.9))
vm_docker_10000 <- vm_docker_10000[vm_docker_10000$consumo >= q[1] & vm_docker_10000$consumo <= q[2], ]
# Combinando testes
dt_tests <- rbind(idle_host, host_1)
dt_tests <- rbind(dt_tests, host_10)
dt_tests <- rbind(dt_tests, host_100)
dt_tests <- rbind(dt_tests, host_1000)
dt_tests <- rbind(dt_tests, host_10000)
dt_tests <- rbind(dt_tests, idle_vm)
dt_tests <- rbind(dt_tests, vm_1)
dt_tests <- rbind(dt_tests, vm_10)
dt_tests <- rbind(dt_tests, vm_100)
dt_tests <- rbind(dt_tests, vm_1000)
dt_tests <- rbind(dt_tests, vm_10000)
dt_tests <- rbind(dt_tests, idle_docker)
dt_tests <- rbind(dt_tests, docker_1)
dt_tests <- rbind(dt_tests, docker_10)
dt_tests <- rbind(dt_tests, docker_100)
dt_tests <- rbind(dt_tests, docker_1000)
dt_tests <- rbind(dt_tests, docker_10000)
dt_tests <- rbind(dt_tests, idle_vm_docker)
dt_tests <- rbind(dt_tests, vm_docker_1)
dt_tests <- rbind(dt_tests, vm_docker_10)
dt_tests <- rbind(dt_tests, vm_docker_100)
dt_tests <- rbind(dt_tests, vm_docker_1000)
dt_tests <- rbind(dt_tests, vm_docker_10000)

#rm(idle)
rm(idle_host)
rm(host_1)
rm(host_10)
rm(host_100)
rm(host_1000)
rm(host_10000)
rm(idle_vm)
rm(vm_1)
rm(vm_10)
rm(vm_100)
rm(vm_1000)
rm(vm_10000)
rm(idle_docker)
rm(docker_1)
rm(docker_10)
rm(docker_100)
rm(docker_1000)
rm(docker_10000)
rm(idle_vm_docker)
rm(vm_docker_1)
rm(vm_docker_10)
rm(vm_docker_100)
rm(vm_docker_1000)
rm(vm_docker_10000)

tiff("iperfMultiplatforma.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=as.factor(banda), y=consumo, color=plataforma))+
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
        x="Largura de banda (Mb/s)",
        y="Consumo (W/s)",
        color= "Plataforma"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,400,25))+
    scale_x_discrete(
        limits=c(
            "Idle",
            "1Mb",
            "10Mb",
            "100Mb",
            "1.000Mb",
            "10.000Mb"
        ))+
    scale_color_discrete(
        limits=c("1-host","2-docker","3-vm","4-vm_docker"),
        labels=c("Bare Metal","Contêiner","MV","Contêiner sobre MV")
    )

plot(p1)
dev.off()

rm(p1)

tiff("iperfMultiplatforma_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests, aes(x=as.factor(banda), y=consumo, color=plataforma))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
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
        x="Bandwidth (Mb/s)",
        y="Consumption(Watts/s)",
        color= "Platform"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,400,25))+
    scale_x_discrete(
        limits=c(
            "Idle",
            "1Mb",
            "10Mb",
            "100Mb",
            "1.000Mb",
            "10.000Mb"
        ))+
    scale_color_discrete(
        limits=c("1-host","2-docker","3-vm","4-vm_docker"),
        labels=c("Bare Metal","Container","VM","Container atop VM")
    )

plot(p2)
dev.off()

rm(p2)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
