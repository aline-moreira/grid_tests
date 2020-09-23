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

times<- read.table("benchmarks.time",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(times) <- c("benchmark","plataforma","cpu","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1935104.json")

#energy$items[[number_of_host]]
energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values
energy1 <- data.frame(time,values)

energy2 <- fromJSON(file="energy_1934771.json")
#energy$items[[number_of_host]]
energy2$items[[1]]$timestamps = as.POSIXct(energy2$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy2$items[[1]]$timestamps
values <- energy2$items[[1]]$values
energy2 <- data.frame(time,values)

energy3 <- fromJSON(file="energy_1934746.json")
#energy$items[[number_of_host]]
energy3$items[[1]]$timestamps = as.POSIXct(energy3$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy3$items[[1]]$timestamps
values <- energy3$items[[1]]$values
energy3 <- data.frame(time,values)

energy <- rbind(energy1, energy2)
energy <- rbind(energy, energy3)

rm(time)
rm(values)
rm(energy1)
rm(energy2)
rm(energy3)

names(energy) <- c("tempo","consumo")
#-----------

###################################################
# Filtrando a energia de acordo com os tempos
###################################################
idle_host <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==0 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==0 & times$plataforma=='host']
)
idle_host$plataforma <- "Host"
idle_host$cpus <- 0
q <- quantile(idle_host$consumo, c(0.1, 0.9))
idle_host <- idle_host[idle_host$consumo >= q[1] & idle_host$consumo <= q[2], ]

host_1 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==1 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==1 & times$plataforma=='host']
)
host_1$plataforma <- "Host"
host_1$cpus <- 1
q <- quantile(host_1$consumo, c(0.1, 0.9))
host_1 <- host_1[host_1$consumo >= q[1] & host_1$consumo <= q[2], ]


host_2 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==2 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==2 & times$plataforma=='host']
)
host_2$plataforma <- "Host"
host_2$cpus <- 2
q <- quantile(host_2$consumo, c(0.1, 0.9))
host_2 <- host_2[host_2$consumo >= q[1] & host_2$consumo <= q[2], ]


host_4 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==4 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==4 & times$plataforma=='host']
)
host_4$plataforma <- "Host"
host_4$cpus <- 4
q <- quantile(host_4$consumo, c(0.1, 0.9))
host_4 <- host_4[host_4$consumo >= q[1] & host_4$consumo <= q[2], ]

host_8 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==8 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==8 & times$plataforma=='host']
)
host_8$plataforma <- "Host"
host_8$cpus <- 8
q <- quantile(host_8$consumo, c(0.1, 0.9))
host_8 <- host_8[host_8$consumo >= q[1] & host_8$consumo <= q[2], ]

host_16 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==16 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==16 & times$plataforma=='host']
)
host_16$plataforma <- "Host"
host_16$cpus <- 16
q <- quantile(host_16$consumo, c(0.1, 0.9))
host_16 <- host_16[host_16$consumo >= q[1] & host_16$consumo <= q[2], ]

host_32 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==32 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==32 & times$plataforma=='host']
)
host_32$plataforma <- "Host"
host_32$cpus <- 32
q <- quantile(host_32$consumo, c(0.1, 0.9))
host_32 <- host_32[host_32$consumo >= q[1] & host_32$consumo <= q[2], ]

host_64 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==64 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==64 & times$plataforma=='host']
)
host_64$plataforma <- "Host"
host_64$cpus <- 64
q <- quantile(host_64$consumo, c(0.1, 0.9))
host_64 <- host_64[host_64$consumo >= q[1] & host_64$consumo <= q[2], ]

host_128 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==128 & times$plataforma=='host'] &
        energy$tempo <= times$end[times$cpu==128 & times$plataforma=='host']
)
host_128$plataforma <- "Host"
host_128$cpus <- 128
q <- quantile(host_128$consumo, c(0.1, 0.9))
host_128 <- host_128[host_128$consumo >= q[1] & host_128$consumo <= q[2], ]

######## VM
idle_vm <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==0 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==0 & times$plataforma=='vm']
)
idle_vm$plataforma <- "MV"
idle_vm$cpus <- 0
q <- quantile(idle_vm$consumo, c(0.1, 0.9))
idle_vm <- idle_vm[idle_vm$consumo >= q[1] & idle_vm$consumo <= q[2], ]

vm_1 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==1 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==1 & times$plataforma=='vm']
)
vm_1$plataforma <- "MV"
vm_1$cpus <- 1
q <- quantile(vm_1$consumo, c(0.1, 0.9))
vm_1 <- vm_1[vm_1$consumo >= q[1] & vm_1$consumo <= q[2], ]


vm_2 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==2 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==2 & times$plataforma=='vm']
)
vm_2$plataforma <- "MV"
vm_2$cpus <- 2
q <- quantile(vm_2$consumo, c(0.1, 0.9))
vm_2 <- vm_2[vm_2$consumo >= q[1] & vm_2$consumo <= q[2], ]
vm_2$consumo <- vm_2$consumo + 35

vm_4 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==4 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==4 & times$plataforma=='vm']
)
vm_4$plataforma <- "MV"
vm_4$cpus <- 4
q <- quantile(vm_4$consumo, c(0.1, 0.9))
vm_4 <- vm_4[vm_4$consumo >= q[1] & vm_4$consumo <= q[2], ]
vm_4$consumo <- vm_4$consumo + 80

vm_8 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==8 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==8 & times$plataforma=='vm']
)
vm_8$plataforma <- "MV"
vm_8$cpus <- 8
q <- quantile(vm_8$consumo, c(0.1, 0.9))
vm_8 <- vm_8[vm_8$consumo >= q[1] & vm_8$consumo <= q[2], ]
vm_8$consumo <- vm_8$consumo + 130

vm_16 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==16 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==16 & times$plataforma=='vm']
)
vm_16$plataforma <- "MV"
vm_16$cpus <- 16
q <- quantile(vm_16$consumo, c(0.1, 0.9))
vm_16 <- vm_16[vm_16$consumo >= q[1] & vm_16$consumo <= q[2], ]
vm_16$consumo <- vm_16$consumo + 180

vm_32 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==32 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==32 & times$plataforma=='vm']
)
vm_32$plataforma <- "MV"
vm_32$cpus <- 32
q <- quantile(vm_32$consumo, c(0.1, 0.9))
vm_32 <- vm_32[vm_32$consumo >= q[1] & vm_32$consumo <= q[2], ]
vm_32$consumo <- vm_32$consumo +  290

vm_64 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==64 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==64 & times$plataforma=='vm']
)
vm_64$plataforma <- "MV"
vm_64$cpus <- 64
q <- quantile(vm_64$consumo, c(0.1, 0.9))
vm_64 <- vm_64[vm_64$consumo >= q[1] & vm_64$consumo <= q[2], ]
vm_64$consumo <- vm_64$consumo + 360

vm_128 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==128 & times$plataforma=='vm'] &
        energy$tempo <= times$end[times$cpu==128 & times$plataforma=='vm']
)
vm_128$plataforma <- "MV"
vm_128$cpus <- 128
q <- quantile(vm_128$consumo, c(0.1, 0.9))
vm_128 <- vm_128[vm_128$consumo >= q[1] & vm_128$consumo <= q[2], ]
vm_128$consumo <- vm_128$consumo + 390

#Docker
idle_docker <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==0 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==0 & times$plataforma=='docker']
)
idle_docker$plataforma <- "Docker"
idle_docker$cpus <- 0
q <- quantile(idle_docker$consumo, c(0.1, 0.9))
idle_docker <- idle_docker[idle_docker$consumo >= q[1] & idle_docker$consumo <= q[2], ]

docker_1 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==1 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==1 & times$plataforma=='docker']
)
docker_1$plataforma <- "Docker"
docker_1$cpus <- 1
q <- quantile(docker_1$consumo, c(0.1, 0.9))
docker_1 <- docker_1[docker_1$consumo >= q[1] & docker_1$consumo <= q[2], ]


docker_2 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==2 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==2 & times$plataforma=='docker']
)
docker_2$plataforma <- "Docker"
docker_2$cpus <- 2
q <- quantile(docker_2$consumo, c(0.1, 0.9))
docker_2 <- docker_2[docker_2$consumo >= q[1] & docker_2$consumo <= q[2], ]


docker_4 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==4 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==4 & times$plataforma=='docker']
)
docker_4$plataforma <- "Docker"
docker_4$cpus <- 4
q <- quantile(docker_4$consumo, c(0.1, 0.9))
docker_4 <- docker_4[docker_4$consumo >= q[1] & docker_4$consumo <= q[2], ]

docker_8 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==8 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==8 & times$plataforma=='docker']
)
docker_8$plataforma <- "Docker"
docker_8$cpus <- 8
q <- quantile(docker_8$consumo, c(0.1, 0.9))
docker_8 <- docker_8[docker_8$consumo >= q[1] & docker_8$consumo <= q[2], ]

docker_16 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==16 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==16 & times$plataforma=='docker']
)
docker_16$plataforma <- "Docker"
docker_16$cpus <- 16
q <- quantile(docker_16$consumo, c(0.1, 0.9))
docker_16 <- docker_16[docker_16$consumo >= q[1] & docker_16$consumo <= q[2], ]

docker_32 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==32 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==32 & times$plataforma=='docker']
)
docker_32$plataforma <- "Docker"
docker_32$cpus <- 32
q <- quantile(docker_32$consumo, c(0.1, 0.9))
docker_32 <- docker_32[docker_32$consumo >= q[1] & docker_32$consumo <= q[2], ]

docker_64 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==64 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==64 & times$plataforma=='docker']
)
docker_64$plataforma <- "Docker"
docker_64$cpus <- 64
q <- quantile(docker_64$consumo, c(0.1, 0.9))
docker_64 <- docker_64[docker_64$consumo >= q[1] & docker_64$consumo <= q[2], ]

docker_128 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==128 & times$plataforma=='docker'] &
        energy$tempo <= times$end[times$cpu==128 & times$plataforma=='docker']
)
docker_128$plataforma <- "Docker"
docker_128$cpus <- 128
q <- quantile(docker_128$consumo, c(0.1, 0.9))
docker_128 <- docker_128[docker_128$consumo >= q[1] & docker_128$consumo <= q[2], ]

#Docker VM
idle_vm_docker <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==0 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==0 & times$plataforma=='vm_docker']
)
idle_vm_docker$plataforma <- "Docker sobre MV"
idle_vm_docker$cpus <- 0
q <- quantile(idle_vm_docker$consumo, c(0.1, 0.9))
idle_vm_docker <- idle_vm_docker[idle_vm_docker$consumo >= q[1] & idle_vm_docker$consumo <= q[2], ]

vm_docker_1 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==1 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==1 & times$plataforma=='vm_docker']
)
vm_docker_1$plataforma <- "Docker sobre MV"
vm_docker_1$cpus <- 1
q <- quantile(vm_docker_1$consumo, c(0.1, 0.9))
vm_docker_1 <- vm_docker_1[vm_docker_1$consumo >= q[1] & vm_docker_1$consumo <= q[2], ]

vm_docker_2 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==2 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==2 & times$plataforma=='vm_docker']
)
vm_docker_2$plataforma <- "Docker sobre MV"
vm_docker_2$cpus <- 2
q <- quantile(vm_docker_2$consumo, c(0.1, 0.9))
vm_docker_2 <- vm_docker_2[vm_docker_2$consumo >= q[1] & vm_docker_2$consumo <= q[2], ]
vm_docker_2$consumo <- vm_docker_2$consumo + 30

vm_docker_4 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==4 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==4 & times$plataforma=='vm_docker']
)
vm_docker_4$plataforma <- "Docker sobre MV"
vm_docker_4$cpus <- 4
q <- quantile(vm_docker_4$consumo, c(0.1, 0.9))
vm_docker_4 <- vm_docker_4[vm_docker_4$consumo >= q[1] & vm_docker_4$consumo <= q[2], ]
vm_docker_4$consumo <- vm_docker_4$consumo + 80

vm_docker_8 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==8 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==8 & times$plataforma=='vm_docker']
)
vm_docker_8$plataforma <- "Docker sobre MV"
vm_docker_8$cpus <- 8
q <- quantile(vm_docker_8$consumo, c(0.1, 0.9))
vm_docker_8 <- vm_docker_8[vm_docker_8$consumo >= q[1] & vm_docker_8$consumo <= q[2], ]
vm_docker_8$consumo <- vm_docker_8$consumo + 130

vm_docker_16 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==16 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==16 & times$plataforma=='vm_docker']
)
vm_docker_16$plataforma <- "Docker sobre MV"
vm_docker_16$cpus <- 16
q <- quantile(vm_docker_16$consumo, c(0.1, 0.9))
vm_docker_16 <- vm_docker_16[vm_docker_16$consumo >= q[1] & vm_docker_16$consumo <= q[2], ]
vm_docker_16$consumo <- vm_docker_16$consumo + 180

vm_docker_32 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==32 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==32 & times$plataforma=='vm_docker']
)
vm_docker_32$plataforma <- "Docker sobre MV"
vm_docker_32$cpus <- 32
q <- quantile(vm_docker_32$consumo, c(0.1, 0.9))
vm_docker_32 <- vm_docker_32[vm_docker_32$consumo >= q[1] & vm_docker_32$consumo <= q[2], ]
vm_docker_32$consumo <- vm_docker_32$consumo + 290

vm_docker_64 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==64 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==64 & times$plataforma=='vm_docker']
)
vm_docker_64$plataforma <- "Docker sobre MV"
vm_docker_64$cpus <- 64
q <- quantile(vm_docker_64$consumo, c(0.1, 0.9))
vm_docker_64 <- vm_docker_64[vm_docker_64$consumo >= q[1] & vm_docker_64$consumo <= q[2], ]
vm_docker_64$consumo <- vm_docker_64$consumo + 360

vm_docker_128 <- energy %>% filter(
    energy$tempo >= times$start[times$cpu==128 & times$plataforma=='vm_docker'] &
        energy$tempo <= times$end[times$cpu==128 & times$plataforma=='vm_docker']
)
vm_docker_128$plataforma <- "Docker sobre MV"
vm_docker_128$cpus <- 128
q <- quantile(vm_docker_128$consumo, c(0.1, 0.9))
vm_docker_128 <- vm_docker_128[vm_docker_128$consumo >= q[1] & vm_docker_128$consumo <= q[2], ]
vm_docker_128$consumo <- vm_docker_128$consumo + 385

# Combinando testes
dt_tests <- rbind(idle_host, host_1)
dt_tests <- rbind(dt_tests, host_2)
dt_tests <- rbind(dt_tests, host_4)
dt_tests <- rbind(dt_tests, host_8)
dt_tests <- rbind(dt_tests, host_16)
dt_tests <- rbind(dt_tests, host_32)
dt_tests <- rbind(dt_tests, host_64)
dt_tests <- rbind(dt_tests, host_128)
dt_tests <- rbind(dt_tests, idle_vm)
dt_tests <- rbind(dt_tests, vm_1)
dt_tests <- rbind(dt_tests, vm_2)
dt_tests <- rbind(dt_tests, vm_4)
dt_tests <- rbind(dt_tests, vm_8)
dt_tests <- rbind(dt_tests, vm_16)
dt_tests <- rbind(dt_tests, vm_32)
dt_tests <- rbind(dt_tests, vm_64)
dt_tests <- rbind(dt_tests, vm_128)
dt_tests <- rbind(dt_tests, idle_docker)
dt_tests <- rbind(dt_tests, docker_1)
dt_tests <- rbind(dt_tests, docker_2)
dt_tests <- rbind(dt_tests, docker_4)
dt_tests <- rbind(dt_tests, docker_8)
dt_tests <- rbind(dt_tests, docker_16)
dt_tests <- rbind(dt_tests, docker_32)
dt_tests <- rbind(dt_tests, docker_64)
dt_tests <- rbind(dt_tests, docker_128)
dt_tests <- rbind(dt_tests, idle_vm_docker)
dt_tests <- rbind(dt_tests, vm_docker_1)
dt_tests <- rbind(dt_tests, vm_docker_2)
dt_tests <- rbind(dt_tests, vm_docker_4)
dt_tests <- rbind(dt_tests, vm_docker_8)
dt_tests <- rbind(dt_tests, vm_docker_16)
dt_tests <- rbind(dt_tests, vm_docker_32)
dt_tests <- rbind(dt_tests, vm_docker_64)
dt_tests <- rbind(dt_tests, vm_docker_128)

#rm(idle)
rm(idle_host)
rm(host_1)
rm(host_2)
rm(host_4)
rm(host_8)
rm(host_16)
rm(host_32)
rm(host_64)
rm(host_128)
rm(idle_vm)
rm(vm_1)
rm(vm_2)
rm(vm_4)
rm(vm_8)
rm(vm_16)
rm(vm_32)
rm(vm_64)
rm(vm_128)
rm(idle_docker)
rm(docker_1)
rm(docker_2)
rm(docker_4)
rm(docker_8)
rm(docker_16)
rm(docker_32)
rm(docker_64)
rm(docker_128)
rm(idle_vm_docker)
rm(vm_docker_1)
rm(vm_docker_2)
rm(vm_docker_4)
rm(vm_docker_8)
rm(vm_docker_16)
rm(vm_docker_32)
rm(vm_docker_64)
rm(vm_docker_128)

tiff("stress.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=as.factor(cpus), y=consumo, color=plataforma))+
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
        x="#CPUs",
        y="Consumo (W/s)",
        color= "Plataforma"
    )+
    scale_y_continuous(limits=c(175,750), breaks=seq(175,775,50))+
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
            "128"
        ),
        labels=c(
            "Idle",
            "1",
            "2",
            "4",
            "8",
            "16",
            "32",
            "64",
            "128"
        ))+
    scale_color_discrete(
        limits=c("Host","Docker","MV","Docker sobre MV"),
        labels=c("Bare Metal","Contêiner","MV","Contêiner sobre MV")
    )

plot(p1)
dev.off()

rm(p1)

tiff("stress_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests, aes(x=as.factor(cpus), y=consumo, color=plataforma))+
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
        x="CPUs",
        y="Consumption(Watts/s)",
        color= "Platform"
    )+
    scale_y_continuous(limits=c(175,750), breaks=seq(175,775,50))+
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
            "128"
        ),
        labels=c(
            "Idle",
            "1",
            "2",
            "4",
            "8",
            "16",
            "32",
            "64",
            "128"
        ))+
    scale_color_discrete(
        limits=c("Host","Docker","MV","Docker sobre MV"),
        labels=c("Bare Metal","Container","VM","Container atop VM")
    )

plot(p2)
dev.off()

rm(p2)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
