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

times<- read.table("fio.times",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(times) <- c("plataforma","IO","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1928307.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values

energy <- data.frame(time,values)

rm(time)
rm(values)

names(energy) <- c("tempo","consumo")

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
#50G
host_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==50]
)
host_50$IO <- "50G"
host_50$plataforma <- "Bare Metal"
q <- quantile(host_50$consumo, c(0.1, 0.9))
host_50 <- host_50[host_50$consumo >= q[1] & host_50$consumo <= q[2], ]
#100G
host_100 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==100] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==100]
)
host_100$IO <- "100G"
host_100$plataforma <- "Bare Metal"
q <- quantile(host_100$consumo, c(0.1, 0.9))
host_100 <- host_100[host_100$consumo >= q[1] & host_100$consumo <= q[2], ]
#500G
host_500 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==500] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==500]
)
host_500$IO <- "500G"
host_500$plataforma <- "Bare Metal"
q <- quantile(host_500$consumo, c(0.1, 0.9))
host_500 <- host_500[host_500$consumo >= q[1] & host_500$consumo <= q[2], ]
#1000G
host_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==1000] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==1000]
)
host_1000$IO <- "1000G"
host_1000$plataforma <- "Bare Metal"
q <- quantile(host_1000$consumo, c(0.1, 0.9))
host_1000 <- host_1000[host_1000$consumo >= q[1] & host_1000$consumo <= q[2], ]
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
#50G
docker_50 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==50] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==50]
)
docker_50$IO <- "50G"
docker_50$plataforma <- "Contêiner"
q <- quantile(docker_50$consumo, c(0.1, 0.9))
docker_50 <- docker_50[docker_50$consumo >= q[1] & docker_50$consumo <= q[2], ]
#100G
docker_100 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==100] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==100]
)
docker_100$IO <- "100G"
docker_100$plataforma <- "Contêiner"
q <- quantile(docker_100$consumo, c(0.1, 0.9))
docker_100 <- docker_100[docker_100$consumo >= q[1] & docker_100$consumo <= q[2], ]
#500G
docker_500 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==500] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==500]
)
docker_500$IO <- "500G"
docker_500$plataforma <- "Contêiner"
q <- quantile(docker_500$consumo, c(0.1, 0.9))
docker_500 <- docker_500[docker_500$consumo >= q[1] & docker_500$consumo <= q[2], ]
#1000G
docker_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==1000] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==1000]
)
docker_1000$IO <- "1000G"
docker_1000$plataforma <- "Contêiner"
q <- quantile(docker_1000$consumo, c(0.1, 0.9))
docker_1000 <- docker_1000[docker_1000$consumo >= q[1] & docker_1000$consumo <= q[2], ]


###############
# Combinando testes
dt_tests <- rbind(idle_host, idle_docker)
dt_tests <- rbind(dt_tests, host_1)
dt_tests <- rbind(dt_tests, host_5)
dt_tests <- rbind(dt_tests, host_10)
dt_tests <- rbind(dt_tests, host_50)
dt_tests <- rbind(dt_tests, host_100)
dt_tests <- rbind(dt_tests, host_500)
dt_tests <- rbind(dt_tests, host_1000)
dt_tests <- rbind(dt_tests, docker_1)
dt_tests <- rbind(dt_tests, docker_5)
dt_tests <- rbind(dt_tests, docker_10)
dt_tests <- rbind(dt_tests, docker_50)
dt_tests <- rbind(dt_tests, docker_100)
dt_tests <- rbind(dt_tests, docker_500)
dt_tests <- rbind(dt_tests, docker_1000)

#rm(idle)
rm(idle_host)
rm(idle_docker)
rm(host_1)
rm(host_5)
rm(host_10)
rm(host_50)
rm(host_100)
rm(host_500)
rm(host_1000)
rm(docker_1)
rm(docker_5)
rm(docker_10)
rm(docker_50)
rm(docker_100)
rm(docker_500)
rm(docker_1000)

tiff("sysbench_kubernetes_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
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
        x="Quantidade de Contêineres",
        y="Consumo (W/s)",
        color= "Tipo de hospedeiro"
    )+
    scale_y_continuous(limits=c(175,400), breaks=seq(175,425,25))+
    scale_x_discrete(
        limits=c(
            "idle",
            "1G",
            "5G",
            "10G",
            "50G",
            "100G",
            "500G",
            "1000G"
        ),
        labels=c(
            "Idle",
            "1",
            "5",
            "10",
            "50",
            "100",
            "500",
            "1000"
        ))

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
