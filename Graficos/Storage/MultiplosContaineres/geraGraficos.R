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
names(times) <- c("plataforma","IO","dockers","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1933697.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values

energy <- data.frame(time,values)

rm(time)
rm(values)

names(energy) <- c("tempo","consumo")
#-----------

###################################################
# Filtrando a energia de acordo com os tempos
###################################################
idle_docker <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==0 & times$IO==0] &
        energy$tempo <= times$end[times$dockers==0 & times$IO==0]
)
idle_docker$IO <- "idle"
idle_docker$dockers <- "Idle"
idle_docker$total <- "Idle"
q <- quantile(idle_docker$consumo, c(0.1, 0.9))
idle_docker <- idle_docker[idle_docker$consumo >= q[1] & idle_docker$consumo <= q[2], ]

#3 DOCKER 20IO
docker_3_20 <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==3 & times$IO==20] &
        energy$tempo <= times$end[times$dockers==3 & times$IO==20]
)
docker_3_20$IO <- "20G"
docker_3_20$dockers <- 3
docker_3_20$total <- "60G"
q <- quantile(docker_3_20$consumo, c(0.1, 0.9))
docker_3_20 <- docker_3_20[docker_3_20$consumo >= q[1] & docker_3_20$consumo <= q[2], ]
#2 DOCKER 30IO
docker_2_30 <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==2 & times$IO==30] &
        energy$tempo <= times$end[times$dockers==2 & times$IO==30]
)
docker_2_30$IO <- "30G"
docker_2_30$dockers <- 2
docker_2_30$total <- "60G"
q <- quantile(docker_2_30$consumo, c(0.1, 0.9))
docker_2_30 <- docker_2_30[docker_2_30$consumo >= q[1] & docker_2_30$consumo <= q[2], ]
#6 DOCKER 10IO
docker_6_10 <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==6 & times$IO==10] &
        energy$tempo <= times$end[times$dockers==6 & times$IO==10]
)
docker_6_10$IO <- "10G"
docker_6_10$dockers <- 6
docker_6_10$total <- "60G"
q <- quantile(docker_6_10$consumo, c(0.1, 0.9))
docker_6_10 <- docker_6_10[docker_6_10$consumo >= q[1] & docker_6_10$consumo <= q[2], ]
#8 DOCKER 10IO
docker_8_10 <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==8 & times$IO==10] &
        energy$tempo <= times$end[times$dockers==8 & times$IO==10]
)
docker_8_10$IO <- "10G"
docker_8_10$dockers <- 8
docker_8_10$total <- "80G"
q <- quantile(docker_8_10$consumo, c(0.1, 0.9))
docker_8_10 <- docker_8_10[docker_8_10$consumo >= q[1] & docker_8_10$consumo <= q[2], ]
#4 DOCKER 20IO
docker_4_20 <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==4 & times$IO==20] &
        energy$tempo <= times$end[times$dockers==4 & times$IO==20]
)
docker_4_20$IO <- "20G"
docker_4_20$dockers <- 4
docker_4_20$total <- "80G"
q <- quantile(docker_4_20$consumo, c(0.1, 0.9))
docker_4_20 <- docker_4_20[docker_4_20$consumo >= q[1] & docker_4_20$consumo <= q[2], ]
#2 DOCKER 40IO
docker_2_40 <- energy %>% filter(
    energy$tempo >= times$start[times$dockers==2 & times$IO==40] &
        energy$tempo <= times$end[times$dockers==2 & times$IO==40]
)
docker_2_40$IO <- "40G"
docker_2_40$dockers <- 2
docker_2_40$total <- "80G"
q <- quantile(docker_2_40$consumo, c(0.1, 0.9))
docker_2_40 <- docker_2_40[docker_2_40$consumo >= q[1] & docker_2_40$consumo <= q[2], ]


times<- read.table("fio2.times",header=FALSE,sep=";", stringsAsFactors = FALSE)
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



#1 Docker 60IO
docker_1_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==60]
)
docker_1_60$IO <- "60G"
docker_1_60$dockers <- 1
docker_1_60$total <- "60G"
q <- quantile(docker_1_60$consumo, c(0.1, 0.9))
docker_1_60 <- docker_1_60[docker_1_60$consumo >= q[1] & docker_1_60$consumo <= q[2], ]

#1 docker 80IO
docker_1_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="docker" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="docker" & times$IO==80]
)
docker_1_80$IO <- "80G"
docker_1_80$dockers <- 1
docker_1_80$total <- "80G"
q <- quantile(docker_1_80$consumo, c(0.1, 0.9))
docker_80 <- docker_1_80[docker_1_80$consumo >= q[1] & docker_1_80$consumo <= q[2], ]

# Combinando testes
dt_tests <- rbind(docker_3_20, docker_2_30)
dt_tests <- rbind(dt_tests, docker_6_10)
dt_tests <- rbind(dt_tests, docker_8_10)
dt_tests <- rbind(dt_tests, docker_4_20)
dt_tests <- rbind(dt_tests, docker_2_40)
dt_tests <- rbind(dt_tests, docker_1_60)
dt_tests <- rbind(dt_tests, docker_1_80)

#rm(idle)
rm(docker_3_20)
rm(docker_2_30)
rm(docker_6_10)
rm(docker_8_10)
rm(docker_4_20)
rm(docker_2_40)
rm(docker_1_60)
rm(docker_1_80)

tiff("fio_multiplatform_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=total, y=consumo, color=as.factor(dockers)))+
    geom_boxplot(outlier.shape=NA, notch=FALSE, position = position_dodge2(preserve = "single"))+
    geom_hline( yintercept=summary(idle_docker$consumo)[[3]], color='dark green',linetype="dashed")+
    annotate(geom="text", x=2.5, y=195, label="Idle",
             color="dark green", size=6)+
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
        color= "Quantidade de contêineres"
    )+
    scale_y_continuous(limits=c(175,450), breaks=seq(175,450,25))+
    scale_x_discrete(
        limits=c(
            "60G",
            "80G"
        ))
# labels=c(
#         "Idle",
#         "1GB",
#         "5GB",
#         "10GB",
#         "20GB",
#         "30GB",
#         "40GB",
#         "50GB",
#         "60GB",
#         "70GB",
#         "80GB"
#     ))

plot(p1)
dev.off()

rm(p1)

tiff("fio_multiplatform_benchmark_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests, aes(x=total, y=consumo, color=as.factor(dockers)))+
    geom_boxplot(outlier.shape=NA, notch=FALSE, position = position_dodge2(preserve = "single"))+
    geom_hline( yintercept=summary(idle_docker$consumo)[[3]], color='dark green',linetype="dashed")+
    annotate(geom="text", x=2.5, y=195, label="Idle",
             color="dark green", size=6)+
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
        color= "#Containers"
    )+
    scale_y_continuous(limits=c(175,450), breaks=seq(175,450,25))+
    scale_x_discrete(
        limits=c(
            "60G",
            "80G"
        ))
# labels=c(
#         "Idle",
#         "1GB",
#         "5GB",
#         "10GB",
#         "20GB",
#         "30GB",
#         "40GB",
#         "50GB",
#         "60GB",
#         "70GB",
#         "80GB"
#     ))

plot(p2)
dev.off()

rm(p2)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
