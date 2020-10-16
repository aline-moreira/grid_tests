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
energy1 <- fromJSON(file="energy_1941835.json")

#energy$items[[number_of_host]]
energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values
energy1 <- data.frame(time,values)

energy2 <- fromJSON(file="energy_1941834.json")
#energy$items[[number_of_host]]
energy2$items[[1]]$timestamps = as.POSIXct(energy2$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy2$items[[1]]$timestamps
values <- energy2$items[[1]]$values
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
#STREAM HOST IDLE
docker_idle <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==0] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==0]
)
docker_idle$teste <- "stream"
docker_idle$plataforma <- "docker"
docker_idle$ram <- 0
docker_idle$consumo <- docker_idle$consumo - 60
q <- quantile(docker_idle$consumo, c(0.1, 0.8))
docker_idle <- docker_idle[docker_idle$consumo >= q[1] & docker_idle$consumo <= q[2], ]

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

#STREAM 150GB
docker_150 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==150] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==150]
)
docker_150$teste <- "stream"
docker_150$plataforma <- "docker"
docker_150$ram <- 150
# q <- quantile(docker_250$consumo, c(0.05, 0.95))
# docker_250 <- docker_250[docker_250$consumo >= q[1] & docker_250$consumo <= q[2], ]

#STREAM 200GB
docker_200 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==200] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==200]
)
docker_200$teste <- "stream"
docker_200$plataforma <- "docker"
docker_200$ram <- 200
# q <- quantile(docker_500$consumo, c(0.05, 0.95))
# docker_500 <- docker_500[docker_500$consumo >= q[1] & docker_500$consumo <= q[2], ]

#STREAM 250GB
docker_250 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==250] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$plataforma=="docker" & times$ram==250]
)
docker_250$teste <- "stream"
docker_250$plataforma <- "docker"
docker_250$ram <- 250
# q <- quantile(docker_730$consumo, c(0.05, 0.95))
# docker_730 <- docker_730[docker_730$consumo >= q[1] & docker_730$consumo <= q[2], ]


# Combinando testes
dt_tests <- rbind(idle_docker, docker_1)
dt_tests <- rbind(dt_tests, docker_2)
dt_tests <- rbind(dt_tests, docker_4)

#rm(idle)
rm(idle_docker)
rm(docker_1)
rm(docker_2)
rm(docker_4)

somaGrid_0 <- mean(dt_tests$consumo[dt_tests$cpus==0]) * 0.000000114 * 3600
somaGrid_1 <- mean(dt_tests$consumo[dt_tests$cpus==1]) * 0.000000114 * 3600 - somaGrid_0
somaGrid_2 <- mean(dt_tests$consumo[dt_tests$cpus==2]) * 0.000000114 * 3600 - somaGrid_0
somaGrid_3 <- 400 * 0.000000114 * 3600 - somaGrid_0
somaGrid_4 <- mean(dt_tests$consumo[dt_tests$cpus==4]) * 0.000000114 * 3600 - somaGrid_0

somas <- c(somaGrid_1, somaGrid_2, somaGrid_3, somaGrid_4, 0.1321,0.1321,0.1321,0.1321)

somas <- data.frame(somas, c("grid","grid","grid","grid","aws","aws","aws","aws"), c(1,2,3,4,1,2,3,4))
names(somas) <- c("Preço","ModeloCusto","cpus")

tiff("precoStress.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=somas, aes(x=cpus, y=Preço, color = ModeloCusto ))+
    geom_point()+
    geom_line()+
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
        y="Preço (R$/h)",
        color= "Modelo de Custo"
    )+
    scale_y_continuous(limits=c(0.07,0.14), breaks=seq(0.07,0.14,0.005))+
    scale_x_continuous(
        labels=c(
            "25%",
            "50%",
            "75%",
            "100%"
        ))+
    scale_color_discrete(
        limits=c("aws","grid"),
        labels=c("Amazon Fargate", "Modelo Proposto")
    )

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
