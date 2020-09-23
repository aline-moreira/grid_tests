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

somaGrid_dolar_0 <- mean(dt_tests$consumo[dt_tests$cpus==0]) * 0.000000114/5.44 * 3600
somaGrid_dolar_1 <- mean(dt_tests$consumo[dt_tests$cpus==1]) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0
somaGrid_dolar_2 <- mean(dt_tests$consumo[dt_tests$cpus==2]) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0
somaGrid_dolar_3 <- 400 * 0.000000114/5.44 * 3600 - somaGrid_dolar_0
somaGrid_dolar_4 <- mean(dt_tests$consumo[dt_tests$cpus==4]) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0

somas <- c(somaGrid_1, somaGrid_2, somaGrid_3, somaGrid_4, 
           0.1321,0.1321,0.1321,0.1321, 
           0.0881,0.0881,0.0881,0.0881, 
           0.04403, 0.04403, 0.04403, 0.04403)

somas_dolar <- c(somaGrid_dolar_1, somaGrid_dolar_2, somaGrid_dolar_3, somaGrid_dolar_4, 
            0.1321/5.44,0.1321/5.44,0.1321/5.44,0.1321/5.44, 
            0.0881/5.44,0.0881/5.44,0.0881/5.44,0.0881/5.44, 
            0.04403/5.44, 0.04403/5.44, 0.04403/5.44, 0.04403/5.44)

somas <- data.frame(somas, c("grid","grid","grid","grid",
                             "aws15","aws15","aws15","aws15",
                             "aws10","aws10","aws10","aws10",
                             "aws5","aws5","aws5","aws5"), 
                    c(1,2,3,4,
                      1,2,3,4,
                      1,2,3,4,
                      1,2,3,4))

somas_dolar <- data.frame(somas_dolar, c("grid","grid","grid","grid",
                             "aws15","aws15","aws15","aws15",
                             "aws10","aws10","aws10","aws10",
                             "aws5","aws5","aws5","aws5"), 
                    c(1,2,3,4,
                      1,2,3,4,
                      1,2,3,4,
                      1,2,3,4))
 names(somas) <- c("Preço","ModeloCusto","cpus")
 names(somas_dolar) <- c("Preço","ModeloCusto","cpus")

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
    scale_y_continuous(limits=c(0.04,0.135), breaks=seq(0.04,0.14,0.005))+
    scale_x_continuous(
        labels=c(
            "25%",
            "50%",
            "75%",
            "100%"
        ))+
    scale_color_discrete(
        limits=c("aws5","aws10","aws15","grid"),
        labels=c("Amazon 5%","Amazon 10%","Amazon 15%", "Modelo Proposto")
    )

plot(p1)
dev.off()

rm(p1)

tiff("precoStress_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=somas_dolar, aes(x=cpus, y=Preço, color = ModeloCusto ))+
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
        y="Price (US$/h)",
        color= "Model Price"
    )+
    scale_y_continuous(limits=c(0.007,0.025), breaks=seq(0.007,0.025,0.001))+
    scale_x_continuous(
        labels=c(
            "25%",
            "50%",
            "75%",
            "100%"
        ))+
    scale_color_discrete(
        limits=c("aws5","aws10","aws15","grid"),
        labels=c("5% Amazon","10% Amazon","15% Amazon", "Proposed model")
    )

plot(p2)
dev.off()

rm(p2)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
