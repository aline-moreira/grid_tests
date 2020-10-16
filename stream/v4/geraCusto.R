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

times<- read.table("./benchmarks.time",header=FALSE,sep=";")
names(times) <- c("teste","cpu","ram","inicio","fim")
times$inicio <- as.POSIXct(times$inicio,tz="UTC",  format="%c")
times$fim <- as.POSIXct(times$fim,tz="UTC",  format="%c")

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1922862.json")

#energy$items[[number_of_host]]
energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")
time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values
energy1 <- data.frame(time,values)

energy2 <- fromJSON(file="energy_1922343.json")
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

#idle
idle_docker <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STRESS" & times$cpu==4 & times$ram==32] &
        energy$tempo <= times$fim[times$teste=="STRESS" & times$cpu==4 & times$ram==32]
)
idle_docker$teste <- "stress"
idle_docker$cpu <- 4
idle_docker$ram <- NA
q <- quantile(idle_docker$consumo, c(0.05, 0.95))
idle_docker <- idle_docker[idle_docker$consumo >= q[1] & idle_docker$consumo <= q[2], ]

#4 4
docker_8 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==4 & times$ram==4] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==4 & times$ram==4]
)
docker_8$teste <- "stream"
docker_8$cpu <- 4
docker_8$ram <- 8
q <- quantile(docker_8$consumo, c(0.05, 0.95))
docker_8 <- docker_8[docker_8$consumo >= q[1] & docker_8$consumo <= q[2], ]

#4 16
docker_16 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==4 & times$ram==16] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==4 & times$ram==16]
)
docker_16$teste <- "stream"
docker_16$cpu <- 4
docker_16$ram <- 16
q <- quantile(docker_16$consumo, c(0.05, 0.95))
docker_16 <- docker_16[docker_16$consumo >= q[1] & docker_16$consumo <= q[2], ]

#4 32
docker_32 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==4 & times$ram==32] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==4 & times$ram==32]
)
docker_32$teste <- "stream"
docker_32$cpu <- 4
docker_32$ram <- 32
q <- quantile(docker_32$consumo, c(0.05, 0.95))
docker_32 <- docker_32[docker_32$consumo >= q[1] & docker_32$consumo <= q[2], ]


# Combinando testes
dt_tests <- rbind(idle_docker, docker_8)
dt_tests <- rbind(dt_tests, docker_16)
dt_tests <- rbind(dt_tests, docker_32)

# #rm(idle)
# rm(idle_docker)
# rm(docker_8)
# rm(docker_16)
# rm(docker_32)

somaGrid_0 <- mean(idle_docker$consumo) * 0.000000114 * 3600
somaGrid_1 <- mean(docker_8$consumo) * 0.000000114 * 3600 - somaGrid_0
somaGrid_2 <- mean(docker_16$consumo) * 0.000000114 * 3600 - somaGrid_0
somaGrid_3 <- 464 * 0.000000114 * 3600 - somaGrid_0
somaGrid_4 <- mean(docker_32$consumo) * 0.000000114 * 3600 - somaGrid_0

somaGrid_dolar_0 <- mean(idle_docker$consumo) * 0.000000114 / 5.44 * 3600
somaGrid_dolar_1 <- mean(docker_8$consumo) * 0.000000114 / 5.44  * 3600 - somaGrid_dolar_0
somaGrid_dolar_2 <- mean(docker_16$consumo) * 0.000000114 / 5.44  * 3600 - somaGrid_dolar_0
somaGrid_dolar_3 <- 464 * 0.000000114 / 5.44  * 3600 - somaGrid_dolar_0
somaGrid_dolar_4 <- mean(docker_32$consumo) * 0.000000114 / 5.44  * 3600 - somaGrid_dolar_0

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
                    c(8,16,24,32,
                      8,16,24,32,
                      8,16,24,32,
                      8,16,24,32))

somas_dolar <- data.frame(somas_dolar, c("grid","grid","grid","grid",
                             "aws15","aws15","aws15","aws15",
                             "aws10","aws10","aws10","aws10",
                             "aws5","aws5","aws5","aws5"), 
                    c(8,16,24,32,
                      8,16,24,32,
                      8,16,24,32,
                      8,16,24,32))
names(somas) <- c("Preço","ModeloCusto","ram")
names(somas_dolar) <- c("Preço","ModeloCusto","ram")

tiff("precoStream.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=somas, aes(x=ram, y=Preço, color = ModeloCusto ))+
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
        x="Memória (Gb)",
        y="Preço (R$/h)",
        color= "Modelo de Custo"
    )+
     scale_y_continuous(limits=c(0,0.14), breaks=seq(0.0,0.14,0.01))+
    scale_x_continuous(
        breaks=c(
            8,
            16,
            24,
            32
        ),
        labels=c(
            "25%",
            "50%",
            "75%",
            "100%"
        ))+
    scale_color_discrete(
        limits=c("aws5","aws10","aws15","grid"),
        labels=c("Amazon  5%","Amazon  10%","Amazon  15%", "Modelo Proposto")
    )

plot(p1)
dev.off()
rm(p1)


tiff("precoStream_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=somas_dolar, aes(x=ram, y=Preço, color = ModeloCusto ))+
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
        x="Memory (Gb)",
        y="Price (US$/h)",
        color= "Price Model"
    )+
    scale_y_continuous(limits=c(0.0,0.026), breaks=seq(0.0,0.026,0.002))+
    scale_x_continuous(
        breaks=c(
            8,
            16,
            24,
            32
        ),
        labels=c(
            "25%",
            "50%",
            "75%",
            "100%"
        ))+
    scale_color_discrete(
        limits=c("aws5","aws10","aws15","grid"),
        labels=c("5% Amazon","10% Amazon","15% Amazon", "Proposed Model")
    )

plot(p2)
dev.off()
rm(p2)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
