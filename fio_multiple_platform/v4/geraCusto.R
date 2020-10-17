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

#20G
docker_20 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==20] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==20]
)
docker_20$IO <- "20G"
docker_20$plataforma <- "Bare Metal"
q <- quantile(docker_20$consumo, c(0.1, 0.9))
docker_20 <- docker_20[docker_20$consumo >= q[1] & docker_20$consumo <= q[2], ]

docker_40 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==40] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==40]
)
docker_40$IO <- "40G"
docker_40$plataforma <- "Bare Metal"
q <- quantile(docker_40$consumo, c(0.1, 0.9))
docker_40 <- docker_40[docker_40$consumo >= q[1] & docker_40$consumo <= q[2], ]

#60G
docker_60 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==60] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==60]
)
docker_60$IO <- "60G"
docker_60$plataforma <- "Bare Metal"
q <- quantile(docker_60$consumo, c(0.1, 0.9))
docker_60 <- docker_60[docker_60$consumo >= q[1] & docker_60$consumo <= q[2], ]

#80G
docker_80 <- energy %>% filter(
    energy$tempo >= times$start[times$plataforma=="host" & times$IO==80] &
        energy$tempo <= times$end[times$plataforma=="host" & times$IO==80]
)
docker_80$IO <- "80G"
docker_80$plataforma <- "Bare Metal"
q <- quantile(docker_80$consumo, c(0.1, 0.9))
docker_80 <- docker_80[docker_80$consumo >= q[1] & docker_80$consumo <= q[2], ]

# Combinando testes
dt_tests <- rbind(docker_20, docker_40)
dt_tests <- rbind(dt_tests, docker_60)
dt_tests <- rbind(dt_tests, docker_80)

somaGrid_0 <- 183  * 0.000000114 * 3600 + 0.000113333 * 0
somaGrid_1 <- mean(docker_20$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.000113333 * 20
somaGrid_2 <- mean(docker_40$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.000113333 * 40
somaGrid_3 <- mean(docker_60$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.000113333 * 60
somaGrid_4 <- mean(docker_80$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.000113333 * 80


somaGrid_dolar_0 <- 183  * 0.000000114/5.44 * 3600 #+ 0.000113333 * 0 /5.44
somaGrid_dolar_1 <- mean(docker_20$consumo) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0 #+ 0.000113333 * 20 /5.44
somaGrid_dolar_2 <- mean(docker_40$consumo) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0 #+ 0.000113333 * 40 /5.44
somaGrid_dolar_3 <- mean(docker_60$consumo) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0 #+ 0.000113333 * 60 /5.44
somaGrid_dolar_4 <- mean(docker_80$consumo) * 0.000000114/5.44 * 3600 - somaGrid_dolar_0 #+ 0.000113333 * 80 /5.44

somas <- c(somaGrid_1, somaGrid_2, somaGrid_3, somaGrid_4, 
           0.000755556 * 80,0.000755556 * 80,0.000755556 * 80,0.000755556 * 80, 
           0.000503704 * 80,0.000503704 * 80,0.000503704 * 80,0.000503704 * 80, 
           0.000251852 * 80,0.000251852 * 80,0.000251852 * 80,0.000251852 * 80
)

somas_dolar <- c(somaGrid_dolar_1, somaGrid_dolar_2, somaGrid_dolar_3, somaGrid_dolar_4, 
                 0.000755556 * 80 /5.44,0.000755556 * 80 /5.44,0.000755556 * 80 /5.44,0.000755556 * 80 /5.44, 
                 0.000503704 * 80 /5.44,0.000503704 * 80 /5.44,0.000503704 * 80 /5.44,0.000503704 * 80 /5.44, 
                 0.000251852 * 80 /5.44,0.000251852 * 80 /5.44,0.000251852 * 80 /5.44,0.000251852 * 80 /5.44
)

somas <- data.frame(somas, c("grid","grid","grid","grid",
                             "aws15","aws15","aws15","aws15",
                             "aws10","aws10","aws10","aws10",
                             "aws5","aws5","aws5","aws5"
                            ), c(8,16,24,32,
                                 8,16,24,32,
                                 8,16,24,32,
                                 8,16,24,32
                            ))
names(somas) <- c("Preço","ModeloCusto","ram")

somas_dolar <- data.frame(somas_dolar, c("grid","grid","grid","grid",
                                         "aws15","aws15","aws15","aws15",
                                         "aws10","aws10","aws10","aws10",
                                         "aws5","aws5","aws5","aws5"
                            ), c(8,16,24,32,
                                 8,16,24,32,
                                 8,16,24,32,
                                 8,16,24,32
                            ))
names(somas_dolar) <- c("Preço","ModeloCusto","ram")

tiff("precoFio.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
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
        x="Armazenamento (Gb)",
        y="Preço (R$/h)",
        color= "Modelo de Custo"
    )+
    scale_y_continuous(limits=c(0,0.09), breaks=seq(0.0,0.09,0.01))+
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
        labels=c("Amazon 5%","Amazon 10%","Amazon 15%", "Modelo Proposto")
    )

plot(p1)
dev.off()
rm(p1)


tiff("precoFio_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
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
        x="Storage (Gb)",
        y="Price (US$/h)",
        color= "Price Model"
    )+
    scale_y_continuous(limits=c(0,0.016), breaks=seq(0.0,0.016,0.001))+
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
