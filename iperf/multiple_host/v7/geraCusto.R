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

times<- read.table("iperf.times",header=FALSE,sep=";")
names(times) <- c("Configuração de Rede","Tipo de teste","Repetições","Quantidade Clientes","Largura de Banda","start","end")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1927167.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values
host <- "Servidor"

energy1 <- data.frame(time,values, host)

#energy$items[[number_of_host]]
energy$items[[2]]$timestamps = as.POSIXct(energy$items[[2]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[2]]$timestamps
values <- energy$items[[2]]$values
host <- "Cliente"

energy2 <- data.frame(time,values, host)

energy <- rbind(energy1,energy2)

rm(time)
rm(values)
rm(host)
rm(energy1)
rm(energy2)

names(energy) <- c("tempo","consumo","node")
###################################################
# Filtrando a energia de acordo com os tempos

#2.5GB
docker2_5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='100M']
)
docker2_5$`Largura de Banda` <- '100Mb'
docker2_5$`Quantidade Clientes` <- 25
q <- quantile(docker2_5$consumo, c(0.1, 0.9))
docker2_5 <- docker2_5[docker2_5$consumo >= q[1] & docker2_5$consumo <= q[2], ]

#5GB
docker5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='100M']
)
docker5$`Largura de Banda` <- '100Mb'
docker5$`Quantidade Clientes` <- 50
q <- quantile(docker5$consumo, c(0.1, 0.9))
docker5 <- docker5[docker5$consumo >= q[1] & docker5$consumo <= q[2], ]

#10GB
docker10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='100M']
)
docker10$`Largura de Banda` <- '100Mb'
docker10$`Quantidade Clientes` <- 100
q <- quantile(docker10$consumo, c(0.1, 0.9))
docker10 <- docker10[docker10$consumo >= q[1] & docker10$consumo <= q[2], ]

# Combinando testes
dt_tests <- rbind(docker2_5, docker5)
dt_tests <- rbind(dt_tests, docker10)

somaGrid_0 <- 183  * 0.000000114 * 3600 #+ 0.054 * 0
somaGrid_1 <- mean(docker2_5$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.054 * 2.5
somaGrid_2 <- mean(docker5$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.054 * 5
somaGrid_3 <- 370 * 0.000000114 * 3600 - somaGrid_0 #+ 0.054 * 7.5
somaGrid_4 <- mean(docker10$consumo) * 0.000000114 * 3600 - somaGrid_0 #+ 0.054 * 10

somaGrid_dolar_0 <- 183  * 0.000000114 * 3600 /5.44 #+ 0.054 * 0 /5.44
somaGrid_dolar_1 <- mean(docker2_5$consumo) * 0.000000114 / 5.44 *3600 - somaGrid_dolar_0 #+ 0.054 / 5.44 * 2.5
somaGrid_dolar_2 <- mean(docker5$consumo) * 0.000000114/ 5.44 * 3600 - somaGrid_dolar_0 #+ 0.054/ 5.44 * 5
somaGrid_dolar_3 <- 370 * 0.000000114/ 5.44 * 3600 - somaGrid_dolar_0 #+ 0.054/ 5.44 * 7.5
somaGrid_dolar_4 <- mean(docker10$consumo) * 0.000000114/ 5.44 * 3600 - somaGrid_dolar_0 #+ 0.054/ 5.44 * 10


somas <- c(somaGrid_1, somaGrid_2, somaGrid_3, somaGrid_4,
           0.054 * 2.5,0.054 * 5,0.054 * 7.5,0.054 * 10,
           0.036 * 2.5,0.036 * 5,0.036 * 7.5,0.036 * 10,
           0.018 * 2.5,0.018 * 5,0.018 * 7.5,0.018 * 10
)

somas_dolar <- c(somaGrid_dolar_1, somaGrid_dolar_2, somaGrid_dolar_3, somaGrid_dolar_4,
           0.054 * 2.5 / 5.44,0.054 * 5 / 5.44,0.054 * 7.5 / 5.44,0.054 * 10 / 5.44,
           0.036 * 2.5 / 5.44,0.036 * 5 / 5.44,0.036 * 7.5 / 5.44,0.036 * 10 / 5.44,
           0.018 * 2.5 / 5.44,0.018 * 5 / 5.44,0.018 * 7.5 / 5.44,0.018 * 10 / 5.44
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

tiff("precoIperf.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
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
        x="Rede (Gb)",
        y="Preço (R$/h)",
        color= "Modelo de Custo"
    )+
    scale_y_continuous(limits=c(0.0,0.55), breaks=seq(0.00,0.65,0.05))+
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


tiff("precoIperf_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
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
        x="Network (Gb)",
        y="Price (US$/h)",
        color= "Price Model"
    )+
    scale_y_continuous(limits=c(0.0,0.10), breaks=seq(0.0,0.12,0.01))+
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
system("for f in *.png; do convert $f ${f%.*}.pdf; done;")
system("rm *.tiff")
