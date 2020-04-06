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
names(times) <- c("Configuração de Rede","Largura de Banda","start","end")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%a %d %b %Y %H:%M:%S")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%a %d %b %Y %H:%M:%S")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1924493.json")

energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values

energy <- data.frame(time,values)

names(energy) <- c("tempo","consumo")

###################################################
# Filtrando a energia de acordo com os tempos
###################################################

bridge0 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="bridge" & times$`Largura de Banda`=='0M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="bridge"& times$`Largura de Banda`=='0M']
)
bridge0$`Largura de Banda` <- 'idle'
bridge0$`Configuração de Rede` <- 'bridge'
q <- quantile(bridge0$consumo, c(0.1, 0.9))
bridge0 <- bridge0[bridge0$consumo >= q[1] & bridge0$consumo <= q[2], ]

bridge1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="bridge" & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="bridge"& times$`Largura de Banda`=='1M']
)
bridge1$`Largura de Banda` <- '1Mb'
bridge1$`Configuração de Rede` <- 'bridge'
q <- quantile(bridge1$consumo, c(0.1, 0.9))
bridge1 <- bridge1[bridge1$consumo >= q[1] & bridge1$consumo <= q[2], ]

bridge10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="bridge" & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="bridge"& times$`Largura de Banda`=='10M']
)
bridge10$`Largura de Banda` <- '10Mb'
bridge10$`Configuração de Rede` <- 'bridge'
q <- quantile(bridge10$consumo, c(0.1, 0.9))
bridge10 <- bridge10[bridge10$consumo >= q[1] & bridge10$consumo <= q[2], ]

bridge100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="bridge" & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="bridge"& times$`Largura de Banda`=='100M']
)
bridge100$`Largura de Banda` <- '100Mb'
bridge100$`Configuração de Rede` <- 'bridge'
q <- quantile(bridge100$consumo, c(0.1, 0.9))
bridge100 <- bridge100[bridge100$consumo >= q[1] & bridge100$consumo <= q[2], ]

bridge1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="bridge" & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="bridge"& times$`Largura de Banda`=='1000M']
)
bridge1000$`Largura de Banda` <- '1000Mb'
bridge1000$`Configuração de Rede` <- 'bridge'
q <- quantile(bridge1000$consumo, c(0.1, 0.9))
bridge1000 <- bridge1000[bridge1000$consumo >= q[1] & bridge1000$consumo <= q[2], ]

########################

host0 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="host" & times$`Largura de Banda`=='0M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="host"& times$`Largura de Banda`=='0M']
)
host0$`Largura de Banda` <- 'idle'
host0$`Configuração de Rede` <- 'host'
q <- quantile(host0$consumo, c(0.1, 0.9))
host0 <- host0[host0$consumo >= q[1] & host0$consumo <= q[2], ]

host1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="host" & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="host"& times$`Largura de Banda`=='1M']
)
host1$`Largura de Banda` <- '1Mb'
host1$`Configuração de Rede` <- 'host'
q <- quantile(host1$consumo, c(0.1, 0.9))
host1 <- host1[host1$consumo >= q[1] & host1$consumo <= q[2], ]

host10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="host" & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="host"& times$`Largura de Banda`=='10M']
)
host10$`Largura de Banda` <- '10Mb'
host10$`Configuração de Rede` <- 'host'
q <- quantile(host10$consumo, c(0.1, 0.9))
host10 <- host10[host10$consumo >= q[1] & host10$consumo <= q[2], ]

host100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="host" & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="host"& times$`Largura de Banda`=='100M']
)
host100$`Largura de Banda` <- '100Mb'
host100$`Configuração de Rede` <- 'host'
q <- quantile(host100$consumo, c(0.1, 0.9))
host100 <- host100[host100$consumo >= q[1] & host100$consumo <= q[2], ]

host1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Configuração de Rede`=="host" & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Configuração de Rede`=="host"& times$`Largura de Banda`=='1000M']
)
host1000$`Largura de Banda` <- '1000Mb'
host1000$`Configuração de Rede` <- 'host'
q <- quantile(host1000$consumo, c(0.1, 0.9))
host1000 <- host1000[host1000$consumo >= q[1] & host1000$consumo <= q[2], ]

dt_tests <- rbind(bridge0, bridge1)
dt_tests <- rbind(dt_tests,bridge10)
dt_tests <- rbind(dt_tests,bridge100)
dt_tests <- rbind(dt_tests,bridge1000)
dt_tests <- rbind(dt_tests,host0)
dt_tests <- rbind(dt_tests,host1)
dt_tests <- rbind(dt_tests,host10)
dt_tests <- rbind(dt_tests,host100)
dt_tests <- rbind(dt_tests,host1000)

rm(bridge0)
rm(host0)
rm(bridge1)
rm(host1)
rm(bridge10)
rm(host10)
rm(bridge100)
rm(host100)
rm(bridge1000)
rm(host1000)

tiff("iperf_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=`Largura de Banda`, y=consumo, color=`Configuração de Rede`))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 0,
            hjust = 0.7,
            size=12
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
        x="Largura de Banda",
        y="Consumo (W/s)",
        color= "Configuração de Rede do Contêiner"
    )+
    scale_y_continuous(limits=c(175,195), breaks=seq(175,200,2))+
    scale_x_discrete(
        limits=c(
            "idle",
            "1Mb",
            "10Mb",
            "100Mb",
            "1000Mb"
        ))

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
