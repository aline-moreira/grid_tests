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
###################################################
idle <- energy %>% filter(
    energy$tempo >= times$start[times$`Largura de Banda`=='0'] &
        energy$tempo <= times$end[times$`Largura de Banda`=='0']
)
idle$`Largura de Banda` <- 'idle'
idle$`Quantidade Clientes` <- "Idle"
q <- quantile(idle$consumo, c(0.1, 0.9))
idle <- idle[idle$consumo >= q[1] & idle$consumo <= q[2], ]

host1_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='1M']
)
host1_1$`Largura de Banda` <- '1Mb'
host1_1$`Quantidade Clientes` <- 1
q <- quantile(host1_1$consumo, c(0.1, 0.9))
host1_1 <- host1_1[host1_1$consumo >= q[1] & host1_1$consumo <= q[2], ]

host1_2.5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='2.5M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='2.5M']
)
host1_2.5$`Largura de Banda` <- '2.5Mb'
host1_2.5$`Quantidade Clientes` <- 1
q <- quantile(host1_2.5$consumo, c(0.1, 0.9))
host1_2.5 <- host1_2.5[host1_2.5$consumo >= q[1] & host1_2.5$consumo <= q[2], ]


host1_10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='10M']
)
host1_10$`Largura de Banda` <- '10Mb'
host1_10$`Quantidade Clientes` <- 1
q <- quantile(host1_10$consumo, c(0.1, 0.9))
host1_10 <- host1_10[host1_10$consumo >= q[1] & host1_10$consumo <= q[2], ]


host1_100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='100M']
)
host1_100$`Largura de Banda` <- '100Mb'
host1_100$`Quantidade Clientes` <- 1
q <- quantile(host1_100$consumo, c(0.1, 0.9))
host1_100 <- host1_100[host1_100$consumo >= q[1] & host1_100$consumo <= q[2], ]


host1_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==1 & times$`Largura de Banda`=='1000M']
)
host1_1000$`Largura de Banda` <- '1000Mb'
host1_1000$`Quantidade Clientes` <- 1
q <- quantile(host1_1000$consumo, c(0.1, 0.9))
host1_1000 <- host1_1000[host1_1000$consumo >= q[1] & host1_1000$consumo <= q[2], ]

#####
host5_1 <- energy %>% filter(
energy$tempo >= times$start[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='1M'] &
    energy$tempo <= times$end[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='1M']
)
host5_1$`Largura de Banda` <- '1Mb'
host5_1$`Quantidade Clientes` <- 5
q <- quantile(host5_1$consumo, c(0.1, 0.9))
host5_1 <- host5_1[host5_1$consumo >= q[1] & host5_1$consumo <= q[2], ]

host5_2.5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='2.5M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='2.5M']
)
host5_2.5$`Largura de Banda` <- '2.5Mb'
host5_2.5$`Quantidade Clientes` <- 5
q <- quantile(host5_2.5$consumo, c(0.1, 0.9))
host5_2.5 <- host5_2.5[host5_2.5$consumo >= q[1] & host5_2.5$consumo <= q[2], ]

host5_10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='10M']
)
host5_10$`Largura de Banda` <- '10Mb'
host5_10$`Quantidade Clientes` <- 5
q <- quantile(host5_10$consumo, c(0.1, 0.9))
host5_10 <- host5_10[host5_10$consumo >= q[1] & host5_10$consumo <= q[2], ]


host5_100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='100M']
)
host5_100$`Largura de Banda` <- '100Mb'
host5_100$`Quantidade Clientes` <- 5
q <- quantile(host5_100$consumo, c(0.1, 0.9))
host5_100 <- host5_100[host5_100$consumo >= q[1] & host5_100$consumo <= q[2], ]


host5_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==5 & times$`Largura de Banda`=='1000M']
)
host5_1000$`Largura de Banda` <- '1000Mb'
host5_1000$`Quantidade Clientes` <- 5
q <- quantile(host5_1000$consumo, c(0.1, 0.9))
host5_1000 <- host5_1000[host5_1000$consumo >= q[1] & host5_1000$consumo <= q[2], ]

#####
host10_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='1M']
)
host10_1$`Largura de Banda` <- '1Mb'
host10_1$`Quantidade Clientes` <- 10
q <- quantile(host1_10$consumo, c(0.1, 0.9))
host10_1 <- host10_1[host10_1$consumo >= q[1] & host10_1$consumo <= q[2], ]

host10_2.5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='2.5M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='2.5M']
)
host10_2.5$`Largura de Banda` <- '2.5Mb'
host10_2.5$`Quantidade Clientes` <- 10
q <- quantile(host10_2.5$consumo, c(0.1, 0.9))
host10_2.5 <- host10_2.5[host10_2.5$consumo >= q[1] & host10_2.5$consumo <= q[2], ]

host10_10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='10M']
)
host10_10$`Largura de Banda` <- '10Mb'
host10_10$`Quantidade Clientes` <- 10
q <- quantile(host10_10$consumo, c(0.1, 0.9))
host10_10 <- host10_10[host10_10$consumo >= q[1] & host10_10$consumo <= q[2], ]


host10_100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='100M']
)
host10_100$`Largura de Banda` <- '100Mb'
host10_100$`Quantidade Clientes` <- 10
q <- quantile(host10_100$consumo, c(0.1, 0.9))
host10_100 <- host10_100[host10_100$consumo >= q[1] & host10_100$consumo <= q[2], ]

host10_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='1000M']
)
host10_1000$`Largura de Banda` <- '1000Mb'
host10_1000$`Quantidade Clientes` <- 10
q <- quantile(host10_1000$consumo, c(0.1, 0.9))
host10_1000 <- host10_1000[host10_1000$consumo >= q[1] & host10_1000$consumo <= q[2], ]

#####
#####
host25_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='1M']
)
host25_1$`Largura de Banda` <- '1Mb'
host25_1$`Quantidade Clientes` <- 25
q <- quantile(host25_1$consumo, c(0.1, 0.9))
host25_1 <- host25_1[host25_1$consumo >= q[1] & host25_1$consumo <= q[2], ]

host25_2.5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='2.5M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='2.5M']
)
host25_2.5$`Largura de Banda` <- '2.5Mb'
host25_2.5$`Quantidade Clientes` <- 25
q <- quantile(host25_2.5$consumo, c(0.1, 0.9))
host25_2.5 <- host25_2.5[host25_2.5$consumo >= q[1] & host25_2.5$consumo <= q[2], ]

host25_10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='10M']
)
host25_10$`Largura de Banda` <- '10Mb'
host25_10$`Quantidade Clientes` <- 25
q <- quantile(host25_10$consumo, c(0.1, 0.9))
host25_10 <- host25_10[host25_10$consumo >= q[1] & host25_10$consumo <= q[2], ]


host25_100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='100M']
)
host25_100$`Largura de Banda` <- '100Mb'
host25_100$`Quantidade Clientes` <- 25
q <- quantile(host25_100$consumo, c(0.1, 0.9))
host25_100 <- host25_100[host25_100$consumo >= q[1] & host25_100$consumo <= q[2], ]


host25_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==25 & times$`Largura de Banda`=='1000M']
)
host25_1000$`Largura de Banda` <- '1000Mb'
host25_1000$`Quantidade Clientes` <- 25
q <- quantile(host25_1000$consumo, c(0.1, 0.9))
host25_1000 <- host25_1000[host25_1000$consumo >= q[1] & host25_1000$consumo <= q[2], ]
######

host50_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='1M']
)
host50_1$`Largura de Banda` <- '1Mb'
host50_1$`Quantidade Clientes` <- 50
q <- quantile(host50_1$consumo, c(0.1, 0.9))
host50_1 <- host50_1[host50_1$consumo >= q[1] & host50_1$consumo <= q[2], ]

host50_2.5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='2.5M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='2.5M']
)
host50_2.5$`Largura de Banda` <- '2.5Mb'
host50_2.5$`Quantidade Clientes` <- 50
q <- quantile(host50_2.5$consumo, c(0.1, 0.9))
host50_2.5 <- host50_2.5[host50_2.5$consumo >= q[1] & host50_2.5$consumo <= q[2], ]

host50_10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='10M']
)
host50_10$`Largura de Banda` <- '10Mb'
host50_10$`Quantidade Clientes` <- 50
q <- quantile(host50_10$consumo, c(0.1, 0.9))
host50_10 <- host50_10[host50_10$consumo >= q[1] & host50_10$consumo <= q[2], ]

host50_100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='100M']
)
host50_100$`Largura de Banda` <- '100Mb'
host50_100$`Quantidade Clientes` <- 50
q <- quantile(host50_100$consumo, c(0.1, 0.9))
host50_100 <- host50_100[host50_100$consumo >= q[1] & host50_100$consumo <= q[2], ]

host50_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='1000M']
)
host50_1000$`Largura de Banda` <- '1000Mb'
host50_1000$`Quantidade Clientes` <- 50
q <- quantile(host50_1000$consumo, c(0.1, 0.9))
host50_1000 <- host50_1000[host50_1000$consumo >= q[1] & host50_1000$consumo <= q[2], ]

#####
host100_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='1M']
)
host100_1$`Largura de Banda` <- '1Mb'
host100_1$`Quantidade Clientes` <- 100
q <- quantile(host100_1$consumo, c(0.1, 0.9))
host100_1 <- host100_1[host100_1$consumo >= q[1] & host100_1$consumo <= q[2], ]

host100_2.5 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='2.5M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='2.5M']
)
host100_2.5$`Largura de Banda` <- '2.5Mb'
host100_2.5$`Quantidade Clientes` <- 100
q <- quantile(host100_2.5$consumo, c(0.1, 0.9))
host100_2.5 <- host100_2.5[host100_2.5$consumo >= q[1] & host100_2.5$consumo <= q[2], ]

host100_10 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='10M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='10M']
)
host100_10$`Largura de Banda` <- '10Mb'
host100_10$`Quantidade Clientes` <- 100
q <- quantile(host100_10$consumo, c(0.1, 0.9))
host100_10 <- host100_10[host100_10$consumo >= q[1] & host100_10$consumo <= q[2], ]

host100_100 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='100M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='100M']
)
host100_100$`Largura de Banda` <- '100Mb'
host100_100$`Quantidade Clientes` <- 100
q <- quantile(host100_100$consumo, c(0.1, 0.9))
host100_100 <- host100_100[host100_100$consumo >= q[1] & host100_100$consumo <= q[2], ]

host100_1000 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='1000M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==100 & times$`Largura de Banda`=='1000M']
)
host100_1000$`Largura de Banda` <- '1000Mb'
host100_1000$`Quantidade Clientes` <- 100
q <- quantile(host100_1000$consumo, c(0.1, 0.9))
host100_1000 <- host100_1000[host100_1000$consumo >= q[1] & host100_1000$consumo <= q[2], ]

#####
dt_tests <- rbind(host1_1, host1_10)
dt_tests <- rbind(dt_tests, host1_2.5)
dt_tests <- rbind(dt_tests,host1_100)
dt_tests <- rbind(dt_tests,host1_1000)
dt_tests <- rbind(dt_tests,host5_1)
dt_tests <- rbind(dt_tests, host5_2.5)
dt_tests <- rbind(dt_tests,host5_10)
dt_tests <- rbind(dt_tests,host5_100)
dt_tests <- rbind(dt_tests,host5_1000)
dt_tests <- rbind(dt_tests,host10_1)
dt_tests <- rbind(dt_tests, host10_2.5)
dt_tests <- rbind(dt_tests,host10_10)
dt_tests <- rbind(dt_tests,host10_100)
dt_tests <- rbind(dt_tests,host10_1000)
dt_tests <- rbind(dt_tests,host25_1)
dt_tests <- rbind(dt_tests, host25_2.5)
dt_tests <- rbind(dt_tests,host25_10)
dt_tests <- rbind(dt_tests,host25_100)
dt_tests <- rbind(dt_tests,host25_1000)
dt_tests <- rbind(dt_tests,host50_1)
dt_tests <- rbind(dt_tests, host50_2.5)
dt_tests <- rbind(dt_tests,host50_10)
dt_tests <- rbind(dt_tests,host50_100)
dt_tests <- rbind(dt_tests,host50_1000)
dt_tests <- rbind(dt_tests,host100_1)
dt_tests <- rbind(dt_tests, host100_2.5)
dt_tests <- rbind(dt_tests,host100_10)
dt_tests <- rbind(dt_tests,host100_100)
dt_tests <- rbind(dt_tests,host100_1000)

rm(host1_1)
rm(host1_2.5)
rm(host1_10)
rm(host1_100)
rm(host1_1000)
rm(host5_1)
rm(host5_2.5)
rm(host5_10)
rm(host5_100)
rm(host5_1000)
rm(host25_1)
rm(host25_2.5)
rm(host25_10)
rm(host25_100)
rm(host25_1000)
rm(host10_1)
rm(host10_2.5)
rm(host10_10)
rm(host10_100)
rm(host10_1000)
rm(host50_1)
rm(host50_2.5)
rm(host50_10)
rm(host50_100)
rm(host50_1000)
rm(host100_1)
# rm(host100_2.5)
rm(host100_10)
rm(host100_100)
rm(host100_1000)

dt_tests$grp <- paste(dt_tests$`Largura de Banda`, dt_tests$`Quantidade Clientes`)
# dt_tests$grp <- as.numeric(dt_tests$`Quantidade Clientes`) * as.numeric(strsplit(dt_tests$`Largura de Banda`,'Mb'))

tiff("iperf_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=grp, y=consumo, color=as.factor(node)))+
    geom_abline( mapping=aes(slope=0, intercept=summary(idle$consumo)[[3]], 
                             colour=as.factor(idle$`Quantidade Clientes`[[1]])), linetype="dashed")+
    annotate(geom="text", x=26.5, y=195, label="Idle",
             color="dark green", size=6)+
    geom_boxplot(outlier.shape=NA)+
    # 1MB
    geom_rect(mapping=aes(xmin=0.55, xmax=1.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=0.55+(1.45-0.55)/2, y=460, label="1 MB"), size=2, color='black') +
    #2.5MB
    geom_rect(mapping=aes(xmin=1.55, xmax=2.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=1.55+(2.45-1.55)/2, y=460, label="2.5 MB"), size=2, color='black') +
    #5MB
    geom_rect(mapping=aes(xmin=2.55, xmax=3.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=2.55+(3.45-2.55)/2, y=460, label="5 MB"), size=2, color='black') +
    #10MB
    geom_rect(mapping=aes(xmin=3.55, xmax=5.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=3.55+(5.45-3.55)/2, y=460, label="10 MB"), size=2, color='black') +
    #12.5MB
    geom_rect(mapping=aes(xmin=5.55, xmax=6.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=5.55+(6.45-5.55)/2, y=460, label="12.5 MB"), size=2, color='black') +
    # #25MB
    geom_rect(mapping=aes(xmin=6.55, xmax=8.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=6.55+(8.45-6.55)/2, y=460, label="25 MB"), size=2, color='black') +
    # #50MB
    geom_rect(mapping=aes(xmin=8.55, xmax=10.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=8.55+(10.45-8.55)/2, y=460, label="50 MB"), size=2, color='black') +
    # #62.5MB
    geom_rect(mapping=aes(xmin=10.55, xmax=11.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=10.55+(11.45-10.55)/2, y=460, label="62.5 MB"), size=2, color='black') +
    #100MB
    geom_rect(mapping=aes(xmin=11.55, xmax=14.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=11.55+(14.45-11.55)/2, y=460, label="100 MB"), size=2, color='black') +
    #125MB
    geom_rect(mapping=aes(xmin=14.55, xmax=15.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=14.55+(15.45-14.55)/2, y=460, label="125 MB"), size=2, color='black') +
    #250MB
    geom_rect(mapping=aes(xmin=15.55, xmax=17.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=15.55+(17.45-15.55)/2, y=460, label="250 MB"), size=2, color='black') +
    #500MB
    geom_rect(mapping=aes(xmin=17.55, xmax=19.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=17.55+(19.45-17.55)/2, y=460, label="500 MB"), size=2, color='black') +
    #1GB
    geom_rect(mapping=aes(xmin=19.55, xmax=22.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=19.55+(22.45-19.55)/2, y=460, label="1 GB"), size=2, color='black') +
    #2.5GB
    geom_rect(mapping=aes(xmin=22.55, xmax=23.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=22.55+(23.45-22.55)/2, y=460, label="2.5 GB"), size=2, color='black') +
    #5GB
    geom_rect(mapping=aes(xmin=23.55, xmax=25.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=23.55+(25.45-23.55)/2, y=460, label="5 GB"), size=2, color='black') +
    #10GB
    geom_rect(mapping=aes(xmin=25.55, xmax=27.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0),  linetype=1, size=0.2) +
    geom_text(aes(x=25.55+(27.45-25.55)/2, y=460, label="10 GB"), size=2, color='black') +
    #25GB
    geom_rect(mapping=aes(xmin=27.55, xmax=28.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0),  linetype=1, size=0.2) +
    geom_text(aes(x=27.55+(28.45-27.55)/2, y=460, label="25 GB"), size=2, color='black') +
    #50GB
    geom_rect(mapping=aes(xmin=28.55, xmax=29.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0),  linetype=1, size=0.2) +
    geom_text(aes(x=28.55+(29.45-28.55)/2, y=460, label="50 GB"), size=2, color='black') +
    #100MB
    geom_rect(mapping=aes(xmin=29.55, xmax=30.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0), linetype=1, size=0.2) +
    geom_text(aes(x=29.55+(30.45-29.55)/2, y=460, label="100 GB"), size=2, color='black') +
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 90,
            hjust = 0.7,
            size=12,
            vjust= 0.5
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
        x="(Quantidade de contêineres/Largura de Banda)",
        y="Consumo (W/s)",
        color= "Tipo de configuração Iperf3"
    )+
    scale_color_discrete(breaks = c("Cliente","Servidor"))+
    scale_y_continuous(limits=c(175,480), breaks=seq(175,450,25))+
    scale_x_discrete(
        limits=c(
            #1MB
            "1Mb 1",
            #2.5MB
            "2.5Mb 1",
            #5MB
            "1Mb 5",
            #10MB
            "10Mb 1",
            "1Mb 10",
            #12.5MB
            "2.5Mb 5",
            #25MB
            "1Mb 25",
            "2.5Mb 10",
            #50MB
            "10Mb 5",
            "1Mb 50",
            #62.5MB
            "2.5Mb 25",
            #100MB
            "100Mb 1",
            "10Mb 10",
            "1Mb 100",
            #125MB
            "2.5Mb 50",
            #250MB
            "10Mb 25",
            "2.5Mb 100",
            #500MB
            "100Mb 5",
            "10Mb 50",
            #1GB
            "1000Mb 1",
            "100Mb 10",
            "10Mb 100",
            #2.5GB
            "100Mb 25",
            #5GB
            "1000Mb 5",
            "100Mb 50",
            #10GB
            "1000Mb 10",
            "100Mb 100",
            #25GB
            "1000Mb 25",
            #50GB
            "1000Mb 50",
            #100GB
            "1000Mb 100"
        ),
        labels=c(
            "(1/1MB)",
            "(1/2.5MB)",
            "(5/1MB)",
            "(1/10MB)",
            "(10/1MB)",
            "(5/2.5MB)",
            "(25/1MB)",
            "(10/2.5MB)",
            "(5/10MB)",
            "(50/1MB)",
            "(25/2.5MB)",
            "(1/100MB)",
            "(10/10MB)",
            "(100/1MB)",
            "(50/2.5MB)",
            "(25/10MB)",
            "(100/2.5MB)",
            "(5/100MB)",
            "(50/10MB)",
            "(1/1000MB)",
            "(10/100MB)",
            "(100/10MB)",
            "(25/100MB)",
            "(5/1000MB)",
            "(50/100MB)",
            "(10/1000MB)",
            "(100/100MB)",
            "(25/1000MB)",
            "(50/1000MB)",
            "(100/1000MB)"
        ))

plot(p1)
dev.off()

tiff("iperf_benchmark_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests, aes(x=grp, y=consumo, color=as.factor(node)))+
    geom_abline( mapping=aes(slope=0, intercept=summary(idle$consumo)[[3]], 
                             colour=as.factor(idle$`Quantidade Clientes`[[1]])), linetype="dashed")+
    annotate(geom="text", x=26.5, y=195, label="Idle",
             color="dark green", size=6)+
    geom_boxplot(outlier.shape=NA)+
    # 1MB
    geom_rect(mapping=aes(xmin=0.55, xmax=1.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=0.55+(1.45-0.55)/2, y=460, label="1 MB"), size=2, color='black') +
    #2.5MB
    geom_rect(mapping=aes(xmin=1.55, xmax=2.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=1.55+(2.45-1.55)/2, y=460, label="2.5 MB"), size=2, color='black') +
    #5MB
    geom_rect(mapping=aes(xmin=2.55, xmax=3.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=2.55+(3.45-2.55)/2, y=460, label="5 MB"), size=2, color='black') +
    #10MB
    geom_rect(mapping=aes(xmin=3.55, xmax=5.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=3.55+(5.45-3.55)/2, y=460, label="10 MB"), size=2, color='black') +
    #12.5MB
    geom_rect(mapping=aes(xmin=5.55, xmax=6.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=5.55+(6.45-5.55)/2, y=460, label="12.5 MB"), size=2, color='black') +
    # #25MB
    geom_rect(mapping=aes(xmin=6.55, xmax=8.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=6.55+(8.45-6.55)/2, y=460, label="25 MB"), size=2, color='black') +
    # #50MB
    geom_rect(mapping=aes(xmin=8.55, xmax=10.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=8.55+(10.45-8.55)/2, y=460, label="50 MB"), size=2, color='black') +
    # #62.5MB
    geom_rect(mapping=aes(xmin=10.55, xmax=11.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=10.55+(11.45-10.55)/2, y=460, label="62.5 MB"), size=2, color='black') +
    #100MB
    geom_rect(mapping=aes(xmin=11.55, xmax=14.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=11.55+(14.45-11.55)/2, y=460, label="100 MB"), size=2, color='black') +
    #125MB
    geom_rect(mapping=aes(xmin=14.55, xmax=15.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=14.55+(15.45-14.55)/2, y=460, label="125 MB"), size=2, color='black') +
    #250MB
    geom_rect(mapping=aes(xmin=15.55, xmax=17.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=15.55+(17.45-15.55)/2, y=460, label="250 MB"), size=2, color='black') +
    #500MB
    geom_rect(mapping=aes(xmin=17.55, xmax=19.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=17.55+(19.45-17.55)/2, y=460, label="500 MB"), size=2, color='black') +
    #1GB
    geom_rect(mapping=aes(xmin=19.55, xmax=22.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=19.55+(22.45-19.55)/2, y=460, label="1 GB"), size=2, color='black') +
    #2.5GB
    geom_rect(mapping=aes(xmin=22.55, xmax=23.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=22.55+(23.45-22.55)/2, y=460, label="2.5 GB"), size=2, color='black') +
    #5GB
    geom_rect(mapping=aes(xmin=23.55, xmax=25.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light grey',0.01),  linetype=1, size=0.2) +
    geom_text(aes(x=23.55+(25.45-23.55)/2, y=460, label="5 GB"), size=2, color='black') +
    # #10GB
    geom_rect(mapping=aes(xmin=25.55, xmax=27.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0),  linetype=1, size=0.2) +
    geom_text(aes(x=25.55+(27.45-25.55)/2, y=460, label="10 GB"), size=2, color='black') +
    # #25GB
    geom_rect(mapping=aes(xmin=27.55, xmax=28.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0),  linetype=1, size=0.2) +
    geom_text(aes(x=27.55+(28.45-27.55)/2, y=460, label="25 GB"), size=2, color='black') +
    # #50GB
    geom_rect(mapping=aes(xmin=28.55, xmax=29.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0),  linetype=1, size=0.2) +
    geom_text(aes(x=28.55+(29.45-28.55)/2, y=460, label="50 GB"), size=2, color='black') +
    #100MB
    geom_rect(mapping=aes(xmin=29.55, xmax=30.45, ymin=175, ymax=450),
              color='grey20', fill=alpha('light green',0), linetype=1, size=0.2) +
    geom_text(aes(x=29.55+(30.45-29.55)/2, y=460, label="100 GB"), size=2, color='black') +
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 90,
            hjust = 0.7,
            size=12,
            vjust= 0.5
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
        x="(Number of Containers/Bandwidth)",
        y="Consumption(Watts/s)",
        color= "iPerf3 settings"
    )+
    scale_color_discrete(breaks = c("Cliente","Servidor"), labels= c("Client","Server"))+
    scale_y_continuous(limits=c(175,480), breaks=seq(175,450,25))+
    scale_x_discrete(
        limits=c(
            #1MB
            "1Mb 1",
            #2.5MB
            "2.5Mb 1",
            #5MB
            "1Mb 5",
            #10MB
            "10Mb 1",
            "1Mb 10",
            #12.5MB
            "2.5Mb 5",
            #25MB
            "1Mb 25",
            "2.5Mb 10",
            #50MB
            "10Mb 5",
            "1Mb 50",
            #62.5MB
            "2.5Mb 25",
            #100MB
            "100Mb 1",
            "10Mb 10",
            "1Mb 100",
            #125MB
            "2.5Mb 50",
            #250MB
            "10Mb 25",
            "2.5Mb 100",
            #500MB
            "100Mb 5",
            "10Mb 50",
            #1GB
            "1000Mb 1",
            "100Mb 10",
            "10Mb 100",
            #2.5GB
            "100Mb 25",
            #5GB
            "1000Mb 5",
            "100Mb 50",
            #10GB
            "1000Mb 10",
            "100Mb 100",
            #25GB
            "1000Mb 25",
            #50GB
            "1000Mb 50",
            #100GB
            "1000Mb 100"
        ),
        labels=c(
            "(1/1MB)",
            "(1/2.5MB)",
            "(5/1MB)",
            "(1/10MB)",
            "(10/1MB)",
            "(5/2.5MB)",
            "(25/1MB)",
            "(10/2.5MB)",
            "(5/10MB)",
            "(50/1MB)",
            "(25/2.5MB)",
            "(1/100MB)",
            "(10/10MB)",
            "(100/1MB)",
            "(50/2.5MB)",
            "(25/10MB)",
            "(100/2.5MB)",
            "(5/100MB)",
            "(50/10MB)",
            "(1/1000MB)",
            "(10/100MB)",
            "(100/10MB)",
            "(25/100MB)",
            "(5/1000MB)",
            "(50/100MB)",
            "(10/1000MB)",
            "(100/100MB)",
            "(25/1000MB)",
            "(50/1000MB)",
            "(100/1000MB)"
        ))

plot(p2)
dev.off()

files <- list.files(path = "./logs/", pattern = "*.log")
consumo_cpu_bench <- data.frame(consumo_cpu=double(),
                                teste=character(),
                                cpu=character(),
                                ram=character(),
                                stringsAsFactors=FALSE
)

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.png; do convert $f ${f%.*}.pdf; done;")
system("rm *.tiff")
