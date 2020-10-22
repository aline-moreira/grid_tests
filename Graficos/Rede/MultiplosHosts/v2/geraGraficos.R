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
names(times) <- c("Configuração de Rede","Quantidade Clientes","Largura de Banda","start","end")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1925189.json")

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
    energy$tempo >= times$start[times$`Largura de Banda`=='0M'] &
        energy$tempo <= times$end[times$`Largura de Banda`=='0M']
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
host10_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==10 & times$`Largura de Banda`=='1M']
)
host10_1$`Largura de Banda` <- '1Mb'
host10_1$`Quantidade Clientes` <- 10
q <- quantile(host1_10$consumo, c(0.1, 0.9))
host10_1 <- host10_1[host10_1$consumo >= q[1] & host10_1$consumo <= q[2], ]

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
host50_1 <- energy %>% filter(
    energy$tempo >= times$start[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='1M'] &
        energy$tempo <= times$end[times$`Quantidade Clientes`==50 & times$`Largura de Banda`=='1M']
)
host50_1$`Largura de Banda` <- '1Mb'
host50_1$`Quantidade Clientes` <- 50
q <- quantile(host50_1$consumo, c(0.1, 0.9))
host50_1 <- host50_1[host50_1$consumo >= q[1] & host50_1$consumo <= q[2], ]

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
dt_tests <- rbind(dt_tests,host1_100)
dt_tests <- rbind(dt_tests,host1_1000)
dt_tests <- rbind(dt_tests,host10_1)
dt_tests <- rbind(dt_tests,host10_10)
dt_tests <- rbind(dt_tests,host10_100)
dt_tests <- rbind(dt_tests,host10_1000)
dt_tests <- rbind(dt_tests,host50_1)
dt_tests <- rbind(dt_tests,host50_10)
dt_tests <- rbind(dt_tests,host50_100)
dt_tests <- rbind(dt_tests,host50_1000)
dt_tests <- rbind(dt_tests,host100_1)
dt_tests <- rbind(dt_tests,host100_10)
dt_tests <- rbind(dt_tests,host100_100)
dt_tests <- rbind(dt_tests,host100_1000)

rm(host1_1)
rm(host1_10)
rm(host1_100)
rm(host1_1000)
rm(host10_1)
rm(host10_10)
rm(host10_100)
rm(host10_1000)
rm(host50_1)
rm(host50_10)
rm(host50_100)
rm(host50_1000)
rm(host100_1)
rm(host100_10)
rm(host100_100)
rm(host100_1000)

dt_tests$grp <- paste(dt_tests$`Largura de Banda`, dt_tests$`Quantidade Clientes`)

tiff("iperf_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=grp, y=consumo, color=as.factor(node)))+
    geom_abline( mapping=aes(slope=0, intercept=summary(idle$consumo)[[3]], 
                             colour=as.factor(idle$`Quantidade Clientes`[[1]])), linetype="dashed")+
    geom_boxplot(outlier.shape=NA)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 90,
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
        x="Largura de Banda e quantidade de contêineres",
        y="Consumo (W/s)",
        color= "Tipo de configuração Iperf3"
    )+
    scale_color_discrete(breaks = c("Idle","Cliente","Servidor"))+
    scale_y_continuous(limits=c(175,450), breaks=seq(175,450,25))+
    scale_x_discrete(
        limits=c(
            "1Mb 1",
            "10Mb 1",
            "100Mb 1",
            "1000Mb 1",
            "1Mb 10",
            "10Mb 10",
            "100Mb 10",
            "1000Mb 10",
            "1Mb 50",
            "10Mb 50",
            "100Mb 50",
            "1000Mb 50",
            "1Mb 100",
            "10Mb 100",
            "100Mb 100",
            "1000Mb 100"
        ),
        labels=c(
            "1 Cliente\ncom 1 MB",
            "1 Cliente\ncom 10 MB",
            "1 Cliente\ncom 100 MB",
            "1 Cliente\ncom 1000 MB",
            "10 Clientes\ncom 1 MB",
            "10 Clientes\ncom 10 MB",
            "10 Clientes\ncom 100 MB",
            "10 Clientes\ncom 1000 MB",
            "50 Clientes\ncom 1 MB",
            "50 Clientes\ncom 10 MB",
            "50 Clientes\ncom 100 MB",
            "50 Clientes\ncom 1000 MB",
            "100 Clientes\ncom 1 MB",
            "100 Clientes\ncom 10 MB",
            "100 Clientes\ncom 100 MB",
            "100 Clientes\ncom 1000 MB"
        ))

plot(p1)
dev.off()

files <- list.files(path = "./logs/", pattern = "*.log")
consumo_cpu_bench <- data.frame(consumo_cpu=double(),
                                teste=character(),
                                cpu=character(),
                                ram=character(),
                                stringsAsFactors=FALSE
)

for (file in files) {
    name <- strsplit(strsplit(file,".cpu_log")[[1]], "-")[[1]]
    clients <- as.numeric(name[2])
    
    dt_temp <- read.table(paste0("logs/",file),header=FALSE,sep="")
    dt_temp$teste <- name[1]
    dt_temp$`Quantidade de Clientes` <- clients
    dt_temp$`Largura de Banda` <- name[3]

    names(dt_temp) <- c("consumo_cpu","Tipo de Hospedeiro","Quantidade de Clientes","Largura de Banda")
    
    consumo_cpu_bench <- rbind(consumo_cpu_bench,dt_temp)
}

rm(dt_temp)

#Fazendo o plot
consumo_cpu_bench$grp <- paste(consumo_cpu_bench$`Quantidade de Clientes`,consumo_cpu_bench$`Largura de Banda`)

tiff("consumo_cpu_benchmarks.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=consumo_cpu_bench, aes(x=grp, y=consumo_cpu, color=`Tipo de Hospedeiro`))+
    geom_boxplot(outlier.shape = NA)+
    theme_classic()+
    theme(
        legend.position="top",
        axis.text.x = element_text(
            angle = 90,
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
        x="Configuração clientes e largura de banda do contêiner",
        y="Consumo CPU (%)",
        color="Tipo de hospedeiro"
    )+
    scale_color_brewer(palette="Dark2")+
    # scale_y_continuous(limits=c(50,210), breaks=seq(50,210,10))+
    scale_x_discrete(
        limits=c(
            "1 1M",
            "1 10M",
            "1 100M",
            "1 1000M",
            "10 1M",
            "10 10M",
            "10 100M",
            "10 1000M",
            "50 1M",
            "50 10M",
            "50 100M",
            "50 1000M",
            "100 1M",
            "100 10M",
            "100 100M",
            "100 1000M"
        ),
        labels=c(
            "1 Cliente\ncom 1 MB",
            "1 Cliente\ncom 10 MB",
            "1 Cliente\ncom 100 MB",
            "1 Cliente\ncom 1000 MB",
            "10 Clientes\ncom 1 MB",
            "10 Clientes\ncom 10 MB",
            "10 Clientes\ncom 100 MB",
            "10 Clientes\ncom 1000 MB",
            "50 Clientes\ncom 1 MB",
            "50 Clientes\ncom 10 MB",
            "50 Clientes\ncom 100 MB",
            "50 Clientes\ncom 1000 MB",
            "100 Clientes\ncom 1 MB",
            "100 Clientes\ncom 10 MB",
            "100 Clientes\ncom 100 MB",
            "100 Clientes\ncom 1000 MB"
        )
    )
plot(p2)
dev.off()

rm(p2)
rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
