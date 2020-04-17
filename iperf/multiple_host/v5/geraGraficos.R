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
names(times) <- c("Configuração de Rede","Tipo de Teste","Execucao","Quantidade Clientes","Largura de Banda","start","end")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%a %d %b %Y %I:%M:%S %p")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1926614.json")

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

## HOST COM 1MB
normal1_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal']
)
normal1_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal']
)
normal1_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal']
)

normal1 <- rbind(normal1_1, normal1_2);
normal1 <- rbind(normal1, normal1_3);
rm(normal1_1)
rm(normal1_2)
rm(normal1_3)

normal1$`Largura de Banda` <- '1Mb'
q <- quantile(normal1$consumo, c(0.1, 0.9))
## HOST COM 10MB
normal10_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='normal']
)
normal10_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='normal']
)
normal10_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='normal']
)

normal10 <- rbind(normal10_1, normal10_2);
normal10 <- rbind(normal10, normal10_3);
rm(normal10_1)
rm(normal10_2)
rm(normal10_3)

normal10$`Largura de Banda` <- '10Mb'
q <- quantile(normal10$consumo, c(0.1, 0.9))

###

## HOST COM 100MB
normal100_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='normal']
)
normal100_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='normal']
)
normal100_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='normal']
)

normal100 <- rbind(normal100_1, normal100_2);
normal100 <- rbind(normal100, normal100_3);
rm(normal100_1)
rm(normal100_2)
rm(normal100_3)

normal100$`Largura de Banda` <- '100Mb'
q <- quantile(normal100$consumo, c(0.1, 0.9))
#####

## HOST COM 1000MB
normal1000_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='normal']
)
normal1000_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='normal']
)
normal1000_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='normal'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='normal']
)

normal1000 <- rbind(normal1000_1, normal1000_2);
normal1000 <- rbind(normal1000, normal1000_3);
rm(normal1000_1)
rm(normal1000_2)
rm(normal1000_3)

normal1000$`Largura de Banda` <- '1000Mb'
q <- quantile(normal1000$consumo, c(0.1, 0.9))
###
## HOST COM 1MB
cpu1_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu']
)
cpu1_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu']
)
cpu1_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu']
)

cpu1 <- rbind(cpu1_1, cpu1_2);
cpu1 <- rbind(cpu1, cpu1_3);
rm(cpu1_1)
rm(cpu1_2)
rm(cpu1_3)

cpu1$`Largura de Banda` <- '1Mb'
q <- quantile(cpu1$consumo, c(0.1, 0.9))
## HOST COM 10MB
cpu10_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='cpu']
)
cpu10_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='cpu']
)
cpu10_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='10M' & times$`Tipo de Teste`=='cpu']
)

cpu10 <- rbind(cpu10_1, cpu10_2);
cpu10 <- rbind(cpu10, cpu10_3);
rm(cpu10_1)
rm(cpu10_2)
rm(cpu10_3)

cpu10$`Largura de Banda` <- '10Mb'
q <- quantile(cpu10$consumo, c(0.1, 0.9))

###

## HOST COM 100MB
cpu100_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='cpu']
)
cpu100_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='cpu']
)
cpu100_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='100M' & times$`Tipo de Teste`=='cpu']
)

cpu100 <- rbind(cpu100_1, cpu100_2);
cpu100 <- rbind(cpu100, cpu100_3);
rm(cpu100_1)
rm(cpu100_2)
rm(cpu100_3)

cpu100$`Largura de Banda` <- '100Mb'
q <- quantile(cpu100$consumo, c(0.1, 0.9))
#####

## HOST COM 1000MB
cpu1000_1 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==1 &times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==1 & times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='cpu']
)
cpu1000_2 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==2 &times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==2 & times$`Largura de Banda`=='1M' & times$`Tipo de Teste`=='cpu']
)
cpu1000_3 <- energy %>% filter(
    energy$tempo >= times$start[times$Execucao==3 &times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='cpu'] &
        energy$tempo <= times$end[times$Execucao==3 & times$`Largura de Banda`=='1000M' & times$`Tipo de Teste`=='cpu']
)

cpu1000 <- rbind(cpu1000_1, cpu1000_2);
cpu1000 <- rbind(cpu1000, cpu1000_3);
rm(cpu1000_1)
rm(cpu1000_2)
rm(cpu1000_3)

cpu1000$`Largura de Banda` <- '1000Mb'
q <- quantile(cpu1000$consumo, c(0.1, 0.9))
###

normal <- rbind (normal1, normal10)
normal <- rbind (normal, normal100)
normal <- rbind (normal, normal1000)
cpu <- rbind (cpu1, cpu10)
cpu <- rbind (cpu, cpu100)
cpu <- rbind (cpu, cpu1000)

normal$teste <- "sem vmstat"
cpu$teste <- "com vmstat"

rm(normal1)
rm(cpu1)
rm(normal10)
rm(cpu10)
rm(normal100)
rm(cpu100)
rm(normal1000)
rm(cpu1000)

dt_tests <- rbind(normal,cpu)
dt_tests$grp <- paste(dt_tests$`Largura de Banda`, dt_tests$teste)

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
            "1Mb sem vmstat",
            "1Mb com vmstat",
            "10Mb sem vmstat",
            "10Mb com vmstat",
            "100Mb sem vmstat",
            "100Mb com vmstat",
            "1000Mb sem vmstat",
            "1000Mb com vmstat"
        ),
    )

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
consumo_cpu_bench$grp <- paste(consumo_cpu_bench$teste,consumo_cpu_bench$`Largura de Banda`)

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
        x="Configuração de 100 clientes e largura de banda do contêiner",
        y="Consumo CPU (%)",
        color="Tipo de hospedeiro"
    )+
    scale_color_brewer(palette="Dark2")#+
    # scale_y_continuous(limits=c(50,210), breaks=seq(50,210,10))+
    # scale_x_discrete(
    #     limits=c(
    #         "1M",
    #         "10M",
    #         "100M",
    #         "1000M"
    #     ),
    #     labels=c(
    #         "1 MB",
    #         "10 MB",
    #         "100 MB",
    #         "1000 MB"
    #     )
    # )
plot(p2)
dev.off()

rm(p2)
rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("rm *.tiff")
