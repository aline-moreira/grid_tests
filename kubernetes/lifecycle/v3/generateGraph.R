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

times<- read.table("lifecycle.times",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(times) <- c("pods","containers","start","end")
times$start <- as.POSIXct(times$start,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")
times$end <- as.POSIXct(times$end,tz="UTC", format="%a %d %b %Y %I:%M:%S %p")

times$total_containers[1] <- "idle"
times$total_containers[2:length(times$total_containers)] <- as.numeric(times$pods[2:length(times$pods)]) * as.numeric(times$containers[2:length(times$pods)])

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1925045.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values
host <- "master"

energy_1 <- data.frame(time,values, host)

rm(time)
rm(values)
rm(host)

names(energy_1) <- c("tempo","consumo","node")
##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1927167.json")

#energy$items[[number_of_host]]
energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values
host <- "master"

energy_2 <- data.frame(time,values, host)

rm(time)
rm(values)
rm(host)

names(energy_2) <- c("tempo","consumo","node")

energy <- rbind(energy_1, energy_2)
rm(energy_1, energy_2)

###################################################
# Filtrando a energia de acordo com os tempos
###################################################
idle <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers=="idle"] &
        energy$tempo <= times$end[times$total_containers=="idle"]
)
idle$total_containers <- "idle"
q <- quantile(idle$consumo, c(0.1, 0.9))
idle <- idle[idle$consumo >= q[1] & idle$consumo <= q[2], ]

pod_0 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==0] &
        energy$tempo <= times$end[times$total_containers==0]
)
pod_0$total_containers <- 0
q <- quantile(pod_0$consumo, c(0.1, 0.9))
pod_0 <- pod_0[pod_0$consumo >= q[1] & pod_0$consumo <= q[2], ]


pod_1 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==1] &
        energy$tempo <= times$end[times$total_containers==1]
)
pod_1$total_containers <- 1
q <- quantile(pod_1$consumo, c(0.1, 0.9))
pod_1 <- pod_1[pod_1$consumo >= q[1] & pod_1$consumo <= q[2], ]

pod_256 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==256] &
        energy$tempo <= times$end[times$total_containers==256]
)
pod_256$total_containers <- 256
q <- quantile(pod_256$consumo, c(0.1, 0.9))
pod_256 <- pod_256[pod_256$consumo >= q[1] & pod_256$consumo <= q[2], ]

pod_512 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==512] &
        energy$tempo <= times$end[times$total_containers==512]
)
pod_512$total_containers <- 512
q <- quantile(pod_512$consumo, c(0.1, 0.9))
pod_512 <- pod_512[pod_512$consumo >= q[1] & pod_512$consumo <= q[2], ]

pod_1024 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==1024] &
        energy$tempo <= times$end[times$total_containers==1024]
)
pod_1024$total_containers <- 1024
q <- quantile(pod_1024$consumo, c(0.1, 0.9))
pod_1024 <- pod_1024[pod_1024$consumo >= q[1] & pod_1024$consumo <= q[2], ]

pod_2048 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==2048] &
        energy$tempo <= times$end[times$total_containers==2048]
)
pod_2048$total_containers <- 2048
q <- quantile(pod_2048$consumo, c(0.1, 0.9))
pod_2048 <- pod_2048[pod_2048$consumo >= q[1] & pod_2048$consumo <= q[2], ]

pod_4096 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==4096] &
        energy$tempo <= times$end[times$total_containers==4096]
)
pod_4096$total_containers <- 4096
q <- quantile(pod_4096$consumo, c(0.1, 0.9))
pod_4096 <- pod_4096[pod_4096$consumo >= q[1] & pod_4096$consumo <= q[2], ]

pod_8192 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==8192] &
        energy$tempo <= times$end[times$total_containers==8192]
)
pod_8192$total_containers <- 8192
q <- quantile(pod_8192$consumo, c(0.1, 0.9))
pod_8192 <- pod_8192[pod_8192$consumo >= q[1] & pod_8192$consumo <= q[2], ]

pod_16384 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==16384] &
        energy$tempo <= times$end[times$total_containers==16384]
)
pod_16384$total_containers <- 16384
q <- quantile(pod_16384$consumo, c(0.1, 0.9))
pod_16384 <- pod_16384[pod_16384$consumo >= q[1] & pod_16384$consumo <= q[2], ]

pod_32768 <- energy %>% filter(
    energy$tempo >= times$start[times$total_containers==32768] &
        energy$tempo <= times$end[times$total_containers==32768]
)
pod_32768$total_containers <- 32768
q <- quantile(pod_32768$consumo, c(0.1, 0.9))
pod_32768 <- pod_32768[pod_32768$consumo >= q[1] & pod_32768$consumo <= q[2], ]

dt_tests <- rbind(pod_0, pod_1)
#dt_tests <- rbind(dt_tests, idle)
dt_tests <- rbind(dt_tests, pod_256)
dt_tests <- rbind(dt_tests,pod_512)
dt_tests <- rbind(dt_tests,pod_1024)
dt_tests <- rbind(dt_tests,pod_2048)
dt_tests <- rbind(dt_tests,pod_4096)
dt_tests <- rbind(dt_tests,pod_8192)
dt_tests <- rbind(dt_tests,pod_16384)
dt_tests <- rbind(dt_tests,pod_32768)

#rm(idle)
rm(pod_0)
rm(pod_1)
rm(pod_256)
rm(pod_512)
rm(pod_1024)
rm(pod_2048)
rm(pod_4096)
rm(pod_8192)
rm(pod_16384)
rm(pod_32768)

tiff("sysbench_kubernetes_benchmark.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests, aes(x=as.factor(total_containers), y=consumo, color=as.factor(node)))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
    geom_abline( mapping=aes(slope=0, intercept=summary(idle$consumo)[[3]], 
                             colour=as.factor(idle$total_containers[[1]])), linetype="dashed")+
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
    scale_y_continuous(limits=c(175,425), breaks=seq(175,425,25))+
    labs(
        x="Quantidade de Contêineres",
        y="Consumo (W/s)",
        color= "Tipo de hospedeiro"
    )+
    scale_x_discrete(
        limits=c(
            "0",
            "1",
            "256",
            "512",
            "1024",
            "2048",
            "4096",
            "8192",
            "16384",
            "32768"
        ),
        labels=c(
            "0",
            "1",
            "256",
            "512",
            "1024",
            "2048",
            "4096",
            "8192",
            "16384",
            "32768"
        ))

plot(p1)
dev.off()

rm(p1)

tiff("sysbench_kubernetes_benchmark_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests, aes(x=as.factor(total_containers), y=consumo, color=as.factor(node)))+
    geom_boxplot(outlier.shape=NA, notch=FALSE)+
    geom_abline( mapping=aes(slope=0, intercept=summary(idle$consumo)[[3]], 
                             colour=as.factor(idle$total_containers[[1]])), linetype="dashed")+
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
    scale_y_continuous(limits=c(175,425), breaks=seq(175,425,25))+
    labs(
        x="#Containers",
        y="Consumption (Watts/s)",
        color= "Host Type"
    )+
    scale_x_discrete(
        limits=c(
            "0",
            "1",
            "256",
            "512",
            "1024",
            "2048",
            "4096",
            "8192",
            "16384",
            "32768"
        ),
        labels=c(
            "0",
            "1",
            "256",
            "512",
            "1024",
            "2048",
            "4096",
            "8192",
            "16384",
            "32768"
        ))

plot(p2)
dev.off()

rm(p2)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
