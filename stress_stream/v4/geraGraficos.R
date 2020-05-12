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

times<- read.table("./benchmarks.time",header=FALSE,sep=";")
names(times) <- c("teste","cpu","ram","inicio","fim")
times$inicio <- as.POSIXct(times$inicio,tz="UTC",  format="%c")
times$fim <- as.POSIXct(times$fim,tz="UTC",  format="%c")

##########GETTING THE INFO OF JSON######################
energy1 <- fromJSON(file="energy_1922862.json")

energy1$items[[1]]$timestamps = as.POSIXct(energy1$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy1$items[[1]]$timestamps
values <- energy1$items[[1]]$values

energy1 <- data.frame(time,values)

names(energy1) <- c("tempo","consumo")
#----------
energy2 <- fromJSON(file="energy_1922343.json")

energy2$items[[1]]$timestamps = as.POSIXct(energy2$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy2$items[[1]]$timestamps
values <- energy2$items[[1]]$values

energy2 <- data.frame(time,values)

names(energy2) <- c("tempo","consumo")

energy <- rbind(energy1,energy2)
rm(energy1, energy2)

###################################################
# Filtrando a energia de acordo com os tempos
###################################################

idle <- energy %>% filter(
    energy$tempo >= "2020-03-25 21:35:10 UTC" &
        energy$tempo <= "2020-03-25 21:40:10 UTC"
)
idle$teste <- "idle"
idle$cpu <- NA
idle$ram <- NA
q <- quantile(idle$consumo, c(0.05, 0.95))
idle <- idle[idle$consumo >= q[1] & idle$consumo <= q[2], ]

#STRESS 0.25 CPU
stress_025 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STRESS" & times$cpu==0.25 & times$ram==0.5] &
        energy$tempo <= times$fim[times$teste=="STRESS" & times$cpu==0.25 & times$ram==0.5]
)
stress_025$teste <- "stress"
stress_025$cpu <- 0.25
stress_025$ram <- NA
q <- quantile(stress_025$consumo, c(0.05, 0.95))
stress_025 <- stress_025[stress_025$consumo >= q[1] & stress_025$consumo <= q[2], ]

#0.25 0.5
stream_025_05 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.25 & times$ram==0.5] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.25 & times$ram==0.5]
)
stream_025_05$teste <- "stream"
stream_025_05$cpu <- 0.25
stream_025_05$ram <- 0.5
q <- quantile(stream_025_05$consumo, c(0.05, 0.95))
stream_025_05 <- stream_025_05[stream_025_05$consumo >= q[1] & stream_025_05$consumo <= q[2], ]

#0.25 1
stream_025_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.25 & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.25 & times$ram==1]
)
stream_025_1$teste <- "stream"
stream_025_1$cpu <- 0.25
stream_025_1$ram <- 1
q <- quantile(stream_025_1$consumo, c(0.05, 0.95))
stream_025_1 <- stream_025_1[stream_025_1$consumo >= q[1] & stream_025_1$consumo <= q[2], ]

#0.25 2
stream_025_2 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.25 & times$ram==2] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.25 & times$ram==2]
)
stream_025_2$teste <- "stream"
stream_025_2$cpu <- 0.25
stream_025_2$ram <- 2
q <- quantile(stream_025_2$consumo, c(0.05, 0.95))
stream_025_2 <- stream_025_2[stream_025_2$consumo >= q[1] & stream_025_2$consumo <= q[2], ]

#STRESS 0.5 CPU
stress_05 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STRESS" & times$cpu==0.5 & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STRESS" & times$cpu==0.5 & times$ram==1]
)
stress_05$teste <- "stress"
stress_05$cpu <- 0.5
stress_05$ram <- NA
q <- quantile(stress_05$consumo, c(0.05, 0.95))
stress_05 <- stress_05[stress_05$consumo >= q[1] & stress_05$consumo <= q[2], ]

#0.5 1
stream_05_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.5 & times$ram==1] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.5 & times$ram==1]
)
stream_05_1$teste <- "stream"
stream_05_1$cpu <- 0.5
stream_05_1$ram <- 1
q <- quantile(stream_05_1$consumo, c(0.05, 0.95))
stream_05_1 <- stream_05_1[stream_05_1$consumo >= q[1] & stream_05_1$consumo <= q[2], ]

#0.5 2
stream_05_2 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.5 & times$ram==2] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.5 & times$ram==2]
)
stream_05_2$teste <- "stream"
stream_05_2$cpu <- 0.5
stream_05_2$ram <- 2
q <- quantile(stream_05_2$consumo, c(0.05, 0.95))
stream_05_2 <- stream_05_2[stream_05_2$consumo >= q[1] & stream_05_2$consumo <= q[2], ]

#0.5 3
stream_05_3 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.5 & times$ram==3] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.5 & times$ram==3]
)
stream_05_3$teste <- "stream"
stream_05_3$cpu <- 0.5
stream_05_3$ram <- 3
q <- quantile(stream_05_3$consumo, c(0.05, 0.95))
stream_05_3 <- stream_05_3[stream_05_3$consumo >= q[1] & stream_05_3$consumo <= q[2], ]

#0.5 4
stream_05_4 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==0.5 & times$ram==4] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==0.5 & times$ram==4]
)
stream_05_4$teste <- "stream"
stream_05_4$cpu <- 0.5
stream_05_4$ram <- 4
q <- quantile(stream_05_4$consumo, c(0.05, 0.95))
stream_05_4 <- stream_05_4[stream_05_4$consumo >= q[1] & stream_05_4$consumo <= q[2], ]

#STRESS 1 CPU
stress_1 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STRESS" & times$cpu==1 & times$ram==2] &
        energy$tempo <= times$fim[times$teste=="STRESS" & times$cpu==1 & times$ram==2]
)
stress_1$teste <- "stress"
stress_1$cpu <- 1
stress_1$ram <- NA
q <- quantile(stress_1$consumo, c(0.05, 0.95))
stress_1 <- stress_1[stress_1$consumo >= q[1] & stress_1$consumo <= q[2], ]

#1 2
stream_1_2 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==1 & times$ram==2] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==1 & times$ram==2]
)
stream_1_2$teste <- "stream"
stream_1_2$cpu <- 1
stream_1_2$ram <- 2
q <- quantile(stream_1_2$consumo, c(0.05, 0.95))
stream_1_2 <- stream_1_2[stream_1_2$consumo >= q[1] & stream_1_2$consumo <= q[2], ]

#1 4
stream_1_4 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==1 & times$ram==4] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==1 & times$ram==4]
)
stream_1_4$teste <- "stream"
stream_1_4$cpu <- 1
stream_1_4$ram <- 4
q <- quantile(stream_1_4$consumo, c(0.05, 0.95))
stream_1_4 <- stream_1_4[stream_1_4$consumo >= q[1] & stream_1_4$consumo <= q[2], ]

#1 8
stream_1_8 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==1 & times$ram==8] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==1 & times$ram==8]
)
stream_1_8$teste <- "stream"
stream_1_8$cpu <- 1
stream_1_8$ram <- 8
q <- quantile(stream_1_8$consumo, c(0.05, 0.95))
stream_1_8 <- stream_1_8[stream_1_8$consumo >= q[1] & stream_1_8$consumo <= q[2], ]

#STRESS 2 CPU
stress_2 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STRESS" & times$cpu==2 & times$ram==4] &
        energy$tempo <= times$fim[times$teste=="STRESS" & times$cpu==2 & times$ram==4]
)
stress_2$teste <- "stress"
stress_2$cpu <- 2
stress_2$ram <- NA
q <- quantile(stress_2$consumo, c(0.05, 0.95))
stress_2 <- stress_2[stress_2$consumo >= q[1] & stress_2$consumo <= q[2], ]

#2 4
stream_2_4 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==2 & times$ram==4] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==2 & times$ram==4]
)
stream_2_4$teste <- "stream"
stream_2_4$cpu <- 2
stream_2_4$ram <- 4
q <- quantile(stream_2_4$consumo, c(0.05, 0.95))
stream_2_4 <- stream_2_4[stream_2_4$consumo >= q[1] & stream_2_4$consumo <= q[2], ]

#2 8
stream_2_8 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==2 & times$ram==8] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==2 & times$ram==8]
)
stream_2_8$teste <- "stream"
stream_2_8$cpu <- 2
stream_2_8$ram <- 8
q <- quantile(stream_2_8$consumo, c(0.05, 0.95))
stream_2_8 <- stream_2_8[stream_2_8$consumo >= q[1] & stream_2_8$consumo <= q[2], ]

#2 16
stream_2_16 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==2 & times$ram==16] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==2 & times$ram==16]
)
stream_2_16$teste <- "stream"
stream_2_16$cpu <- 2
stream_2_16$ram <- 16
q <- quantile(stream_2_16$consumo, c(0.05, 0.95))
stream_2_16 <- stream_2_16[stream_2_16$consumo >= q[1] & stream_2_16$consumo <= q[2], ]

#STRESS 4 CPU
stress_4 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STRESS" & times$cpu==4 & times$ram==32] &
        energy$tempo <= times$fim[times$teste=="STRESS" & times$cpu==4 & times$ram==32]
)
stress_4$teste <- "stress"
stress_4$cpu <- 4
stress_4$ram <- NA
q <- quantile(stress_4$consumo, c(0.05, 0.95))
stress_4 <- stress_4[stress_4$consumo >= q[1] & stress_4$consumo <= q[2], ]

#4 4
stream_4_4 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==4 & times$ram==4] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==4 & times$ram==4]
)
stream_4_4$teste <- "stream"
stream_4_4$cpu <- 4
stream_4_4$ram <- 4
q <- quantile(stream_4_4$consumo, c(0.05, 0.95))
stream_4_4 <- stream_4_4[stream_4_4$consumo >= q[1] & stream_4_4$consumo <= q[2], ]
# stream_4_4$consumo <- stream_4_4$consumo + 80

#4 16
stream_4_16 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==4 & times$ram==16] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==4 & times$ram==16]
)
stream_4_16$teste <- "stream"
stream_4_16$cpu <- 4
stream_4_16$ram <- 16
q <- quantile(stream_4_16$consumo, c(0.05, 0.95))
stream_4_16 <- stream_4_16[stream_4_16$consumo >= q[1] & stream_4_16$consumo <= q[2], ]

#4 32
stream_4_32 <- energy %>% filter(
    energy$tempo >= times$inicio[times$teste=="STREAM" & times$cpu==4 & times$ram==32] &
        energy$tempo <= times$fim[times$teste=="STREAM" & times$cpu==4 & times$ram==32]
)
stream_4_32$teste <- "stream"
stream_4_32$cpu <- 4
stream_4_32$ram <- 32
q <- quantile(stream_4_32$consumo, c(0.05, 0.95))
stream_4_32 <- stream_4_32[stream_4_32$consumo >= q[1] & stream_4_32$consumo <= q[2], ]

#Juntando os logs em um unico df
energy_bench <- rbind(stress_025, stress_05)
energy_bench <- rbind(energy_bench, stress_1)
energy_bench <- rbind(energy_bench, stress_2)
energy_bench <- rbind(energy_bench, stream_025_05)
energy_bench <- rbind(energy_bench, stream_025_1)
energy_bench <- rbind(energy_bench, stream_025_2)
energy_bench <- rbind(energy_bench, stream_05_1)
energy_bench <- rbind(energy_bench, stream_05_2)
energy_bench <- rbind(energy_bench, stream_05_3)
energy_bench <- rbind(energy_bench, stream_05_4)
energy_bench <- rbind(energy_bench, stream_1_2)
energy_bench <- rbind(energy_bench, stream_1_4)
energy_bench <- rbind(energy_bench, stream_1_8)
energy_bench <- rbind(energy_bench, stream_2_4)
energy_bench <- rbind(energy_bench, stream_2_8)
energy_bench <- rbind(energy_bench, stream_2_16)
energy_bench <- rbind(energy_bench, stress_4)
energy_bench <- rbind(energy_bench, stream_4_4)
energy_bench <- rbind(energy_bench, stream_4_16)
energy_bench <- rbind(energy_bench, stream_4_32)
#energy_bench <- rbind(energy_bench, idle)

#Limpando a memoria
rm(stress_025)
rm(stress_05)
rm(stress_1)
rm(stress_2)
rm(stream_025_05)
rm(stream_025_1)
rm(stream_025_2)
rm(stream_05_1)
rm(stream_05_2)
rm(stream_05_3)
rm(stream_05_4)
rm(stream_1_2)
rm(stream_1_4)
rm(stream_1_8)
rm(stream_2_4)
rm(stream_2_8)
rm(stream_2_16)
rm(stream_4)
rm(stream_4_4)
rm(stream_4_16)
rm(stream_4_32)
#rm(idle)

# Lendo os logs de CPU.

files <- list.files(path = "./logs/", pattern = "*.log")

consumo_cpu_bench <- data.frame(consumo_cpu=double(),
                 teste=character(),
                 cpu=character(),
                 ram=character(),
                 stringsAsFactors=FALSE
                 )

for (file in files) {
    name <- strsplit(strsplit(file,".log")[[1]], "_")[[1]]
    cpu <- as.numeric(strsplit(name[2],"cpu")[[1]])
    
    dt_temp <- read.table(paste0("logs/",file),header=FALSE,sep="")
    dt_temp$teste <- name[1]
    dt_temp$cpu <- name[2]
    if(dt_temp$teste=="stream"){
        dt_temp$ram <- name[3]
    } else {
        dt_temp$ram <- NA
    }
    names(dt_temp) <- c("consumo_cpu","teste","cpu","ram")
    
    dt_temp$consumo_cpu <- dt_temp$consumo_cpu * (128 / cpu)

    consumo_cpu_bench <- rbind(consumo_cpu_bench,dt_temp)
}

rm(dt_temp)

#Fazendo o plot
energy_bench$grp <- paste(energy_bench$cpu,energy_bench$ram)

tiff("consumo_energia_benchmarks.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=energy_bench, aes(x=grp, y=consumo, color=teste))+
    geom_abline( mapping=aes(slope=0, intercept=summary(idle$consumo)[[3]], colour=idle$teste[[1]]), linetype="dashed")+
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
        x="Configuração de CPU e Memória do contêiner",
        y="Consumo (W/s)",
        color="Benchmark"
    )+
    scale_color_brewer(palette="Dark2")+
    scale_y_continuous(limits=c(150,550), breaks=seq(0,550,50))+
    scale_x_discrete(
       limits=c(
            "0.25 0.5",
            "0.25 1",
            "0.25 2",
            "0.25 NA",
            "0.5 1",
            "0.5 2",
            "0.5 3",
            "0.5 4",
            "0.5 NA",
            "1 2",
            "1 4",
            "1 8",
            "1 NA",
            "2 4",
            "2 8",
            "2 16",
            "2 NA",
            "4 4",
            "4 16",
            "4 32",
            "4 NA"
            ),
            labels=c(
            "0.25 CPUs\n0.5Gb RAM",
            "0.25 CPUs\n1Gb RAM",
            "0.25 CPUs\n2Gb RAM",
            "0.25 CPUs",
            "0.5 CPUs\n1Gb RAM",
            "0.5 CPUs\n2Gb RAM",
            "0.5 CPUs\n3Gb RAM",
            "0.5 CPUs\n4Gb RAM",
            "0.5 CPUs",
            "1 CPUs\n2Gb RAM",
            "1 CPUs\n4Gb RAM",
            "1 CPUs\n8Gb RAM",
            "1 CPUs",
            "2 CPUs\n4Gb RAM",
            "2 CPUs\n8Gb RAM",
            "2 CPUs\n16Gb RAM",
            "2 CPUs",
            "4 CPUs\n4Gb RAM",
            "4 CPUs\n16Gb RAM",
            "4 CPUs\n30Gb RAM",
            "4 CPUs"
            ))

plot(p1)
dev.off()
rm(p1)

consumo_cpu_bench$grp <- paste(consumo_cpu_bench$cpu,consumo_cpu_bench$ram)

tiff("consumo_cpu_benchmarks.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=consumo_cpu_bench, aes(x=grp, y=consumo_cpu, color=teste))+
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
        x="Configuração de CPU e Memória do contêiner",
        y="Consumo CPU (%)",
        color="Benchmark"
    )+
    scale_color_brewer(palette="Dark2")+
    scale_y_continuous(limits=c(50,210), breaks=seq(50,210,10))+
    scale_x_discrete(
        limits=c(
            "0.25cpu 0.5gb",
            "0.25cpu 1gb",
            "0.25cpu 2gb",
            "0.25cpu NA",
            "0.5cpu 1gb",
            "0.5cpu 2gb",
            "0.5cpu 3gb",
            "0.5cpu 4gb",
            "0.5cpu NA",
            "1cpu 2gb",
            "1cpu 4gb",
            "1cpu 8gb",
            "1cpu NA",
            "2cpu 4gb",
            "2cpu 8gb",
            "2cpu 16gb",
            "2cpu NA",
            "4cpu 4gb",
            "4cpu 16gb",
            "4cpu 32gb",
            "4cpu NA"
        ),
        labels=c(
            "0.25 CPUs\n0.5Gb RAM",
            "0.25 CPUs\n1Gb RAM",
            "0.25 CPUs\n2Gb RAM",
            "0.25 CPUs",
            "0.5 CPUs\n1Gb RAM",
            "0.5 CPUs\n2Gb RAM",
            "0.5 CPUs\n3Gb RAM",
            "0.5 CPUs\n4Gb RAM",
            "0.5 CPUs",
            "1 CPUs\n2Gb RAM",
            "1 CPUs\n4Gb RAM",
            "1 CPUs\n8Gb RAM",
            "1 CPUs",
            "2 CPUs\n4Gb RAM",
            "2 CPUs\n8Gb RAM",
            "2 CPUs\n16Gb RAM",
            "2 CPUs",
            "4 CPUs\n4Gb RAM",
            "4 CPUs\n16Gb RAM",
            "4 CPUs\n30Gb RAM",
            "4 CPUs"
        ))
plot(p2)
dev.off()
rm(p2)

#Convertendo e movendo os graficos para a respectiva pasta
system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("mv *.png graficos")
system("rm *.tiff")
