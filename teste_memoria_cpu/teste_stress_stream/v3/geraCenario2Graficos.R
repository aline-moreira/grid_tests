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

times<- read.table("cenario2/benchmarks.time",header=TRUE,sep=";")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%c")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%c")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1922343.json")

energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values

energy <- data.frame(time,values)

names(energy) <- c("tempo","consumo")

###################################################
# Filtrando a energia de acordo com os tempos
###################################################

#1
stream1 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==1] &
        energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==1]
)
stream1$cpu <- 1
stream1$teste <- "stream"
#q <- quantile(stream1$consumo, c(0.05, 0.95))
#stream1 <- stream1[stream1$consumo >= q[1] & stream1$consumo <= q[2], ]

stress1 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==1] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==1]
)
stress1$cpu <- 1
stress1$teste <- "stress"
q <- quantile(stress1$consumo, c(0.05, 0.95))
stress1 <- stress1[stress1$consumo >= q[1] & stress1$consumo <= q[2], ]

#2
stream2 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==2] &
        energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==2]
)
stream2$cpu <- 2
stream2$teste <- "stream"
#q <- quantile(stream2$consumo, c(0.05, 0.95))
#stream2 <- stream2[stream2$consumo >= q[1] & stream2$consumo <= q[2], ]

stress2 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==2] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==2]
)
stress2$cpu <- 2
stress2$teste <- "stress"
q <- quantile(stress2$consumo, c(0.05, 0.95))
stress2 <- stress2[stress2$consumo >= q[1] & stress2$consumo <= q[2], ]

#3
stream3 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==3] &
        energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==3]
)
stream3$cpu <- 3
stream3$teste <- "stream"
#q <- quantile(stream3$consumo, c(0.05, 0.95))
#stream3 <- stream3[stream3$consumo >= q[1] & stream3$consumo <= q[2], ]

stress3 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==3] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==3]
)
stress3$cpu <- 3
stress3$teste <- "stress"
q <- quantile(stress3$consumo, c(0.05, 0.95))
stress3 <- stress3[stress3$consumo >= q[1] & stress3$consumo <= q[2], ]

#4
stream4 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==4] &
        energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==4]
)
stream4$cpu <- 4
stream4$teste <- "stream"
#q <- quantile(stream4$consumo, c(0.05, 0.95))
#stream4 <- stream4[stream4$consumo >= q[1] & stream4$consumo <= q[2], ]

stress4 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==4] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==4]
)
stress4$cpu <- 4
stress4$teste <- "stress"
q <- quantile(stress4$consumo, c(0.05, 0.95))
stress4 <- stress4[stress4$consumo >= q[1] & stress4$consumo <= q[2], ]

#32
stream32 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==32] &
        energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==32]
)
stream32$cpu <- 32
stream32$teste <- "stream"
#q <- quantile(stream32$consumo, c(0.05, 0.95))
#stream32 <- stream32[stream32$consumo >= q[1] & stream32$consumo <= q[2], ]

stress32 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==32] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==32]
)
stress32$cpu <- 32
stress32$teste <- "stress"
q <- quantile(stress32$consumo, c(0.05, 0.95))
stress32 <- stress32[stress32$consumo >= q[1] & stress32$consumo <= q[2], ]

#64
stream64 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==64] &
        energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==64]
)
stream64$cpu <- 64
stream64$teste <- "stream"
#q <- quantile(stream64$consumo, c(0.05, 0.95))
#stream64 <- stream64[stream64$consumo >= q[1] & stream64$consumo <= q[2], ]

stress64 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==64] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==64]
)
stress64$cpu <- 64
stress64$teste <- "stress"
q <- quantile(stress64$consumo, c(0.05, 0.95))
stress64 <- stress64[stress64$consumo >= q[1] & stress64$consumo <= q[2], ]

#128
stream128 <- energy %>% filter(
energy$tempo >= times$start[times$teste=="STREAM" & times$cpu==128] &
    energy$tempo <= times$end[times$teste=="STREAM" & times$cpu==128]
)
stream128$cpu <- 128
stream128$teste <- "stream"
#q <- quantile(stream128$consumo, c(0.05, 0.95))
#stream128 <- stream128[stream128$consumo >= q[1] & stream128$consumo <= q[2], ]

stress128 <- energy %>% filter(
    energy$tempo >= times$start[times$teste=="STRESS" & times$cpu==128] &
        energy$tempo <= times$end[times$teste=="STRESS" & times$cpu==128]
)
stress128$cpu <- 128
stress128$teste <- "stress"
q <- quantile(stress128$consumo, c(0.05, 0.95))
stress128 <- stress128[stress128$consumo >= q[1] & stress128$consumo <= q[2], ]

#Juntando os scripts
energy_bench <- rbind(stream1, stream2)
energy_bench <- rbind(energy_bench, stream3)
energy_bench <- rbind(energy_bench, stream4)
energy_bench <- rbind(energy_bench, stream32)
energy_bench <- rbind(energy_bench, stream64)
energy_bench <- rbind(energy_bench, stream128)

energy_bench <- rbind(energy_bench, stress1)
energy_bench <- rbind(energy_bench, stress2)
energy_bench <- rbind(energy_bench, stress3)
energy_bench <- rbind(energy_bench, stress4)
energy_bench <- rbind(energy_bench, stress32)
energy_bench <- rbind(energy_bench, stress64)
energy_bench <- rbind(energy_bench, stress128)

rm(stream1)
rm(stream2)
rm(stream3)
rm(stream4)
rm(stream32)
rm(stream64)
rm(stream128)

rm(stress1)
rm(stress2)
rm(stress3)
rm(stress4)
rm(stress32)
rm(stress64)
rm(stress128)

tiff("consumo_energia_benchmarks.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')

p1 <- ggplot(data=energy_bench, aes(x=as.factor(cpu), y=consumo, fill=teste))+
    geom_boxplot()+
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
        x="Quantidade CPU",
        y="Consumo (W/s)",
        fill="CPUs contêiner"
    )+
    scale_y_continuous(limits=c(0,900), breaks=seq(0,900,50))
    #ylim(0,1000)

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("mv *.png graficos")
system("rm *.tiff")
