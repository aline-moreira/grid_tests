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

times<- read.table("cenario1/benchmarks.time",header=TRUE,sep=";")
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
    energy$tempo >= times$start[times$ram==1] & energy$tempo <= times$end[times$ram==1]
)
stream1$ram <- 1
#q <- quantile(stream1$consumo, c(0.05, 0.95))
#stream1 <- stream1[stream1$consumo >= q[1] & stream1$consumo <= q[2], ]

#4
stream4 <- energy %>% filter(
    energy$tempo >= times$start[times$ram==4] & energy$tempo <= times$end[times$ram==4]
)
stream4$ram <- 4
q <- quantile(stream4$consumo, c(0.05, 0.95))
stream4 <- stream4[stream4$consumo >= q[1] & stream4$consumo <= q[2], ]

#16
stream16 <- energy %>% filter(
    energy$tempo >= times$start[times$ram==16] & energy$tempo <= times$end[times$ram==16]
)
stream16$ram <- 16
q <- quantile(stream16$consumo, c(0.05, 0.95))
stream16 <- stream16[stream16$consumo >= q[1] & stream16$consumo <= q[2], ]

#32
stream32 <- energy %>% filter(
    energy$tempo >= times$start[times$ram==32] & energy$tempo <= times$end[times$ram==32]
)
stream32$ram <- 32
q <- quantile(stream32$consumo, c(0.05, 0.95))
stream32 <- stream32[stream32$consumo >= q[1] & stream32$consumo <= q[2], ]

#256
stream256 <- energy %>% filter(
    energy$tempo >= times$start[times$ram==256] & energy$tempo <= times$end[times$ram==256]
)
stream256$ram <- 256
q <- quantile(stream256$consumo, c(0.05, 0.95))
stream256 <- stream256[stream256$consumo >= q[1] & stream256$consumo <= q[2], ]

#748
stream748 <- energy %>% filter(
    energy$tempo >= times$start[times$ram==748] & energy$tempo <= times$end[times$ram==748]
)
stream748$ram <- 758
q <- quantile(stream748$consumo, c(0.05, 0.95))
stream748 <- stream748[stream748$consumo >= q[1] & stream748$consumo <= q[2], ]

energy_stream <- rbind(stream1, stream4)
energy_stream <- rbind(energy_stream, stream16)
energy_stream <- rbind(energy_stream, stream32)
energy_stream <- rbind(energy_stream, stream256)
energy_stream <- rbind(energy_stream, stream748)

rm(stream1)
rm(stream4)
rm(stream16)
rm(stream32)
rm(stream256)
rm(stream748)

tiff("consumo_energia_stream.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')

p1 <- ggplot(data=energy_stream, aes(x=as.factor(ram), y=consumo, fill=as.factor(ram)))+
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
        x="Quantidade Memória",
        y="Consumo (W/s)",
        fill="Memória do contêiner"
    )+
    scale_y_continuous(limits=c(300,550), breaks=seq(0,550,50))

plot(p1)
dev.off()

rm(p1)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("mv *.png graficos")
system("rm *.tiff")
