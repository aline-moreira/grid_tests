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

times<- read.table("tempos_testes.times",header=TRUE,sep=";")
times$start <- as.POSIXct(times$start,tz="UTC",  format="%c")
times$end <- as.POSIXct(times$end,tz="UTC",  format="%c")

##########GETTING THE INFO OF JSON######################
energy <- fromJSON(file="energy_1922237.json")

energy$items[[1]]$timestamps = as.POSIXct(energy$items[[1]]$timestamps, origin="1970-01-01",tz="UTC")

time <- energy$items[[1]]$timestamps
values <- energy$items[[1]]$values

energy <- data.frame(time,values)

names(energy) <- c("tempo","consumo")

###################################################
# Filtrando a energia de acordo com os tempos
###################################################

stream4 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="STREAM" & times$cpu==4] & energy$tempo <= times$end[times$test=="STREAM"& times$cpu==4]
)
stream4$cpu <- 4
stream4$ram <- 32
stream4$test <- "stream"
q <- quantile(stream4$cpu, c(0.1, 0.9))
stream4 <- stream4[stream4$cpu >= q[1] & stream4$cpu <= q[2], ]


stream128 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="STREAM" & times$cpu==128] & energy$tempo <= times$end[times$test=="STREAM"& times$cpu==128]
)
stream128$cpu <- 128
stream128$ram <- 754
stream128$test <- "stream"
q <- quantile(stream128$cpu, c(0.1, 0.9))
stream128 <- stream128[stream128$cpu >= q[1] & stream128$cpu <= q[2], ]

stress4 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="STRESS" & times$cpu==4] & energy$tempo <= times$end[times$test=="STRESS"& times$cpu==4]
)
stress4$cpu <- 4
stress4$ram <- 32
stress4$test <- "stress"
q <- quantile(stress4$cpu, c(0.1, 0.9))
stress4 <- stress4[stress4$cpu >= q[1] & stress4$cpu <= q[2], ]

stress128 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="STRESS" & times$cpu==128] & energy$tempo <= times$end[times$test=="STRESS"& times$cpu==128]
)
stress128$cpu <- 128
stress128$ram <- 754
stress128$test <- "stress"
q <- quantile(stress128$cpu, c(0.1, 0.9))
stress128 <- stress128[stress128$cpu >= q[1] & stress128$cpu <= q[2], ]

dt_tests_4 <- rbind(stream4,stress4)
dt_tests_128 <- rbind(stream128,stress128)

diff <- data.frame(
    stream4=c("stream", 4, 32, mean(stream4$consumo)),
    stream128=c("stream", 128, 754, mean(stream128$consumo)),
    stress4=c("stress", 4, 32, mean(stress4$consumo)),
    stress128=c("stress", 128, 754, mean(stress128$consumo))
)

rownames(diff) <- c("teste","cpu","ram","consumo")
diff <- as.data.frame(t(diff))

diff$consumo <- as.numeric(levels(diff$consumo))

#rownames(diff) <- c("stream4","stream128","stress4","stress128")  
    
#diff$cpu <- as.numeric(diff$cpu)
#diff$consumo <- as.numeric(diff$consumo)

tiff("diff_4.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=dt_tests_4, aes(x=tempo, y=consumo, color=test))+
    geom_line()+
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
        x="Tempo",
        y="Consumo (W/s)",
        color= "Benchmark"
    )

plot(p1)
dev.off()


tiff("diff_128.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=dt_tests_128, aes(x=tempo, y=consumo, color=test))+
    geom_line()+
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
        x="Tempo",
        y="Consumo (W/s)",
        color= "Benchmark"
    )

plot(p2)
dev.off()

all <- rbind(dt_tests_4, dt_tests_128)

tiff("diff_all.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p3 <- ggplot(data=all, aes(x=as.factor(cpu), y=consumo, color=test))+
    geom_boxplot(outlier.colour="black", outlier.shape=16, outlier.size=2, notch=FALSE)+
    #geom_bar(stat = "identity", position=position_dodge())+
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
        x="CPU",
        y="Consumo (W/s)",
        color= "Benchmark"
    )+
    scale_y_continuous(limits=c(0,750), breaks=seq(0,750,50))
    
plot(p3)
dev.off()


rm(p1)
rm(p2)
rm(p3)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
#system("mv *.png graphs")
system("rm *.tiff")
