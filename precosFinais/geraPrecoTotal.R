packages <- c("ggplot2","dplyr","scales","ggsci","rjson", "gridExtra", "scales",
              "tibble", 'ggradar',"data.table","fmsb")

suppressPackageStartupMessages(library(dplyr))


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

valores<- read.table("tabelaCustos.csv",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(valores) <- c("benchmark","plataforma","v1","v2","v3","v4")

idle =  183 * 0.000000114 * 3600

sum_25_grid <- sum(valores$v1[valores$plataforma=="grid"]) + idle
sum_25_aws5 <- sum(valores$v1[valores$plataforma=="aws5"])
sum_25_aws10 <- sum(valores$v1[valores$plataforma=="aws10"])
sum_25_aws15 <- sum(valores$v1[valores$plataforma=="aws15"])
sum_50_grid <- sum(valores$v2[valores$plataforma=="grid"]) + idle
sum_50_aws5 <- sum(valores$v2[valores$plataforma=="aws5"])
sum_50_aws10 <- sum(valores$v2[valores$plataforma=="aws10"])
sum_50_aws15 <- sum(valores$v2[valores$plataforma=="aws15"])
sum_75_grid <- sum(valores$v3[valores$plataforma=="grid"]) + idle
sum_75_aws5 <- sum(valores$v3[valores$plataforma=="aws5"])
sum_75_aws10 <- sum(valores$v3[valores$plataforma=="aws10"])
sum_75_aws15 <- sum(valores$v3[valores$plataforma=="aws15"])
sum_100_grid <- sum(valores$v4[valores$plataforma=="grid"]) + idle
sum_100_aws5 <- sum(valores$v4[valores$plataforma=="aws5"])
sum_100_aws10 <- sum(valores$v4[valores$plataforma=="aws10"])
sum_100_aws15 <- sum(valores$v4[valores$plataforma=="aws15"])


valores<- read.table("tabelaCustosDolar.csv",header=FALSE,sep=";", stringsAsFactors = FALSE)
names(valores) <- c("benchmark","plataforma","v1","v2","v3","v4")

idle_dolar =  183 * 0.000000114/5.44 * 3600

sum_25_grid_dolar <- sum(valores$v1[valores$plataforma=="grid"]) + idle_dolar
sum_25_aws5_dolar <- sum(valores$v1[valores$plataforma=="aws5"])
sum_25_aws10_dolar <- sum(valores$v1[valores$plataforma=="aws10"])
sum_25_aws15_dolar <- sum(valores$v1[valores$plataforma=="aws15"])
sum_50_grid_dolar <- sum(valores$v2[valores$plataforma=="grid"]) + idle_dolar
sum_50_aws5_dolar <- sum(valores$v2[valores$plataforma=="aws5"])
sum_50_aws10_dolar <- sum(valores$v2[valores$plataforma=="aws10"])
sum_50_aws15_dolar <- sum(valores$v2[valores$plataforma=="aws15"])
sum_75_grid_dolar <- sum(valores$v3[valores$plataforma=="grid"]) + idle_dolar
sum_75_aws5_dolar <- sum(valores$v3[valores$plataforma=="aws5"])
sum_75_aws10_dolar <- sum(valores$v3[valores$plataforma=="aws10"])
sum_75_aws15_dolar <- sum(valores$v3[valores$plataforma=="aws15"])
sum_100_grid_dolar <- sum(valores$v4[valores$plataforma=="grid"]) + idle_dolar
sum_100_aws5_dolar <- sum(valores$v4[valores$plataforma=="aws5"])
sum_100_aws10_dolar <- sum(valores$v4[valores$plataforma=="aws10"])
sum_100_aws15_dolar <- sum(valores$v4[valores$plataforma=="aws15"])

spider_epcc_0 <- data.frame(c(idle_dolar,idle_dolar,idle_dolar,idle_dolar,idle_dolar))
rownames(spider_epcc_0) <- spider_epcc_0$benchmark
spider_epcc_0$benchmark <- NULL
colnames(spider_epcc_0) <- "0%"

spider_epcc_25 <- valores[valores$plataforma=="grid",c("benchmark","v1")]
rownames(spider_epcc_25) <- spider_epcc_25$benchmark
spider_epcc_25$benchmark <- NULL
colnames(spider_epcc_25) <- "25%"

spider_epcc_50 <- valores[valores$plataforma=="grid",c("benchmark","v2")]
rownames(spider_epcc_50) <- spider_epcc_50$benchmark
spider_epcc_50$benchmark <- NULL
colnames(spider_epcc_50) <- "50%"

spider_epcc_75 <- valores[valores$plataforma=="grid",c("benchmark","v3")]
rownames(spider_epcc_75) <- spider_epcc_75$benchmark
spider_epcc_75$benchmark <- NULL
colnames(spider_epcc_75) <- "75%"

spider_epcc_100 <- valores[valores$plataforma=="grid",c("benchmark","v4")]
rownames(spider_epcc_100) <- spider_epcc_100$benchmark
spider_epcc_100$benchmark <- NULL
colnames(spider_epcc_100) <- "100%"

spider_aws_5_0 <- data.frame(c(idle_dolar,idle_dolar,idle_dolar,idle_dolar,idle_dolar))
rownames(spider_aws_5_0) <- spider_aws_5_0$benchmark
spider_aws_5_0$benchmark <- NULL
colnames(spider_aws_5_0) <- "0%"

spider_aws_5_25 <- valores[valores$plataforma=="aws5",c("benchmark","v1")]
rownames(spider_aws_5_25) <- spider_aws_5_25$benchmark
spider_aws_5_25$benchmark <- NULL
colnames(spider_aws_5_25) <- "25%"

spider_aws_5_50 <- valores[valores$plataforma=="aws5",c("benchmark","v2")]
rownames(spider_aws_5_50) <- spider_aws_5_50$benchmark
spider_aws_5_50$benchmark <- NULL
colnames(spider_aws_5_50) <- "50%"

spider_aws_5_75 <- valores[valores$plataforma=="aws5",c("benchmark","v3")]
rownames(spider_aws_5_75) <- spider_aws_5_75$benchmark
spider_aws_5_75$benchmark <- NULL
colnames(spider_aws_5_75) <- "75%"

spider_aws_5_100 <- valores[valores$plataforma=="aws5",c("benchmark","v4")]
rownames(spider_aws_5_100) <- spider_aws_5_100$benchmark
spider_aws_5_100$benchmark <- NULL
colnames(spider_aws_5_100) <- "100%"

spider_aws_10_0 <- data.frame(c(idle_dolar,idle_dolar,idle_dolar,idle_dolar,idle_dolar))
rownames(spider_aws_10_0) <- spider_aws_10_0$benchmark
spider_aws_10_0$benchmark <- NULL
colnames(spider_aws_10_0) <- "0%"

spider_aws_10_25 <- valores[valores$plataforma=="aws10",c("benchmark","v1")]
rownames(spider_aws_10_25) <- spider_aws_10_25$benchmark
spider_aws_10_25$benchmark <- NULL
colnames(spider_aws_10_25) <- "25%"

spider_aws_10_50 <- valores[valores$plataforma=="aws10",c("benchmark","v2")]
rownames(spider_aws_10_50) <- spider_aws_10_50$benchmark
spider_aws_10_50$benchmark <- NULL
colnames(spider_aws_10_50) <- "50%"

spider_aws_10_75 <- valores[valores$plataforma=="aws10",c("benchmark","v3")]
rownames(spider_aws_10_75) <- spider_aws_10_75$benchmark
spider_aws_10_75$benchmark <- NULL
colnames(spider_aws_10_75) <- "75%"

spider_aws_10_100 <- valores[valores$plataforma=="aws10",c("benchmark","v4")]
rownames(spider_aws_10_100) <- spider_aws_10_100$benchmark
spider_aws_10_100$benchmark <- NULL
colnames(spider_aws_10_100) <- "100%"

spider_aws_15_0 <- data.frame(c(idle_dolar,idle_dolar,idle_dolar,idle_dolar,idle_dolar))
rownames(spider_aws_15_0) <- spider_aws_15_0$benchmark
spider_aws_15_0$benchmark <- NULL
colnames(spider_aws_15_0) <- "0%"

spider_aws_15_25 <- valores[valores$plataforma=="aws15",c("benchmark","v1")]
rownames(spider_aws_15_25) <- spider_aws_15_25$benchmark
spider_aws_15_25$benchmark <- NULL
colnames(spider_aws_15_25) <- "25%"

spider_aws_15_50 <- valores[valores$plataforma=="aws15",c("benchmark","v2")]
rownames(spider_aws_15_50) <- spider_aws_15_50$benchmark
spider_aws_15_50$benchmark <- NULL
colnames(spider_aws_15_50) <- "50%"

spider_aws_15_75 <- valores[valores$plataforma=="aws15",c("benchmark","v3")]
rownames(spider_aws_15_75) <- spider_aws_15_75$benchmark
spider_aws_15_75$benchmark <- NULL
colnames(spider_aws_15_75) <- "75%"

spider_aws_15_100 <- valores[valores$plataforma=="aws15",c("benchmark","v4")]
rownames(spider_aws_15_100) <- spider_aws_15_100$benchmark
spider_aws_15_100$benchmark <- NULL
colnames(spider_aws_15_100) <- "100%"


spider_0 <- data.frame(spider_epcc_0,spider_aws_5_0,spider_aws_10_0,spider_aws_15_0)
colnames(spider_0) <- c("EPCC","5% AWS","10% AWS", "15% AWS")
rownames(spider_0) <- rownames(spider_epcc_25)

spider_25 <- data.frame(spider_epcc_25,spider_aws_5_25,spider_aws_10_25,spider_aws_15_25)
colnames(spider_25) <- c("EPCC","5% AWS","10% AWS", "15% AWS")
rownames(spider_25) <- rownames(spider_epcc_25)

spider_50 <- data.frame(spider_epcc_50,spider_aws_5_50,spider_aws_10_50,spider_aws_15_50)
colnames(spider_50) <- c("EPCC","5% AWS","10% AWS", "15% AWS")
rownames(spider_50) <- rownames(spider_epcc_25)

spider_75 <- data.frame(spider_epcc_75,spider_aws_5_75,spider_aws_10_75,spider_aws_15_75)
colnames(spider_75) <- c("EPCC","5% AWS","10% AWS", "15% AWS")
rownames(spider_75) <- rownames(spider_epcc_25)

spider_100 <- data.frame(spider_epcc_100,spider_aws_5_100,spider_aws_10_100,spider_aws_15_100)
colnames(spider_100) <- c("EPCC","5% AWS","10% AWS", "15% AWS")
rownames(spider_100) <- rownames(spider_epcc_25)

spider_0['Storage','EPCC'] <- spider_100['Storage','EPCC'] * 0
spider_25['Storage','EPCC'] <- spider_100['Storage','EPCC'] *0.25
spider_50['Storage','EPCC'] <- spider_100['Storage','EPCC'] * 0.50
spider_75['Storage','EPCC'] <- spider_100['Storage','EPCC'] * 0.75

rm(spider_epcc_0)
rm(spider_epcc_25)
rm(spider_epcc_50)
rm(spider_epcc_75)
rm(spider_epcc_100)
rm(spider_aws_5_0)
rm(spider_aws_5_25)
rm(spider_aws_5_50)
rm(spider_aws_5_75)
rm(spider_aws_5_100)
rm(spider_aws_10_0)
rm(spider_aws_10_25)
rm(spider_aws_10_50)
rm(spider_aws_10_75)
rm(spider_aws_10_100)
rm(spider_aws_15_0)
rm(spider_aws_15_25)
rm(spider_aws_15_50)
rm(spider_aws_15_75)
rm(spider_aws_15_100)



somas <- data.frame(c(idle,sum_25_grid, sum_50_grid, sum_75_grid, sum_100_grid, 
                      0.088066667, sum_25_aws5, sum_50_aws5, sum_75_aws5, sum_100_aws5,
                      0.176133333, sum_25_aws10, sum_50_aws10, sum_75_aws10, sum_100_aws10,
                      0.2642, sum_25_aws15, sum_50_aws15, sum_75_aws15, sum_100_aws15
),
c("grid","grid","grid","grid","grid",
  "aws5","aws5","aws5","aws5","aws5",
  "aws10","aws10","aws10","aws10","aws10",
  "aws15","aws15","aws15","aws15","aws15"
),
c(0, 25,50,75,100)
)
names(somas) <- c("Preço","ModeloCusto","Utilizacao")

somas_dolar <- data.frame(c(idle_dolar,sum_25_grid_dolar, sum_50_grid_dolar, sum_75_grid_dolar, sum_100_grid_dolar, 
                      0.088066667/5.44, sum_25_aws5_dolar, sum_50_aws5_dolar, sum_75_aws5_dolar, sum_100_aws5_dolar,
                      0.176133333/5.44, sum_25_aws10_dolar, sum_50_aws10_dolar, sum_75_aws10_dolar, sum_100_aws10_dolar,
                      0.2642/5.44, sum_25_aws15_dolar, sum_50_aws15_dolar, sum_75_aws15_dolar, sum_100_aws15_dolar
),
c("grid","grid","grid","grid","grid",
  "aws5","aws5","aws5","aws5","aws5",
  "aws10","aws10","aws10","aws10","aws10",
  "aws15","aws15","aws15","aws15","aws15"
),
c(0, 25,50,75,100)
)
names(somas_dolar) <- c("Preço","ModeloCusto","Utilizacao")

tiff("precoFinal.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=somas, aes(x=Utilizacao, y=Preço, color = ModeloCusto ))+
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
        x="Utilização do servidor",
        y="Preço (R$/h)",
        color= "Modelo de Custo"
    )+
    scale_y_continuous(limits=c(0,1), breaks=seq(0.0,1,0.05))+
    scale_x_continuous(
        breaks=c(
            0,
            25,
            50,
            75,
            100
        ),
        labels=c(
            "Idle",
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

tiff("precoFinal_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=somas_dolar, aes(x=Utilizacao, y=Preço, color = ModeloCusto ))+
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
        x="Server Utility",
        y="Price (US$/h)",
        color= "Price Model"
    )+
    scale_y_continuous(limits=c(0,0.2), breaks=seq(0.0,0.2,0.01))+
    scale_x_continuous(
        breaks=c(
            0,
            25,
            50,
            75,
            100
        ),
        labels=c(
            "Idle",
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


colors_border=c( rgb(255/255,89/255,89/255,1), 
                 rgb(250/255, 207/255, 90/255, 1),
                 rgb(73/255,190/255,183/255,1), 
                 rgb(8/255,95/255,99/255,1),
                 rgb(0/255,0/255,153/255,1))

colors_in=c( rgb(255/255,89/255,89/255,0.4),
             rgb(250/255, 207/255, 90/255, 0.4),
             rgb(73/255,190/255,183/255,0.4),
             rgb(8/255,95/255,99/255,0.4),
             rgb(0/255,0/255,153/255,0.4))
# ------------------------
t_spider_0 <- transpose(spider_0)
colnames(t_spider_0) <- rownames(spider_0)
rownames(t_spider_0) <- colnames(spider_0)

t_spider_0 %>%
  mutate_at(vars(),funs(rescale)) -> spider_0_radar

rownames(spider_0_radar) <- rownames(t_spider_0)

spider_0_radar$group <- NULL

tiff("radar0.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
r1 <- radarchart(spider_0_radar, axistype=1 , maxmin=F,
                 #custom polygon
                 pcol=colors_border ,  plwd=2 , plty=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, caxislabels=seq(0,100,25), 
                 #custom labels
                 vlcex=0.8 
)
legend("bottom", legend = rownames(spider_0_radar), inset=c(0,1), xpd=TRUE, horiz=TRUE, pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=3)
dev.off()
#---------------------------------------
#------------------------
t_spider_25 <- transpose(spider_25)
colnames(t_spider_25) <- rownames(spider_25)
rownames(t_spider_25) <- colnames(spider_25)

t_spider_25 %>%
  mutate_at(vars(),funs(rescale)) -> spider_25_radar

rownames(spider_25_radar) <- rownames(t_spider_25)

spider_25_radar$group <- NULL

tiff("radar25.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
r2 <- radarchart(spider_25_radar, axistype=1 , maxmin=F,
                 #custom polygon
                 pcol=colors_border ,  plwd=2 , plty=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, caxislabels=seq(0,100,25), 
                 #custom labels
                 vlcex=0.8 
)
legend("bottom", legend = rownames(spider_25_radar), inset=c(0,1), xpd=TRUE, horiz=TRUE, pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=3)
#plot(r2)
dev.off()
#---------------------------------------
#------------------------
t_spider_50 <- transpose(spider_50)
colnames(t_spider_50) <- rownames(spider_50)
rownames(t_spider_50) <- colnames(spider_50)

t_spider_50 %>%
  mutate_at(vars(),funs(rescale)) -> spider_50_radar

rownames(spider_50_radar) <- rownames(t_spider_50)

spider_50_radar$group <- NULL
tiff("radar50.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
r3 <- radarchart(spider_50_radar, axistype=1 , maxmin=F,
                 #custom polygon
                 pcol=colors_border ,  plwd=2 , plty=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, caxislabels=seq(0,100,25), 
                 #custom labels
                 vlcex=0.8 
)
legend("bottom", legend = rownames(spider_50_radar), inset=c(0,1), xpd=TRUE, horiz=TRUE, pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=3)
#plot(r3)
dev.off()
#---------------------------------------
#------------------------
t_spider_75 <- transpose(spider_75)
colnames(t_spider_75) <- rownames(spider_75)
rownames(t_spider_75) <- colnames(spider_75)

t_spider_75 %>%
  mutate_at(vars(),funs(rescale)) -> spider_75_radar

rownames(spider_75_radar) <- rownames(t_spider_75)

spider_75_radar$group <- NULL
tiff("radar75.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
r4 <- radarchart(spider_75_radar, axistype=1 , maxmin=F,
                 #custom polygon
                 pcol=colors_border ,  plwd=2 , plty=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, caxislabels=seq(0,100,25), 
                 #custom labels
                 vlcex=0.8 
)
legend("bottom", legend = rownames(spider_75_radar), inset=c(0,1), xpd=TRUE, horiz=TRUE, pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=3)

dev.off()
#---------------------------------------
#------------------------
t_spider_100 <- transpose(spider_100)
colnames(t_spider_100) <- rownames(spider_100)
rownames(t_spider_100) <- colnames(spider_100)

t_spider_100 %>%
  mutate_at(vars(),funs(rescale)) -> spider_100_radar

rownames(spider_100_radar) <- rownames(t_spider_100)

spider_100_radar$group <- NULL
tiff("radar100.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
r5 <- radarchart(spider_100_radar, axistype=1 , maxmin=F,
                 #custom polygon
                 pcol=colors_border ,  plwd=2 , plty=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, caxislabels=seq(0,100,25), 
                 #custom labels
                 vlcex=0.8 
)
legend("bottom", legend = rownames(spider_100_radar), inset=c(0,1), xpd=TRUE, horiz=TRUE, pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=3)
#plot(r5)
dev.off()
#---------------------------------------
#------------------------
spider_total <- data.frame(somas_dolar$Preço[somas_dolar$ModeloCusto=="grid"],
                           somas_dolar$Preço[somas_dolar$ModeloCusto=="aws5"],
                           somas_dolar$Preço[somas_dolar$ModeloCusto=="aws10"],
                           somas_dolar$Preço[somas_dolar$ModeloCusto=="aws15"]
                           )
colnames(spider_total) <- c("EPCC","5% AWS","10% AWS","15% AWS")
rownames(spider_total) <- c("0%","25%","50%","75%","100%")


t_spider_total <- transpose(spider_total)
colnames(t_spider_total) <- rownames(spider_total)
rownames(t_spider_total) <- colnames(spider_total)

t_spider_total %>%
  mutate_at(vars(),funs(rescale)) -> spider_total_radar

rownames(spider_total_radar) <- rownames(t_spider_total)

spider_total_radar$group <- NULL
tiff("radarTotal.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
r6 <- radarchart(spider_total_radar, axistype=1 , maxmin=F,
                 #custom polygon
                 pcol=colors_border ,  plwd=2 , plty=1, 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.8, caxislabels=seq(0,100,25),

                 #custom labels
                 vlcex=1 
)
legend("bottom", legend = rownames(spider_total_radar), inset=c(0,1), xpd=TRUE, horiz=TRUE, pch=20 , col=colors_border , text.col = "black", cex=1, pt.cex=3)
#plot(r5)
dev.off()
#---------------------------------------

rm(r1)
rm(r2)
rm(r3)
rm(r4)
rm(r5)
rm(r6)

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.png; do convert $f ${f%.*}.pdf; done;")
system("rm *.tiff")
