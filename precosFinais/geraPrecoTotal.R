packages <- c("ggplot2","dplyr","scales","ggsci","rjson", "gridExtra")

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

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("for f in *.tiff; do tiff2pdf -o ${f%.*}.pdf $f; done;")
system("rm *.tiff")
