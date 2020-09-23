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

# Combinando testes
dt_tests <- rbind(pod_8192, pod_16384)
dt_tests <- rbind(dt_tests, pod_32768)

somaGrid_0 <- 183  * 0.000000114 * 3600
somaGrid_1 <- mean(pod_8192$consumo) * 0.000000114 * 3600 - somaGrid_0
somaGrid_2 <- mean(pod_16384$consumo) * 0.000000114 * 3600 - somaGrid_0
somaGrid_3 <- 394 * 0.000000114 * 3600 - somaGrid_0
somaGrid_4 <- mean(pod_32768$consumo) * 0.000000114 * 3600 - somaGrid_0

somaGrid_dolar_0 <- 183  * 0.000000114 / 5.44 * 3600
somaGrid_dolar_1 <- mean(pod_8192$consumo) * 0.000000114 / 5.44 * 3600 - somaGrid_dolar_0
somaGrid_dolar_2 <- mean(pod_16384$consumo) * 0.000000114 / 5.44 * 3600 - somaGrid_dolar_0
somaGrid_dolar_3 <- 394 * 0.000000114 / 5.44 * 3600 - somaGrid_dolar_0
somaGrid_dolar_4 <- mean(pod_32768$consumo) * 0.000000114 / 5.44 * 3600 - somaGrid_dolar_0

somas <- c(somaGrid_1, somaGrid_2, somaGrid_3, somaGrid_4, 
           0.0272,0.0272,0.0272,0.0272,
           0.0544,0.0544,0.0544,0.0544,
           0.0816, 0.0816, 0.0816, 0.0816
)

somas_dolar <- c(somaGrid_dolar_1, somaGrid_dolar_2, somaGrid_dolar_3, somaGrid_dolar_4, 
            0.0272 / 5.44,0.0272 / 5.44,0.0272 / 5.44,0.0272 / 5.44,
            0.0544 / 5.44,0.0544 / 5.44,0.0544 / 5.44,0.0544 / 5.44,
            0.0816 / 5.44,0.0816 / 5.44,0.0816 / 5.44,0.0816 / 5.44
)

somas <- data.frame(somas, c("grid","grid","grid","grid",
                             "aws5","aws5","aws5","aws5",
                             "aws10","aws10","aws10","aws10",
                             "aws15","aws15","aws15","aws15"
), 
                    c(8,16,24,32,
                      8,16,24,32,
                      8,16,24,32,
                      8,16,24,32
                      ))
somas_dolar <- data.frame(somas_dolar, c("grid","grid","grid","grid",
                             "aws5","aws5","aws5","aws5",
                             "aws10","aws10","aws10","aws10",
                             "aws15","aws15","aws15","aws15"
                        ), 
                        c(8,16,24,32,
                          8,16,24,32,
                          8,16,24,32,
                          8,16,24,32
                        ))
names(somas) <- c("Preço","ModeloCusto","ram")
names(somas_dolar) <- c("Preço","ModeloCusto","ram")

tiff("precoKubernetes.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p1 <- ggplot(data=somas, aes(x=ram, y=Preço, color = ModeloCusto ))+
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
        x=" Quantidade de Contêineres",
        y="Preço (R$/h)",
        color= "Modelo de Custo"
    )+
    scale_y_continuous(limits=c(0.025,0.09), breaks=seq(0.025,0.09,0.005))+
    scale_x_continuous(
        breaks=c(
            8,
            16,
            24,
            32
        ),
        labels=c(
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
tiff("precoKubernetes_en.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
p2 <- ggplot(data=somas_dolar, aes(x=ram, y=Preço, color = ModeloCusto ))+
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
        x="#Containers",
        y="Price (US$/h)",
        color= "Price Model"
    )+
    scale_y_continuous(limits=c(0.004,0.018), breaks=seq(0.004,0.018,0.001))+
    scale_x_continuous(
        breaks=c(
            8,
            16,
            24,
            32
        ),
        labels=c(
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
system("rm *.tiff")
