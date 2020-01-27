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

times<- read.table("all_tests.log",header=TRUE,sep=";")
times$start <- as.POSIXct(times$start,tz="UTC", format="%c")
times$end <- as.POSIXct(times$end,tz="UTC", format="%c")

scaleFUN <- function(x) round(as.numeric(x), digits=0)

##########GETTING THE INFO OF JSON######################
energy_host_docker <- fromJSON(file="energy_host_docker.json")
energy_vm <- fromJSON(file="energy_vm.json")
energy_rede_host_docker <- fromJSON(file="energy_rede_host_docker.json")
energy_rede_vm <- fromJSON(file="energy_rede_vm_e_dockerVM.json")

energy_host_docker <- as.data.frame(energy_host_docker)
energy_vm <- as.data.frame(energy_vm)
energy_rede_host_docker <- as.data.frame(energy_rede_host_docker)
energy_rede_vm <- as.data.frame(energy_rede_vm)

##EXECUTANDO COM HOST E DOCKER
energy_host_docker$items.timestamps = as.POSIXct(energy_host_docker$items.timestamps, origin="1970-01-01",tz="UTC")

energy_vm$items.timestamps = as.POSIXct(energy_vm$items.timestamps, origin="1970-01-01",tz="UTC")
energy_vm$items.timestamps.1 = as.POSIXct(energy_vm$items.timestamps.1, origin="1970-01-01",tz="UTC")

energy_rede_host_docker$items.timestamps = as.POSIXct(energy_rede_host_docker$items.timestamps, origin="1970-01-01",tz="UTC")
energy_rede_host_docker$items.timestamps.1 = as.POSIXct(energy_rede_host_docker$items.timestamps.1, origin="1970-01-01",tz="UTC")

energy_rede_vm$items.timestamps = as.POSIXct(energy_rede_vm$items.timestamps, origin="1970-01-01",tz="UTC")
energy_rede_vm$items.timestamps.1 = as.POSIXct(energy_rede_vm$items.timestamps.1, origin="1970-01-01",tz="UTC")

energy_host_docker <- energy_host_docker %>% select(items.timestamps,items.values)
energy_host_docker$items.values.1 = NA

energy_vm <- energy_vm %>% select(items.timestamps,items.values, items.values.1)

energy_rede_host_docker <- energy_rede_host_docker %>% select(items.timestamps,items.values, items.values.1)

energy_rede_vm <- energy_rede_vm %>% select(items.timestamps,items.values, items.values.1)

energy <- rbind(energy_host_docker, energy_vm)
energy <- rbind(energy, energy_rede_host_docker)
energy <- rbind(energy, energy_rede_vm)

names(energy) <- c("tempo","servidor1","servidor2")

rm(energy_rede_host_docker)
rm(energy_rede_vm)
rm(energy_vm)
rm(energy_host_docker)
###################################################
# IDLE
###################################################

idle_h <- energy %>% filter(
    energy$tempo >= times$start[times$test=="idle" & times$type=="host"] & energy$tempo <= times$end[times$test=="idle"& times$type=="host"] 
)
idle_h$type <- "host"
idle_h$value <- times$value[times$test=="idle" & times$type=="host"]

idle_d <- energy %>% filter(
    energy$tempo >= times$start[times$test=="idle" & times$type=="docker" ] & energy$tempo <= times$end[times$test=="idle" & times$type=="docker"]
)
idle_d$type <- "docker"
idle_d$value <- times$value[times$test=="idle" & times$type=="docker"]

idle_vm <- energy %>% filter(
    energy$tempo >= times$start[times$test=="idle" & times$type=="MV"] & energy$tempo <= times$end[times$test=="idle" & times$type=="MV"]
)
idle_vm$type <- "MV"
idle_vm$value <- times$value[times$test=="idle" & times$type=="MV"]

idle_dvm <- energy %>% filter(
    energy$tempo >= times$start[times$test=="idle" & times$type=="dockerMV"] & energy$tempo <= times$end[times$test=="idle" & times$type=="dockerMV"]
)
idle_dvm$type <- "dockerMV"
idle_dvm$value <- times$value[times$test=="idle" & times$type=="dockerMV"]

# idle_h <- idle_h[idle_h$servidor1 >= quantile(idle_h$servidor1,0.1) & idle_h$servidor1 <= quantile(idle_h$servidor1,0.9),]
# 
# idle_d <- idle_d[idle_d$servidor1 >= quantile(idle_d$servidor1,0.1) & idle_d$servidor1 <= quantile(idle_d$servidor1,0.9),]
# 
# idle_vm <- idle_vm[idle_vm$servidor1 >= quantile(idle_vm$servidor1,0.1) & idle_vm$servidor1 <= quantile(idle_vm$servidor1,0.9),]
# 
# idle_dvm <- idle_dvm[idle_dvm$servidor1 >= quantile(idle_dvm$servidor1,0.1) & idle_dvm$servidor1 <= quantile(idle_dvm$servidor1,0.9),]

idle <- as.data.frame(t(data.frame(
    c("host",scaleFUN(mean(idle_h$servidor1))),
    c("docker",scaleFUN(mean(idle_d$servidor1))),
    c("MV",scaleFUN(mean(idle_vm$servidor1))),
    c("dockerMV",scaleFUN(mean(idle_dvm$servidor1)))
)))
colnames(idle)<- c("type","power")
rownames(idle)<- c("host","docker","MV","dockerMV")

tiff("idle.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
idle_plot <- ggplot(data=idle, aes(x=type, y=power, fill=type))+
    geom_bar(colour="black",stat="identity")+
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
        x=("Ambiente"),
        y="Consumo (W/s)",
        fill= "Ambiente"
    )+
    #guides(fill=FALSE)
    scale_x_discrete(labels=c("Docker","Docker sobre MV","Host","MV"))+
    scale_fill_d3(labels = c("Docker", "Docker sobre MV", "Host", "MV"))
plot(idle_plot)
dev.off()
###################################################
# CPU
###################################################
#############
d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="1"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="1"] 
)
d_1$type <- "host"
d_1$value <- 1

d_2 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="2"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="2"] 
)
d_2$type <- "host"
d_2$value <- 2

d_4 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="4"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="4"] 
)
d_4$type <- "host"
d_4$value <- 4

d_8 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="8"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="8"] 
)
d_8$type <- "host"
d_8$value <- 8

d_16 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="16"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="16"] 
)
d_16$type <- "host"
d_16$value <- 16

d_32 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="32"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="32"] 
)
d_32$type <- "host"
d_32$value <- 32

d_64 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="64"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="64"] 
)
d_64$type <- "host"
d_64$value <- 64

d_80 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="host" & times$value=="80"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="host" & times$value=="80"] 
)
d_80$type <- "host"
d_80$value <- 80

cpu_host <- rbind(d_1, d_2)
cpu_host <- rbind(cpu_host, d_4)
cpu_host <- rbind(cpu_host, d_8)
cpu_host <- rbind(cpu_host, d_16)
cpu_host <- rbind(cpu_host, d_32)
cpu_host <- rbind(cpu_host, d_64)
cpu_host <- rbind(cpu_host, d_80)

##################################
#############
d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="1"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="1"] 
)
d_1$type <- "docker"
d_1$value <- 1

d_2 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="2"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="2"] 
)
d_2$type <- "docker"
d_2$value <- 2

d_4 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="4"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="4"] 
)
d_4$type <- "docker"
d_4$value <- 4

d_8 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="8"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="8"] 
)
d_8$type <- "docker"
d_8$value <- 8

d_16 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="16"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="16"] 
)
d_16$type <- "docker"
d_16$value <- 16

d_32 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="32"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="32"] 
)
d_32$type <- "docker"
d_32$value <- 32

d_64 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="64"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="64"] 
)
d_64$type <- "docker"
d_64$value <- 64

d_80 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="docker" & times$value=="80"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="docker" & times$value=="80"] 
)
d_80$type <- "docker"
d_80$value <- 80

cpu_docker <- rbind(d_1, d_2)
cpu_docker <- rbind(cpu_docker, d_4)
cpu_docker <- rbind(cpu_docker, d_8)
cpu_docker <- rbind(cpu_docker, d_16)
cpu_docker <- rbind(cpu_docker, d_32)
cpu_docker <- rbind(cpu_docker, d_64)
cpu_docker <- rbind(cpu_docker, d_80)
##################################

#############
d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="1"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="1"] 
)
d_1$type <- "MV"
d_1$value <- 1

d_2 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="2"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="2"] 
)
d_2$type <- "MV"
d_2$value <- 2

d_4 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="4"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="4"] 
)
d_4$type <- "MV"
d_4$value <- 4

d_8 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="8"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="8"] 
)
d_8$type <- "MV"
d_8$value <- 8

d_16 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="16"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="16"] 
)
d_16$type <- "MV"
d_16$value <- 16

d_32 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="32"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="32"] 
)
d_32$type <- "MV"
d_32$value <- 32

d_64 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="64"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="64"] 
)
d_64$type <- "MV"
d_64$value <- 64

d_80 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="MV" & times$value=="80"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="MV" & times$value=="80"] 
)
d_80$type <- "MV"
d_80$value <- 80

cpu_MV<- rbind(d_1, d_2)
cpu_MV <- rbind(cpu_MV, d_4)
cpu_MV <- rbind(cpu_MV, d_8)
cpu_MV <- rbind(cpu_MV, d_16)
cpu_MV <- rbind(cpu_MV, d_32)
cpu_MV <- rbind(cpu_MV, d_64)
cpu_MV <- rbind(cpu_MV, d_80)
##################################
#############
d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="1"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="1"] 
)
d_1$type <- "dockerMV"
d_1$value <- 1

d_2 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="2"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="2"] 
)
d_2$type <- "dockerMV"
d_2$value <- 2

d_4 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="4"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="4"] 
)
d_4$type <- "dockerMV"
d_4$value <- 4

d_8 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="8"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="8"] 
)
d_8$type <- "dockerMV"
d_8$value <- 8

d_16 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="16"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="16"] 
)
d_16$type <- "dockerMV"
d_16$value <- 16

d_32 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="32"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="32"] 
)
d_32$type <- "dockerMV"
d_32$value <- 32

d_64 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="64"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="64"] 
)
d_64$type <- "dockerMV"
d_64$value <- 64

d_80 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="cpu" & times$type=="dockerMV" & times$value=="80"] & energy$tempo <= times$end[times$test=="cpu" & times$type=="dockerMV" & times$value=="80"] 
)
d_80$type <- "dockerMV"
d_80$value <- 80

cpu_dockerMV<- rbind(d_1, d_2)
cpu_dockerMV <- rbind(cpu_dockerMV, d_4)
cpu_dockerMV <- rbind(cpu_dockerMV, d_8)
cpu_dockerMV <- rbind(cpu_dockerMV, d_16)
cpu_dockerMV <- rbind(cpu_dockerMV, d_32)
cpu_dockerMV <- rbind(cpu_dockerMV, d_64)
cpu_dockerMV <- rbind(cpu_dockerMV, d_80)
##########

cpu <- as.data.frame(t(data.frame(
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==1])),1),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==2])),2),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==4])),4),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==8])),8),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==16])),16),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==32])),32),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==64])),64),
    c("host",scaleFUN(mean(cpu_host$servidor1[cpu_host$value==80])),80),
    
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==1])),1),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==2])),2),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==4])),4),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==8])),8),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==16])),16),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==32])),32),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==64])),64),
    c("docker",scaleFUN(mean(cpu_docker$servidor1[cpu_docker$value==80])),80),
    
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==1])),1),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==2])),2),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==4])),4),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==8])),8),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==16])),16),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==32])),32),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==64])),64),
    c("MV",scaleFUN(mean(cpu_MV$servidor2[cpu_MV$value==80])),80),
    
    
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==1])),1),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==2])),2),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==4])),4),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==8])),8),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==16])),16),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==32])),32),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==64])),64),
    c("dockerMV",scaleFUN(mean(cpu_dockerMV$servidor2[cpu_dockerMV$value==80])),80)
)))
colnames(cpu)<- c("type","power","cpu")

tiff("cpu.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
cpu_plot <- ggplot(data=cpu, aes(x=cpu, y=power, fill=type))+
    geom_bar(colour="black",position="dodge",stat="identity")+
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
        x=("vCPU (unidades)"),
        y="Consumo (W/s)",
        fill= "Ambiente"
    )+
    scale_x_discrete(limits=c("1","2","4","8","16","32","64","80"))+
    scale_fill_d3(labels = c("Docker", "Docker sobre MV", "Host", "MV"))
    
plot(cpu_plot)
dev.off()

###################################################
# RAM
###################################################

mem_h <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Memoria" & times$type=="host"] & energy$tempo <= times$end[times$test=="Memoria" & times$type=="host"] 
)
mem_h$type <- "host"
mem_h$value <- times$value[times$test=="Memoria" & times$type=="host"]

mem_d <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Memoria" & times$type=="docker"] & energy$tempo <= times$end[times$test=="Memoria" & times$type=="docker"]
)
mem_d$type <- "docker"
mem_d$value <- times$value[times$test=="Memoria" & times$type=="docker"]

mem_vm <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Memoria" & times$type=="MV"] & energy$tempo <= times$end[times$test=="Memoria" & times$type=="MV"]
)
mem_vm$type <- "MV"
mem_vm$value <- times$value[times$test=="Memoria" & times$type=="MV"]

mem_dvm <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Memoria" & times$type=="dockerMV"] & energy$tempo <= times$end[times$test=="Memoria" & times$type=="dockerMV"]
)
mem_dvm$type <- "dockerMV"
mem_dvm$value <- times$value[times$test=="Memoria" & times$type=="dockerMV"]

# mem_h <- mem_h[mem_h$servidor1 >= quantile(mem_h$servidor1,0.1) & mem_h$servidor1 <= quantile(mem_h$servidor1,0.9),]
# 
# mem_d <- mem_d[mem_d$servidor1 >= quantile(mem_d$servidor1,0.1) & mem_d$servidor1 <= quantile(mem_d$servidor1,0.9),]
# 
# mem_vm <- mem_vm[mem_vm$servidor1 >= quantile(mem_vm$servidor1,0.1) & mem_vm$servidor1 <= quantile(mem_vm$servidor1,0.9),]
# 
# mem_dvm <- mem_dvm[mem_dvm$servidor1 >= quantile(mem_dvm$servidor1,0.1) & mem_dvm$servidor1 <= quantile(mem_dvm$servidor1,0.9),]

mem <- as.data.frame(t(data.frame(
    c("host",scaleFUN(mean(mem_h$servidor1))),
    c("docker",scaleFUN(mean(mem_d$servidor1))),
    c("MV",scaleFUN(mean(mem_vm$servidor2))),
    c("dockerMV",scaleFUN(mean(mem_dvm$servidor2)))
)))
colnames(mem)<- c("type","power")
rownames(mem)<- c("host","docker","MV","dockerMV")

tiff("mem.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')

mem_plot <- ggplot(data=mem, aes(x=type, y=power, fill=type))+
    geom_bar(colour="black",stat="identity")+
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
        x=("Ambiente"),
        y="Consumo (W/s)",
        fill= "Ambiente"
    )+
    #guides(fill=FALSE)
    scale_x_discrete(labels=c("Docker","Docker sobre MV","Host","MV"))+
    scale_fill_d3(labels = c("Docker", "Docker sobre MV", "Host", "MV"))

plot(mem_plot)
dev.off()
###################################################
# IO
###################################################

io_h <- energy %>% filter(
    energy$tempo >= times$start[times$test=="IO" & times$type=="host"] & energy$tempo <= times$end[times$test=="IO" & times$type=="host"] 
)
io_h$type <- "host"
io_h$value <- times$value[times$test=="IO" & times$type=="host"]

io_d <- energy %>% filter(
    energy$tempo >= times$start[times$test=="IO" & times$type=="docker"] & energy$tempo <= times$end[times$test=="IO" & times$type=="docker"]
)
io_d$type <- "docker"
io_d$value <- times$value[times$test=="IO" & times$type=="docker"]

io_vm <- energy %>% filter(
    energy$tempo >= times$start[times$test=="IO" & times$type=="MV"] & energy$tempo <= times$end[times$test=="IO" & times$type=="MV"]
)
io_vm$type <- "MV"
io_vm$value <- times$value[times$test=="IO" & times$type=="MV"]

io_dvm <- energy %>% filter(
    energy$tempo >= times$start[times$test=="IO" & times$type=="dockerMV"] & energy$tempo <= times$end[times$test=="IO" & times$type=="dockerMV"]
)
io_dvm$type <- "dockerMV"
io_dvm$value <- times$value[times$test=="IO" & times$type=="dockerMV"]

# io_h <- io_h[io_h$servidor1 >= quantile(io_h$servidor1,0.1) & io_h$servidor1 <= quantile(io_h$servidor1,0.9),]
# 
# io_d <- io_d[io_d$servidor1 >= quantile(io_d$servidor1,0.1) & io_d$servidor1 <= quantile(io_d$servidor1,0.9),]
# 
# io_vm <- io_vm[io_vm$servidor1 >= quantile(io_vm$servidor1,0.1) & io_vm$servidor1 <= quantile(io_vm$servidor1,0.9),]
# 
# io_dvm <- io_dvm[io_dvm$servidor1 >= quantile(io_dvm$servidor1,0.1) & io_dvm$servidor1 <= quantile(io_dvm$servidor1,0.9),]

mem <- as.data.frame(t(data.frame(
    c("host",scaleFUN(mean(io_h$servidor1))),
    c("docker",scaleFUN(mean(io_d$servidor1))),
    c("MV",scaleFUN(mean(io_vm$servidor2))),
    c("dockerMV",scaleFUN(mean(io_dvm$servidor2)))
)))
colnames(mem)<- c("type","power")
rownames(mem)<- c("host","docker","MV","dockerMV")

tiff("io.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
io_plot <- ggplot(data=mem, aes(x=type, y=power, fill=type))+
    geom_bar(colour="black", stat="identity")+
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
        x=("Ambiente"),
        y="Consumo (W/s)",
        fill= "Ambiente"
    )+
    #guides(fill=FALSE)
    scale_x_discrete(labels=c("Docker","Docker sobre MV","Host","MV"))+
    scale_fill_d3(labels = c("Docker", "Docker sobre MV", "Host", "MV"))

plot(io_plot)
dev.off()

###################################################
# REDE
###################################################

d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="host" & times$value=="1"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="host" & times$value=="1"] 
)
d_1$type <- "host"
d_1$value <- 1

d_10 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="host" & times$value=="10"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="host" & times$value=="10"] 
)
d_10$type <- "host"
d_10$value <- 10

d_100 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="host" & times$value=="100"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="host" & times$value=="100"] 
)
d_100$type <- "host"
d_100$value <- 100

d_1k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="host" & times$value=="1000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="host" & times$value=="1000"] 
)
d_1k$type <- "host"
d_1k$value <- 1000

d_10k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="host" & times$value=="10000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="host" & times$value=="10000"] 
)
d_10k$type <- "host"
d_10k$value <- 10000

rede_h <- rbind(d_1, d_10)
rede_h <- rbind(rede_h, d_10)
rede_h <- rbind(rede_h, d_100)
rede_h <- rbind(rede_h, d_1k)
rede_h <- rbind(rede_h, d_10k)

#######

d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="docker" & times$value=="1"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="docker" & times$value=="1"] 
)
d_1$type <- "docker"
d_1$value <- 1

d_10 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="docker" & times$value=="10"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="docker" & times$value=="10"] 
)
d_10$type <- "docker"
d_10$value <- 10

d_100 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="docker" & times$value=="100"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="docker" & times$value=="100"] 
)
d_100$type <- "docker"
d_100$value <- 100

d_1k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="docker" & times$value=="1000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="docker" & times$value=="1000"] 
)
d_1k$type <- "docker"
d_1k$value <- 1000

d_10k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="docker" & times$value=="10000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="docker" & times$value=="10000"] 
)
d_10k$type <- "docker"
d_10k$value <- 10000

rede_docker <- rbind(d_1, d_10)
rede_docker <- rbind(rede_docker, d_10)
rede_docker <- rbind(rede_docker, d_100)
rede_docker <- rbind(rede_docker, d_1k)
rede_docker <- rbind(rede_docker, d_10k)

###############

#######

d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="MV" & times$value=="1"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="MV" & times$value=="1"] 
)
d_1$type <- "MV"
d_1$value <- 1

d_10 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="MV" & times$value=="10"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="MV" & times$value=="10"] 
)
d_10$type <- "MV"
d_10$value <- 10

d_100 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="MV" & times$value=="100"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="MV" & times$value=="100"] 
)
d_100$type <- "MV"
d_100$value <- 100

d_1k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="MV" & times$value=="1000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="MV" & times$value=="1000"] 
)
d_1k$type <- "MV"
d_1k$value <- 1000

d_10k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="MV" & times$value=="10000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="MV" & times$value=="10000"] 
)
d_10k$type <- "MV"
d_10k$value <- 10000

rede_MV <- rbind(d_1, d_10)
rede_MV <- rbind(rede_MV, d_10)
rede_MV <- rbind(rede_MV, d_100)
rede_MV <- rbind(rede_MV, d_1k)
rede_MV <- rbind(rede_MV, d_10k)

##########

d_1 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="dockerMV" & times$value=="1"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="dockerMV" & times$value=="1"] 
)
d_1$type <- "dockerMV"
d_1$value <- 1

d_10 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="dockerMV" & times$value=="10"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="dockerMV" & times$value=="10"] 
)
d_10$type <- "dockerMV"
d_10$value <- 10

d_100 <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="dockerMV" & times$value=="100"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="dockerMV" & times$value=="100"] 
)
d_100$type <- "dockerMV"
d_100$value <- 100

d_1k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="dockerMV" & times$value=="1000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="dockerMV" & times$value=="1000"] 
)
d_1k$type <- "dockerMV"
d_1k$value <- 1000

d_10k <- energy %>% filter(
    energy$tempo >= times$start[times$test=="Rede" & times$type=="dockerMV" & times$value=="10000"] & energy$tempo <= times$end[times$test=="Rede" & times$type=="dockerMV" & times$value=="10000"] 
)
d_10k$type <- "dockerMV"
d_10k$value <- 10000

rede_dockerMV <- rbind(d_1, d_10)
rede_dockerMV <- rbind(rede_dockerMV, d_10)
rede_dockerMV <- rbind(rede_dockerMV, d_100)
rede_dockerMV <- rbind(rede_dockerMV, d_1k)
rede_dockerMV <- rbind(rede_dockerMV, d_10k)
##########

rede <- as.data.frame(t(data.frame(
    c("host",scaleFUN(mean(rede_h$servidor2[rede_h$value==1])),1),
    c("host",scaleFUN(mean(rede_h$servidor2[rede_h$value==10])),10),
    c("host",scaleFUN(mean(rede_h$servidor2[rede_h$value==100])),100),
    c("host",scaleFUN(mean(rede_h$servidor2[rede_h$value==1000])),1000),
    c("host",scaleFUN(mean(rede_h$servidor2[rede_h$value==10000])),10000),
    
    c("docker",scaleFUN(mean(rede_docker$servidor2[rede_docker$value==1])),1),
    c("docker",scaleFUN(mean(rede_docker$servidor2[rede_docker$value==10])),10),
    c("docker",scaleFUN(mean(rede_docker$servidor2[rede_docker$value==100])),100),
    c("docker",scaleFUN(mean(rede_docker$servidor2[rede_docker$value==1000])),1000),
    c("docker",scaleFUN(mean(rede_docker$servidor2[rede_docker$value==10000])),10000),
    
    c("MV",scaleFUN(mean(rede_MV$servidor2[rede_MV$value==1])),1),
    c("MV",scaleFUN(mean(rede_MV$servidor2[rede_MV$value==10])),10),
    c("MV",scaleFUN(mean(rede_MV$servidor2[rede_MV$value==100])),100),
    c("MV",scaleFUN(mean(rede_MV$servidor2[rede_MV$value==1000])),1000),
    c("MV",scaleFUN(mean(rede_MV$servidor2[rede_MV$value==10000])),10000),
    
    c("dockerMV",scaleFUN(mean(rede_dockerMV$servidor2[rede_dockerMV$value==1])),1),
    c("dockerMV",scaleFUN(mean(rede_dockerMV$servidor2[rede_dockerMV$value==10])),10),
    c("dockerMV",scaleFUN(mean(rede_dockerMV$servidor2[rede_dockerMV$value==100])),100),
    c("dockerMV",scaleFUN(mean(rede_dockerMV$servidor2[rede_dockerMV$value==1000])),1000),
    c("dockerMV",scaleFUN(mean(rede_dockerMV$servidor2[rede_dockerMV$value==10000])),10000),
    stringsAsFactors = FALSE
)))
colnames(rede)<- c("type","power","BW")

tiff("rede.tiff", width= 3600, height= 2200, units="px", res=400,compression = 'lzw')
rede_plot <- ggplot(data=rede, aes(x=BW, y=power, fill=type))+
    geom_bar(colour="black",position="dodge",stat="identity")+
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
        x=("Largura de Banda (Mb/s)"),
        y="Consumo (W/s)",
        fill= "Ambiente"
    )+
    scale_x_discrete(labels=c("1","10","100","1.000","10.000"))+
    scale_fill_d3(labels = c("Docker", "Docker sobre MV", "Host", "MV"))

plot(rede_plot)
dev.off()

system("for f in *.tiff; do convert -trim $f ${f%.*}.png; done;")
system("mv *.png graphs")
system("rm *.tiff")
