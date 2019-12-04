packages <- c("ggplot2","dplyr","scales","gridExtra")

#to install ggradar 
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)

for(package in packages){
    if(!require(package, character.only = TRUE)){
        install.packages(package, dep=TRUE)
        if(!require(package, character.only=TRUE)) stop("Pacote indisponivel")
        
    }
    library(package, character.only=TRUE)
}
rm(packages)
rm(package)

library(ggradar)

splitmean <- function(df) {
    s <- split( df, df$benchmark)
    sapply( s, function(x) mean(x$time) )
}

data <- read.table("data.log",header=TRUE,sep=";", stringsAsFactors=FALSE)
host<- data[data$type=="host",]
host$time<- as.numeric(host$time)  
host<- splitmean(host)

docker<- data[data$type=="docker",]
docker$time<- as.numeric(docker$time)  
docker<- splitmean(docker)

vm<- data[data$type=="vm",]
vm$time<- as.numeric(vm$time)  
vm<- splitmean(vm)

df <- rbind(host, docker)
df <- rbind(df, vm)
df <- as.data.frame(df, stringsAsFactors= FALSE)
df_radar <- df %>% as_tibble(rownames="group") %>% mutate_at(vars(-group), rescale)

png(filename = "data.png", width=480,height=480,bg = "white")
grid.table(df)
dev.off()

png(filename = "data_radar.png", width=480,height=480,bg = "white")
grid.table(df_radar)
dev.off()

# plot with default options:
png(filename = "radar.png", width=480,height=480,bg = "white")
ggradar(df_radar)
dev.off()
