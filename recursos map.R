###

options(scipen=999)

#install.packages("sf")
#install.packages("tidyverse")

library(sf)
library(tidyverse)
library(readxl)
library(ggrepel)

Dep <- read_sf("DEPARTAMENTOS.shp")
Dist <- read_sf("DISTRITOS.shp")
agencias<- read_excel("lat_lon.xlsx")

ggplot(data=Dep)+ 
  geom_sf(fill="#d60800", alpha=0.7)+
  geom_point(data=agencias, aes(x=Longitud, y=Latitud), color='black', fill='white', pch=21, size=3)+
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
    )

Dist$centroide <- st_centroid(Dist$geometry)

Lat <- c()
for(i in 1:1873){
  latitud<-st_coordinates(Dist$centroide)[i,2]
  Lat<-c(Lat,latitud)
}

Long <- c()
for(i in 1:1873){
  longitud<-st_coordinates(Dist$centroide)[i,1]
  Long<-c(Long,longitud)
}


Dist$Latitud <- Lat
Dist$Longitud <- Long

# Gráfica de IDDIST por distrito
ggplot(data=Dist %>% filter(DEPARTAMEN=="ICA"))+ 
  geom_sf(fill="gray", alpha=0.2)+
  geom_point(data=Dist %>% filter(DEPARTAMEN=="ICA"),
             aes(x=Longitud, y=Latitud), fill='black', pch=19, size=1)+
  geom_text_repel(data=Dist %>% filter(DEPARTAMEN=="ICA"),
           aes(x=Longitud, y=Latitud,label=IDDIST), size = 3)+
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Gráfica de IDDIST por distrito

Urb_Rural<-read_excel("Distritos_urbano_rural.xlsx", sheet="Data_merge")
Dist_final<-full_join(x=Dist, y=Urb_Rural, by=c("IDDIST"))

ggplot(data=Dist_final)+ 
  geom_sf(aes(fill=factor(zonificación)))+
  scale_fill_manual(values=c("#fddb68", "#005487"))+
  labs(fill = "Tipología")+
  theme(
    panel.background = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

library(rio)
export(Dist, "Distritos.xlsx")
