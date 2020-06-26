
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)

dataset <- read.csv("recursos_participacion.csv") 


setnames(x = dataset, old = "ï..Agencia", new = "Agencia")
dataset <- dataset %>% mutate(Agencia = as.factor(Agencia))
dataset <- dataset %>% mutate(Valor =(Valor)/1000000)



ggplot(data = dataset, aes(x= Recuento  ,y=Valor))+
        geom_point(color="#4A8DDC", size=4, shape=18 )+ 
        geom_text(aes(label=Agencia),hjust=-0.25, vjust=0)+
        geom_smooth(method = "lm", se = FALSE)+
        theme_bw()
        
        
      