
library(readr)
library(dplyr)
library(data.table)
library(ggplot2)

dataset <- read.csv("alianzas_g2_oit.csv") 
dataset <- dataset %>% mutate(outcome = as.factor(outcome))

setnames(x = dataset, old = "ï..Agencia", new = "Agencia")


dataExistente <- dataset %>% filter(Atributo == "existente") %>% 
                group_by( outcome) %>%
                summarise(existenteAgencia = sum(Valor), existenteTotal = min(Total)) 

dataProyectada <- dataset %>% filter(Atributo == "proyectada") %>% 
        group_by( outcome) %>%
        summarise(proyectadaAgencia = sum(Valor), proyectadaTotal = min(Total)) 


dataExistente <- dataExistente %>% mutate(existenteTotal = existenteTotal- existenteAgencia)

dataProyectada <- dataProyectada %>% mutate(proyectadaTotal = proyectadaTotal- proyectadaAgencia)

data <- merge(dataExistente, dataProyectada, by = "outcome")

str(data)

dataMelted <- melt(data,"outcome")

dataMelted <-dataMelted %>% mutate(responsable = sub("[A-Z].*$","",variable))%>%
                         mutate(variable = paste( responsable, sub("^[a-z]*","",variable),sep = " "))%>%
                         mutate(variable = sub("existente","Existentes",variable))%>%
                         mutate(variable = sub("proyectada","Proyectadas",variable))%>%
                         mutate(variable = sub("Total",".Sistema",variable))%>%
                         mutate(responsable = sub("(xistente|royectada)","",responsable))%>%
                         mutate(labelvalue = ifelse(value< 1,".",value) )

                


ggplot(data = dataMelted, aes(x= reorder(responsable, value),y=value, fill= variable))+
        scale_fill_manual(values=c("#4A8DDC","#FFA500","#92BBEA","#ffd280"))+
        geom_bar(stat = 'identity', position = 'stack')+ theme_bw()+ 
        facet_wrap( ~ outcome,ncol =  1, strip.position = "left")+
        #labs(x="Organizacion",y="numero de Alinzas")+         #establece los titulos de los ejes
        theme(legend.position = "none",          #elimina la leyenda 
              axis.text.y=element_text(size=8),       #elimina las marcas del eje x
              panel.grid.major.y =  element_blank(),      #elimina la cuadricula
              panel.grid.minor.y =  element_blank(),      #elimina la cuadricula
              panel.grid.minor.x =  element_blank(),
              axis.ticks.y = element_blank(),    #elimina las marcas del eje x
              #axis.text=element_text(size=32),   #cambia el tamaño del texto de las marcas de los ejes
              axis.title = element_blank(), #element_text(size=32) # cambia el tamaño 
              panel.spacing = unit(0, "lines"),
              panel.border =  element_rect(color="#E5E5E5"),
              #strip.background = element_rect(color="black", fill="#ffffff", linetype = "blank"),
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size=8),
              plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm")
              
        )+      
        geom_text(aes(label=labelvalue), position = position_stack(vjust = .5),size= 3,check_overlap = TRUE, color = "#030303")+
        coord_flip()
       
