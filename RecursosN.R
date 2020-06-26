library(readr)
library(dplyr)
library(data.table)
library(ggplot2)




#dataset <- read.csv("RecursosN.csv") 
#setnames(x = dataset, old = "ï..Agencia", new = "Agencia")

seleccionada <- as.character(dataset$Seleccionada[[1]])

seleccionada <-"PNUD"

variables <- c("eje", "outcome")
variablesLables <- c("Eje", "Outcome")

dataset <- dataset %>% mutate(outcome = as.factor(outcome))%>%
        mutate(Agencia = as.factor(Agencia))%>%
        mutate(eje = as.factor(eje))%>%
        mutate(organizacion = as.factor(organizacion))%>%
        mutate(periodo = as.factor(periodo))%>%
        mutate(tipo = as.factor(tipo))%>%
        mutate(Seleccionada = NULL)%>%
        mutate(valor = ifelse(is.na(valor),0,valor))




dataAgrupadaSistema <- dataset  %>% group_by(eje,outcome)%>%
        summarise(ValorTotal = sum(valor))

dataAgrupadaAgencia <- dataset  %>% filter(Agencia == seleccionada)%>%
        group_by(eje,outcome)%>%
        summarise(ValorAgencia = sum(valor))



data <- merge(dataAgrupadaAgencia, dataAgrupadaSistema, by = variables)

data <- data%>% mutate(ValorSistema = ValorTotal - ValorAgencia)%>%
        mutate(ValorTotal = NULL)


dataMelted <- melt(data,variables)

dataMelted <- dataMelted%>% mutate(variable = sub("Valor","",variable))%>% 
        mutate(variable = sub("Sistema",".Sistema",variable))%>%
        mutate(value = value/1000000) %>%
        mutate(valueLabel = ifelse(value < 1,".",ifelse(value<5,round(value, digits = 0),round(value, digits = 1))) )


ggplot(data = dataMelted, aes(x= reorder(outcome, value),y=value, fill= variable))+
        scale_fill_manual(values=c("#4A8DDC","#FFA500"))+
        geom_bar(stat = 'identity', position = 'stack')+ theme_bw()+ 
        facet_wrap( ~ eje,ncol =  1, strip.position = "left")+
        labs(y="Recursos (Millones de dólares)",x=paste(variablesLables, collapse = " y "))+         #establece los titulos de los ejes
        theme(axis.text.y=element_text(size=8, color="#386AA5"), #elimina las marcas del eje x
              panel.grid.major.y =  element_blank(),      #elimina la cuadricula
              panel.grid.minor.y =  element_blank(),      #elimina la cuadricula
              #panel.grid.minor.x =  element_blank(),
              axis.ticks.y = element_blank(),    #elimina las marcas del eje x
              axis.text=element_text(color="#386AA5"),   #cambia el tamaño del texto de las marcas de los ejes
              axis.title = element_text(color="#386AA5"), #element_blank() # cambia el tamaño 
              panel.spacing = unit(0, "lines"),
              panel.border =  element_rect(color="#808080"),
              #strip.background = element_rect(color="black", fill="#ffffff", linetype = "blank"),
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size=8,color="#386AA5"),
              strip.text.y.left = element_text(angle=0),
             
              plot.margin = unit(c(0.2,0.2,0.2,0.2),"cm"),
              legend.position = "none",          #elimina la leyenda 
              
        )+      
        geom_text(aes(label=valueLabel), position = position_stack(vjust = 0.5),size= 4,check_overlap = TRUE, fontface="bold",  color = "#ffffff")+
        coord_flip()+
        scale_y_continuous(n.breaks=10, expand = expansion(mult = c(0,0.1)))