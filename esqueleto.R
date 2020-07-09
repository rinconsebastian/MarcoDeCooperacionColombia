library(readr)
library(dplyr)
library(data.table)
library(ggplot2)
library(scales)




dataset <- read.csv("esqueleto.csv", encoding = "UTF-8") 
setnames(x = dataset, old = "X.U.FEFF.Agencia", new = "Agencia")
setnames(x = dataset, old = "La.agencia.participa.en.el.Output", new = "Participa")


dataset <- dataset %>% mutate(color = as.factor(ifelse(Participa == 1,2,1)))
dataset <- dataset %>% mutate(Nombre2 = as.factor(substr(Nombre, 1, 28)))


ggplot(data = dataset)+
         scale_fill_manual(values=c("#FFFFFF","#4A8DDC"))+
         geom_point(aes(x=Nombre2 ,y=reorder(Agencia, Participa) ,fill=color),shape=22, size=4)+ 
         geom_rect(aes(xmin=(as.integer(Nombre2)-0.5),           #establece las coordenadas de cada cuadro de fondo a partir del número de fila 
                       xmax=(as.integer(Nombre2)+0.5),
                       ymin=(as.integer(reorder(Agencia, Participa))-0.5),
                       ymax=(as.integer(reorder(Agencia, Participa))+0.5),
                       fill = color
                       ),
                   color="#D9D9D9")+
         labs(y="Acengia", x= "Output")+
         theme(axis.text=element_text(color="#386AA5"),
               axis.text.x=element_text(size=9, color="#386AA5", angle = 90),
               legend.position = "none",
               axis.title = element_text(color="#386AA5"),
               plot.margin=unit(c(0.6,0,0,0),"cm")
               
         )+
        scale_y_discrete(position = "right")             
                       
                       #color=color,               # define los colores de los cuadros por número de osd
                       #fill = color),              # define los colores de los bordes de los cuadros por número de osd
                  alpha=0.5)


#geom_point(aes(fill=color),shape=22, size=4)+


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