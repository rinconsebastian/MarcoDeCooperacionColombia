#  Autor: Félix Sebastián Rincón Tobo
#  Fecha: 18/05/2020
# Contenido: este programa tiene como objetivo ontener información de un formulario de KoboCollect
#            para su posterior limpieza transformacion ydespliegue en un taqblero de control Power Bi


#0. Librerias y funciones
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)


#0  Obteniendo la información de Kobotoolbox
data <- read_excel("datos.xlsx") 

setDT(data)

#1 RECURSOS AGENCIA

##1.1. organizando recursos Disponibles por agencia
  recDisponibles <- data %>%   #selecciona las columnas correspondientes a esta sección
                    select(
                            "General-Agencia" |
                            (starts_with("General-group_my01j37_") & contains("</span>")))
                            
  

  recDisponibles <- melt(recDisponibles,   # hace un pivot a la tabla de maner
                         id=c("General-Agencia"), #los valores esten en una sola columna
                         value.name="valor")
  
  
 
  recDisponibles <- recDisponibles %>%
                    mutate(variable = gsub('^General.*none">',"",variable)) %>% #elimina inicio de la col variable
                    mutate(variable = gsub('<\\/span>$',"",variable)) %>% #elimina fin de la col variable
                    mutate(variable = gsub('_'," ",variable)) %>% #elimina fin de la col variable
                    separate(col =variable,into=c("fuente","periodo"),sep="-", remove=TRUE) # separa col variable

  
  recDisponibles <- recDisponibles %>% 
                    mutate(fuente = gsub('nacio',"nacional",fuente),  #correccion texto nacional
                           fuente = gsub('cooperaci n',"cooperación",fuente))  #corrección texto cooperación
  
  recDisponibles <- recDisponibles %>% mutate(tipo = "Disponibles") # agrega columna tipo para luego combinar con por movilizar
  
  
##1.2. organizando recursos Por movilizar por agencia
  recMovilizar <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        (starts_with("General-group_lt5aw27_") & contains("</span>")))
  
  
  
  recMovilizar <- melt(recMovilizar,   # hace un pivot a la tabla de maner
                         id=c("General-Agencia"), #los valores esten en una sola columna
                         value.name="valor")
  
  
  
  recMovilizar <- recMovilizar %>%
    mutate(variable = gsub('^General.*none">',"",variable)) %>% #elimina inicio de la col variable
    mutate(variable = gsub('<\\/span>$',"",variable)) %>% #elimina fin de la col variable
    mutate(variable = gsub('_'," ",variable)) %>% #elimina fin de la col variable
    separate(col =variable,into=c("fuente","periodo"),sep="-", remove=TRUE) # separa col variable
  
  
  recMovilizar <- recMovilizar %>% 
    mutate(fuente = gsub('nacio',"nacional",fuente),  #correccion texto nacional
           fuente = gsub('cooperaci n',"cooperación",fuente))  #corrección texto cooperación
  
  recMovilizar <- recMovilizar %>% mutate(tipo = "Por movilizar") # agrega columna tipo para luego combinar con por movilizar
  
 #1.3. Unieindo recursos disponibles y por movilizar
  
  recursosAgencia <- rbind(recDisponibles,recMovilizar )

#2 DATOS OUTCOMES
## 2.1. Seleccionar columnas relacionadas
  
  Outcome <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        ends_with("¿Su agencia participa en este Outcome?"))
  
  
  Outcome <- melt(Outcome,   # hace un pivot a la tabla de manera que
                       id=c("General-Agencia"), #los valores esten en una sola columna
                       value.name="La agencia participa en el Outcome")
  
  Outcome <- Outcome %>% mutate(eje = gsub('\\..*$','', variable), # Crea la columna EJe
                                eje = gsub('Eje ','',eje))
  
  Outcome <- Outcome %>% mutate(outcome = gsub('^.*Outcome ','', variable), # Crea la columna Outcome
                                outcome = gsub('-.*$','',outcome))
  

  Outcome <- Outcome %>% mutate(variable = NULL)  #elimina columna variable 


#2.2. Obteniendo los nombres de los outcomes  
  outcomeNombres  <- data %>%          #selecciona las columnas correspondientes al nomre de los outcomes
        select(starts_with("Eje") &
                 contains("-Outcome ")&
                 !contains("¿")&
                 !contains("Outputs")&
                 !contains("Alianzas")&
                 !contains("-Recursos-")) %>% 
        names() %>%                     #toma el nombre de las vcolumnas
        as.data.frame() %>%             #guarda los nombres en un dataframe
        setnames("nombre") %>%          #establece el nombre de la unica columna del dataframe
        mutate(outcome = gsub('^.*Outcome ','', nombre), # Crea la columna Outcome con el numero de cada outcome
               outcome = gsub('-.*$','',outcome)) %>%
        mutate(nombre = sub("^.*Outcome [0-9]\\.[0-9]-","",nombre)) #extrae el nombre del outcome
  
  
  Outcome <- merge(x = Outcome, y= outcomeNombres, by.x = "outcome", by.y = "outcome") #une el output con su nombre
  
  
#3 OUTPUTS
##3.1. Indentificando los outputs
  
  Output <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        contains("-Outputs-")&
        !contains("Indique"))
    
  Output <- melt(Output,   # hace un pivot a la tabla de manera que
                  id=c("General-Agencia"), #los valores esten en una sola columna
                  value.name="La agencia participa en el Output")

  Output <- Output%>%
            mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
                  eje = gsub('Eje ','',eje))
  
  Output <- Output%>%
    mutate(outcome = gsub('-Output.*$','', variable), # Crea la columna Outcome con el número del outcome
           outcome = gsub('^.*-Outcome ','',outcome))
 
   Output <- Output%>%
    mutate(output = gsub('^.*-Outputs-','', variable), # Crea la columna output con el número del output
           output = gsub('( |: |\\.[a-zA-Z]|\\.(\\t| )[a-zA-Z]).*$','',output))
   
   Output <- Output%>%                                #crea la columna nombre con el nombre del output
     mutate(nombre = gsub('^.*Outputs-[0-9]\\.[0-9]\\.[0-9]( |: |\\.[a-zA-Z]|\\.(\\t| )[a-zA-Z])','', variable))
   
   Output <- Output%>%  mutate(variable = NULL)  #elimina columna variable                           
   

#4 ALIANZAS
##4.1. Alianzas Existentes
   AlianzasE <- data %>%   #selecciona las columnas correspondientes a esta sección
                       select(
                         "General-Agencia" |
                           contains("-Alianzas-Alianzas Existentes:-"))   
   
  setDT(AlianzasE)
  
  AlianzasE <- melt(AlianzasE,   # hace un pivot a la tabla de manera que
                  id=c("General-Agencia"), #los valores esten en una sola columna
                  value.name="La agencia esta aliada")                 

  AlianzasE <- AlianzasE%>%
    mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
           eje = gsub('Eje ','',eje))%>%
    mutate(outcome = gsub('-Alianzas-Alianzas.*$','', variable), # Crea la columna Outcome con el número del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(organizacion = gsub('^.*Existentes:-','', variable))%>% # Crea la columna Organizacion con el tipo de organización
    mutate(tipo = "existente") # Crea la columna tipo con el tipo de alizanza
  
  AlianzasE <- AlianzasE%>%  mutate(variable = NULL)  #elimina columna variable         

##4.2. Alianzas Proyectadas
  Alianzasp <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        contains("-Alianzas-Alianzas proyectadas (2020 - 2023):-")|
        contains("-Alianzas-Generación de Alianzas proyectadas (2020 - 2023):-"))   
  
  setDT(Alianzasp)
  
  Alianzasp <- melt(Alianzasp,   # hace un pivot a la tabla de manera que
                    id=c("General-Agencia"), #los valores esten en una sola columna
                    value.name="La agencia esta aliada")                 
  
  Alianzasp <- Alianzasp%>%
    mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
           eje = gsub('Eje ','',eje))%>%
    mutate(outcome = gsub('(-Alianzas-Alianzas|-Alianzas-Generación).*$','', variable), # Crea la columna Outcome con el número del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(organizacion = gsub('^.*proyectadas \\(2020 - 2023\\):-','', variable))%>% # Crea la columna Organizacion con el tipo de organización
    mutate(tipo = "proyectada") # Crea la columna tipo con el tipo de alizanza
  
  Alianzasp <- Alianzasp%>%  mutate(variable = NULL)  #elimina columna variable  

  #4.3. Uniendo las alianzas
  alianzas <- rbind(AlianzasE,Alianzasp )
  
  
#5 RECURSOS
##5.1.  Seleccionando las columnas de recursos
  Recursos <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        contains("-Recursos-Recursos")&
        !contains("total")&
        ends_with("</span>"))   
  
  setDT(Recursos)
  
  Recursos <- melt(Recursos,   # hace un pivot a la tabla de manera que
                    id=c("General-Agencia"), #los valores esten en una sola columna
                    value.name="valor")   
  
  Recursos <- Recursos%>%
    mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
           eje = gsub('Eje ','',eje))%>%
    mutate(outcome = gsub('-Recursos-Recursos.*$','', variable), # Crea la columna Outcome con el número del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(periodo = gsub('-Recursos-Recursos.*$','', variable), # Crea la columna Outcome con el número del outcome
           periodo = gsub('^.*-Outcome ','',outcome))%>%
    mutate(organizacion = gsub('^.*proyectadas \\(2020 - 2023\\):-','', variable))%>% # Crea la columna Organizacion con el tipo de organización
    mutate(tipo = "proyectada") # Crea la columna tipo con el tipo de alizanza