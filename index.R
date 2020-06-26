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
logos <- read_excel("logos.xlsx") 


#reajustando nombres de agencias

data <- data%>% mutate(`General-Agencia` = sub("Oficina de la Alta Comisionada de Naciones Unidas para los Derechos Humanos en Colombia", "HCHR",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("Organización Internacional del Trabajo", "OIT",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("Organización Internacional para las Migraciones", "OIM",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("ONU Mujeres", "ONUM",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("Alto Comisionando de las Naciones Unidas para los Refugiados- ACNUR", "ACNUR",`General-Agencia`))

setDT(data)
#1 RECURSOS AGENCIA

#1.0 Agencias
Agencias <- data %>%   #selecciona las columnas correspondientes a esta sección
  select("General-Agencia") %>% distinct()
setnames(x = Agencias, old = "General-Agencia", new = "Agencia")



Agencias <- merge(Agencias, logos, by = "Agencia",all.x = T )

##1.1. organizando recursos Disponibles por agencia
  recDisponibles <- data %>%   #selecciona las columnas correspondientes a esta sección
                    select(
                            "General-Agencia" |
                            (starts_with("General-group_my01j37_") & contains("</span>")))
                            
  

  recDisponibles <- melt(recDisponibles,   # hace un pivot a la tabla de maner
                         id=c("General-Agencia"), #los valores esten en una sola columna
                         value.name="valor")
  
  setnames(x = recDisponibles, old = "General-Agencia", new = "Agencia")
  
 
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
  
  setnames(x = recMovilizar, old = "General-Agencia", new = "Agencia")
  
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
  
  setnames(x = Outcome, old = "General-Agencia", new = "Agencia")
  
  Outcome <- melt(Outcome,   # hace un pivot a la tabla de manera que
                       id=c("Agencia"), #los valores esten en una sola columna
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
        setnames("Nombre") %>%          #establece el nombre de la unica columna del dataframe
        mutate(outcome = gsub('^.*Outcome ','', Nombre), # Crea la columna Outcome con el numero de cada outcome
               outcome = gsub('-.*$','',outcome)) %>%
        mutate(Nombre = sub("^.*Outcome [0-9]\\.[0-9]-","",Nombre))%>% #extrae el nombre del outcome
        mutate(Nombre =  paste(outcome,Nombre,sep = " "))
  
  Outcome <- merge(x = Outcome, y= outcomeNombres, by.x = "outcome", by.y = "outcome") #une el output con su nombre
  
  Outcome <- Outcome%>%  mutate(`La agencia participa en el Outcome` =
                                ifelse(is.na(`La agencia participa en el Outcome`), "No", `La agencia participa en el Outcome`))%>%
    mutate(`La agencia participa en el Outcome` = 
             ifelse(`La agencia participa en el Outcome` == "Si", 1,0))
  
  
#3 OUTPUTS
##3.1. Indentificando los outputs
  
  Output <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        contains("-Outputs-")&
        !contains("Indique"))
  
  setnames(x = Output, old = "General-Agencia", new = "Agencia")
    
  Output <- melt(Output,   # hace un pivot a la tabla de manera que
                  id=c("Agencia"), #los valores esten en una sola columna
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
     mutate(Nombre = gsub('^.*Outputs-[0-9]\\.[0-9]\\.[0-9]( |: |\\.[a-zA-Z]|\\.(\\t| )[a-zA-Z])','', variable))%>%
     mutate(Nombre =  paste(output,Nombre,sep = " "))
   
   Output <- Output%>%  mutate(variable = NULL)  #elimina columna variable                           
   
   Output <- Output%>%  mutate(`La agencia participa en el Output` =
                                ifelse(is.na(`La agencia participa en el Output`), "No", `La agencia participa en el Output`))%>%
                        mutate(`La agencia participa en el Output` = 
                                 ifelse(`La agencia participa en el Output` == "Si", 1,0))
   
  
   

#4 ALIANZAS
##4.1. Alianzas Existentes
   AlianzasE <- data %>%   #selecciona las columnas correspondientes a esta sección
                       select(
                         "General-Agencia" |
                           contains("-Alianzas-Alianzas Existentes:-"))   
   setnames(x = AlianzasE, old = "General-Agencia", new = "Agencia")
   
  setDT(AlianzasE)
  
  AlianzasE <- melt(AlianzasE,   # hace un pivot a la tabla de manera que
                  id=c("Agencia"), #los valores esten en una sola columna
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
  
  setnames(x = Alianzasp, old = "General-Agencia", new = "Agencia")
  
  setDT(Alianzasp)
  
  Alianzasp <- melt(Alianzasp,   # hace un pivot a la tabla de manera que
                    id=c("Agencia"), #los valores esten en una sola columna
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
  
  alianzas <- alianzas%>%  mutate(`La agencia esta aliada` =
                                ifelse(is.na(`La agencia esta aliada`), 0, `La agencia esta aliada`))
  
  #4.4. Eliminando las alizansas con "ninguno"
  alianzas <- alianzas%>% filter(organizacion != "Ninguna")
  
  
  
#5 RECURSOS
##5.1.  Seleccionando las columnas de recursos
  Recursos <- data %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "General-Agencia" |
        contains("-Recursos-Recursos")&
        !contains("total")&
        ends_with("</span>"))   
  
  
  setnames(x = Recursos, old = "General-Agencia", new = "Agencia")
  setDT(Recursos)
  
  Recursos <- melt(Recursos,   # hace un pivote a la tabla de manera que
                    id=c("Agencia"), #los valores estén en una sola columna
                    value.name="valor")   
  
  Recursos <- Recursos%>%
    mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
           eje = gsub('Eje ','',eje))%>%
    mutate(outcome = gsub('-Recursos-Recursos.*$','', variable), # Crea la columna Outcome con el número del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(periodo = gsub('-recursos_.*$','', variable), # Crea la columna periodo con el número del año
           periodo = gsub('^.*-Recursos-Recursos ','',periodo))%>%
    mutate(organizacionTipo = gsub('</span>$','', variable), # Crea la columna organizaciónTipo con el nombre de la organización y tipo de recurso
           organizacionTipo = gsub('^.*<span style="display:none">','',organizacionTipo))%>%
    separate(col =organizacionTipo,into=c("organizacion","tipo"),sep="-", remove=TRUE) # separa col variable en dos columnas
  
  
  Recursos <- Recursos%>%  mutate(variable = NULL)  #elimina columna variable  
  
  
  #openxlsx::write.xlsx(Recursos, file = "recursos.xlsx", colNames = TRUE)
  #fwrite(Recursos,"recursos.csv")
  #fwrite(Output,"outputs.csv")
  #fwrite(Outcome,"outcome.csv")

#6 ====================================== VALOR AGREGADO =============================================
  
  dataValor <- read_excel("datosEncuestaAnterior.xlsx") 
  
  dataValor <- dataValor %>%   #selecciona las columnas correspondientes a esta sección
    select(
      "Agencia:" |
        starts_with("Información para la")|
        starts_with("Insumos política")|
        starts_with("Fortalecimiento de ")|
        starts_with("Formulación y ejecución de")|
        starts_with("Gestión del conocimiento")|
        starts_with("Generación de Alianzas"))
  
  setDT(dataValor)
      dataValor <- reshape2::melt(data = dataValor, "Agencia:")
  
     
      
        
  dataValor <- dataValor %>%  mutate(nOutcome = as.integer(gsub("^.*\\.","",variable)))%>%
                              mutate(variable = gsub("\\.\\.\\..*$","",variable)) %>%
                              mutate(outcome = ifelse(nOutcome>=34,outcomeNombres$Nombre[1],""))%>%
                              mutate(outcome = ifelse(nOutcome>=57,outcomeNombres$Nombre[2],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=76,outcomeNombres$Nombre[3],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=95,outcomeNombres$Nombre[4],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=114,outcomeNombres$Nombre[5],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=134,outcomeNombres$Nombre[6],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=154,outcomeNombres$Nombre[7],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=174,outcomeNombres$Nombre[8],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=193,outcomeNombres$Nombre[9],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=215,outcomeNombres$Nombre[10],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=235,outcomeNombres$Nombre[11],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=256,outcomeNombres$Nombre[12],outcome))%>%
                              mutate(outcome = ifelse(nOutcome>=276,outcomeNombres$Nombre[13],outcome))%>%
                              mutate(nOutcome = NULL)%>%
                              mutate(value = as.integer(value))
    

  setnames(x = dataValor, old = "Agencia:", new = "Agencia")
  
  dataValor <- dataValor%>% mutate(Agencia = sub("Organización de las Naciones Unidas para el Desarrollo Industrial \\(ONUDI\\)", "ONUDI",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("Organización Internacional para las Migraciones \\(OIM\\)", "OIM",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("COMISION ECONÓMICA PARA AMÉRICA LATINA, CEPAL, OFICINA DE COLOMBIA", "CEPAL",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("ORGANIZACION PANAMERICANA DE LA SALUD/ORGANIZACION MUNDIAL DE LA SALUD\\- OPS/OMS", "OPS",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("ONU Mujeres", "ONUM",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("Oficina del Alto Comisionado de las Naciones Unidas para los Derechos Humanos \\(OACNUDH \\)", "HCHR",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("Oficina de las Naciones Unidas contra la Droga y el Delito \\(UNODC\\)", "UNODC",Agencia))
  
  
#7 PROCESAMINETO DE INFORMACIÓN PARA GENERAR GRÁFICAS
  
l <- list("Agencias"= Agencias, "Outcome" = Outcome,"Output" = Output, "Alianzas" = alianzas, "Recursos" = Recursos, "ValorAgregado" = dataValor )
openxlsx::write.xlsx(l, "salida.xlsx")

