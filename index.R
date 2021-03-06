#  Autor: F�lix Sebasti�n Rinc�n Tobo
#  Fecha: 18/05/2020
# Contenido: este programa tiene como objetivo ontener informaci�n de un formulario de KoboCollect
#            para su posterior limpieza transformacion ydespliegue en un taqblero de control Power Bi


#0. Librerias y funciones
library(readxl)
library(dplyr)
library(data.table)
library(tidyr)
library(splitstackshape)


#0  Obteniendo la informaci�n de Kobotoolbox
data <- read_excel("datos.xlsx") 
logos <- read_excel("logos.xlsx") 


#reajustando nombres de agencias

data <- data%>% mutate(`General-Agencia` = sub("Oficina de la Alta Comisionada de Naciones Unidas para los Derechos Humanos en Colombia", "OACNUDH",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("Organizaci�n Internacional del Trabajo", "OIT",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("Organizaci�n Internacional para las Migraciones", "OIM",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("ONU Mujeres", "ONU_M",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("Alto Comisionando de las Naciones Unidas para los Refugiados- ACNUR", "ACNUR",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("UN World Food Programme", "PMA",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("OPS", "OMS/OPS",`General-Agencia`))
data <- data%>% mutate(`General-Agencia` = sub("UNODC", "UNODC",`General-Agencia`))

setDT(data)
#1 RECURSOS AGENCIA

#1.0 Agencias
Agencias <- data %>%   #selecciona las columnas correspondientes a esta secci�n
  select("General-Agencia") %>% distinct()
setnames(x = Agencias, old = "General-Agencia", new = "Agencia")



Agencias <- merge(Agencias, logos, by = "Agencia",all.x = T )

##1.1. organizando recursos Disponibles por agencia
  recDisponibles <- data %>%   #selecciona las columnas correspondientes a esta secci�n
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
                           fuente = gsub('cooperaci n',"cooperaci�n",fuente))  #correcci�n texto cooperaci�n
  
  recDisponibles <- recDisponibles %>% mutate(tipo = "Disponibles") # agrega columna tipo para luego combinar con por movilizar
  
  
##1.2. organizando recursos Por movilizar por agencia
  recMovilizar <- data %>%   #selecciona las columnas correspondientes a esta secci�n
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
           fuente = gsub('cooperaci n',"cooperaci�n",fuente))  #correcci�n texto cooperaci�n
  
  recMovilizar <- recMovilizar %>% mutate(tipo = "Por movilizar") # agrega columna tipo para luego combinar con por movilizar
  
 #1.3. Unieindo recursos disponibles y por movilizar
  
  recursosAgencia <- rbind(recDisponibles,recMovilizar )

#2 DATOS OUTCOMES
## 2.1. Seleccionar columnas relacionadas
  
  Outcome <- data %>%   #selecciona las columnas correspondientes a esta secci�n
    select(
      "General-Agencia" |
        ends_with("�Su agencia participa en este Outcome?"))
  
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
                 !contains("�")&
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
  
  Output <- data %>%   #selecciona las columnas correspondientes a esta secci�n
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
    mutate(outcome = gsub('-Output.*$','', variable), # Crea la columna Outcome con el n�mero del outcome
           outcome = gsub('^.*-Outcome ','',outcome))
 
   Output <- Output%>%
    mutate(output = gsub('^.*-Outputs-','', variable), # Crea la columna output con el n�mero del output
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
   AlianzasE <- data %>%   #selecciona las columnas correspondientes a esta secci�n
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
    mutate(outcome = gsub('-Alianzas-Alianzas.*$','', variable), # Crea la columna Outcome con el n�mero del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(organizacion = gsub('^.*Existentes:-','', variable))%>% # Crea la columna Organizacion con el tipo de organizaci�n
    mutate(tipo = "existente") # Crea la columna tipo con el tipo de alizanza
  
  AlianzasE <- AlianzasE%>%  mutate(variable = NULL)  #elimina columna variable         

##4.2. Alianzas Proyectadas
  Alianzasp <- data %>%   #selecciona las columnas correspondientes a esta secci�n
    select(
      "General-Agencia" |
        contains("-Alianzas-Alianzas proyectadas (2020 - 2023):-")|
        contains("-Alianzas-Generaci�n de Alianzas proyectadas (2020 - 2023):-"))   
  
  setnames(x = Alianzasp, old = "General-Agencia", new = "Agencia")
  
  setDT(Alianzasp)
  
  Alianzasp <- melt(Alianzasp,   # hace un pivot a la tabla de manera que
                    id=c("Agencia"), #los valores esten en una sola columna
                    value.name="La agencia esta aliada")                 
  
  Alianzasp <- Alianzasp%>%
    mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
           eje = gsub('Eje ','',eje))%>%
    mutate(outcome = gsub('(-Alianzas-Alianzas|-Alianzas-Generaci�n).*$','', variable), # Crea la columna Outcome con el n�mero del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(organizacion = gsub('^.*proyectadas \\(2020 - 2023\\):-','', variable))%>% # Crea la columna Organizacion con el tipo de organizaci�n
    mutate(tipo = "proyectada") # Crea la columna tipo con el tipo de alizanza
    
  
  Alianzasp <- Alianzasp%>%  mutate(variable = NULL)  #elimina columna variable  

  #4.3. Uniendo las alianzas
  alianzas <- rbind(AlianzasE,Alianzasp )
  
  alianzas <- alianzas%>%  mutate(`La agencia esta aliada` =
                                ifelse(is.na(`La agencia esta aliada`), 0, `La agencia esta aliada`))
  
  #4.4. Eliminando las alizansas con "ninguno"
  alianzas <- alianzas%>% filter(organizacion != "Ninguna")
  
  alianzas <- alianzas%>%mutate(organizacion = ifelse(organizacion== "Gobierno Nacional","Gob. Nacional",organizacion))
  alianzas <- alianzas%>%mutate(organizacion = ifelse(organizacion== "Gobiernos Territoriales(Gobernaciones, Alcald�as, CARs)","Gobs. Territoriales",organizacion))
  alianzas <- alianzas%>%mutate(organizacion = ifelse(organizacion== "Organizaciones de Base","Org. de Base",organizacion))
  alianzas <- alianzas%>%mutate(organizacion = ifelse(organizacion== "Organizaciones Sociales","Org. Sociales",organizacion))
  alianzas <- alianzas%>%mutate(organizacion = ifelse(organizacion== "Sector Privado - Empresas","Empresas",organizacion))
  alianzas <- alianzas%>%mutate(organizacion = ifelse(organizacion== "Sector Privado - Fundaciones","Fundaciones",organizacion))
  
  
#5 RECURSOS
##5.1.  Seleccionando las columnas de recursos
  Recursos <- data %>%   #selecciona las columnas correspondientes a esta secci�n
    select(
      "General-Agencia" |
        contains("-Recursos-Recursos")&
        !contains("total")&
        ends_with("</span>"))   
  
  
  setnames(x = Recursos, old = "General-Agencia", new = "Agencia")
  setDT(Recursos)
  
  Recursos <- melt(Recursos,   # hace un pivote a la tabla de manera que
                    id=c("Agencia"), #los valores est�n en una sola columna
                    value.name="valor")   
  
  Recursos <- Recursos%>%
    mutate(eje = gsub('\\..*$','', variable), # Crea la columna Eje con el numero del eje
           eje = gsub('Eje ','',eje))%>%
    mutate(outcome = gsub('-Recursos-Recursos.*$','', variable), # Crea la columna Outcome con el n�mero del outcome
           outcome = gsub('^.*-Outcome ','',outcome))%>%
    mutate(periodo = gsub('-recursos_.*$','', variable), # Crea la columna periodo con el n�mero del a�o
           periodo = gsub('^.*-Recursos-Recursos ','',periodo))%>%
    mutate(organizacionTipo = gsub('</span>$','', variable), # Crea la columna organizaci�nTipo con el nombre de la organizaci�n y tipo de recurso
           organizacionTipo = gsub('^.*<span style="display:none">','',organizacionTipo))%>%
    separate(col =organizacionTipo,into=c("organizacion","tipo"),sep="-", remove=TRUE) # separa col variable en dos columnas
  
  
  Recursos <- Recursos%>%  mutate(variable = NULL)  #elimina columna variable 
  Recursos <- Recursos%>%  mutate(tipo = ifelse(tipo =="caja","En caja",tipo))  #elimina columna variable 
  
  Recursos <- Recursos%>%mutate(organizacion = ifelse(organizacion== "cooperacion","Cooperantes",organizacion))
  Recursos <- Recursos%>%mutate(organizacion = ifelse(organizacion== "identificar","Por Identificar",organizacion))
  Recursos <- Recursos%>%mutate(organizacion = ifelse(organizacion== "local","Gobs. Locales",organizacion))
  Recursos <- Recursos%>%mutate(organizacion = ifelse(organizacion== "nacional","Gob. Nacional",organizacion))
  Recursos <- Recursos%>%mutate(organizacion = ifelse(organizacion== "privado","Sec. Privado",organizacion))
  Recursos <- Recursos%>%mutate(organizacion = ifelse(organizacion== "propios","Rec. Propios",organizacion))
  
  
  
  openxlsx::write.xlsx(Recursos, file = "recursos.xlsx", colNames = TRUE)
  #fwrite(Recursos,"recursos.csv")
  #fwrite(Output,"outputs.csv")
  #fwrite(Outcome,"outcome.csv")

#6 ====================================== VALOR AGREGADO =============================================
  
  dataValor <- read_excel("datosEncuestaAnterior.xlsx") 
  
  dataValor <- dataValor %>%   #selecciona las columnas correspondientes a esta secci�n
    select(
      "Agencia:" |
        starts_with("Informaci�n para la")|
        starts_with("Insumos pol�tica")|
        starts_with("Fortalecimiento de ")|
        starts_with("Formulaci�n y ejecuci�n de")|
        starts_with("Gesti�n del conocimiento")|
        starts_with("Generaci�n de Alianzas"))
  
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
  
  dataValor <- dataValor%>% mutate(Agencia = sub("Organizaci�n de las Naciones Unidas para el Desarrollo Industrial \\(ONUDI\\)", "ONUDI",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("Organizaci�n Internacional para las Migraciones \\(OIM\\)", "OIM",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("COMISION ECON�MICA PARA AM�RICA LATINA, CEPAL, OFICINA DE COLOMBIA", "CEPAL",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("ORGANIZACION PANAMERICANA DE LA SALUD/ORGANIZACION MUNDIAL DE LA SALUD\\- OPS/OMS", "OMS/OPS",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("ONU Mujeres", "ONU_M",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("Oficina del Alto Comisionado de las Naciones Unidas para los Derechos Humanos \\(OACNUDH \\)", "OACNUDH",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("Oficina de las Naciones Unidas contra la Droga y el Delito \\(UNODC\\)", "UNODC",Agencia))
  dataValor <- dataValor%>% mutate(Agencia = sub("WFP", "PMA",Agencia))
  
  dataValor <- dataValor%>% mutate(variable = sub("Formulaci�n y ejecuci�n de proyectos", "4.Formulaci�n y ejecuci�n de proyectos",variable))
  dataValor <- dataValor%>% mutate(variable = sub("Fortalecimiento de capacidades", "3. Fortalecimiento de capacidades",variable))
  dataValor <- dataValor%>% mutate(variable = sub("Generaci�n de Alianzas", "6. Generaci�n de Alianzas",variable))
  dataValor <- dataValor%>% mutate(variable = sub("Gesti�n del conocimiento", "5. Gesti�n del conocimiento",variable))
  dataValor <- dataValor%>% mutate(variable = sub("Informaci�n para la toma de decisiones", "1. Informaci�n para la toma de decisiones",variable))
  dataValor <- dataValor%>% mutate(variable = sub("Insumos pol�tica p�blica", "2. Insumos pol�tica p�blica",variable))
 
  
#ESTRUCTURA
  estructura <- Output %>% select(Nombre, output, outcome,eje) %>% distinct()
  estructura <- merge(x = estructura, y= outcomeNombres, by.x = "outcome", by.y = "outcome") #une el output con su nombre
  estructura <- estructura %>% mutate(outcome = NULL) %>% mutate(output = NULL)
  setnames(x = estructura, old = "Nombre.x", new = "Output")
  setnames(x = estructura, old = "Nombre.y", new = "Outcome")
  estructura <- estructura %>% mutate(eje = ifelse(eje == 1, "1. Paz con Legalidad",eje)) %>%
                           mutate(eje = ifelse(eje == 2, "2. Migraci�n como factor de Desarrollo",eje)) %>%
                           mutate(eje = ifelse(eje == 3, "3. Asistencia T�cnica para la Aceleraci�n de los ODS Catalizadores",eje)) %>%
                           mutate(Marco = "Marco de cooperaci�n 2020 - 2023") %>%
                           mutate(Outputs = 1)
  setnames(x = estructura, old = "eje", new = "Eje")
    
#PROGRAMAS PROYECTADOS
  dataProyectos <- read_excel("datos.xlsx", sheet="prorgamaProyectado")
  names(dataProyectos) <- c("Programa","Agencias","Outcome","1.1","1.2","1.3","2.1","2.2","2.3","2.4","3.1","3.2","3.3","3.4","3.5","3.6","adicional","indice","formulario","Padre","id","uuid","fecha","estado")
  
  dataProyectos <- dataProyectos %>% select(-Outcome,-adicional, -indice,-formulario,-id,-uuid,-fecha,-estado)
  encuestaprincipal <- read_excel("datos.xlsx") %>% select(`General-Agencia`,`_index`)
  dataProyectos <- merge(x = dataProyectos, y= encuestaprincipal, by.x = "Padre", by.y = "_index")
  
  dataProyectos <- dataProyectos %>% select(-Padre)
  setnames(x = dataProyectos, old = "General-Agencia", new = "Agencia")
  
 
  
  dataProyectos <- dataProyectos %>% mutate(Agencia = ifelse(Agencia == "Alto Comisionando de las Naciones Unidas para los Refugiados- ACNUR", "ACNUR", Agencia))
  dataProyectos <- dataProyectos %>% mutate(Agencia = ifelse(Agencia == "ONU Mujeres", "ONU_M", Agencia))
  dataProyectos <- dataProyectos %>% mutate(Agencia = ifelse(Agencia == "Organizaci�n Internacional del Trabajo", "OIT", Agencia))
  dataProyectos <- dataProyectos %>% mutate(Agencia = ifelse(Agencia == "Organizaci�n Internacional para las Migraciones", "OIM", Agencia))
  dataProyectos <- dataProyectos %>% mutate(Agencia = ifelse(Agencia == "UN World Food Programme", "PMA", Agencia))
  dataProyectos <- dataProyectos %>% mutate(Agencia = ifelse(Agencia == "UNODC", "UNODC", Agencia))
  dataProyectos <- dataProyectos %>% mutate(Agencia = as.factor(Agencia))
  
  
  
  dataProyectos <- dataProyectos %>% mutate(
                    Agencias =gsub("\\-",",",Agencias),
                    Agencias =gsub("[yY]",",",Agencias),
                    Agencias =gsub("(ONUMUJERES|ONU Mujeres)","ONU_M",Agencias),
                    Agencias =gsub("ECLAC","CELAC",Agencias),
                    Agencias =gsub("OPS","OMS/OPS",Agencias),
                    Agencias =gsub("(ONU DDHH|DDHH)","OACNUDH",Agencias),
                    Agencias =gsub("UNIDO","ONUDI",Agencias),
                    Agencias =gsub("por definir","",Agencias),
                    Agencias =gsub(" ","",Agencias),
                    Agencias =gsub("ONUH�bitat,","",Agencias),
                    Agencias =gsub("(ECLAC|CELAC)","CEPAL",Agencias),
                    Agencias =gsub("OMS/OMS/OPS","OMS/OPS",Agencias),
                    Agencias =gsub("UNODC","UNODC",Agencias),
                    Agencias =gsub("UNDP","PNUD",Agencias),
                    Agencias =gsub("WFP","PMA",Agencias),)
  
  
  dataProyectos <-arrange(dataProyectos,dataProyectos$Programa)
  
  dataProyectos <-cSplit(dataProyectos, "Agencias", ",")
  
  proyectosMelted <- melt(dataProyectos,   # hace un pivot a la tabla de maner
                          id=c("Programa","1.1","1.2","1.3","2.1","2.2","2.3","2.4","3.1","3.2","3.3","3.4","3.5","3.6"), #los valores esten en una sola columna
                          value.name="Agencia" )
  
  proyectosMelted <- proyectosMelted %>% filter(!is.na(variable), !is.na(Agencia)) %>% select(-variable)
  
  setDT(proyectosMelted)
  proyectosMelted <- melt(proyectosMelted,   # hace un pivot a la tabla de maner
                          id=c("Programa","Agencia"), #los valores esten en una sola columna
                          value.name="Participa" ,variable.name = "Outcome")
  
  programasProyectaods <- proyectosMelted %>% arrange(proyectosMelted$Programa) %>% distinct() %>% mutate(Tipo="Proyectado")
  
  summary(as.factor(programasProyectaods$Agencia))

  
  #PROGRAMAS PRESENTES
  dataConjuntos <- read_excel("datos.xlsx", sheet="programaConjunto")
  names(dataConjuntos) <- c("Programa","Codigo","Agencias","Outcome","1.1","1.2","1.3","2.1","2.2","2.3","2.4","3.1","3.2","3.3","3.4","3.5","3.6","adicional","indice","formulario","Padre","id","uuid","fecha","estado")
  
  dataConjuntos <- dataConjuntos %>% select(-Outcome,-adicional, -indice,-formulario,-id,-uuid,-fecha,-estado,- Codigo)
  encuestaprincipal <- read_excel("datos.xlsx") %>% select(`General-Agencia`,`_index`)
  dataConjuntos <- merge(x = dataConjuntos, y= encuestaprincipal, by.x = "Padre", by.y = "_index")
  
  dataConjuntos <- dataConjuntos %>% select(-Padre)
  setnames(x = dataConjuntos, old = "General-Agencia", new = "Agencia")
  
  
 
  
  
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = ifelse(Agencia == "Alto Comisionando de las Naciones Unidas para los Refugiados- ACNUR", "ACNUR", Agencia))
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = ifelse(Agencia == "ONU Mujeres", "ONU_M", Agencia))
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = ifelse(Agencia == "Organizaci�n Internacional del Trabajo", "OIT", Agencia))
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = ifelse(Agencia == "Organizaci�n Internacional para las Migraciones", "OIM", Agencia))
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = ifelse(Agencia == "UN World Food Programme", "PMA", Agencia))
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = ifelse(Agencia == "UNODC", "UNODC", Agencia))
  dataConjuntos <- dataConjuntos %>% mutate(Agencia = as.factor(Agencia))
  
  
  
  dataConjuntos <- dataConjuntos %>% mutate(
    Agencias =gsub("\\-",",",Agencias),
    Agencias =gsub("\n",",",Agencias),
    Agencias =gsub("[yY]",",",Agencias),
    Agencias =gsub("(ONUMUJERES|ONU Mujeres|ONU MUJERES)","ONU_M",Agencias),
    Agencias =gsub("ECLAC","CELAC",Agencias),
    Agencias =gsub("OPS","OMS/OPS",Agencias),
    Agencias =gsub("(ONU DDHH|DDHH)","OACNUDH",Agencias),
    Agencias =gsub("UNIDO","ONUDI",Agencias),
    Agencias =gsub("por definir","",Agencias),
    Agencias =gsub(" ","",Agencias),
    Agencias =gsub("(ONUH�bitat|ONUHabit�t),","",Agencias),
    Agencias =gsub("ECLAC","CEPAL",Agencias),
    Agencias =gsub("OMS/OMS/OPS","OMS/OPS",Agencias),
    Agencias =gsub("UNODC","UNODC",Agencias),
    Agencias =gsub("WFP","PMA",Agencias),)
  
  
  dataConjuntos <- dataConjuntos %>% mutate(Agencias = as.factor(Agencias))
 
  
  dataConjuntos <-arrange(dataConjuntos,dataConjuntos$Programa)
  
  dataConjuntos <-cSplit(dataConjuntos, "Agencias", ",")
  
  ConjuntosMelted <- melt(dataConjuntos,   # hace un pivot a la tabla de maner
                          id=c("Programa","1.1","1.2","1.3","2.1","2.2","2.3","2.4","3.1","3.2","3.3","3.4","3.5","3.6"), #los valores esten en una sola columna
                          value.name="Agencia" )
  
  ConjuntosMelted <- ConjuntosMelted %>% filter(!is.na(variable), !is.na(Agencia)) %>% select(-variable)
  
  setDT(ConjuntosMelted)
  ConjuntosMelted <- melt(ConjuntosMelted,   # hace un pivot a la tabla de maner
                          id=c("Programa","Agencia"), #los valores esten en una sola columna
                          value.name="Participa" ,variable.name = "Outcome")
  
  programasConjuntos <- ConjuntosMelted %>% arrange(ConjuntosMelted$Programa) %>% distinct()  %>% mutate(Tipo="Existente")
  
  summary(as.factor(programasConjuntos$Agencia))
  
  
  programas <- rbind(programasConjuntos, programasProyectaods)
  
  #8.1 matriz colaboracion de agencias
  
  matrizColaboracion <- Agencias
  
  matrizColaboracion <- matrizColaboracion%>%mutate(Logo = NULL)
  
  matrizColaboracion <- matrizColaboracion%>%mutate(Agencia2 =Agencia)
  matrizColaboracion <- matrizColaboracion%>%mutate(valor =0)
  
  matrizColaboracion <- dcast(data=matrizColaboracion,Agencia ~ Agencia2, fill = 0 )
  matrizColaboracion <- matrizColaboracion%>%mutate(Agencia = NULL)
  row.names(matrizColaboracion) <- Agencias[,Agencia]
  
 
  
  
  
  for(ProgramaN in programasConjuntos$Programa){
    
    programasN <- programasConjuntos%>%filter(Programa == ProgramaN) %>% group_by(Agencia) %>% summarise(Participa = sum(as.integer(Participa)))
    
   
    for(x in 1:nrow(programasN)){
     
       for(y in x:nrow(programasN)){
        
        if(x != y){
        
        matrizColaboracion [programasN[x,]$Agencia, programasN[y,]$Agencia] <- matrizColaboracion [programasN[x,]$Agencia, programasN[y,]$Agencia] +1
        }
       }
     
      
    }
  }
  
  matrizColaboracion[matrizColaboracion >0]<-1
  
  matrizColaboracion <-  matrizColaboracion %>% mutate(Agencia = names(matrizColaboracion))
  
  setDT(matrizColaboracion)
  matrizColaboracion <- melt(matrizColaboracion, id="Agencia")
 
   #8.2 matriz Proyectada colaboracion de agencias
  
  matrizColaboracionP <- Agencias
  
  matrizColaboracionP <- matrizColaboracionP%>%mutate(Logo = NULL)
  
  matrizColaboracionP <- matrizColaboracionP%>%mutate(Agencia2 =Agencia)
  matrizColaboracionP <- matrizColaboracionP%>%mutate(valor =0)
  
  matrizColaboracionP <- dcast(data=matrizColaboracionP,Agencia ~ Agencia2, fill = 0 )
  matrizColaboracionP <- matrizColaboracionP%>%mutate(Agencia = NULL)
  row.names(matrizColaboracionP) <- Agencias[,Agencia]
  
  
  
  
  
  for(ProgramaN in programasProyectaods$Programa){
    
    programasN <- programasProyectaods%>%filter(Programa == ProgramaN) %>% group_by(Agencia) %>% summarise(Participa = sum(as.integer(Participa)))
    
    
    for(x in 1:nrow(programasN)){
      
      for(y in x:nrow(programasN)){
        
        if(x != y){
          
          matrizColaboracionP [programasN[x,]$Agencia, programasN[y,]$Agencia] <- matrizColaboracionP [programasN[x,]$Agencia, programasN[y,]$Agencia] +1
        }
      }
      
      
    }
  }
  
  matrizColaboracionP[matrizColaboracionP >0]<-1
  
  matrizColaboracionP <-  matrizColaboracionP %>% mutate(Agencia = names(matrizColaboracionP))
  
  setDT(matrizColaboracionP)
  matrizColaboracionP <- melt(matrizColaboracionP, id="Agencia")
  
  #8 PROCESAMINETO DE INFORMACI�N PARA GENERAR GR�FICAS
  
l <- list("Estructura"= estructura,"Agencias"= Agencias, "Outcome" = Outcome,"Output" = Output, "Alianzas" = alianzas, "Recursos" = Recursos, "ValorAgregado" = dataValor , "Programas" = programas, "ColabAgencias" = matrizColaboracion , "ColabProyAge" = matrizColaboracionP )
openxlsx::write.xlsx(l, "salida.xlsx")

