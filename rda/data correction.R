library(tidyr)
library(dplyr)
library(readxl)
library(XML)
library(openxlsx)
setwd("~/Desktop/Data")

### Costa Rica (CRI) ###
# when we need to correct the data for the previous date (-1) !

cases <- data.frame(read.csv(file = "CSV_POSITIVOS.csv", header = TRUE, sep = ","))
names(cases)[ncol(cases)-1] <- "yesterday_cases"
cases_cri <- aggregate(cases$yesterday_cases, by=list(Category=cases$provincia), FUN=sum)

deaths <- data.frame(read.csv(file = "CSV_FALLECIDOS.csv", header = TRUE, sep = ","))
names(deaths)[ncol(deaths)-1] <- "yesterday_deaths"
deaths_cri <- aggregate(deaths$yesterday_deaths, by=list(Category=deaths$provincia), FUN=sum)

#check the totals
sum(cases_cri$x)
sum(deaths_cri$x)

data.frame(cases_cri, deaths_cri)

### Paraguay (PRY) ### 
#Descargar_datos_Full_Data_data

PRY <- read.csv(file = "Descargar_datos_data.csv", skipNul = TRUE, sep ="\t" , fileEncoding = "UCS-2LE")
head(PRY)
PRY$Fecha.Confirmacion <- as.Date(PRY$Fecha.Confirmacion, format = "%m/%d/%Y")
PRY$Departamento.Residencia[PRY$Departamento.Residencia == "ASUNCIÓN"] <- "ASUNCION"
pry <- PRY %>%
  filter(Fecha.Confirmacion < "2021-07-29")
PRY2 <- pry %>% group_by(Departamento.Residencia) %>% count(Departamento.Residencia)
sum(PRY2$n)

pry.death <- read.csv(file = "FALLECIDOS_data.csv", skipNul = TRUE, sep ="\t" , fileEncoding = "UCS-2LE")
pry.death
pry.death$Fecha.Obito <- as.Date(pry.death$Fecha.Obito, format = "%m/%d/%Y")
pry.death$Departamento.Residencia[pry.death$Departamento.Residencia == "ASUNCIÓN"] <- "ASUNCION"
pry.death2 <- pry.death %>% filter(Fecha.Obito < "2021-07-28")
pry.death2 <- pry.death %>% group_by(Departamento.Residencia) %>% count(Departamento.Residencia)
sum(pry.death2$n)

pry11 <- left_join(PRY2, pry.death2, by = "Departamento.Residencia", suffix = c("Cases", "deaths"))
write.xlsx(pry11, file="PRY2.xlsx")

### Guatemala (GTM) ###
g_cases <- data.frame(read_excel("confirmados_mapa.xlsx", skip=1))
g_cases <- subset(g_cases, select=-c(1, municipio, poblacion))
print(g_cases)

g_cases_2 <- aggregate(g_cases$casos, by=list(Category=g_cases$departamento), FUN=sum)
print(g_cases_2)

g_cases_2$Category[g_cases_2$Category == "PETEN"] <- "EL PETEN"
g_cases_2$Category[g_cases_2$Category == "SIN DATOS"] <- "UNASSIGNED"

g_deaths <- data.frame(read_excel("fallecidos_mapa.xlsx", skip=1))
g_deaths <- subset(g_deaths, select=-c(1, municipio, poblacion))
print(g_deaths)

g_deaths_2 <- aggregate(g_deaths$casos, by=list(Category=g_deaths$departamento), FUN=sum)
print(g_deaths_2)

g_deaths_2$Category[g_deaths_2$Category == "PETEN"] <- "EL PETEN"
g_deaths_2$Category[g_deaths_2$Category == "SIN DATOS"] <- "UNASSIGNED"

g <- left_join(g_cases_2, g_deaths_2, by=c("Category"))

# alphabetical order (ISO_NAME)
g2 <- g %>% arrange(Category)
write.xlsx(g2, file="GTM.xlsx")

## peru
peru <- read.csv("positivos_covid.csv", sep = ";")
tail(peru)
peru2<- peru %>% group_by(DEPARTAMENTO) %>% count(DEPARTAMENTO)
write.xlsx(peru2, file="Peru_Adm1.xlsx")


### peru data adjustment

peru.pos <- read.csv("Peru_positivos.csv", sep = ";")
tail(peru.pos)

case_adm0 <- peru.pos %>% group_by(FECHA_RESULTADO) %>% count(FECHA_RESULTADO)
head(case_adm0)
write.xlsx(case_adm0, file="case_adm0.xlsx")

case_adm1 <- peru.pos %>% group_by(FECHA_RESULTADO, DEPARTAMENTO) %>% count()
case_adm1 <- case_adm1 %>% group_by(DEPARTAMENTO) %>%
  mutate(cumulative_sum = cumsum(n))

write.xlsx(case_adm1, file="case_adm1.xlsx")

peru.death <- read.csv("Peru_fallecidos.csv", sep = ";")
tail(peru.death)

death_adm0 <- peru.death %>% group_by(FECHA_FALLECIMIENTO) %>% count(FECHA_FALLECIMIENTO)
tail(death_adm0)
write.xlsx(death_adm0, file="death_adm0.xlsx")
View(peru.death)

peru.death$DEPARTAMENTO[peru.death$DEPARTAMENTO == "LIMA METROPOLITANA"] <- "LIMA"
peru.death$DEPARTAMENTO[peru.death$DEPARTAMENTO == "LIMA REGION"] <- "LIMA"
death_adm1 <- peru.death %>% group_by(FECHA_FALLECIMIENTO, DEPARTAMENTO) %>% count()
death_adm1_1 <- death_adm1 %>% group_by(DEPARTAMENTO) %>%
  mutate(cumulative_death = cumsum(n))

write.xlsx(death_adm1_1, file="death_adm1.xlsx")

### COL

COL <- read.csv("COL.csv", sep = ",")
tail(COL)

COL$Nombre.departamento[COL$Nombre.departamento == "BARRANQUILLA"] <- "ATLANTICO"
COL$Nombre.departamento[COL$Nombre.departamento == "CARTAGENA"] <- "BOLIVAR"
COL$Nombre.departamento[COL$Nombre.departamento == "BOGOTA"] <- "Santa Fe de Bogota DC"
COL$Nombre.departamento[COL$Nombre.departamento == "STA MARTA D.E."] <- "MAGDALENA"
COL$Nombre.departamento[COL$Nombre.departamento == "SAN ANDRES"] <- "Archipielago de San Andres, Providencia y Santa Catalina"
COL$Nombre.departamento[COL$Nombre.departamento == "GUAJIRA"] <- "LA GUAJIRA"

COL2<- COL %>% group_by(Nombre.departamento) %>% count(Nombre.departamento)
head(COL2)
write.xlsx(COL2, file="COL2.xlsx")

## PRI

pri <- read.csv("dataset_casos.csv", sep = ",")
pri$City[pri$City == "FUERA_DE_PR"] <- "Unassigned"
pri$City[pri$City == "N/A"] <- "Unassigned"
pri2 <- pri %>% group_by(City) %>% count(City)

#pri_death <- read.csv("dataset_defunciones.csv", sep = ",")
#pri.death <- pri_death %>% group_by(CO_REGION) %>% count(CO_REGION) --> only regional level

#check the totals
sum(pri2$n)

write.xlsx(pri2, file="pri_adm1")


