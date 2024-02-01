#Evaluación Caracterización de la hipertensión arterial en México: resultados de la Triple M 2021 
#Analisis: Neftali Eduardo Antonio Villa
#9 de Noviembre de 2023

####Dataset: Library Managment#####
library(tidyverse)
library(broom.mixed)
library(ggthemes)
#library(flipPlots)
library(networkD3)
library(tidygraph)
library(epiR)
library(scales)
library(readxl)
library(ggsci)
library(ggpubr)
library(RColorBrewer)
library(jtools)
library(gtsummary)
library(data.table)
library(ggalluvial)
library(ggsci)


####Dataset: Load#####

setwd("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/DRA PALOMO/Triple M 2021")
base <- read_excel("TRIPLE_M_2021.xlsx")
base<-base%>%mutate(base, id = rownames(base))

####Dataset: Imputation of Variables####

base2<-base%>%dplyr::select(id,Edad,PAS_1,PAD_1,FC_1,PAS_2,PAD_2,FC_2,PAS_3,PAD_3,FC_3,Peso)
base2_imp<-mice::mice(base2, m=5, maxit=5,seed = 123)
base2_imp_2<-complete(base2_imp,1)
base2_imp_2<-base2_imp_2%>%na.omit()
base<-base%>%left_join(base2_imp_2,by="id")%>%
  dplyr::select(-c(Edad.x,PAS_1.x,PAD_1.x,FC_1.x,PAS_2.x,PAD_2.x,FC_2.x,PAS_3.x,PAD_3.x,FC_3.x,Peso.x))%>%
  rename("Edad"=Edad.y,
         "PAS_1"=PAS_1.y,
         "PAD_1"=PAD_1.y,
         "FC_1"=FC_1.y,
         "PAS_2"=PAS_2.y,
         "PAD_2"=PAD_2.y,
         "FC_2"=FC_2.y,
         "PAS_3"=PAS_3.y,
         "PAD_3"=PAD_3.y,
         "FC_3"=FC_3.y,
         "Peso"=Peso.y)%>%
  filter(Edad>=18)


####Dataset: Recoding Variables#####

#Ciudad
base$Ciudad_REC<-NULL
base$Ciudad_REC[base$Ciudad=="Benito Juárez"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="edo. De mex"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="Edo. Mex."]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="estado de mexico"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="Estado de mexico"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="MEXCO"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="MEXICO"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="toluca"]<-"Area Metropolitana"
base$Ciudad_REC[base$Ciudad=="Toluca"]<-"Area Metropolitana"
base$Ciudad_REC<-na.tools::na.replace(base$Ciudad_REC,"Fuera Area Metropolitana")

#Regiones

base$ESTADO<-NULL
#Norte
base$ESTADO[base$Ciudad=="California"]<-"Baja California"
base$ESTADO[base$Ciudad=="Tijuana"]<-"Baja California"
base$ESTADO[base$Ciudad=="Cuiacán"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="CULIACAN"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="Culiacán"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="CULIACAN, SINALOA"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="GUAMUCHIL"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="GUASAVE"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="GUIASAVE"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="Los Mochis"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="Los Mochis, Ahome, Sinaloa"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="MAZALAN"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="Mazatlan"]<-"Sinaloa"
base$ESTADO[base$Ciudad=="Monclova"]<-"Coahuila"
base$ESTADO[base$Ciudad=="Nueva Rosita"]<-"Coahuila"
base$ESTADO[base$Ciudad=="Parras"]<-"Coahuila"
base$ESTADO[base$Ciudad=="Piedras Negras"]<-"Coahuila"
base$ESTADO[base$Ciudad=="Ramos Arizpe"]<-"Coahuila"
base$ESTADO[base$Ciudad=="Saltillo"]<-"Coahuila"
base$ESTADO[base$Ciudad=="Torreón"]<-"Coahuila"
base$ESTADO[base$Ciudad=="tamaulipas"]<-"Tamaulipas"

#Centro
base$ESTADO[base$Ciudad=="Benito Juárez"]<-"CDMX"
base$ESTADO[base$Ciudad=="edo. De mex"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="Edo. Mex."]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="estado de mexico"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="Estado de mexico"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="MEXCO"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="MEXICO"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="toluca"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="Toluca"]<-"Estado de Mexico"
base$ESTADO[base$Ciudad=="Apatzingán"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Cacaracuaro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Caracuaro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Huetamo"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Indaparapeo"]<-"Michoacan"
base$ESTADO[base$Ciudad=="maravatio"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Cenobio Moreno"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Cracuaro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Michoacan"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Michoacán, Mrl"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Moreli"]<-"Michoacan"
base$ESTADO[base$Ciudad=="morelia"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Morelia"]<-"Michoacan"
base$ESTADO[base$Ciudad=="MORELIA"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Morelia, Mich"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Nocupetaro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Otra"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Paracho Michoacán"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Paricuaro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Puruándiro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Tacámbaro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Tupataro"]<-"Michoacan"
base$ESTADO[base$Ciudad=="uruapan"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Uruapan"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Zacapu"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Zamora"]<-"Michoacan"
base$ESTADO[base$Ciudad=="Salvatierra"]<-"Guanajuato"
base$ESTADO[base$Ciudad=="Tarimoro"]<-"Guanajuato"
base$ESTADO[base$Ciudad=="guanajuato"]<-"Guanajuato"
base$ESTADO[base$Ciudad=="Guanajuato"]<-"Guanajuato"
base$ESTADO[base$Ciudad=="celaya"]<-"Guanajuato"
base$ESTADO[base$Ciudad=="Talpa"]<-"Jalisco"
base$ESTADO[base$Ciudad=="Puerto Vallarta"]<-"Jalisco"
base$ESTADO[base$Ciudad=="Tlaxcala"]<-"Tlaxcala"

#Sur
base$ESTADO[base$Ciudad=="Merida"]<-"Yucatan"
base$ESTADO[base$Ciudad=="CAMPCEHE"]<-"Campeche"
base$ESTADO[base$Ciudad=="campeche"]<-"Campeche"
base$ESTADO[base$Ciudad=="Campeche"]<-"Campeche"
base$ESTADO[base$Ciudad=="CAMPECHE"]<-"Campeche"
base$ESTADO[base$Ciudad=="CAPECHE"]<-"Campeche"
base$ESTADO[base$Ciudad=="champoton"]<-"Campeche"
base$ESTADO[base$Ciudad=="Coyuca de Catalan"]<-"Guerrero"
base$ESTADO[base$Ciudad=="Coyuca de Catalán"]<-"Guerrero"
base$ESTADO[base$Ciudad=="Acámbaro Gto"]<-"Guerrero"
base$ESTADO[base$Ciudad=="Guerrero"]<-"Guerrero"
base$ESTADO[base$Ciudad=="Cd. Altamirano"]<-"Guerrero"
base$ESTADO[base$Ciudad=="LZC"]<-"Guerrero"


#4 Regiones
base$REGIONES_4<-NULL
base$REGIONES_4[base$ESTADO=="Baja California"]<-1
base$REGIONES_4[base$ESTADO=="Coahuila"]<-1
base$REGIONES_4[base$ESTADO=="Sinaloa"]<-1
base$REGIONES_4[base$ESTADO=="Tamaulipas"]<-1


base$REGIONES_4[base$ESTADO=="Guanajuato"]<-2
base$REGIONES_4[base$ESTADO=="Jalisco"]<-2
base$REGIONES_4[base$ESTADO=="Michoacan"]<-2
base$REGIONES_4[base$ESTADO=="Tlaxcala"]<-2

base$REGIONES_4[base$ESTADO=="CDMX"]<-3
base$REGIONES_4[base$ESTADO=="Estado de Mexico"]<-3

base$REGIONES_4[base$ESTADO=="Guerrero"]<-4
base$REGIONES_4[base$ESTADO=="Campeche"]<-4
base$REGIONES_4[base$ESTADO=="Yucatan"]<-4
base$REGIONES_4<-na.tools::na.replace(base$REGIONES_4,1)

#Etnicidad

base$Etnicidad_Raza[base$Etnicidad_Raza=="Blanco"]<-1
base$Etnicidad_Raza[base$Etnicidad_Raza=="Mestizo"]<-2
base$Etnicidad_Raza[base$Etnicidad_Raza=="Otro"]<-2
base$Etnicidad_Raza[base$Etnicidad_Raza=="Negro"]<-3
base$Etnicidad_Raza[base$Etnicidad_Raza=="No Especificado"]<-3
base$Etnicidad_Raza[base$Etnicidad_Raza=="Oriente Medio"]<-3
base$Etnicidad_Raza[is.na(base$Etnicidad_Raza)]<-3

#Ultima PA

base$Ultima_vez_PA_CAT[base$Ultima_vez_PA_DIC==1]<-1
base$Ultima_vez_PA_CAT[base$Ultima_vez_PA_DIC==2]<-2
base$Ultima_vez_PA_CAT[base$Ultima_vez_PA=="nunca"]<-3
base$Ultima_vez_PA_CAT[base$Ultima_vez_PA=="Nunca"]<-3

#Tipos de remedio
base$Tipos_Remedios_PA[base$Tipos_Remedios_PA=="5 o mas"]<-5
base$Tipos_Remedios_PA[is.na(base$Tipos_Remedios_PA)]<-0
base$Tipos_Remedios_PA<-as.numeric(base$Tipos_Remedios_PA)

#Sexo
base$Sexo[base$Sexo=="Mujer"]<-0
base$Sexo[base$Sexo=="MUJER"]<-0
base$Sexo[base$Sexo=="mujer"]<-0
base$Sexo[base$Sexo=="Hombre"]<-1
base$Sexo[base$Sexo=="NA"]<-NA
base$Sexo[is.na(base$Sexo)]<-0


#Categorias de Edad

base$EDAD_CAT<-NULL
base$EDAD_CAT[base$Edad>=18 & base$Edad<35]<-1
base$EDAD_CAT[base$Edad>=35 & base$Edad<50]<-2
base$EDAD_CAT[base$Edad>=50 & base$Edad<65]<-3
base$EDAD_CAT[base$Edad>=65]<-4
base$EDAD_CAT<-factor(base$EDAD_CAT,labels = c("18-35","36-49","50-64",">65"))

#Alcohol

base$Alcohol_CAT[base$Alcohol=="Nunca o raramente"]<-0
base$Alcohol_CAT[base$Alcohol=="Nunca"]<-0
base$Alcohol_CAT[base$Alcohol=="Nunca/raramente"]<-0

base$Alcohol_CAT[base$Alcohol=="1 a 3 veces por mes"]<-1
base$Alcohol_CAT[base$Alcohol=="1  A LA SEMANA"]<-1
base$Alcohol_CAT[base$Alcohol=="1 A LA SEMANA"]<-1
base$Alcohol_CAT[base$Alcohol=="1 AL MES"]<-1
base$Alcohol_CAT[base$Alcohol=="1 vez por semana aprox"]<-1
base$Alcohol_CAT[base$Alcohol=="1-3 por mes"]<-1
base$Alcohol_CAT[base$Alcohol=="1-3 veces"]<-1
base$Alcohol_CAT[base$Alcohol=="1-3 veces por mes"]<-1

base$Alcohol_CAT[base$Alcohol=="1-6 veces por semana"]<-2
base$Alcohol_CAT[base$Alcohol=="1 a 6 veces por semana"]<-2
base$Alcohol_CAT[base$Alcohol=="Todos los dias"]<-2

base$Alcohol_CAT[is.na(base$Alcohol_CAT)]<-0

#APP HAS
base$APP_HAS[base$APP_HAS=="no"]<-0
base$APP_HAS[base$APP_HAS=="No"]<-0
base$APP_HAS[base$APP_HAS=="NO"]<-0
base$APP_HAS[base$APP_HAS=="NA"]<-0
base$APP_HAS[base$APP_HAS=="Sí"]<-1

#Embarazo actual

base$Embarazo_actual[base$Embarazo_actual=="no"]<-0
base$Embarazo_actual[base$Embarazo_actual=="No"]<-0
base$Embarazo_actual[base$Embarazo_actual=="NO"]<-0
base$Embarazo_actual[base$Embarazo_actual=="Sí"]<-1
base$Embarazo_actual[base$Embarazo_actual=="sí"]<-1
base$Embarazo_actual[base$Embarazo_actual=="NA"]<-0
base$Embarazo_actual[base$Embarazo_actual==1 & base$Sexo==1]<-NA
base$Embarazo_actual[base$Embarazo_actual==0 & base$Sexo==1]<-NA

#Ant Embarazo Hax

base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo=="no"]<-0
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo=="No"]<-0
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo=="NO"]<-0
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo=="Sí"]<-1
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo=="sí"]<-1
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo=="NA"]<-0
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo==1 & base$Sexo==1]<-NA
base$Incremento_PA_Embarazo[base$Incremento_PA_Embarazo==0 & base$Sexo==1]<-NA

#Estatina

base$Estatinas[base$Estatinas=="no"]<-0
base$Estatinas[base$Estatinas=="No"]<-0
base$Estatinas[base$Estatinas=="NO"]<-0
base$Estatinas[base$Estatinas=="NA"]<-0
base$Estatinas[base$Estatinas=="No sé"]<-0
base$Estatinas[base$Estatinas=="Sí"]<-1
base$Estatinas[base$Estatinas=="sí"]<-1

#Aspirina


base$Aspirinas[base$Aspirinas=="no"]<-0
base$Aspirinas[base$Aspirinas=="No"]<-0
base$Aspirinas[base$Aspirinas=="NO"]<-0
base$Aspirinas[base$Aspirinas=="NA"]<-0
base$Aspirinas[base$Aspirinas=="50"]<-0
base$Aspirinas[base$Aspirinas=="si"]<-1
base$Aspirinas[base$Aspirinas=="No sé"]<-0
base$Aspirinas[base$Aspirinas=="Sí"]<-1
base$Aspirinas[base$Aspirinas=="sí"]<-1
base$Aspirinas[is.na(base$Aspirinas)]<-0

#Diabetes

base$PA_Diabetes[base$PA_Diabetes=="NA"]<-0
base$PA_Diabetes[base$PA_Diabetes=="No"]<-0
base$PA_Diabetes[base$PA_Diabetes=="Sí"]<-1

#Antecedente At. Cora


base$PA_Ataque_Corazon[base$PA_Ataque_Corazon=="NA"]<-0
base$PA_Ataque_Corazon[base$PA_Ataque_Corazon=="No"]<-0
base$PA_Ataque_Corazon[base$PA_Ataque_Corazon=="no"]<-0
base$PA_Ataque_Corazon[base$PA_Ataque_Corazon=="No"]<-0
base$PA_Ataque_Corazon[base$PA_Ataque_Corazon=="NO"]<-0
base$PA_Ataque_Corazon[base$PA_Ataque_Corazon=="Sí"]<-1

#Antecedente EVC

base$PA_EVC[base$PA_EVC=="NA"]<-0
base$PA_EVC[base$PA_EVC=="No"]<-0
base$PA_EVC[base$PA_EVC=="no"]<-0
base$PA_EVC[base$PA_EVC=="No"]<-0
base$PA_EVC[base$PA_EVC=="NO"]<-0
base$PA_EVC[base$PA_EVC=="Sí"]<-1

#TAS (Promedio)

base$PAS_PROM<-rowMeans(as.matrix(base%>%dplyr::select(PAS_2,PAS_3)))

#TAD (Promedio)

base$PAD_PROM<-rowMeans(as.matrix(base%>%dplyr::select(PAD_2,PAD_3)))

#FC (Promedio)

base$FC_PROM<-rowMeans(as.matrix(base%>%dplyr::select(FC_2,FC_3)))

#TA ≥130/≥80
base$TA_ELEVADA<-NULL
base$TA_ELEVADA[(base$PAS_PROM>=130 |base$PAD_PROM>=80)]<-1
base$TA_ELEVADA<-na.tools::na.replace(base$TA_ELEVADA,0)

#Remedio 

base$Remedios_PA[base$Tipos_Remedios_PA>=1]<-1
base$Remedios_PA[base$Tipos_Remedios_PA<=0]<-0
base$Remedios_PA[is.na(base$Tipos_Remedios_PA)]<-0


#Remedios REC
base$REMEDIOS_REC<-NULL
base$REMEDIOS_REC[base$Tipos_Remedios_PA==0]<-0
base$REMEDIOS_REC[base$Tipos_Remedios_PA==1]<-1
base$REMEDIOS_REC[base$Tipos_Remedios_PA==2]<-2
base$REMEDIOS_REC[base$Tipos_Remedios_PA>=3]<-3
base$REMEDIOS_REC<-na.tools::na.replace(base$REMEDIOS_REC,0)

#Educacion 

base$EDU_REC[base$Anos_Educacion=="0"]<-1
base$EDU_REC[is.na(base$Anos_Educacion)]<-1
base$EDU_REC[base$Anos_Educacion=="1-6 años"]<-1
base$EDU_REC[base$Anos_Educacion=="7-12 años"]<-2
base$EDU_REC[base$Anos_Educacion=="Más de 12 anos"]<-3
base$EDU_REC<-na.tools::na.replace(base$EDU_REC,1)

#Ejercicio
base$Ejercicio_DIC[base$Ejercicio_DIC=="NA"]<-0
base$Ejercicio_DIC[base$Ejercicio_DIC=="No"]<-0
base$Ejercicio_DIC[base$Ejercicio_DIC=="no"]<-0
base$Ejercicio_DIC[base$Ejercicio_DIC=="No"]<-0
base$Ejercicio_DIC[base$Ejercicio_DIC=="NO"]<-0
base$Ejercicio_DIC[base$Ejercicio_DIC=="Sí"]<-1
base$Ejercicio_DIC[base$Ejercicio_DIC=="sí"]<-1

#COVID
base$COVID_PRUEBA[base$COVID_PRUEBA=="NA"]<-0
base$COVID_PRUEBA[base$COVID_PRUEBA=="No"]<-0
base$COVID_PRUEBA[base$COVID_PRUEBA=="Sí"]<-1

#FUMA
base$Fuma[base$Fuma=="NA"]<-1
base$Fuma[base$Fuma=="Nunca"]<-1
base$Fuma[base$Fuma=="No - pero fumé en el pasado"]<-2
base$Fuma[base$Fuma=="Sí"]<-3

#Tratamiento

base$PAGA_TRATAMIENTO[base$PAGA_TRATAMIENTO=="NA"]<-1
base$PAGA_TRATAMIENTO[base$PAGA_TRATAMIENTO=="No pago"]<-1
base$PAGA_TRATAMIENTO[base$PAGA_TRATAMIENTO=="No estoy seguro si pago una parte o todo"]<-2
base$PAGA_TRATAMIENTO[base$PAGA_TRATAMIENTO=="Pago una parte"]<-2
base$PAGA_TRATAMIENTO[base$PAGA_TRATAMIENTO=="Pago por todo"]<-3

#Razones
base$MANEJO_RAZON_DIC<-NULL
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Lo hago"]<-1
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="lo hago"]<-1
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Si"]<-1
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Sí"]<-1
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="no"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="No"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="NA"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="NO"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="No esta disponible fácilmente"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Prefiero medicina alternativa"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Se me olvida"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Efectos secundarios"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Es muy caro"]<-0
base$MANEJO_RAZON_DIC[base$MANEJO_RAZON=="Sólo las tomo cuando las necesito"]<-0
base$MANEJO_RAZON_DIC<-na.tools::na.replace(base$MANEJO_RAZON_DIC,1)

#Tratamiento Afectado

base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Antiguos medicamentos cambiaron"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Dege mis medicamentos"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Dejé mis medicamentos"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Medicamentos usuales no disponibles"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="No se tiene acceso a los servicios de atención médica"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="No se tiene accesso a servicios de atención médica"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="No, Ya no tengo acceso a medicamentos"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="SI"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="si"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Sí"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Si, Mis medicamentos cambiaron"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Si, pero mis medicamentos no están disponibles"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Suspendí todos mis medicamentos"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="Todos los dias"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="1-3 veces por mes"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC[base$TRATAMIENTO_AFECTADO_COVID=="1-6 veces por semana"]<-1
base$TRATAMIENTO_AFECTADO_COVID_DIC<-na.tools::na.replace(base$TRATAMIENTO_AFECTADO_COVID_DIC,0)

#CVD
base$CVD_DIC[base$PA_EVC==1 | base$PA_Ataque_Corazon==1]<-1
base$CVD_DIC<-na.tools::na.replace(base$CVD_DIC,0)

#Tipo de Recultamiento

base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Área Pública (Aire libre)"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Área pública (dentro)"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Área Pública (Espacio cerrado)"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Área pública (fuera)"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Casa"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="lugar de trabajo"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Lugar de trabajo"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Lugar de Trabajo"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Lugar de Trabajo (Ej. Trabajadores IMSS)"]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Otro"]<-1
base$Tipo_Reclutamiento_REC[is.na(base$Tipo_Reclutamiento_REC)]<-1
base$Tipo_Reclutamiento_REC[base$Tipo_Reclutamiento=="Hospital/Clínica/Farmacia"]<-2

#HAS 

base$HAS_ACTUAL<-NULL
base$HAS_ACTUAL[(base$PAS_PROM>=140 |base$PAD_PROM>=90) | base$APP_HAS==1 | base$REMEDIOS_REC>=1]<-1
base$HAS_ACTUAL<-na.tools::na.replace(base$HAS_ACTUAL,0)

#Categorias de HAS
base$CATEGORIA_HAS<-NULL
base$CATEGORIA_HAS[base$HAS_ACTUAL==1 & (base$APP_HAS==0)]<-1
base$CATEGORIA_HAS[base$HAS_ACTUAL==1 & (base$APP_HAS==1 & base$REMEDIOS_REC==0)]<-2
base$CATEGORIA_HAS[base$HAS_ACTUAL==1 & (base$APP_HAS==1 & base$REMEDIOS_REC>=1 & base$TA_ELEVADA==1)]<-3
base$CATEGORIA_HAS[base$HAS_ACTUAL==1 & (base$APP_HAS==1 & base$REMEDIOS_REC>=1 & base$TA_ELEVADA==0)]<-4
base$CATEGORIA_HAS[base$CATEGORIA_HAS>=1 & base$HAS_ACTUAL==0]<-NA
base$REMEDIOS_REC[base$REMEDIOS_REC>=1 & base$CATEGORIA_HAS==1]<-0

#Vacuna COVID
base$VACUNA_COVID_REC<-NULL
base$VACUNA_COVID_REC[base$VACUNA_COVID=="Sí"]<-1
base$VACUNA_COVID_REC[base$VACUNA_COVID!="Sí"]<-0

#Typos
base$Embarazo_actual<-as.numeric(base$Embarazo_actual)
base$Estatinas<-as.numeric(base$Estatinas)
base$Aspirinas<-as.numeric(base$Aspirinas)
base$PA_Diabetes<-as.numeric(base$PA_Diabetes)
base$COVID_PRUEBA<-as.numeric(base$COVID_PRUEBA)
base$Ejercicio_DIC<-as.numeric(base$Ejercicio_DIC)
base$PA_EVC<-as.numeric(base$PA_EVC)
base$PA_Ataque_Corazon<-as.numeric(base$PA_Ataque_Corazon)

####Analysis: Prevalence Stratification by Sociodemographic and COVID-19 Variables (Figure 1)#####

#Overall Prevalence
ncas <- table(base$HAS_ACTUAL)[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.1<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
                       conf.level = 0.95) * 100

#Sex 

ncas <- table(base[base$Sexo==1,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$Sexo==1,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.2.1<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100


ncas <- table(base[base$Sexo==0,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$Sexo==0,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.2.2<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

Fig.1.df.2<-rbind(Fig.1.df.2.1,Fig.1.df.2.2)
Fig.1.df.2$subgroup<-factor(c("Men","Women"),levels = c("Men","Women"))
Fig.1.df.2$group<-c("Sex")

#Regions
ncas <- table(base[base$REGIONES_4==1,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$REGIONES_4==1,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.3.1<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$REGIONES_4==2,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$REGIONES_4==2,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.3.2<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$REGIONES_4==3,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$REGIONES_4==3,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.3.3<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$REGIONES_4==4,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$REGIONES_4==4,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.3.4<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

Fig.1.df.3<-rbind(Fig.1.df.3.1,Fig.1.df.3.2,Fig.1.df.3.3,Fig.1.df.3.4)
Fig.1.df.3$subgroup<-factor(c("North","Central","Metropolitan Area","South"),levels = c("South","Central","North","Metropolitan Area"))
Fig.1.df.3$group<-c("Regions")

#Etnicity
ncas <- table(base[base$Etnicidad_Raza==1,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$Etnicidad_Raza==1,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.4.1<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$Etnicidad_Raza==2,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$Etnicidad_Raza==2,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.4.2<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$Etnicidad_Raza==3,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$Etnicidad_Raza==3,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.4.3<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

Fig.1.df.4<-rbind(Fig.1.df.4.1,Fig.1.df.4.2,Fig.1.df.4.3)
Fig.1.df.4$subgroup<-factor(c("Caucassian","Mexican-Mestizo","Afro-Descendant"),levels = c("Mexican-Mestizo","Caucassian","Afro-Descendant"))
Fig.1.df.4$group<-c("Ethnicity")

# Education
ncas <- table(base[base$EDU_REC==1,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$EDU_REC==1,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.5.1<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$EDU_REC==2,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$EDU_REC==2,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.5.2<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

ncas <- table(base[base$EDU_REC==3,]$HAS_ACTUAL)[2]; npop <- nrow(base[base$EDU_REC==3,])
tmp <- as.matrix(cbind(ncas, npop))
Fig.1.df.5.3<-epi.conf(tmp, ctype = "prevalence", method = "exact", N = 10000, design = 1, 
         conf.level = 0.95) * 100

Fig.1.df.5<-rbind(Fig.1.df.5.1,Fig.1.df.5.2,Fig.1.df.5.3)
Fig.1.df.5$subgroup<-factor(c("0-6 Years","7-12 Years",">13 Years"),levels = c("0-6 Years","7-12 Years",">13 Years"))
Fig.1.df.5$group<-c("Education Attainments")

Fig.1.df<-rbind(Fig.1.df.2,Fig.1.df.3,Fig.1.df.4,Fig.1.df.5)
Fig.1.df$est<-round(Fig.1.df$est,1)
Fig.1.df$lower<-round(Fig.1.df$lower,1)
Fig.1.df$upper<-round(Fig.1.df$upper,1)
Fig.1.df$group<-factor(Fig.1.df$group,levels = c("Sex","Regions","Ethnicity","Education Attainments"))

Fig.1.df.A<-ggplot(Fig.1.df,aes(subgroup, est,fill=group)) +
  geom_col() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  facet_wrap(~group,scales = "free_x")+
  xlab("")+
  ylab("Prevalence, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 10.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  scale_fill_tableau()+
  labs(fill="",title = "Sociodemographic Variables")


ggsave(file = "Figure1.1.pdf", 
       Fig.1.df.A,
       bg = "transparent",
       width = 40, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


####Analysis: Sankey Plot (Figure 2)####

base1.1<-base%>%filter(HAS_ACTUAL==1)
round(prop.table(table(base1.1$APP_HAS,useNA = "always"))*100,2)
round(prop.table(table(base1.1$APP_HAS,base1.1$Remedios_PA==1,useNA = "always"))*100,2)
round(prop.table(table(base1.1$APP_HAS,base1.1$Remedios_PA==1,base1.1$TA_ELEVADA,useNA = "always"))*100,2)

base1.1<-base%>%filter(HAS_ACTUAL==1)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
prop.table(table(base1.1$CATEGORIA_HAS))*100
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled
ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4
table(base$HAS_ACTUAL)
# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source=c("(All Subjects with Arterial Hypertension \n n=27,540)",
           "(All Subjects with Arterial Hypertension \n n=27,540)",
           "Diagnosed \n(69% [95%CI: 68.5%-69.6%])",
           "Diagnosed \n(69% [95%CI: 68.5%-69.6%])",
           "Treated \n(62.4% [95%CI: 61.9%-63.0%])",
           "Treated \n(62.4% [95%CI: 61.9%-63.0%])"), 
  target=c("Diagnosed \n(69% [95%CI: 68.5%-69.6%])",
           "Undiagnosed \n(30.9% [95%CI: 30.4%-31.5%])",
           "Treated \n(62.4% [95%CI: 61.9%-63.0%])",
           "Diagnosed but not treated \n(6.6% [95%CI: 6.3%-6.9%])",
           "Treated but no controlled \n(43.4% [95%CI: 42.9%-44.0%])",
           "Controlled \n(19% [95%CI: 18.6%-19.5%])"), 
  value=c(69,30.9,62.4,6.6,43.30,19)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1


brewer.pal(n = 7, name = "RdYlGn")
ColourScal ='d3.scaleOrdinal() .range(["#025189" ,"#025189", "#025189", "#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"])'

# Make the Network. I call my colour scale with the colourScale argument
Figure2A <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                          Value = "value", NodeID = "name",fontSize = 13,nodeWidth = 15, nodePadding = 10,fontFamily = "Arial",colourScale=ColourScal)

Figure2A




####Analysis: Logistic Regression Models (Table 3)####

#Model for undiagnosed diabetes
#Undiagnosed Hypertension

base2.1<-base[base$HAS_ACTUAL==0 | (base$HAS_ACTUAL==1 & base$CATEGORIA_HAS==1),]
base2.1$DIABETES_REC<-base2.1$PA_Diabetes==1
base2.1$COMORBD_REC<-(base2.1$PA_Diabetes==1)+(base2.1$PA_EVC==1)+(base2.1$PA_Ataque_Corazon==1)+(base2.1$Alcohol_CAT>=1)
base2.1$EDU_REC<-factor(base2.1$EDU_REC,levels = c(3,2,1))
base2.1$REGIONES_4<-relevel(factor(base2.1$REGIONES_4),ref = 3)

##Adjusted Regression Model
mod1<-glm(HAS_ACTUAL~Sexo+factor(EDU_REC)+factor(REGIONES_4)+factor(EDAD_CAT)+factor(Ejercicio_DIC==0)+factor(COVID_PRUEBA)+factor(Estatinas)+factor(Aspirinas),family = "binomial",data=base2.1)
summary(mod1)
summ(mod1,confint = T,exp = T)

mod1 %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()%>%
  as_flex_table()%>%
  flextable::save_as_docx(path="Table_2A.docx")

#Model for uncontroled blood pressure

base3.1<-base[base$HAS_ACTUAL==1 & (base$CATEGORIA_HAS==3 | base$CATEGORIA_HAS==4),]
base3.1$COMORBD_REC<-(base3.1$PA_Diabetes==1)+(base3.1$PA_EVC==1)+(base3.1$PA_Ataque_Corazon==1)+(base3.1$Alcohol_CAT>=1)
base3.1$EDU_REC<-factor(base3.1$EDU_REC,levels = c(3,2,1))
base3.1$REGIONES_4<-relevel(factor(base3.1$REGIONES_4),ref = 3)

#Uncontroled Hypertension

mod2<-glm(CATEGORIA_HAS!=4~Sexo+factor(EDAD_CAT)+factor(REMEDIOS_REC)+factor(Alcohol_CAT)+factor(Ejercicio_DIC==0)+factor(EDU_REC)+
            +factor(TRATAMIENTO_AFECTADO_COVID_DIC)+factor(REGIONES_4)+factor(PA_EVC),family = "binomial",data=base3.1)
summary(mod2)
summ(mod2,confint = T,exp = T)

mod2 %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
  ) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()%>%
  as_flex_table()%>%
  flextable::save_as_docx(path="Table_2B.docx")


####Analysis: Characterization of Adults living with arterial hypertension by Sex (Supplementary Figure 1A)####

#Women

base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Sexo==0)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.A.1.1<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Women")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=16,605", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Men

base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Sexo==1)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.A.1.2<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Men")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=10,941", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Sup.Fig.1.A.1<-ggarrange(Sup.Fig.1.A.1.1,Sup.Fig.1.A.1.2,ncol = 2,nrow = 1,labels = c("A",""),common.legend = T)

#Sexo
#Women
sup.base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Sexo==0)
sup.base1.1$CATEGORIA_HAS<-factor(sup.base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base1.1)

Sup.Fig.1.A.2.1<-ggplot(sup.base1.1,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Women \nn=16,605")

#Men
sup.base1.2<-base%>%filter(HAS_ACTUAL==1)%>%filter(Sexo==1)
sup.base1.2$CATEGORIA_HAS<-factor(sup.base1.2$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base1.2)

Sup.Fig.1.A.2.2<-ggplot(sup.base1.2,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Men \nn=10,941")


Sup.Fig.1.A.2<-ggarrange(Sup.Fig.1.A.2.1,Sup.Fig.1.A.2.2,ncol = 2,nrow = 1,labels = c("A"),common.legend = T,legend = "none")
Sup.Fig.1.A<-ggarrange(Sup.Fig.1.A.1,Sup.Fig.1.A.2,ncol = 2,nrow = 1,labels = c("A"),common.legend = T,legend = "none")

####Analysis: Characterization of Adults living with arterial hypertension by Regions (Supplementary Figure 1B)####

#North
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==1)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.B.1.1<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("North Region")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=13,447", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Central
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==2)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.B.1.2<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Central Region")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=2,996", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Metropolitan Area
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==3)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.B.1.3<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Metropolitan Area Region")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=9,006", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#South
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==4)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.B.1.4<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("South Region")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=2,082", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Sup.Fig.1.B.1<-ggarrange(Sup.Fig.1.B.1.1,Sup.Fig.1.B.1.2,Sup.Fig.1.B.1.3,Sup.Fig.1.B.1.4,ncol = 2,nrow = 2,labels = c("B","","",""))

#North
sup.base2.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==1)
sup.base2.1$CATEGORIA_HAS<-factor(sup.base2.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base2.1)

Sup.Fig.1.B.2.1<-ggplot(sup.base2.1,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("North \nn=13,447")

#Central
sup.base2.2<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==2)
sup.base2.2$CATEGORIA_HAS<-factor(sup.base2.2$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base2.2)

Sup.Fig.1.B.2.2<-ggplot(sup.base2.2,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Central \nn=2,996")

#Metropolitan
sup.base2.3<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==3)
sup.base2.3$CATEGORIA_HAS<-factor(sup.base2.3$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base2.3)

Sup.Fig.1.B.2.3<-ggplot(sup.base2.3,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Metropolitan Area \nn=9,006")

#South
sup.base2.4<-base%>%filter(HAS_ACTUAL==1)%>%filter(REGIONES_4==4)
sup.base2.4$CATEGORIA_HAS<-factor(sup.base2.4$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base2.4)

Sup.Fig.1.B.2.4<-ggplot(sup.base2.4,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("South \nn=2,082")

Sup.Fig.1.B.2<-ggarrange(Sup.Fig.1.B.2.1,Sup.Fig.1.B.2.2,Sup.Fig.1.B.2.3,Sup.Fig.1.B.2.4,ncol = 2,nrow = 2,common.legend = T,legend = "none")
Sup.Fig.1.B<-ggarrange(Sup.Fig.1.B.1,Sup.Fig.1.B.2,ncol = 2,nrow = 1,common.legend = T,legend = "none")

####Analysis: Characterization of Adults living with arterial hypertension by Ethicity (Supplementary Figure 1C)####

#White
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Etnicidad_Raza==1)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.C.1.1<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Caucassian")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=35", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Mexican-Meztizo
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Etnicidad_Raza==2)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.C.1.2<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Mexican-Meztizo")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=27,359", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Afro-Descendant
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Etnicidad_Raza==3)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.C.1.3<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("Afro-Descendant")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=152", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


Sup.Fig.1.C.1<-ggarrange(Sup.Fig.1.C.1.1,Sup.Fig.1.C.1.2,Sup.Fig.1.C.1.3,ncol = 3,nrow = 1,labels = c("C",""),common.legend = T)

#White
sup.base3.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(Etnicidad_Raza==1)
sup.base3.1$CATEGORIA_HAS<-factor(sup.base3.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base3.1)

Sup.Fig.1.C.2.1<-ggplot(sup.base3.1,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Caucassian \nn=35")

#Mexican-Meztizo
sup.base3.2<-base%>%filter(HAS_ACTUAL==1)%>%filter(Etnicidad_Raza==2)
sup.base3.2$CATEGORIA_HAS<-factor(sup.base3.2$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base3.2)

Sup.Fig.1.C.2.2<-ggplot(sup.base3.2,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Mexican-Meztizo \nn=27,359")

#Afro-Descendant

sup.base3.3<-base%>%filter(HAS_ACTUAL==1)%>%filter(Etnicidad_Raza==3)
sup.base3.3$CATEGORIA_HAS<-factor(sup.base3.3$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base3.3)

Sup.Fig.1.C.2.3<-ggplot(sup.base3.3,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=2,byrow=T))+
  ggtitle("Afro-Descendant \nn=152")

Sup.Fig.1.C.2<-ggarrange(Sup.Fig.1.C.2.1,Sup.Fig.1.C.2.2,Sup.Fig.1.C.2.3,ncol = 3,nrow = 1,common.legend = T,legend = "none")
Sup.Fig.1.C<-ggarrange(Sup.Fig.1.C.1,Sup.Fig.1.C.2,ncol = 2,nrow = 1,common.legend = T,legend = "none")

####Analysis: Characterization of Adults living with arterial hypertension by Education (Supplementary Figure 1D)####

#0-6 Years
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDU_REC==1)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.D.1.1<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("0-6 Education Years")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=9,672", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#7-12
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDU_REC==2)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.D.1.2<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle("7-12 Education Years")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=12,731", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#≥12
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDU_REC==3)
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

Sup.Fig.1.D.1.3<-ggplot(Sup.figura.1A.1.df,aes(group, est)) +
  geom_col(fill=c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_pubclean()+
  xlab("")+
  ylab("Proportion, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 5.0),
    position = position_dodge(0.9),
    vjust = 0)+
  scale_y_continuous(limits = c(-.1,100))+
  labs(fill="Type")+
  ggtitle(">13 Education Years")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  stat_pvalue_manual(as.data.frame(tribble(
    ~group1, ~group2, ~label, ~y.position,
    1,4, "Living with Arterial Hypertension \n n=5,143", 90,
    2,4, paste("Diagnosed:",paste0(round(dat2.1[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.1[2],2),"%"," ","-"," ",round(dat2.1[3],2),"%",")")), 75,
    3,4, paste("Treated:",paste0(round(dat2.2[1],1),"%"), "\n",paste0("(95% CI: ", round(dat2.2[2],2),"%"," ","-"," ",round(dat2.2[3],2),"%",")")), 60
  )), bracket.shorten = 0.001)+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

Sup.Fig.1.D.1<-ggarrange(Sup.Fig.1.D.1.1,Sup.Fig.1.D.1.2,Sup.Fig.1.D.1.3,ncol = 3,nrow = 1,labels = c("D",""),common.legend = T)
#0-6 years

sup.base4.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDU_REC==1)
sup.base4.1$CATEGORIA_HAS<-factor(sup.base4.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base4.1)

Sup.Fig.1.D.2.1<-ggplot(sup.base4.1,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=1,byrow=T))+
  ggtitle("0-6 Education Years \nn=9,672")

#7-12 Education Years
sup.base4.2<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDU_REC==2)
sup.base4.2$CATEGORIA_HAS<-factor(sup.base4.2$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base4.2)

Sup.Fig.1.D.2.2<-ggplot(sup.base4.2,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=1,byrow=T))+
  ggtitle("7-12 Education Years \nn=12,731")

#>13 Education Years

sup.base4.2<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDU_REC==3)
sup.base4.2$CATEGORIA_HAS<-factor(sup.base4.2$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(sup.base4.2)

Sup.Fig.1.D.2.3<-ggplot(sup.base4.2,aes(EDAD_CAT,fill=factor(CATEGORIA_HAS)))+
  geom_bar(position = "fill")+
  xlab("Age Categories, (Years)")+
  ylab("Proportion, (%)")+
  labs(fill="")+
  scale_fill_manual(values = c("#DE231D" ,"#EF6E18" ,"#F9D43E", "#1A9850"))+
  theme_hc()+
  geom_text(data = . %>% 
              group_by(EDAD_CAT, CATEGORIA_HAS) %>%
              tally() %>%
              mutate(p = round(n / sum(n),2)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p)),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            col="white")+
  guides(fill=guide_legend(nrow=1,byrow=T))+
  ggtitle(">13 Education Years \nn=5,143")

Sup.Fig.1.D.2<-ggarrange(Sup.Fig.1.D.2.1,Sup.Fig.1.D.2.2,Sup.Fig.1.D.2.3,ncol = 3,nrow = 1,common.legend = T,legend = "none")
Sup.Fig.1.D<-ggarrange(Sup.Fig.1.D.1,Sup.Fig.1.D.2,ncol = 2,nrow = 1,common.legend = T,legend = "none")

####Analysis: Characterization of Adults living with arterial hypertension by Age Categories (Supplementary Figure 1E)#####

#18-35
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDAD_CAT=="18-35")
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

#36-49
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDAD_CAT=="36-49")
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))


#50-64
base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDAD_CAT=="50-64")
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))

#65

base1.1<-base%>%filter(HAS_ACTUAL==1)%>%filter(EDAD_CAT==">65")
base1.1$CATEGORIA_HAS<-factor(base1.1$CATEGORIA_HAS,labels = c("Undiagnosed","Diagnosed but no treated", "Treated but no controlled","Controlled"))
nrow(base1.1)

#Undiagnosed
ncas <- table(base1.1$CATEGORIA_HAS)[1]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat1

#Diagnosed but no treated
ncas <- table(base1.1$CATEGORIA_HAS)[2]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat2

#Diagnosed

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[2])+as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.1<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.1

#Treated

ncas <- as.numeric(table(base1.1$CATEGORIA_HAS)[3])+as.numeric(table(base1.1$CATEGORIA_HAS)[4]); npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat2.2<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                  conf.level = 0.95) * 100)[1:3];dat2.2


#Treated but no controlled
ncas <- table(base1.1$CATEGORIA_HAS)[3]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat3<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat3

#Controlled

ncas <- table(base1.1$CATEGORIA_HAS)[4]; npop <- nrow(base1.1)
tmp <- as.matrix(cbind(ncas, npop))
dat4<-(epi.conf(tmp, ctype = "prevalence", method = "exact", N = 1000, design = 1, 
                conf.level = 0.95) * 100)[1:3];dat4

#Mergind Dataset
Sup.figura.1A.1.df<-NULL
Sup.figura.1A.1.df$group<-c(1,2,3,4)
Sup.figura.1A.1.df<-cbind(Sup.figura.1A.1.df,round(rbind(dat1,dat2,dat3,dat4),1))
Sup.figura.1A.1.df$group<-factor(Sup.figura.1A.1.df$group,
                                 levels = c(1,2,3,4),labels = c("Undiagnosed","Diagnosed \nbut no treated", "Treated \nbut no controlled","Controlled"))


####Analysis: Interaction Models#####


#Undiagnosed Hypertension

base2.1<-base[base$HAS_ACTUAL==0 | (base$HAS_ACTUAL==1 & base$CATEGORIA_HAS==1),]
base2.1$DIABETES_REC<-base2.1$PA_Diabetes==1
base2.1$COMORBD_REC<-(base2.1$PA_Diabetes==1)+(base2.1$PA_EVC==1)+(base2.1$PA_Ataque_Corazon==1)+(base2.1$Alcohol_CAT>=1)
base2.1$EDU_REC<-factor(base2.1$EDU_REC,levels = c(3,2,1))
base2.1$REGIONES_4<-relevel(factor(base2.1$REGIONES_4),ref = 3)


mod1.1<-glm(HAS_ACTUAL~Sexo+scale(Edad)+factor(COVID_PRUEBA)+factor(Estatinas)+factor(Ejercicio_DIC==0)*factor(EDU_REC)+factor(Aspirinas)+factor(REGIONES_4),family = "binomial",data=base2.1)
summary(mod1.1)
summ(mod1.1,confint = T,exp = T)

mod1.2<-glm(HAS_ACTUAL~Sexo+scale(Edad)+factor(COVID_PRUEBA)+factor(Estatinas)+factor(Ejercicio_DIC==0)*factor(REGIONES_4)+factor(EDU_REC)+factor(Aspirinas)+factor(REGIONES_4),family = "binomial",data=base2.1)
summary(mod1.2)
summ(mod1.2,confint = T,exp = T)

Sup.Figure3A<-plot_summs(mod1.1,mod1.2, exp=T,coefs = c("No-Physical Activity and 7-12 Education Years"="factor(Ejercicio_DIC == 0)TRUE:factor(EDU_REC)2",
                                                        "No-Physical Activity and 0-6 Education Years"="factor(Ejercicio_DIC == 0)TRUE:factor(EDU_REC)1",
                                                        "No-Physical Activity and North Region"="factor(Ejercicio_DIC == 0)TRUE:factor(REGIONES_4)1",
                                                        "No-Physical Activity and Central Region"="factor(Ejercicio_DIC == 0)TRUE:factor(REGIONES_4)2",
                                                        "No-Physical Activity and South Region"="factor(Ejercicio_DIC == 0)TRUE:factor(REGIONES_4)4"))+
  xlab("Logistic Regression Model: \nOdds Ratio (OR, 95% CI)")+
  ylab("")+
  ggtitle("Undiagnosed Arterial Hypertension")+
  theme_minimal() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())+
  guides(colour = guide_legend(nrow = 3))+
  scale_x_log10()+
  labs(caption = "Adjusted for: sex, age, previous COVID-19 infection, use of statins, and use of aspirins")


#Model for uncontroled blood pressure

base3.1<-base[base$HAS_ACTUAL==1 & (base$CATEGORIA_HAS==3 | base$CATEGORIA_HAS==4),]
base3.1$COMORBD_REC<-(base3.1$PA_Diabetes==1)+(base3.1$PA_EVC==1)+(base3.1$PA_Ataque_Corazon==1)+(base3.1$Alcohol_CAT>=1)
base3.1$EDU_REC<-factor(base3.1$EDU_REC,levels = c(3,2,1))
base3.1$REGIONES_4<-relevel(factor(base3.1$REGIONES_4),ref = 3)

mod2.1<-glm(CATEGORIA_HAS!=4~Sexo+scale(Edad)+factor(REMEDIOS_REC)+factor(Alcohol_CAT)+factor(Ejercicio_DIC==0)+factor(EDU_REC)+
              +factor(MANEJO_RAZON_DIC)+factor(TRATAMIENTO_AFECTADO_COVID_DIC)*factor(REGIONES_4),family = "binomial",data=base3.1)
summary(mod2.1)
summ(mod2.1,confint = T,exp = T)

mod2.2<-glm(CATEGORIA_HAS!=4~Sexo+scale(Edad)+factor(REMEDIOS_REC)+factor(Alcohol_CAT)+factor(Ejercicio_DIC==0)+factor(EDU_REC)+
              +factor(MANEJO_RAZON_DIC)*factor(REGIONES_4)+factor(TRATAMIENTO_AFECTADO_COVID_DIC),family = "binomial",data=base3.1)
summary(mod2.2)
summ(mod2.2,confint = T,exp = T)

mod2.3<-glm(CATEGORIA_HAS!=4~Sexo+scale(Edad)+factor(REMEDIOS_REC)+factor(Alcohol_CAT)+factor(Ejercicio_DIC==0)+factor(EDU_REC)*factor(REGIONES_4)
            +factor(MANEJO_RAZON_DIC)+factor(TRATAMIENTO_AFECTADO_COVID_DIC),family = "binomial",data=base3.1)
summary(mod2.3)
summ(mod2.3,confint = T,exp = T)


Sup.Figure3B<-plot_summs(mod2.1,mod2.2,mod2.3, exp=T,
                         coefs = c("Treatment affected by COVID-19 and North Region"="factor(TRATAMIENTO_AFECTADO_COVID_DIC)1:factor(REGIONES_4)1",
                                   "Treatment affected by COVID-19 and Central Region"="factor(TRATAMIENTO_AFECTADO_COVID_DIC)1:factor(REGIONES_4)2",
                                   "Treatment affected by COVID-19 and South Region"="factor(TRATAMIENTO_AFECTADO_COVID_DIC)1:factor(REGIONES_4)4",
                                   "Non-Fully Adherence and North Region"="factor(MANEJO_RAZON_DIC)1:factor(REGIONES_4)1",
                                   "Non-Fully Adherence and Central Region"="factor(MANEJO_RAZON_DIC)1:factor(REGIONES_4)2",
                                   "Non-Fully Adherence and South Region"="factor(MANEJO_RAZON_DIC)1:factor(REGIONES_4)4",
                                   "7-12 Education Years and South Region"="factor(EDU_REC)2:factor(REGIONES_4)4",
                                   "0-6 Education Years and South Region"="factor(EDU_REC)1:factor(REGIONES_4)4"))+
  xlab("Logistic Regression Model: \nOdds Ratio (OR, 95% CI)")+
  ylab("")+
  ggtitle("Uncontrolled Arterial Hypertension")+
  theme_minimal() +
  theme(axis.line=element_blank(),panel.grid.minor = element_blank(), axis.ticks.y = element_blank())+
  guides(colour = guide_legend(nrow = 3))+
  scale_x_log10()+
  labs(caption = "Adjusted for: sex, age, number of antihypertensives, use of alcohol and physical activity")


Sup.Figure9<-ggarrange(Sup.Figure3A,Sup.Figure3B, labels = c("A", "B"), ncol=2)

ggsave(file = "Supplementary_Figure_9.pdf", 
       Sup.Figure9,
       bg = "transparent",
       width = 50, 
       height = 12,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


####Dataset: Factor####

base$Sexo<-factor(base$Sexo,labels = c("Women","Men"))
base$REGIONES_4<-factor(base$REGIONES_4,labels = c("North","Central","Metropolitan","South"))
base$Etnicidad_Raza<-factor(base$Etnicidad_Raza,labels = c("Caucassian","Mexican−Mestizo","Afro−Descendant"))
base$Ultima_vez_PA_CAT<-factor(base$Ultima_vez_PA_CAT,labels = c("<12 Months","≥12 Months","Never"))
base$Fuma<-factor(base$Fuma,labels = c("Never-Smoking","Quit-Smoking","Active-Smoking"))
base$Alcohol_CAT<-factor(base$Alcohol_CAT,labels = c("Never-Drink","Frequent Intake","Daily Intake"))
base$EDU_REC<-factor(base$EDU_REC,labels = c("0-6","7-12",">13"))
base$REMEDIOS_REC<-factor(base$REMEDIOS_REC,labels = c("No-Theraphy",
                                                       "Mono-Therapy",
                                                       "Dual-Therapy",
                                                       "Triple-Therapy"))
base$CATEGORIA_HAS<-factor(base$CATEGORIA_HAS,labels = c("Undiagnosed","Untreated","Controlled","Uncontrolled"))

####Dataset: Labels#####

###Labels##
setattr(base$CATEGORIA_HAS, "label", "Clinical and Treatment Profiles, (%)")
setattr(base$Sexo, "label", "Sex, (%)")
setattr(base$Edad, "label", "Age, (Years)")
setattr(base$REGIONES_4, "label", "Region, (%)")
setattr(base$Tipo_Reclutamiento_REC, "label", "Type of Recruitment, (%)")
setattr(base$Etnicidad_Raza, "label", "Self-Reported Ethnicity, (%)")
setattr(base$Ultima_vez_PA_CAT, "label", "Time Since Clinical Visit, (%)")
setattr(base$Incremento_PA_Embarazo, "label", "High Arterial Blood Pressure During Pregnancy, (%)")
setattr(base$Embarazo_actual, "label", "Current Pregnancy, (%)")
setattr(base$Estatinas, "label", "Statin Use, (%)")
setattr(base$Aspirinas, "label", "Aspirin Use, (%)")
setattr(base$PA_Diabetes, "label", "Diabetes, (%)")
setattr(base$Fuma, "label", "Smoking Status, (%)")
setattr(base$Alcohol_CAT, "label", "Alcohol Intake, (%)")
setattr(base$COVID_PRUEBA, "label", "Previous COVID-19 Infection, (%)")
setattr(base$TRATAMIENTO_AFECTADO_COVID_DIC, "label", "Antihypertensive Treatment Affected by COVID-19, (Years)")
setattr(base$VACUNA_COVID_REC, "label", "COVID-19 Vaccine, (%)")
setattr(base$Ejercicio_DIC, "label", "Previous Excercise, (%)")
setattr(base$EDU_REC, "label", "Education Years, (%)")
setattr(base$CVD_DIC, "label", "Previous CVD, (%)")
setattr(base$PA_Ataque_Corazon, "label", "Previous Hearth Attack, (%)")
setattr(base$PA_EVC, "label", "Previous Stroke, (%)")
setattr(base$PAS_PROM, "label", "SBP, (mmHg)")
setattr(base$PAD_PROM, "label", "DBP, (mmHg)")
setattr(base$FC_PROM, "label", "HR, (bpm)")
setattr(base$MANEJO_RAZON_DIC, "label", "Fully-Antihypertensive Adherence, (%)")
setattr(base$REMEDIOS_REC, "label", "Antihypertensive Treatment, (%)")
setattr(base$Peso, "label", "Weight, (Kg)")

####Analysis: Descriptive Characteristics of Hypertension By Treatment Profiles (Table 1)####

c("{n}", "{p}%") %>%         
  purrr::map(
    ~base %>% 
      dplyr::filter(HAS_ACTUAL==1)%>%
      select(CATEGORIA_HAS,
             Sexo,Edad,EDU_REC,REGIONES_4,Etnicidad_Raza,
             Ejercicio_DIC,Fuma,Alcohol_CAT,Incremento_PA_Embarazo,Embarazo_actual,
             PA_Diabetes,CVD_DIC,PA_Ataque_Corazon,PA_EVC,Estatinas,Aspirinas,
             COVID_PRUEBA,TRATAMIENTO_AFECTADO_COVID_DIC,VACUNA_COVID_REC,
             Ultima_vez_PA_CAT,REMEDIOS_REC,
             Peso,PAS_PROM,PAD_PROM,FC_PROM) %>% 
      tbl_summary(statistic = all_categorical() ~ .x,
                  missing = "ifany",
                  by = CATEGORIA_HAS,
                  digits = list(
                    all_categorical() ~ ifelse(.x == "{p}%", 1, 0),
                    all_continuous() ~ 0),
                  missing_text = "(Missing)")%>%
      bold_labels()%>%
      add_overall()%>%
      modify_table_body(
        dplyr::mutate,
        label = ifelse(label == "N missing (% missing)",
                       "Unknown",
                       label))%>%
      as_flex_table()%>%
      flextable::save_as_docx(path="Table_1.docx"))

####Analysis: Mexican State of Recluitment (Suplementary Table 2)#####

Sup.Tab.2<-base%>%group_by(ESTADO)%>%summarise(n=n())%>%mutate(prop=round((n/nrow(base))*100,1))%>%
  mutate(label=paste(n,"(",prop,")"))%>%
  dplyr::arrange(-n)
View(Sup.Tab.2)
nrow(base)

prop.table(table(base$REGIONES_4,base$HAS_ACTUAL),2)*100

####Analysis: Descriptive Characteristics Among Hypertension and No-Hypertension (Supplementary Table 3)----

c("{n}", "{p}%") %>%         
  purrr::map(
    ~base %>% 
      select(HAS_ACTUAL,
             Sexo,Edad,EDU_REC,REGIONES_4,Etnicidad_Raza,
             Ejercicio_DIC,Fuma,Alcohol_CAT,Incremento_PA_Embarazo,Embarazo_actual,
             PA_Diabetes,CVD_DIC,PA_Ataque_Corazon,PA_EVC,Estatinas,Aspirinas,
             COVID_PRUEBA,TRATAMIENTO_AFECTADO_COVID_DIC,VACUNA_COVID_REC,
             Ultima_vez_PA_CAT,REMEDIOS_REC,CATEGORIA_HAS,
             Peso,PAS_PROM,PAD_PROM,FC_PROM) %>% 
      tbl_summary(statistic = all_categorical() ~ .x,
                  missing = "ifany",
                  by = HAS_ACTUAL,
                  digits = list(
                    all_categorical() ~ ifelse(.x == "{p}%", 1, 0),
                    all_continuous() ~ 0),
                  missing_text = "(Missing)")%>%
      bold_labels()%>%
      add_overall()%>%
      modify_table_body(
        dplyr::mutate,
        label = ifelse(label == "N missing (% missing)",
                       "Unknown",
                       label))%>%
      as_flex_table()%>%
      flextable::save_as_docx(path="Sup_Tab_3.docx"))




####Analysis: Univariate Logistic Regression Model (Supplementary Table 5) ######

##Univariate Model
base2.1<-base[base$HAS_ACTUAL==0 | (base$HAS_ACTUAL==1 & base$CATEGORIA_HAS==1),]
base2.1$DIABETES_REC<-base2.1$PA_Diabetes==1
base2.1$COMORBD_REC<-(base2.1$PA_Diabetes==1)+(base2.1$PA_EVC==1)+(base2.1$PA_Ataque_Corazon==1)+(base2.1$Alcohol_CAT>=1)
base2.1$EDU_REC<-factor(base2.1$EDU_REC,levels = c(3,2,1))
base2.1$REGIONES_4<-relevel(factor(base2.1$REGIONES_4),ref = 3)
base2.1$Alcohol_CAT<-factor(base2.1$Alcohol_CAT)
base2.1$REMEDIOS_REC<-factor(base2.1$REMEDIOS_REC)
base2.1$Ultima_vez_PA_CAT<-factor(base2.1$Ultima_vez_PA_CAT)

t1.1<-base2.1%>%
  dplyr::select(HAS_ACTUAL,
                Sexo,Edad,EDU_REC,REGIONES_4,Etnicidad_Raza,
                Ejercicio_DIC,Fuma,Alcohol_CAT,Incremento_PA_Embarazo,Embarazo_actual,
                PA_Diabetes,CVD_DIC,PA_Ataque_Corazon,PA_EVC,Estatinas,Aspirinas,
                COVID_PRUEBA,TRATAMIENTO_AFECTADO_COVID_DIC,VACUNA_COVID_REC,
                Ultima_vez_PA_CAT,
                Peso,FC_PROM) %>%
  tbl_uvregression(
    method = glm,
    y = HAS_ACTUAL,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2),
    hide_n = T) %>%
  bold_labels()%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label) )


#Model for uncontroled blood pressure

base3.1<-base[base$HAS_ACTUAL==1 & (base$CATEGORIA_HAS==3 | base$CATEGORIA_HAS==4),]
base3.1$COMORBD_REC<-(base3.1$PA_Diabetes==1)+(base3.1$PA_EVC==1)+(base3.1$PA_Ataque_Corazon==1)+(base3.1$Alcohol_CAT>=1)
base3.1$EDU_REC<-factor(base3.1$EDU_REC,levels = c(3,2,1))
base3.1$REGIONES_4<-relevel(factor(base3.1$REGIONES_4),ref = 3)
base3.1$Alcohol_CAT<-factor(base3.1$Alcohol_CAT)
base3.1$REMEDIOS_REC<-factor(base3.1$REMEDIOS_REC)
base3.1$Ultima_vez_PA_CAT<-factor(base3.1$Ultima_vez_PA_CAT)

base3.1$CATEGORIA_HAS_REC<-NULL
base3.1$CATEGORIA_HAS_REC[base3.1$CATEGORIA_HAS!=4]<-1
base3.1$CATEGORIA_HAS_REC[base3.1$CATEGORIA_HAS==4]<-0


t1.2<-base3.1%>%
  dplyr::select(CATEGORIA_HAS_REC,
                Sexo,Edad,EDU_REC,REGIONES_4,Etnicidad_Raza,
                Ejercicio_DIC,Fuma,Alcohol_CAT,Incremento_PA_Embarazo,Embarazo_actual,
                PA_Diabetes,CVD_DIC,PA_Ataque_Corazon,PA_EVC,Estatinas,Aspirinas,
                COVID_PRUEBA,TRATAMIENTO_AFECTADO_COVID_DIC,VACUNA_COVID_REC,
                Ultima_vez_PA_CAT,REMEDIOS_REC,
                Peso,FC_PROM) %>%
  tbl_uvregression(
    method = glm,
    y = CATEGORIA_HAS_REC,
    method.args = list(family = binomial),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 2),
    hide_n = T) %>%
  bold_labels()%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label) )

tbl_merge(
  tbls = list(t1.1, t1.2),
  tab_spanner = c("**Undiagnosed Hypertension\n(Vs. No-Hypertension)**", "**Uncontrolled Hypertension\n(Vs. Controlled)**"))%>%
  as_flex_table()%>%
  flextable::save_as_docx(path="Supplementary_Table_5.docx")
table(base$CATEGORIA_HAS)
