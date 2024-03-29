library("sf")
library("openxlsx")
library("rgdal")
library("tidyverse")
library("maptools")
library("raster")
library("xlsx")
library("dplyr")

formas <- read_sf('C:/Users/Tamara Dias/OneDrive/�rea de Trabalho/Consultoria - Codeplan/Shapefiles/Regi�es Administrativas IVS.shp')
dados <- openxlsx::read.xlsx("C:/Users/Tamara Dias/OneDrive/�rea de Trabalho/Consultoria - Codeplan/Censo Brasilia 2021 Final Atualizado (3).xlsx")

# Ajustando as RAs e juntando o as vari�veis pertinentes
dados <- dados %>% mutate(Regi�o.Administrativa = ifelse(Regi�o.Administrativa == "SCIA", "SCIA/Estrutural", Regi�o.Administrativa))
names(dados)[189] <- c("ra")
dados$ra <- as.character(dados$ra)
  
juntos <- full_join(dados, formas, by="ra")
juntos <- juntos[,c(187:189,211:274)]
names(juntos)[3] <- c("Regi�o Administrativa")

# Salvando os shapefiles
st_write(juntos, "shape_POP_RUA.shp", driver = "ESRI Shapefile")

#Criando faixas et�rias
dados <- dados %>% mutate(Idade = as.numeric(Idade))

dados <- dados %>% mutate(`Faixa Et�ria` = case_when(Idade <= 11 ~ "At� 11 anos",
                                                     Idade >= 12 & Idade <= 17 ~ "12 a 17 anos",
                                                     Idade >= 18 & Idade <= 30 ~ "18 a 30 anos",
                                                     Idade >= 31 & Idade <= 49 ~ "31 a 49 anos",
                                                     Idade >= 50 & Idade <= 59 ~ "50 a 59 anos",
                                                     Idade >= 60 & Idade <= 69 ~ "60 a 69 anos",
                                                     Idade >= 70 & Idade <= 79 ~ "70 a 79 anos",
                                                     Idade >= 80 & Idade <= 89 ~ "80 a 89 anos",
                                                     Idade >= 90 & Idade <= 96 ~ "90 a 99 anos",
                                                     Idade == 98 ~ "90 a 99 anos",
                                                     Idade == 97  ~ "N�o sabe",
                                                     Idade == 99 ~ "N�o respondeu",
                                                     T ~ "N�o informado"))
faixa <- dados %>% select(`Faixa Et�ria`)
write.xlsx2(as.data.frame(faixa), "faixas.xlsx", append=TRUE, row.names = F)

# S�rie m�xima ou cursando
anos <- dados %>% dplyr::select(C.S�rie.crian�a, C.S�rie.m�xima.crian�a) 
anos.c <- anos %>% dplyr::mutate(C.Anos = ifelse(C.S�rie.crian�a != "N�o se aplica" & C.S�rie.crian�a != "Sem informa��o",
                                          C.S�rie.crian�a, ifelse(C.S�rie.m�xima.crian�a != "N�o se aplica" & 
                                          C.S�rie.m�xima.crian�a != "Sem informa��o", C.S�rie.m�xima.crian�a, C.S�rie.m�xima.crian�a)))

anos.c <- anos %>% dplyr::mutate(C.Anos = ifelse(C.S�rie.m�xima.crian�a != "N�o se aplica" | C.S�rie.m�xima.crian�a != "Sem informa��o", C.S�rie.m�xima.crian�a, "VERIFICAR"))

write.xlsx2(as.data.frame(anos.c), "C.anos.escola.xlsx", append=TRUE, row.names = F)









