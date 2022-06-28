library("sf")
library("openxlsx")
library("rgdal")
library("tidyverse")
library("maptools")
library("raster")
library("xlsx")
library("dplyr")

formas <- read_sf('C:/Users/Tamara Dias/OneDrive/Área de Trabalho/Consultoria - Codeplan/Shapefiles/Regiões Administrativas IVS.shp')
dados <- openxlsx::read.xlsx("C:/Users/Tamara Dias/OneDrive/Área de Trabalho/Consultoria - Codeplan/Censo Brasilia 2021 Final Atualizado (3).xlsx")

# Ajustando as RAs e juntando o as variáveis pertinentes
dados <- dados %>% mutate(Região.Administrativa = ifelse(Região.Administrativa == "SCIA", "SCIA/Estrutural", Região.Administrativa))
names(dados)[189] <- c("ra")
dados$ra <- as.character(dados$ra)
  
juntos <- full_join(dados, formas, by="ra")
juntos <- juntos[,c(187:189,211:274)]
names(juntos)[3] <- c("Região Administrativa")

# Salvando os shapefiles
st_write(juntos, "shape_POP_RUA.shp", driver = "ESRI Shapefile")

#Criando faixas etárias
dados <- dados %>% mutate(Idade = as.numeric(Idade))

dados <- dados %>% mutate(`Faixa Etária` = case_when(Idade <= 11 ~ "Até 11 anos",
                                                     Idade >= 12 & Idade <= 17 ~ "12 a 17 anos",
                                                     Idade >= 18 & Idade <= 30 ~ "18 a 30 anos",
                                                     Idade >= 31 & Idade <= 49 ~ "31 a 49 anos",
                                                     Idade >= 50 & Idade <= 59 ~ "50 a 59 anos",
                                                     Idade >= 60 & Idade <= 69 ~ "60 a 69 anos",
                                                     Idade >= 70 & Idade <= 79 ~ "70 a 79 anos",
                                                     Idade >= 80 & Idade <= 89 ~ "80 a 89 anos",
                                                     Idade >= 90 & Idade <= 96 ~ "90 a 99 anos",
                                                     Idade == 98 ~ "90 a 99 anos",
                                                     Idade == 97  ~ "Não sabe",
                                                     Idade == 99 ~ "Não respondeu",
                                                     T ~ "Não informado"))
faixa <- dados %>% select(`Faixa Etária`)
write.xlsx2(as.data.frame(faixa), "faixas.xlsx", append=TRUE, row.names = F)

# Série máxima ou cursando
anos <- dados %>% dplyr::select(C.Série.criança, C.Série.máxima.criança) 
anos.c <- anos %>% dplyr::mutate(C.Anos = ifelse(C.Série.criança != "Não se aplica" & C.Série.criança != "Sem informação",
                                          C.Série.criança, ifelse(C.Série.máxima.criança != "Não se aplica" & 
                                          C.Série.máxima.criança != "Sem informação", C.Série.máxima.criança, C.Série.máxima.criança)))

anos.c <- anos %>% dplyr::mutate(C.Anos = ifelse(C.Série.máxima.criança != "Não se aplica" | C.Série.máxima.criança != "Sem informação", C.Série.máxima.criança, "VERIFICAR"))

write.xlsx2(as.data.frame(anos.c), "C.anos.escola.xlsx", append=TRUE, row.names = F)









