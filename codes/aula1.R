# Instalação dos pacotes necessários para este código (requer conexão 
# com a internet)
# install.packages("ggplot2")
# install.packages("lubridate")

# Bibliotecas utilizadas
library(ggplot2) # para os gráficos
library(lubridate) # para conversão de texto em datas

# --- Gráficos séries temporais ---

# 1. Séries com intervalos fixos: vazões médias anuais em Foz do Areia
vazoesMediasFozDoAreia <- read.csv('vazoesMediasFozDoAreia.csv')

# Somente dados de vazão média anual
Qmedia <- data.frame(
  anos = vazoesMediasFozDoAreia[,1],
  vazoes = vazoesMediasFozDoAreia[,ncol(vazoesMediasFozDoAreia)])

# Comando para plotar a série
# tiff('serieIntervaloFixo.tif', height=720, width = 1780, res=300)
ggplot(Qmedia, aes(x=anos, y=vazoes)) + 
  expand_limits(y=c(0,1600)) +
  scale_y_continuous(breaks = seq(0, 1600, by = 400)) +
  scale_x_continuous(breaks = seq(1920, 2020, by = 10)) +
  geom_line(color="blue3") +
  labs(x="Ano",y="Vazão (m³/s)") + 
  theme_light() 
# dev.off()

# 2. Séries com intervalos não equidistantes
qualIGU <- read.csv('chem_data.csv')

# Extrair dados de OD (14ª coluna da matriz original)
serieOD <- data.frame(
  dias = qualIGU[qualIGU$PONTO == 'IG1',2],
  OD = qualIGU[qualIGU$PONTO == 'IG1',14])
# Remoção de linhas com valores 'NA'
serieOD <- na.omit(serieOD)
# Conversão da coluna das datas (lida como texto) para o formato apropriado
serieOD$dias <- dmy(serieOD$dias)

# Comando para plotar a série
# tiff('serieIntervaloNEquidistante.tif', height=720, width = 1780, res=300)
ggplot(serieOD, aes(x=serieOD[,1], y=serieOD[,2])) + 
  scale_x_date(breaks = "12 months", date_labels = "%d %b %y") +
  geom_point(color="blue3") +
  labs(x="Ano",y="OD (mg/L)") +
  theme_light() +
  theme(axis.text.x=element_text(angle=30, hjust=1)) 
# dev.off()

# 3. Séries completas: vazões médias diárias em Santa Clara
vazoesDiariasStaClara <- read.csv('vazoesDiariasStaClara.csv')

# Montagem do data frame com os dias e as vazões
Qdiaria <- data.frame(
  dias = vazoesDiariasStaClara[,1],
  vazoes = vazoesDiariasStaClara[,2])
# Conversão da coluna das datas (lida como texto) para o formato apropriado
Qdiaria$dias <- dmy(Qdiaria$dias)

# Comando para plotar a série
# tiff('serieCompleta.tif', height=720, width = 1780, res=300)
ggplot(Qdiaria, aes(x=Qdiaria[,1], y=Qdiaria[,2])) + 
  scale_x_date(breaks = "60 months", date_labels = "%d %b %y") +
  expand_limits(y=c(0,4000)) +
  geom_line(color="blue3") +
  labs(x="Data",y="Vazão (m³/s)") + 
  theme_light() +
  theme(axis.text.x=element_text(angle=30, hjust=1)) 
# dev.off()

# 4. Séries reduzidas: vazões máximas diárias anuais em Santa Clara
vazoesMaxStaClara <- read.csv('vazoesMaxAnuaisStaClara.csv')

# Organização dos dados em um data frame
Qmax <- data.frame(
  anos = vazoesMaxStaClara[,1],
  vazoes = vazoesMaxStaClara[,ncol(vazoesMaxStaClara)])

# Comando para plotar a série
# tiff('serieReduzida.tif', height=720, width = 1780, res=300)
ggplot(Qmax, aes(x=Qmax[,1], y=Qmax[,2])) + 
  expand_limits(y=c(0,4000)) +
  geom_point(color="blue3") +
  labs(x="Ano",y="Vazão (m³/s)") + 
  theme_light() 
# dev.off()

# 5. Séries censuradas: déficits hídricos em União da Vitória
deficitsUV <- read.csv('deficitsUV.csv')

# Somente dados de vazão média anual
defUV <- data.frame(
  anos = deficitsUV[,1],
  deficits = deficitsUV[,ncol(deficitsUV)])

# Comando para plotar a série
# tiff('serieCensurada.tif', height=720, width = 1780, res=300)
ggplot(defUV, aes(x=defUV[,1], y=defUV[,2])) + 
  # expand_limits(y=c(0,4000)) +
  geom_line(color="blue3") +
  labs(x="Ano",y="Déficit (m³/s)") + 
  theme_light() 
# dev.off()

# 6. Exibir os valores mínimos e máximos da série de OD

# Comando para exibir a data (coluna 1) do maior registro de OD (coluna 2)
serieOD[serieOD$OD==max(serieOD$OD),1] # máximo
serieOD[serieOD$OD==min(serieOD$OD),1] # mínimo

# Opcionalmente, é possível extrair toda a linha
serieOD[serieOD$OD==max(serieOD$OD),] # máximo
serieOD[serieOD$OD==min(serieOD$OD),] # mínimo
