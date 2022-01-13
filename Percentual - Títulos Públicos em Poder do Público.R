#Rotina para coletar e calcular o percentual de títulos públicos em poder do público 
#Feito por: Marcelo Vilas Boas de Castro
#última atualização: 13/11/2019

#Definindo diretórios a serem utilizados
getwd()
setwd("C:/Users/User/Documents")

#Carregando pacotes que serão utilizados
library(tidyverse)
library(data.table)
library(RQuantLib)
library(zoo)
library(rio)
library(lubridate)
library(ggplot2)
library(scales)
library(ggrepel)

#Definindo argumentos da coleta
abertura = "D" #D (dia) ou M (mês) ou A (ano)
indice = "M" #M para valores em R$ milhões, R para valores em Reais, S para valores US$ milhões ou U para valores em US$ 
formato = "A" #A (CSV) ou T (tela) ou E (Excel)
data_inicial = "2020-01-01" %>% as.Date()
data_final = as.Date(Sys.time())

#Criando lista com últimos dias úteis do mês
lista_dias <- c(data_inicial, data_final) #Lista com data do início do ano e data de hoje
dias_uteis <- seq(lista_dias[1], lista_dias[2], by="1 day") #Calculando quais são os dias entre essas duas datas 
dias_uteis <- data.frame(dates=dias_uteis, bizday=isBusinessDay("Brazil", dias_uteis)) #Marcando quais desses dias são úteis
dias_uteis <- filter(dias_uteis, bizday == "TRUE") #Filtrando só os dias úteis
dias_uteis <- data.table(dias_uteis) #Transformando em data.table
dias_uteis <- dias_uteis %>% mutate(lista_dias = tapply(dias_uteis$dates, as.yearmon(dias_uteis$dates))) #Criando coluna com a lista_dias

#Como a referência de um mês é o último dia útil do mês anterior, vamos pegar todo primeiro dia útel dos meses (para identificar) e o último dia útil do mês anterior(para ser a referência na busca) de cada mês
ultimo_dia_util <- dias_uteis[,tail(.SD,1),by = lista_dias] #Selecionando o último dia útil de cada mês
ultimo_dia_util <- as.array(ultimo_dia_util$dates) #Transformando em vetor
ultimo_dia_util[length(ultimo_dia_util)] <- format(Sys.time()) #Adicionando dia de hoje
ultimo_dia_util <- format(ultimo_dia_util, "%d/%m/%Y") #Formatando como datas "dd/mm/YYYY"
primeiro_dia_util <- dias_uteis[,head(.SD,1),by = lista_dias] #Selecionando o primeiro dia útil de cada mês
primeiro_dia_util <- as.array(primeiro_dia_util$dates) #Transformando em vetor
dia_do_ultimo_dado <- as.Date(Sys.Date()) #Pegamos o dia do último dado, sabendo que a referência sempre será o dia útil imediatamente anterior
while (isBusinessDay("Brazil", dia_do_ultimo_dado) == F)
  dia_do_ultimo_dado <- dia_do_ultimo_dado + 1
primeiro_dia_util[length(primeiro_dia_util) + 1 ] <- dia_do_ultimo_dado
primeiro_dia_util <- primeiro_dia_util[-1] #Tirando primeiro dado, já que a referência do 1º mês da da série é calculada tendo como referência o último dia útil do mês anterior
primeiro_dia_util <- format(primeiro_dia_util, "%d/%m/%Y") #Formatando como datas "dd/mm/YYYY"

#Criando lista com nome de arquivos
lista_nome_arquivos <- NULL #Vazia, a ser preenchida

#Coleta de dados
for (i in 1:length(ultimo_dia_util)){
  dados <- read.csv(url(paste("http://www4.bcb.gov.br/pom/demab/cronograma/vencdata_csv.asp?data=", ultimo_dia_util[i], "&abertura=", abertura, "&indice=", indice, "&formato=", formato, sep="")),sep=";", skip = 3)
  dados <- data.table(dados) #Transformando em data table para facilitar as manipulações
  dados <- select(dados, Data = VENCIMENTO, Total = TOTAL, Participação = PART..) #Selecionando as colunas que vamos usar
  dados$Data <- as.Date(dados$Data, "%d/%m/%Y") #Transformando a coluna de data em data
  dados <- transform(dados, Total = as.numeric(gsub(",",".",Total))) #Transformando o resto das colunas em números
  dados <- transform(dados, `Participação` = as.numeric(gsub(",",".",`Participação`))) #Transformando o resto das colunas em números
  nome_arquivo <- paste("Ref_", gsub("/", "_", ultimo_dia_util[i]), sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
  assign(nome_arquivo, dados) #Nomeando arquivos
  lista_nome_arquivos[i] <- nome_arquivo #Guardando nome dos arquivos
  if(i==1)
    export(dados, "Percentual - Títulos Públicos em Poder do Público(fonte).xlsx", sheetName = nome_arquivo)
  else
    export(dados, "Percentual - Títulos Públicos em Poder do Público(fonte).xlsx", which = nome_arquivo)
  print(paste(i, length(ultimo_dia_util), sep = '/')) #Printa o progresso da repetição
}
rm(dados)

#Calculando acumulados em 6 e 12 meses
filtros_6_meses <- ymd(as.Date(ultimo_dia_util, format = "%d/%m/%Y") %m+% months(6)) #Calculando 6 meses a frente
filtros_12_meses <- ymd(as.Date(ultimo_dia_util, format = "%d/%m/%Y") %m+% months(12)) #Calculando 12 meses a frente
acumulado_6_meses <- data.table(Data = as.Date(primeiro_dia_util, format = "%d/%m/%Y"), Acumulado = 0) #Criando data.table vazio
acumulado_12_meses <- data.table(Data = as.Date(primeiro_dia_util, format = "%d/%m/%Y"), Acumulado = 0) #Criando data.table vazio

for (i in 1:length(lista_nome_arquivos)){
  acumulado <- get(lista_nome_arquivos[i]) #Chamando arquivos
  acumulado <- filter(acumulado, Data < filtros_6_meses[i]) #Filtrando para datas < que 6 meses
  acumulado <- sum(acumulado$Participação) #Calculando o acumulado pós-filtro
  acumulado_6_meses[i,2] <- acumulado #Adicionando ao data.table de acumulados
  print(paste(i, length(lista_nome_arquivos), sep = '/')) #Printa o progresso da repetição
}

export(acumulado_6_meses, "Percentual - Títulos Públicos em Poder do Público(fonte).xlsx", which = "Acum_6_meses")

for (i in 1:length(lista_nome_arquivos)){
  acumulado <- get(lista_nome_arquivos[i]) #Chamando arquivos
  acumulado <- filter(acumulado, Data < filtros_12_meses[i]) #Filtrando para datas < que 12 meses
  acumulado <- sum(acumulado$Participação) #Calculando o acumulado pós-filtro
  acumulado_12_meses[i,2] <- acumulado #Adicionando ao data.table de acumulados
  print(paste(i, length(lista_nome_arquivos), sep = '/')) #Printa o progresso da repetição
}

export(acumulado_12_meses, "Percentual - Títulos Públicos em Poder do Público(fonte).xlsx", which = "Acum_12_meses")

#Gráficos
  #Acumulado em 6 meses
graf_acum_6_meses <- ggplot(acumulado_6_meses, aes(x = Data, y = Acumulado)) + geom_bar(stat = "identity", fill = "darkblue") + 
  geom_label_repel(aes(label = sprintf("%0.2f", round(Acumulado,2))), force = 0.01) + 
  theme_minimal() + scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%d/%b")) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("") + ylab("") + 
  labs(title = "Vencimento de Títulos Federais em Poder do Público", subtitle = "Percentual da Dívida Total vencendo em até 6 meses",
       caption = "Fonte: BCB")

ggsave("acumulado_6_meses.png", graf_acum_6_meses)
       
graf_acum_12_meses <- ggplot(acumulado_12_meses, aes(x = Data, y = Acumulado)) + geom_bar(stat = "identity", fill = "darkblue") + 
  geom_label_repel(aes(label = sprintf("%0.2f", round(Acumulado,2))), force = 0.01) + 
  theme_minimal() + scale_x_date(breaks = date_breaks("1 month"), labels = date_format("%d/%b")) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + xlab("") + ylab("") + 
  labs(title = "Vencimento de Títulos Federais em Poder do Público", subtitle = "Percentual da Dívida Total vencendo em até 12 meses",
       caption = "Fonte: BCB")

ggsave("acumulado_12_meses.png", graf_acum_12_meses)