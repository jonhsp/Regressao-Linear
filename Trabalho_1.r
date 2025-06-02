# Bibliocas
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(car)



# 1) Leitura dos dados

# Função Padrão para a leitura

leitura <- function(path, names){ 

    # Leitura dos dados
    df <- read.csv(path,
                    header = T,
                    sep = ",",
                    colClasses = "character")

    # Renomear as colunas
    dimnames(df)[[2]] <- names

    # Verificar dados faltantes
    if (dim(df[rowSums(is.na(df))>0,])[1] > 0) {
        print(df[rowSums(is.na(df))>0,])
        print("Existem dados NA no arquivo")
    }

    # Tratamento dos dados
    for (i in 3:ncol(df)){
        df[,i] <- gsub(",",".",gsub("\\.", "", df[,i]))
        df[,i] <- as.numeric(df[,i])
    }

    return(df)
}

# 1.1) Índice de envelecimento

path <-  "https://gist.githubusercontent.com/jonhsp/1e3e549494bfa1efb8fa25467f5bc0c7/raw/c87f3d9cd017d68a1456e352216faa8a6f90492c/%25C3%258Dndice%2520de%2520Envelhecimento"
df_IndiceEnvelhecimento <- leitura(path, c("municipio", "regiao", "indiceEnvelhecimento"))

# 1.2) População Estimada
path <- "https://gist.githubusercontent.com/jonhsp/b2884ca9770d2cbbdecad217f19897f0/raw/c6f2b1665065190b03abd8ce86d41e3f0e92e414/Popula%25C3%25A7%25C3%25A3o%2520Estimada.txt"
df_populacao <- leitura(path, c("municipio", "regiao", "populacaoEstimada"))

# 1.3) Grau de Urbanização
path <- "https://gist.githubusercontent.com/jonhsp/7c147642de87fe433ce086ceabb470a3/raw/c86f93fabe5a583bd701b7db3d298eab7fc1492c/Grau%2520de%2520Urbaniza%25C3%25A7%25C3%25A3o.txt"
df_urbanizacao <- leitura(path, c("municipio", "regiao", "graudeDeUrbanizacao"))

# 1.4) Despesas do Município
path <- "https://gist.githubusercontent.com/jonhsp/d9b55c8645144be9298525417a29b9c9/raw/60dfde981f267bf285f3b218a70fede129d1a186/Despesas%2520Municipio"
df_despesas <- leitura(path, c("municipio", "regiao", "despesas"))

# 1.5) Cultura
path <- "https://gist.githubusercontent.com/jonhsp/0392551ee43bc64b3c9a1430c653bd9e/raw/747eda439e121900c44e50fe1ed9c48b5196b69d/Equipamentos%2520Culturais"
df_cultura <- leitura(path, c("municipio", "regiao", "equipamentosCulturais"))

# 1.6) Escolas
path <- "https://gist.githubusercontent.com/jonhsp/e0142669e1c9f92e8b1b07cdf48614ac/raw/926740925238633aebcbb4a4831b16fe8254b2da/Estabelecimentos"
df_escolas <- leitura(path, c("municipio", "regiao", "escolas"))

# 1.7) Matrículas
path <- "https://gist.githubusercontent.com/jonhsp/af72cb3e56b9c2aad161f528e157c742/raw/e97cf5e93dbf1db3008029a9a3ca4548b20bfd77/Matr%25C3%25ADculas"
df_matriculas <- leitura(path, c("municipio", "regiao", "matriculas"))

# Junção dos dados
dfs <- list(df_IndiceEnvelhecimento, df_populacao, df_urbanizacao, df_despesas, df_cultura, df_escolas, df_matriculas)
df <- Reduce(function(x, y) merge(x, y, by = c("municipio", "regiao")), dfs)
df$logPopulacaoEstimada <- log(df$populacaoEstimada, base = exp(1))
df$despesasPorMHabitantes <- (df$despesas / df$populacaoEstimada)*1000
df$equipamentosCulturaisPorMHabitantes <- (df$equipamentosCulturais / df$populacaoEstimada)*1000
df$escolasPorMHabitantes <- (df$escolas / df$populacaoEstimada)*1000
df$matriculasPorMHabitantes <- (df$matriculas / df$populacaoEstimada)*1000

# Correlação
x11("Correlação e Distribuições")
ggpairs(df[, -c(1,2)])

# Ajuste do Modelo 

ajuste <- lm(indiceEnvelhecimento ~ populacaoEstimada + graudeDeUrbanizacao , data = df)
summary(ajuste)

residualPlots(ajuste)
influenceIndexPlot(ajuste)
influencePlot(ajuste)


