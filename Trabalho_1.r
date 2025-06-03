# Bibliocas
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(car)
library(magrittr)

##### 1) Leitura dos dados #####

#Função Padrão para a leitura

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
        df[,i] <- suppressWarnings(as.numeric(df[,i]))
    }

    return(df)
}

# 1.1) Índice de envelecimento
path <-  "https://gist.githubusercontent.com/jonhsp/1e3e549494bfa1efb8fa25467f5bc0c7/raw/c87f3d9cd017d68a1456e352216faa8a6f90492c/%25C3%258Dndice%2520de%2520Envelhecimento"
df_IndiceEnvelhecimento <- leitura(path, c("municipio", "regiao", "indiceEnvelhecimento"))

# 1.2) População Estimada
path <- "https://gist.githubusercontent.com/jonhsp/b2884ca9770d2cbbdecad217f19897f0/raw/c6f2b1665065190b03abd8ce86d41e3f0e92e414/Popula%25C3%25A7%25C3%25A3o%2520Estimada.txt"
df_populacao <- leitura(path, c("municipio", "regiao", "populacao"))

# 1.3) Grau de Urbanização
path <- "https://gist.githubusercontent.com/jonhsp/7c147642de87fe433ce086ceabb470a3/raw/c86f93fabe5a583bd701b7db3d298eab7fc1492c/Grau%2520de%2520Urbaniza%25C3%25A7%25C3%25A3o.txt"
df_urbanizacao <- leitura(path, c("municipio", "regiao", "graudeU"))

# 1.4) Despesas do Município
path <- "https://gist.githubusercontent.com/jonhsp/d9b55c8645144be9298525417a29b9c9/raw/60dfde981f267bf285f3b218a70fede129d1a186/Despesas%2520Municipio"
df_despesas <- leitura(path, c("municipio", "regiao", "despesas"))

# 1.5) Cultura
path <- "https://gist.githubusercontent.com/jonhsp/0392551ee43bc64b3c9a1430c653bd9e/raw/747eda439e121900c44e50fe1ed9c48b5196b69d/Equipamentos%2520Culturais"
df_cultura <- leitura(path, c("municipio", "regiao", "equipamentosC"))

# 1.6) Escolas
path <- "https://gist.githubusercontent.com/jonhsp/e0142669e1c9f92e8b1b07cdf48614ac/raw/926740925238633aebcbb4a4831b16fe8254b2da/Estabelecimentos"
df_escolas <- leitura(path, c("municipio", "regiao", "escolas"))

# 1.7) Matrículas
path <- "https://gist.githubusercontent.com/jonhsp/af72cb3e56b9c2aad161f528e157c742/raw/e97cf5e93dbf1db3008029a9a3ca4548b20bfd77/Matr%25C3%25ADculas"
df_matriculas <- leitura(path, c("municipio", "regiao", "matriculas"))

# 1.8) Profissionais de Saude
path <- "https://gist.githubusercontent.com/jonhsp/fdaf04aa3f0962d50c54e3ccd9bee5bb/raw/96a9644ad4bc843e52831987b39c08645bf1cf4d/Profissionais%2520de%2520Saude"
df_profissionaisSaude <- leitura(path, c("municipio", "regiao", "profissionaisS"))

# 1.9) Internet Fixa
path <-"https://gist.githubusercontent.com/jonhsp/fdbe5231146530a4a40b7cee277aadb2/raw/724edcc32e72390d2c696afaa9627712251c3f59/Banda%2520Larga%2520Fixa"
df_internetFixa <- leitura(path, c("municipio", "regiao", "internetFixa"))

# 1.10) Consumo Energia
path <- "https://gist.githubusercontent.com/jonhsp/9926d9815a15c1fcf7a03c224009ffca/raw/8b0a3be9879b85fc61089c25524c66064f25fab7/ConsumoEnergia"
df_energia <- leitura(path, c("municipio", "regiao", "energiaTotal", "energiaIndustria"))

# 1.11) Crescimento Geométrico
path <- "https://gist.githubusercontent.com/jonhsp/a42d460e1e92dc5792d253a41f92c56d/raw/b85b22e4707d52e6ebbe95ee5a30c61bccaa3fed/Crescimento%2520Geometrico"
df_crescimento <- leitura(path, c("municipio", "regiao", "crescimento"))

# 1.12) Densiade Demográfica
path <- "https://gist.githubusercontent.com/jonhsp/d04e1e64bbd0017e4416f792303eae62/raw/372722f392f9762f40e511e2a5fe71f8eb464d8c/Densidade%2520Demografica"
df_densidade <- leitura(path, c("municipio", "regiao", "densidade"))

# 1.13) razão de Sexo (homens / mulheres)*100
path <-  "https://gist.githubusercontent.com/jonhsp/ba3daf8403785506204c9abb6427f09c/raw/01500e85718103c889e718db27c4a40239ec86d6/Razao%2520de%2520Sexo"
df_sexo <- leitura(path, c("municipio", "regiao", "sexo"))

# 1.14) Distância à Capital (Km)
path <- "https://gist.githubusercontent.com/jonhsp/ca24fe1500b9e698124b81ce891ac00c/raw/007a0fbcfee458ae9f6fa56f55ab32facbf90455/Dist%25C3%25A2ncia%2520Capital"
df_distancia <- leitura(path, c("municipio", "regiao", "distancia"))
df_distancia$distancia[is.na(df_distancia$distancia)] <- 0 #Curitiba

# 1.15) Roubo e Furto
path <- "https://gist.githubusercontent.com/jonhsp/86b9f07db69b91e816eff7b315ba447a/raw/dc4f35551d09abe544abccb3b9da75129b321521/Crimes%2520Tabela"
df_crimes <- leitura(path, c("municipio", "regiao", "roubo","furto"))

#### 2) Junção dos dados ####
dfs <- list(df_IndiceEnvelhecimento,
            df_populacao,
            df_urbanizacao,
            df_despesas,
            df_cultura,
            df_escolas,
            df_matriculas,
            df_profissionaisSaude,
            df_internetFixa,
            df_energia,
            df_crescimento,
            df_densidade,
            df_sexo,
            df_distancia,
            df_crimes)

df <- Reduce(function(x, y) merge(x, y, by = c("municipio", "regiao")), dfs)

#### 3) trasformações #### 

df <- df %>%
    # Log da população
    mutate(logPopulacao = log(df_populacao$populacao)) %>%  
    relocate(logPopulacao, .after = 4) %>%
    # Ponderação das variáveis por mil habitantes
    mutate(across(c("despesas",
                    "equipamentosC",
                    "escolas",
                    "matriculas",
                    "profissionaisS",
                    "internetFixa",
                    "energiaTotal",
                    "energiaIndustria",
                    "roubo",
                    "furto"),
                ~ (.x / df$populacao) * 1E5)) %>%
    # Transformação de múnicipio e região em nome das linhas
    unite("municipio_regiao", c("municipio", "regiao"), sep = "/") %>% 
    column_to_rownames("municipio_regiao")


#### 4) Correlações ####

x11("GGPairs")
ggpairs(df)


x11("CorrPlot")
corrplot(M)

#### 5) Ajuste do Modelo de Regressão Múltipla ####

ajuste <- lm(indiceEnvelhecimento ~ . - populacao, df)
summary(ajuste)
anova(ajuste)
Anova(ajuste)

rm(list = ls())
