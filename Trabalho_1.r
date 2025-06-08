#### Bibliocas ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
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
    colnames(df) <- names

    # Tratamento dos dados
    for (i in 3:ncol(df)){
        df[,i] <- gsub(",",".",gsub("\\.", "", df[,i]))
        df[,i] <- suppressWarnings(as.numeric(df[,i]))
    }

    # Verificar dados faltantes
    if (any(is.na(df))) {
        print(df[rowSums(is.na(df))>0,])
        print("Existem dados NA no arquivo")
    }

    else{
        print("Nao existem dados NA no arquivo")
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

    # Substituição do NA por Zero
    df_energia[df_energia$municipio == "Mato Rico","energiaIndustria"] <- 0

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

    # Substituição do NA por Zero
    df_distancia[df_distancia$municipio == "Curitiba","distancia"] <- 0
    
# 1.15) Roubo e Furto
path <- "https://gist.githubusercontent.com/jonhsp/86b9f07db69b91e816eff7b315ba447a/raw/dc4f35551d09abe544abccb3b9da75129b321521/Crimes%2520Tabela"
df_crimes <- leitura(path, c("municipio", "regiao", "roubo","furto"))

    # Substituição do NA por Zero
    # 33 municipios foram encontrados com NA furtos, todos com menos de 10 mil habitantes. 
    df_crimes[is.na(df_crimes$furto),"furto"] <- 0
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
    # Transformação de múnicipio e região em nome das linhas
    unite("municipio_regiao", c("municipio", "regiao"), sep = "/") %>% 
    column_to_rownames("municipio_regiao")

# Função para ponderação pela quantidade de habitantes
ponderacao <- function(data){
    # métrica por 100.000 habitantes
    df_ponderado <- (data/df$populacao)*1e5
}

#### 4) Gráficos de correlação ####

x11("GGPairs Parte 1 ")
ggpairs(df[,1:9])

x11("GGPairs Parte 2 ")
ggpairs(df[,c(1,10:18)])

x11("CorrPlot")
correlacoes <- cor(df, use = "pairwise.complete.obs")
corrplot(correlacoes, method = "color")

#### 5) Análise do indicador, correlação com a resposta e ponderações ####

#### 5.1) População ####

# Correlação do total
c_populacao_1 <- round(cor(df$indiceEnvelhecimento, df$populacao, use = "pairwise.complete.obs"),2)

# Correlação do log(população)
c_populacao_2 <- round(cor(df$indiceEnvelhecimento, df$logPopulacao, use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("População")
df %>% 
    ggplot(aes(x = populacao, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("População") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$populacao),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_populacao_1, size = 7)) -> g_populacao_1

# Dispersão da log populacao
df %>% 
    ggplot(aes(x = logPopulacao, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Log População") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$logPopulacao),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_populacao_2, size = 7)) -> g_populacao_2

grid.arrange(g_populacao_1, g_populacao_2, ncol = 2)
#### 5.2) Grau de Urbanização ####

c_urbanizacao <- cor(df$indiceEnvelhecimento, df$graudeU, use = "pairwise.complete.obs")

# Dispersão
x11("Urbanização")
df %>% 
    ggplot(aes(x = graudeU, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Cultura") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$graudeU),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", round(c_urbanizacao, 2)), size = 7)
#### 5.3) Despesas ####

# Correlação do total
c_despesas_1 <- round(cor(df$indiceEnvelhecimento, df$despesas, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_despesas_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$despesas), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_despesas_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$despesas)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("Despesas")
df %>% 
    ggplot(aes(x = despesas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Despesas") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$despesas,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_despesas_1),
                    size = 4) -> g_despesas_1

# Dispersão das despesas ponderadas
df %>% 
    mutate(despesas_ponderadas = ponderacao(despesas)) %>%
    ggplot(aes(x = despesas_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Despesas por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação por 100.000 habitantes") +
    annotate("text", x = 0.9 * max(ponderacao(df$despesas),na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_despesas_2),
                    size = 4) -> g_despesas_2

# Dispersão do log das despesas ponderadas
df %>% 
    mutate(despesas_log = log(ponderacao(despesas))) %>%
    ggplot(aes(x = despesas_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Despesas por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$despesas)),na.rm = T),
                    y = 0.05 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_despesas_3),
                    size = 4) -> g_despesas_3

grid.arrange(g_despesas_1, g_despesas_2,g_despesas_3, ncol = 3)

df %>% dplyr::select(indiceEnvelhecimento, despesas) %>%
    mutate(despesasP = ponderacao(despesas)) %>%
    arrange(desc(despesasP)) %>%
    View(title = "despeas")
    
# A ponderação por 100.000 habitantes apresentou melhor relação linear, contudo

 #### 5.4) Cultura ####

# Correlação do total
c_cultura_1 <- cor(df$indiceEnvelhecimento, df$equipamentosC, use = "pairwise.complete.obs")

# Correlação da ponderação
c_cultura_2 <- cor(df$indiceEnvelhecimento, ponderacao(df$equipamentosC), use = "pairwise.complete.obs")

# Dispersão do total
x11("Cultura | Dispersão do total")
df %>% 
    ggplot(aes(x = equipamentosC, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Cultura") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$equipamentosC),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", round(c_cultura_1, 2)), size = 7) -> g_cultura_1

# Dispersão da ponderação
df %>% 
    mutate(equipamentosC = ponderacao(equipamentosC)) %>% 
    ggplot(aes(x = equipamentosC, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Cultura") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação") +
    annotate("text", x = 0.9 * max(ponderacao(df$equipamentosC)),
                    y = 0.95 * max(df$indiceEnvelhecimento), 
                    label = paste0("Corr: ", round(c_cultura_2, 2)), size = 7) -> g_cultura_2

grid.arrange(g_cultura_1, g_cultura_2, ncol = 2)

# Análise do indicador Ponderado
df %>%
    select(populacao, equipamentosC) %>%
    mutate(equipamentosC_Ponderado = ponderacao(equipamentosC)) %>%
    arrange(desc(equipamentosC_Ponderado)) %>%
    View()

# O indicador é disttorcido em cidades com populações baixas, as cidades com 
# maiores quantidades de equipamentos culturais por 100 mil habitantes apresentam
# poucos equipamentos culturais.

df %<>% select(-equipamentosC)


# Escolas vs Matriculas


x11()
df %>% 
    ggplot(aes(x = matriculas, y = escolas)) +
    geom_point() +
    geom_smooth(method = "lm")


x11()

df %>%
    select(matriculas, escolas, populacao, indiceEnvelhecimento) %>%
    mutate(matriculasT =   (matriculas*populacao)/1e5) %>%
    mutate(escolasT =   (escolas*populacao)/1e5) %>%
    arrange(desc(matriculas)) %>%
    ggplot(aes(x = matriculasT, y = escolasT)) +
    geom_point() +
    geom_smooth(method = "lm")







 #### 5.4) Crescimento Geométrico ####
x11("Despesas")
df %>% 
    ggplot(aes(x = despesas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Despesas") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$despesas,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_despesas_1),
                    size = 4) -> g_despesas_1

# Dispersão das despesas ponderadas
df %>% 
    mutate(despesas_ponderadas = ponderacao(despesas)) %>%
    ggplot(aes(x = despesas_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Despesas por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação por 100.000 habitantes") +
    annotate("text", x = 0.9 * max(ponderacao(df$despesas),na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_despesas_2),
                    size = 4) -> g_despesas_2

# Dispersão do log das despesas ponderadas
df %>% 
    mutate(despesas_log = log(ponderacao(despesas))) %>%
    ggplot(aes(x = despesas_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Despesas por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$despesas)),na.rm = T),
                    y = 0.05 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_despesas_3),
                    size = 4) -> g_despesas_3

grid.arrange(g_despesas_1, g_despesas_2,g_despesas_3, ncol = 3)

df %>% dplyr::select(indiceEnvelhecimento, despesas) %>%
    mutate(despesasP = ponderacao(despesas)) %>%
    arrange(desc(despesasP)) %>%
    View(title = "despeas")
    


ajuste <- lm(indiceEnvelhecimento ~ . - populacao, df)
summary(ajuste)
anova(ajuste)
Anova(ajuste)

rm(list = ls())
