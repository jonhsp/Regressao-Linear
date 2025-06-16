#### Bibliotecas ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(corrplot)
library(GGally)
library(car)
library(magrittr)
library(knitr)
library(kableExtra)
library(sf)
library(geobr)
library(viridis)

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

# Baixa todos os municípios do Brasil (pode demorar um pouco)
mun_br <- read_municipality(year = 2022)

# Filtra apenas os municípios do Paraná
mun_pr <- mun_br %>% filter(abbrev_state == "PR")

# Adiciona informações da base do IPARDES para os dados do geobr para montar o mapa
mapa_dados <- mun_pr %<>%
    left_join(df_IndiceEnvelhecimento, by = c("name_muni" = "municipio"))

# Contorno do estado do PR
estado_pr <- read_state(code_state = "PR", year = 2020)

# Mapa coroplético do paraná sobre o indice de envelhecimento dos municipios
ggplot(mapa_dados) +
    geom_sf(aes(fill = indiceEnvelhecimento), color = NA) +
    scale_fill_viridis(option = "magma", direction = -1, name = "Índice de\nEnvelhecimento") +
    labs(title = "Índice de Envelhecimento dos Municípios do Paraná",
         caption = "Fonte: IPARDES e geobr") +
    theme_minimal() +
    theme(legend.position = "right")+
    geom_sf(data = estado_pr, fill = NA, color = "black", size = 0.5)

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

# 3.1 ) Categorização das cidades dde acordo com a região

# Criar vetores
rgi <- c(
  "Londrina", "Apucarana", "Cornélio Procópio - Bandeirantes", "Santo Antônio da Platina","Ibaiti",
  "Maringá", "Paranavaí", "Cianorte", "Paranacity - Colorado", "Loanda", "Umuarama", 
  "Cascavel",  "Toledo", "Marechal Cândido Rondon", "Foz do Iguaçu",
  "Francisco Beltrão", "Dois Vizinhos", "Pato Branco",
  "Guarapuava", "Irati", "Pitanga", "Laranjeiras do Sul - Quedas do Iguaçu", "Ivaiporã", 
  "Campo Mourão", "União da Vitória",  "Ponta Grossa", "Telêmaco Borba",  "Curitiba",  "Paranaguá"
)

regiaoGeografica <- c(
  rep("Norte", 5),
  rep("Noroeste", 6),
  rep("Oeste", 4),
  rep("Vales do Iguaçu", 3),
  rep("Centro e Centro-Sul", 7),
  rep("Campos Gerais", 2),
  "Grande Curitiba",
  "Litoral"
)

categorizacao <- data.frame(rgi, regiaoGeografica)


df %<>%
    # Log da população
    mutate(logPopulacao = log(df_populacao$populacao)) %>%  
    relocate(logPopulacao, .after = 4) %>%
    #Inclusão da Região Geográfica
    mutate(regiaoTemp = gsub("RGI de ", "", df$regiao)) %>%
    left_join(categorizacao, by = c("regiaoTemp" = "rgi")) %>%
    relocate(regiaoGeografica, .after = 3) %>% 
    select(-regiaoTemp) %>%
    # Transformação de múnicipio e região em nome das linhas
    unite("municipio_regiao", c("municipio", "regiao"), sep = "/") %>% 
    column_to_rownames("municipio_regiao")

# variáveis dummy, para categorização
regioesunicas <- unique(df$regiaoGeografica)[-2] # Exclui a Grande Curitiba, que será a referência
for(i in regioesunicas){
    df[[i]] <- ifelse(df$regiaoGeografica == i, 1, 0)
}

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
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("População") +
    ylab("Indice de Envelhecimento") +
    ggtitle("População Total") +
    annotate("text", x = 0.9 * max(df$populacao),
                    y = 145,
                    label = paste0("Corr: ", c_populacao_1, size = 7), color = "white") + 
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_populacao_1

# Dispersão da log populacao
df %>% 
    ggplot(aes(x = logPopulacao, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Log População") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Log da População") +
    annotate("text", x = 0.9 * max(df$logPopulacao),
                    y = 145,
                    label = paste0("Corr: ", c_populacao_2, size = 7), color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_populacao_2

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
    
# A ponderação por 100.000 habitantes apresentou melhor relação linear.

# Trasformação
df %<>%
    mutate(despesas = ponderacao(despesas))

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

#### 5.5) Escolas ####

# Correlação do total
c_escolas_1 <- round(cor(df$indiceEnvelhecimento, df$escolas, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_escolas_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$escolas), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_escolas_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$escolas)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("Escolas | Dispersão do Total")
df %>% 
    ggplot(aes(x = escolas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Escolas") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$escolas,na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_escolas_1),
             size = 4) -> g_escolas_1

# Dispersão das  ponderadas
df %>% 
    mutate(escolas_ponderadas = ponderacao(escolas)) %>%
    ggplot(aes(x = escolas_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Escolas por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação Escolas por 100.000 habitantes") +
    annotate("text", x = 0.9 * max(ponderacao(df$escolas),na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_escolas_2),
             size = 4) -> g_escolas_2

# Dispersão do log das  ponderadas
df %>% 
    mutate(escolas_log = log(ponderacao(escolas))) %>%
    ggplot(aes(x = escolas_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Escolas) por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$escolas)),na.rm = T),
             y = 0.05 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_escolas_3),
             size = 4) -> g_escolas_3

grid.arrange(g_escolas_1, g_escolas_2,g_escolas_3, ncol = 3)

df %>% dplyr::select(indiceEnvelhecimento, escolas) %>%
    mutate(escolasP = ponderacao(escolas)) %>%
    arrange(desc(escolasP)) %>%
    View(title = "Escolas")
             
#Escolas e Matrículas são correlacionadas, optamos por tirar Escolas por melhor ajuste ao modelo.
             
df %<>%
    dplyr::select(-escolas)             
             
#### 5.6) Matrículas ####

# Correlação do total
c_matriculas_1 <- round(cor(df$indiceEnvelhecimento, df$matriculas, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_matriculas_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$matriculas), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_matriculas_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$matriculas)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("Matriculas | Dispersão do Total")
df %>% 
    ggplot(aes(x = matriculas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Matrículas") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Quantidade de Matrículas") +
    annotate("text", x = 0.9 * max(df$matriculas,na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_matriculas_1),
             size = 4,
             color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_matriculas_1

# Dispersão das  ponderadas
df %>% 
    mutate(matriculas_ponderadas = ponderacao(matriculas)) %>%
    ggplot(aes(x = matriculas_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Matrículas por 100.000 habitantes") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Matrículas Ponderadas") +
    annotate("text", x = 0.9 * max(ponderacao(df$matriculas),na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_matriculas_2),
             size = 4,
             color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_matriculas_2

# Dispersão do log das  ponderadas
df %>% 
    mutate(matriculas_log = log(ponderacao(matriculas))) %>%
    ggplot(aes(x = matriculas_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Matrículas por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Log das Matrículas Ponderadas") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$matriculas)),na.rm = T),
             y = 0.05 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_matriculas_3),
             size = 4,
             color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_matriculas_3

grid.arrange(g_matriculas_1, g_matriculas_2, ncol = 2)

df %>% dplyr::select(indiceEnvelhecimento, matriculas) %>%
    mutate(matriculasP = ponderacao(matriculas)) %>%
    arrange(desc(matriculasP)) %>%
    View(title = "Matriculas")

df %<>%
    mutate(matriculas = ponderacao(matriculas))
             
#### 5.7) Profissionais da Saúde ####

# Correlação do total
c_profissionaisS_1 <- round(cor(df$indiceEnvelhecimento, df$profissionaisS, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_profissionaisS_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$profissionaisS), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_profissionaisS_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$profissionaisS)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("Profissionais da Saúde | Dispersão do Total")
df %>% 
    ggplot(aes(x = profissionaisS, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Profissionais da Saúde") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Quantidade de Profissionais da Saúde") +
    annotate("text", x = 0.9 * max(df$profissionaisS,na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_profissionaisS_1),
             size = 4,
             color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_profissionaisS_1

# Dispersão das  ponderadas
df %>% 
    mutate(profissionaisS_ponderadas = ponderacao(profissionaisS)) %>%
    ggplot(aes(x = profissionaisS_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Profissionais da Saúde por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Quantidade Ponderada de Profissionais") +
    annotate("text", x = 0.9 * max(ponderacao(df$profissionaisS),na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_profissionaisS_2),
             size = 4,
             color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_profissionaisS_2

# Dispersão do log das  ponderadas
df %>% 
    mutate(profissionaisS_log = log(ponderacao(profissionaisS))) %>%
    ggplot(aes(x = profissionaisS_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Profissionais da Saúde por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$profissionaisS)),na.rm = T),
             y = 0.05 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_profissionaisS_3),
             size = 4,
             color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_profissionaisS_3

grid.arrange(g_profissionaisS_1, g_profissionaisS_2, ncol = 2)

df %>% dplyr::select(indiceEnvelhecimento, profissionaisS) %>%
    mutate(profissionaisSP = ponderacao(profissionaisS)) %>%
    arrange(desc(profissionaisSP)) %>%
    View(title = "Profissionais da Saúde")

df %<>%
    mutate(profissionaisS = ponderacao(profissionaisS))

#### 5.8) Internet Fixa ####
             
# Correlação do total
c_internetFixa_1 <- round(cor(df$indiceEnvelhecimento, df$internetFixa, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_internetFixa_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$internetFixa), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_internetFixa_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$internetFixa)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("Internet Fixa | Dispersão do Total")
df %>% 
    ggplot(aes(x = internetFixa, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Internet Fixa") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$internetFixa,na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_internetFixa_1),
             size = 4) -> g_internetFixa_1

# Dispersão das  ponderadas
df %>% 
    mutate(internetFixa_ponderada = ponderacao(internetFixa)) %>%
    ggplot(aes(x = internetFixa_ponderada, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Internet Fixa por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação por 100.000 habitantes") +
    annotate("text", x = 0.9 * max(ponderacao(df$internetFixa),na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_internetFixa_2),
             size = 4) -> g_internetFixa_2

# Dispersão do log das  ponderadas
df %>% 
    mutate(internetFixa_log = log(ponderacao(internetFixa))) %>%
    ggplot(aes(x = internetFixa_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Internet Fixa por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$internetFixa)),na.rm = T),
             y = 0.05 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_internetFixa_3),
             size = 4) -> g_internetFixa_3

grid.arrange(g_internetFixa_1, g_internetFixa_2,g_internetFixa_3, ncol = 3)

df %>% dplyr::select(indiceEnvelhecimento, internetFixa) %>%
    mutate(internetFixaP = ponderacao(internetFixa)) %>%
    arrange(desc(internetFixaP)) %>%
    View(title = "Internet Fixa")

df %<>%
    mutate(internetFixa = ponderacao(internetFixa))

#### 5.9) Energia Total ####

# Correlação do total
c_energiaTotal_1 <- round(cor(df$indiceEnvelhecimento, df$energiaTotal, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_energiaTotal_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$energiaTotal), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_energiaTotal_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$energiaTotal)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11("Energia Total | Dispersão do Total")
df %>% 
    ggplot(aes(x = energiaTotal, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Energia Total") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$energiaTotal,na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_energiaTotal_1),
             size = 4) -> g_energiaTotal_1

# Dispersão das  ponderadas
df %>% 
    mutate(energiaTotal_ponderadas = ponderacao(energiaTotal)) %>%
    ggplot(aes(x = energiaTotal_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Energia Total por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação por 100.000 habitantes") +
    annotate("text", x = 0.9 * max(ponderacao(df$energiaTotal),na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_energiaTotal_2),
             size = 4) -> g_energiaTotal_2

# Dispersão do log das  ponderadas
df %>% 
    mutate(energiaTotal_log = log(ponderacao(energiaTotal))) %>%
    ggplot(aes(x = energiaTotal_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Energia Total por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$energiaTotal)),na.rm = T),
             y = 0.05 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_energiaTotal_3),
             size = 4) -> g_energiaTotal_3

grid.arrange(g_energiaTotal_1, g_energiaTotal_2,g_energiaTotal_3, ncol = 3)

df %>% dplyr::select(indiceEnvelhecimento, energiaTotal) %>%
    mutate(energiaTotalP = ponderacao(energiaTotal)) %>%
    arrange(desc(energiaTotalP)) %>%
    View(title = "Energia Total")

df %<>%
    dplyr::select(-energiaTotal)

#### 5.10) Energia Industria ####

# Correlação do total
c_energiaIndustria_1 <- round(cor(df$indiceEnvelhecimento, df$energiaIndustria, use = "pairwise.complete.obs"),2)

# Correlação por 100.000 habitantes
c_energiaIndustria_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$energiaIndustria), use = "pairwise.complete.obs"),2)

# Corelação do log da ponderação
c_energiaIndustria_3 <- round(cor(df$indiceEnvelhecimento, log(ponderacao(df$energiaIndustria)), use = "pairwise.complete.obs"),2)

# Dispersão do total
x11(" | Dispersão do Total")
df %>% 
    ggplot(aes(x = energiaIndustria, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Energia Industria") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$energiaIndustria,na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_energiaIndustria_1),
             size = 4) -> g_energiaIndustria_1

# Dispersão das  ponderadas
df %>% 
    mutate(energiaIndustria_ponderadas = ponderacao(energiaIndustria)) %>%
    ggplot(aes(x = energiaIndustria_ponderadas, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Energia Industria por 100.000 habitantes") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da ponderação por 100.000 habitantes") +
    annotate("text", x = 0.9 * max(ponderacao(df$energiaIndustria),na.rm = T),
             y = 0.95 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_energiaIndustria_2),
             size = 4) -> g_energiaIndustria_2

# Dispersão do log das  ponderadas
df %>% 
    mutate(energiaIndustria_log = log(ponderacao(energiaIndustria))) %>%
    ggplot(aes(x = energiaIndustria_log, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Energia Industria por 100.000 habitantes)") +
    ylab("Índice de Envelhecimento") +
    ggtitle("Scaterplot do log da ponderação") +
    annotate("text", x = 0.9 * max(log(ponderacao(df$energiaIndustria)),na.rm = T),
             y = 0.05 * max(df$indiceEnvelhecimento),
             label = paste0("Corr: ", c_energiaIndustria_3),
             size = 4) -> g_energiaIndustria_3

grid.arrange(g_energiaIndustria_1, g_energiaIndustria_2,g_energiaIndustria_3, ncol = 3)

df %>% dplyr::select(indiceEnvelhecimento, energiaIndustria) %>%
    mutate(energiaIndustriaP = ponderacao(energiaIndustria)) %>%
    arrange(desc(energiaIndustriaP)) %>%
    View(title = "Energia Industria")

# A transformação log(Gasto energético industrial por 100.000 habitantes) teve melhor ajuste
df %<>%
    mutate(energiaIndustria = log(ponderacao(energiaIndustria)))

#### 5.11) Crescimento Geométrico ####
 
# Correlação do total
c_crescimento_1 <- round(cor(df$indiceEnvelhecimento, df$crescimento, use = "pairwise.complete.obs"),2)

# Correlação do log
c_crescimento_2 <- round(cor(df$indiceEnvelhecimento, log(df$crescimento), use = "pairwise.complete.obs"),2)

x11("Crescimento Geométrico")
df %>% 
    ggplot(aes(x = crescimento, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Crescimento Geométrico") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Crescimento Geométrico") +
    annotate("text", x = 0.9 * max(df$crescimento,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_crescimento_1),
                    size = 4,
                    color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        ) -> g_crescimento_1

g_crescimento_1

# A curva que melhor se ajusta ao gráfico é a curva sem transformações

#### 5.12) Densidade Demográfica ####

# Correlação do total
c_densidade_1 <- round(cor(df$indiceEnvelhecimento, df$densidade, use = "pairwise.complete.obs"),2)

# Correlação do log
c_densidade_2 <- round(cor(df$indiceEnvelhecimento, log(df$densidade), use = "pairwise.complete.obs"),2)

x11("Densidade Demográfica")
df %>% 
    ggplot(aes(x = densidade, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Densidade Demográfica") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Densidade Demográfica") +
    annotate("text", x = 0.9 * max(df$densidade,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_densidade_1),
                    size = 4,
                    color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_densidade_1

df %>% 
    ggplot(aes(x = log(densidade), y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("log(Densidade Demográfica)") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Log da Densidade Demográfica") +
    annotate("text", x = 0.9 * max(log10(df$densidade),na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_densidade_2),
                    size = 4,
                    color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        )-> g_densidade_2

grid.arrange(g_densidade_1, g_densidade_2, ncol = 2)

# Ambas as curvas apresentam comportamentos não lineares, a que mais se adequa é a curva do log da densidade demográfica.

# Trasformação
df %<>% mutate(densidade = log(densidade))

#### 5.13) Sexo ####

# Correlação do total
c_sexo_1 <- round(cor(df$indiceEnvelhecimento, df$sexo, use = "pairwise.complete.obs"),2)

x11("Sexo")
df %>% 
    ggplot(aes(x = sexo, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Porporção de Homens") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Porporção de Homens") +
    annotate("text", x = 0.99 * max(df$sexo,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_sexo_1),
                    size = 4,
                    color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        ) -> g_sexo_1

g_sexo_1

# Sexo não precisa de transformaçõoes, pois já possui relação linear com o indice de envelhecimento

#### 5.14) Distância à Capital ####

# Correlação do total
c_distancia_1 <- round(cor(df$indiceEnvelhecimento, df$distancia, use = "pairwise.complete.obs"),2)

x11("Distância à Capital")
df %>% 
    ggplot(aes(x = distancia, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Distância à Capital") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$distancia,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_distancia_1),
                    size = 4) -> g_distancia_1

g_distancia_1

# Distância à capital não precisa de transformações, pois já possui relação linear com o indice de envelhecimento


#### 5.15) Roubo e Furto ####

# Correlação entre roubo e furto
c_roubo_furto <- round(cor(df$roubo, df$furto, use = "pairwise.complete.obs"),2)

# Correlação do total
c_roubo_1 <- round(cor(df$indiceEnvelhecimento, df$roubo, use = "pairwise.complete.obs"),2)
c_furto_1 <- round(cor(df$indiceEnvelhecimento, df$furto, use = "pairwise.complete.obs"),2)

x11("Roubo")
df %>% 
    ggplot(aes(x = roubo, y = furto)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    xlab("Roubo") +
    ylab("Furto") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$roubo,na.rm = T),
                    y = 0.95 * max(df$furto),
                    label = paste0("Corr: ", c_roubo_furto),
                    size = 4) -> g_roubo_furto

df %>% 
    ggplot(aes(x = roubo, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Roubo") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Quantidade de Roubos") +
    annotate("text", x = 0.9 * max(df$roubo,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_roubo_1),
                    size = 4,
                    color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        ) -> g_roubo_1

df %>% 
    ggplot(aes(x = furto, y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Furto") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot do Total") +
    annotate("text", x = 0.9 * max(df$furto,na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_furto_1),
                    size = 4) -> g_furto_1

grid.arrange(g_roubo_furto, g_roubo_1, g_furto_1, ncol = 3)

# Correlação da ponderação
c_roubo_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$roubo), use = "pairwise.complete.obs"),2)
c_furto_2 <- round(cor(df$indiceEnvelhecimento, ponderacao(df$furto), use = "pairwise.complete.obs"),2)

x11("Roubo")
df %>% 
    ggplot(aes(x = ponderacao(roubo), y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Roubo") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Quantidade Ponderada de Roubos") +
    annotate("text", x = 0.9 * max(ponderacao(df$roubo),na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_roubo_2),
                    size = 4,
                    color = "white") +
    theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),  # fundo da imagem
            panel.background = element_rect(fill = "#1a1a1a", color = NA), # fundo do painel
            legend.background = element_rect(fill = "#1a1a1a", color = NA),
            legend.key = element_rect(fill = "#1a1a1a", color = NA),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 14, face = "bold"),
            plot.caption = element_text(color = "gray80"),
            axis.text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            legend.title = element_text(color = "white")
        ) -> g_roubo_2

df %>% 
    ggplot(aes(x = ponderacao(furto), y = indiceEnvelhecimento)) +
    geom_point(alpha = 0.5, color = "white") + 
    geom_smooth(method = "lm", col = "blue") +
    geom_smooth(col = "red") +
    xlab("Ponderação do Furto") +
    ylab("Indice de Envelhecimento") +
    ggtitle("Scaterplot da Ponderação") +
    annotate("text", x = 0.9 * max(ponderacao(df$furto),na.rm = T),
                    y = 0.95 * max(df$indiceEnvelhecimento),
                    label = paste0("Corr: ", c_furto_2),
                    size = 4) -> g_furto_2

grid.arrange(g_roubo_1, g_roubo_2, ncol = 2)

# As duas variáveis são correlacionadas, usaremos roubo como variável explicativa, ponderada pela população

# Trasformação
df %<>% mutate(roubo = ponderacao(roubo)) %>%
    select(-furto)
View(df)

#### 6) Seleção das variáveis explicativas ####

df %<>%
    select(-populacao)
    
x11("GGPairs")
ggpairs(df)

x11("CorrPlot")
correlacoes <- cor(df, use = "pairwise.complete.obs")
corrplot(correlacoes, method = "color")

# 6.1) Modelos

# Removendo linhas com NA
df %<>%
    na.omit()

# Modelo Base, com todas as variáveis explicativas

modelo_1 <- lm(indiceEnvelhecimento ~ logPopulacao + 
                                    sexo +
                                    matriculas +
                                    distancia +
                                    profissionaisS + 
                                    densidade +
                                    energiaIndustria + 
                                    crescimento +
                                    roubo +
                                    despesas + 
                                    graudeU +
                                    internetFixa +
                                    Norte +
                                    `Centro e Centro-Sul` +
                                    Noroeste +
                                    `Vales do Iguaçu` +
                                    Oeste +
                                    Litoral +
                                    `Campos Gerais`,
                                    data = df)
summary(modelo_1)
anova(modelo_1)
car::Anova(modelo_1, type = "III")

# Proposta 2) modelo sem graudeU e Internet Fixa
modelo_2 <- lm(indiceEnvelhecimento ~ logPopulacao + 
                                    sexo +
                                    matriculas +
                                    distancia +
                                    profissionaisS + 
                                    densidade +
                                    energiaIndustria + 
                                    crescimento +
                                    roubo +
                                    despesas +
                                    Norte +
                                    `Centro e Centro-Sul` +
                                    Noroeste +
                                    `Vales do Iguaçu` +
                                    Oeste +
                                    Litoral +
                                    `Campos Gerais`,
                                    data = df)

# Comparações pelos critérios AIC e BIC
AIC(modelo_1) > AIC(modelo_2)
BIC(modelo_1) > BIC(modelo_2)
# O AIC e o BIC do modelo_2 é menor que o do modelo, portanto, ele é melhor

# Anova para comparação dos modelos
anova(modelo_1, modelo_2)
# A remoção das variáveis não resultou em um aumento significativo da soma de quadrados dos resíduos
# Portanto, o modelo 2 é melhor que o modelo completo

summary(modelo_2)

# Anova para candidatos a serem removidos
anova(modelo_2)
car::Anova(modelo_2, type = "III")

# Proposta 3) Modelo  2  - despesas
modelo_3 <- lm(indiceEnvelhecimento ~ logPopulacao + 
                                    sexo +
                                    matriculas +
                                    distancia +
                                    profissionaisS + 
                                    densidade +
                                    energiaIndustria + 
                                    crescimento +
                                    roubo+
                                    Norte +
                                    `Centro e Centro-Sul` +
                                    Noroeste +
                                    `Vales do Iguaçu` +
                                    Oeste +
                                    Litoral +
                                    `Campos Gerais`, data = df)
# Comparações pelos critérios AIC e BIC
AIC(modelo_1) > AIC(modelo_3)
AIC(modelo_2) > AIC(modelo_3)
BIC(modelo_1) > BIC(modelo_3)
BIC(modelo_2) > BIC(modelo_3)
# O AIC do modelo_3 é menor que o Modelo Completo e maior que o Modelo_2. Já o BIC é menor em ambos os casos.

# Anova para comparação dos modelos
anova(modelo_1, modelo_3)
anova(modelo_2, modelo_3)
# o modelo 3 não aumentou significativamente a SQres em comparação com os dois modelos anteriores.
# Portanto, o modelo 3 é melhor que ambos.

summary(modelo_3)

# Anova para candidatos a serem removidos
anova(modelo_3)
car::Anova(modelo_3, type = "III")

# Proposta 4) Modelo  3  - distancia

modelo_4 <- lm(indiceEnvelhecimento ~ logPopulacao + 
                                    sexo +
                                    matriculas +
                                    profissionaisS + 
                                    densidade +
                                    energiaIndustria + 
                                    crescimento +
                                    roubo+
                                    Norte +
                                    `Centro e Centro-Sul` +
                                    Noroeste +
                                    `Vales do Iguaçu` +
                                    Oeste +
                                    Litoral +
                                    `Campos Gerais`, data = df)

# Comparações pelos critérios AIC e BIC
AIC(modelo_1) > AIC(modelo_4)
AIC(modelo_2) > AIC(modelo_4)
AIC(modelo_3) > AIC(modelo_4)
BIC(modelo_1) > BIC(modelo_4)
BIC(modelo_2) > BIC(modelo_4)
BIC(modelo_3) > BIC(modelo_4)
# Pelo critério AIC o modelo 4 é pior que todos os anteriores, mas pelo critério BIC ele é melhor.

# Anova para comparação dos modelos
anova(modelo_1, modelo_4)
anova(modelo_2, modelo_4)
anova(modelo_3, modelo_4)
# O modelo 4 não aumentou significativamente a SQres em comparação com o modelo completo, 
# apresentou aumento do SQres em relação aos modelos 2 e 3 a 5% de significânica, mas não a 1%
# Portanto, o modelo_4 pode ser escolhido

summary(modelo_4)

# Anova para candidatos a serem removidos
anova(modelo_4)
car::Anova(modelo_4, type = "III")

anova(modelo_1, modelo_2, modelo_3, modelo_4)
A <- df
df%<>%
    select(indiceEnvelhecimento,
        logPopulacao,
        sexo,
        matriculas,
        profissionaisS,
        densidade,
        energiaIndustria,
        crescimento,
        roubo)

x11("GGPairs")
ggpairs(df)

x11("CorrPlot")
correlacoes <- cor(df, use = "pairwise.complete.obs")
corrplot(correlacoes, method = "circle")

#### 7)  Análise de Resíduos ####

# 1. Resíduos Padronizados
residuos <- rstandard(modelo_4)
outliers <- which(abs(residuos) > 2)
valores_ajustados <- fitted(modelo_4)

# 2. Gráfico de Resíduos vs Ajustados
x11("Resíduos vs Ajustados")
# Criar um data frame com os dados
residuos_df <- data.frame(
  valores_ajustados = valores_ajustados,
  residuos = residuos,
  outlier = ifelse(seq_along(residuos) %in% outliers, "Sim", "Não")
)

# Criar o gráfico com ggplot2
ggplot(residuos_df, aes(x = valores_ajustados, y = residuos, color = outlier)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.7) +
  scale_color_manual(values = c("Não" = "white", "Sim" = "red")) +
  labs(x = "Valores Ajustados", 
       y = "Resíduos", 
       title = "Resíduos Padronizados vs Valores Ajustados",
       color = "Outlier") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = "#333333"),
    panel.grid = element_line(color = "#333333"),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 14, face = "bold"),
    axis.text = element_text(color = "white"),
    legend.background = element_rect(fill = "#1a1a1a"),
    legend.key = element_rect(fill = "#1a1a1a"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) -> Residuos_Ajustados

# 3. Identificação de Pontos Influentes (Cook's Distance)
cooksd <- cooks.distance(modelo_4)

limiar_cook <- 4/(nrow(df) - length(coef(modelo_4)) - 1)

x11("Distância de Cook")
plot(cooksd, pch = "*", cex = 2, 
     main = "Pontos Influentes - Distância de Cook",
     ylab = "Distância de Cook", xlab = "Índice")
abline(h = limiar_cook, col = "red")


# 5. Gráficos de Resíduos vs Variáveis Explicativas
x11("Resíduos vs Variáveis Explicativas", width = 12, height = 8)

# Criar uma lista para armazenar os gráficos
plot_list <- list()

# Definir variáveis
variaveis <- c("logPopulacao", "matriculas", "profissionaisS", 'densidade', 
               "energiaIndustria", "crescimento", "sexo", "roubo")

# Criar cada gráfico
for (i in seq_along(variaveis)) {
  var <- variaveis[i]
  df_plot <- data.frame(
    x = df[[var]],
    y = residuos,
    is_outlier = ifelse(seq_along(residuos) %in% outliers, "Sim", "Não")
  )
  
  p <- ggplot(df_plot, aes(x = x, y = y, color = is_outlier)) +
    geom_point(alpha = 0.3, show.legend = FALSE) +  # Remover legenda
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    scale_color_manual(values = c("Não" = "white", "Sim" = "red")) +  # Manter cores
    labs(
      x = var,
      y = "Resíduos Padronizados",
      title = paste("Resíduos vs", var)
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#1a1a1a", color = NA),
      panel.background = element_rect(fill = "#1a1a1a", color = "#333333"),
      panel.grid = element_line(color = "#333333"),
      text = element_text(color = "white"),
      plot.title = element_text(color = "white", size = 12, face = "bold"),
      axis.text = element_text(color = "white"),
      # Remover elementos da legenda
      legend.position = "none"
    )
  
  plot_list[[i]] <- p
}

# Organizar os gráficos em uma grade
grid.arrange(grobs = plot_list, ncol = 4)

# 6. QQplot
x11("QQPlot")
car::qqPlot(resid(modelo_4), 
           ylab = "Resíduos Padronizados",
           main = "QQ-Plot dos Resíduos")


# 6. QQplot personalizado
x11("QQPlot Personalizado")

# Calcular manualmente os dados do QQ-plot
residuos_pad <- resid(modelo_4)
n <- length(residuos_pad)
P <- ppoints(n)
Q <- qnorm(P)
residuos_ordenados <- sort(residuos_pad)

# Criar o data frame com os pontos do QQ-plot
qq_df <- data.frame(
  x = Q,
  y = residuos_ordenados
)

# Calcular a linha de referência (primeiro e terceiro quartis)
y_quantiles <- quantile(qq_df$y, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
x_quantiles <- qnorm(c(0.25, 0.75))
slope <- diff(y_quantiles)/diff(x_quantiles)
intercept <- y_quantiles[1] - slope * x_quantiles[1]

# Adicionar uma coluna de identificação de outliers no data frame
qq_df$is_outlier <- FALSE
qq_df$is_outlier[order(residuos_pad) %in% outliers] <- TRUE

# Criar o gráfico com o tema personalizado
ggplot(qq_df, aes(x = x, y = y, color = is_outlier, shape = is_outlier)) +
  # Pontos
  geom_point(size = 3, alpha = 0.7) +
  # Linha de referência
  geom_abline(slope = slope, intercept = intercept, 
              color = "red", linetype = "dashed", show.legend = FALSE) +
  # Escalas e temas
  scale_color_manual(
    name = "Outlier",
    values = c("FALSE" = "white", "TRUE" = "red"),
    labels = c("FALSE" = "Não", "TRUE" = "Sim")
  ) +
  scale_shape_manual(
    name = "Outlier",
    values = c("FALSE" = 19, "TRUE" = 1),  # 19 = ponto preenchido, 1 = círculo vazio
    labels = c("FALSE" = "Não", "TRUE" = "Sim")
  ) +
  labs(
    x = "Quantis Teóricos", 
    y = "Resíduos Padronizados",
    title = "QQ-Plot dos Resíduos"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#1a1a1a", color = NA),
    panel.background = element_rect(fill = "#1a1a1a", color = "#333333"),
    panel.grid = element_line(color = "#333333"),
    text = element_text(color = "white"),
    plot.title = element_text(color = "white", size = 14, face = "bold"),
    axis.text = element_text(color = "white"),
    axis.line = element_line(color = "white"),
    legend.background = element_rect(fill = "#1a1a1a"),
    legend.key = element_rect(fill = "#1a1a1a"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) -> Residuos_QQ

grid.arrange(Residuos_Ajustados, Residuos_QQ, ncol = 2)

# 7. Gráficos de Valores Ajustados vs Variáveis Preditoras
x11("Valores Ajustados vs Variáveis Preditoras", width = 12, height = 8)
par(mfrow = c(2, 4))

for (var in variaveis) {
  plot(df[[var]], valores_ajustados, 
       xlab = var, ylab = "Valores Ajustados",
       main = paste("Valores Ajustados vs", var))
  
  # Destacar outliers
  if (length(outliers) > 0) {
    points(df[[var]][outliers], valores_ajustados[outliers], 
           col = "red", pch = 19)
  }
}



#### Anális dos Outliers ####

# 1. top 10 outliers 
# Nome das cidadades
cidades <- names(sort(abs(residuos), decreasing = TRUE)[1:5])
cidades <- "Maringá/RGI de Maringá"
modelo_4$fitted.values["Maringá/RGI de Maringá"]

# Tabela dos outliers
df %>% 
  rownames_to_column("row_names") %>% 
  left_join(data.frame(row_names = row.names(df),
                        fitted.values = modelo_4$fitted.values),
            by = "row_names") %>% 
  filter(row_names %in% cidades) %>% 
  arrange(match(row_names, cidades)) %>% 
  View(title = "Outliers")

# 2. Gráficos da posição dos outliers em função das Variáveis Explicativas



c <- "Maringá/RGI de Maringá"
groups <- list()
x11("Análise de Outlier", width = 12, height = 8)
titles <- c("População", "Proporção de Homens", "Matrículas", "Saúde", 
           "Densidade Demográfica", "Energia Industrial", 
           "Crescimento Geométrico", "Roubo")

# Obter os nomes das variáveis na ordem correta
variaveis <- names(df)[-c(1,15:21)]

# Garantir que temos o mesmo número de títulos e variáveis
stopifnot(length(titles) == length(variaveis))

for (i in seq_along(variaveis)) {
    var <- variaveis[i]
    title <- titles[i]
    
    grafic <- df %>% 
        ggplot(aes(x = !!sym(var))) +
        geom_density(fill = "#333333", color = "white", alpha = 0.7) +
        geom_vline(xintercept = df[c, var], color = "red", linetype = "dashed", size = 1) +
        labs(
            title = title,
            x = var,
            y = "Densidade"
        ) +
        theme_minimal() +
        theme(
            plot.background = element_rect(fill = "#1a1a1a", color = NA),
            panel.background = element_rect(fill = "#1a1a1a", color = "#333333"),
            panel.grid = element_line(color = "#333333"),
            text = element_text(color = "white"),
            plot.title = element_text(color = "white", size = 10, face = "bold"),
            axis.text = element_text(color = "white"),
            axis.line = element_line(color = "white")
        )
    
    groups[[i]] <- grafic
}

# Organizar os gráficos em uma grade
grid.arrange(grobs = groups, ncol = 4)


coef <- coefficients(modelo_4)
cidade <- df["Maringá/RGI de Maringá",]

resultados <- coef * cidade

View(resultados)

