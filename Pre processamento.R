#Carregando data frames
goals <- read.csv("C:/Dados/goals.csv", header = TRUE)
matches <- read.csv("C:/Dados/matches.csv", header = TRUE)
players <- read.csv("C:/Dados/players.csv", header = TRUE)

install.packages("tidyverse")
install.packages("factoextra")
install.packages("FactoMineR")
library(dplyr)
library (tibble)
library (stats)
library (cluster)
library (factoextra)
library (FactoMineR)
#Junção dos data frames players e goals
qntgols <- data.frame(inner_join (players, goals, by = "player_id"))
#Selecionando apenas as colunas desejadas
qntgols <- select (d, player_id, goal_id)
#Junção dos data frames players e matches, ela aconteceu duas vezes por causa do player 1 e 2
dmatches2 <- data.frame (inner_join(players, matches, by = c ("player_id" = "player_2")))
dmatches <- data.frame (inner_join(players, matches, by = c ("player_id" = "player_1")))
#Selecionando apenas as colunas desejadas
dmatches <- select (dmatches, player_id, match_id)
dmatches2 <- select (dmatches2, player_id, match_id)
#Realocando os dois data frames juntos
qntmatches <- rbind (dmatches, dmatches2)
#Calcular a quantidade de gols realizados por cada jogador
qntgoljogador <- data.frame(qntgols %>% group_by (player_id) %>% count())
#Calcular a quantidade de partidas jogadas por cada jogador
qntmatchjogador <- data.frame(qntmatches %>% group_by (player_id) %>% count())
#Renomear colunas
qntgoljogador <- rename(qntgoljogador, qntgol = n)
qntmatchjogador <- rename(qntmatchjogador, qntmatch = n)
#Unindo tabelas
qntmatchgol <- data.frame(inner_join (qntgoljogador, qntmatchjogador, by = "player_id"))
#Adicionar coluna da media
qntmatchgol <- mutate (qntmatchgol, media = qntgol/qntmatch)
#Visualizar 
plot(dados_gerais$lista, dados_gerais$media)
#Separar dados para clusterização    
dados <- select (qntmatchgol, qntgol, qntmatch)
#Label nas linhas
row.names(dados) <- qntmatchgol[, 1]
#Colocar dados em escala
dados <- scale (dados)
#Definir quantidade otima de cluster
fviz_nbclust (dados, kmeans, method = "gap_stat")
#Gerar kmeans
dados_kmeans <- kmeans (dados, 6)
#Visualizar grafico de cada grupo
fviz_cluster (dados_kmeans, data = dados)
#Separar dados a partir dos clusters
lista <- dados_kmeans$cluster
dados_gerais <- cbind(qntmatchgol, lista)
