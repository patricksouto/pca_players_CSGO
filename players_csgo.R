#Instalacao e carregamento de Todos os Pacotes ---------------------------

pacotes <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","ggrepel",
             "factoextra","sp","tmap","magick","gridExtra", "readxl")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Problema: Elaborar um ranking dos melhores jogadores de csgo de 2019 que jogaram pelo menos 20 partidas

#Importando e verificando a base de dados
players <- read_excel("players2019.xlsx")
head(players)

#Transformando os nomes dos players em nome das linhas
players2 <- players %>%
  column_to_rownames("player_name")

summary(players2)

#Analisando a correlacao entre variaveis
chart.Correlation(players2, histogram = TRUE, pch = "+")

#Salvando a matriz de correlacoes
rho_players <- cor(players2)

#Construindo mapa de calor a partir das correlacoes
rho_players %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 0))

#Realizando o teste de esfericidade de Bartlett
cortest.bartlett(R = rho_players)

#Padronizando os dados via zscores
players2_padronizado <- players2 %>% 
  scale() %>%
  data.frame()

#Rodando a PCA
afpc_players <- prcomp(players2_padronizado)
summary(afpc_players)

#Visualizando os pesos que cada variável tem em cada componente principal obtido pela PCA
data.frame(afpc_players$rotation) %>%
  mutate(var = names(players2)) %>% 
  melt(id.vars = "var") %>%
  mutate(var = factor(var)) %>%
  ggplot(aes(x = var, y = value, fill = var)) +
  geom_bar(stat = "identity", color = "black") +
  facet_wrap(~variable) +
  labs(x = NULL, y = NULL, fill = "Legenda:") +
  scale_fill_viridis_d() +
  theme_bw()

#Resumindo informacoes importantes
data.frame(eigenvalue = afpc_players$sdev ^ 2,
           var_compartilhada = summary(afpc_players)$importance[2,],
           var_cumulativa = summary(afpc_players)$importance[3,]) -> relatorio

relatorio %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Extraindo cargas fatoriais
k <- sum((afpc_players$sdev ^ 2) > 1)
cargas_fatoriais <- afpc_players$rotation[, 1:k] %*% diag(afpc_players$sdev[1:k])

#Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>% 
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

#Visualizando as comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>% 
  rename(comunalidades = 1) %>% 
           kable() %>% 
           kable_styling(bootstrap_options = "striped",
                         full_width = T,
                         font_size = 12)

#Criando relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>% 
  rename(F1 = X1,
         F2 = X2,
         F3 = X3) %>% 
  mutate(comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

# Plotagem das Cargas Fatoriais
data.frame(cargas_fatoriais) %>%
  ggplot(aes(x = X1, y = X2)) +
  geom_point(color = "dodgerblue4") +
  geom_hline(yintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "darkgoldenrod3", linetype = "dashed") +
  geom_text_repel(label = row.names(cargas_fatoriais)) +
  labs(x = paste("F1", paste0("(",
                              round(summary(afpc_players)$importance[2,1] * 100,
                                    digits = 2),
                              "%)")),
       y = paste("F2", paste0("(",
                              round(summary(afpc_players)$importance[2,2] * 100,
                                    digits = 2),
                              "%)"))) +
  theme_bw()

#scores fatoriais
scores_fatoriais <- t(afpc_players$rotation/afpc_players$sdev)
colnames(scores_fatoriais) <- colnames(players2_padronizado)

scores_fatoriais

scores_fatoriais %>% 
  t() %>% 
  data.frame() %>% 
  rename(PC1 = 1,
         PC2 = 2,
         PC3 = 3) %>% 
  select(PC1, PC2, PC3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

#Construção do ranking

#Calcula-se os scores fatoriais
score_D1 <- scores_fatoriais[1,]
score_D1

Score_D2 <- scores_fatoriais[2,]
Score_D2

Score_D3 <- scores_fatoriais[3,]
Score_D3

#Estabelecendo ranking dos indicadores
F1 <- t(apply(players2_padronizado, 1, function(x) x * score_D1))
F2 <- t(apply(players2_padronizado, 1, function(x) x * Score_D2))
F3 <- t(apply(players2_padronizado, 1, function(x) x * Score_D3))

F1 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

F2 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

F3 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

#multiplicando por 1
F1 <- data.frame(F1) %>% 
  mutate(fator1 = rowSums(.) * 1)

F1 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

F2 <- data.frame(F2) %>% 
  mutate(fator2 = rowSums(.) * 1)

F2 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

F3 <- data.frame(F3) %>% 
  mutate(fator3 = rowSums(.) * 1)

F3 %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)

#Importando as colunas de fatores F1, F2. F3 e F4
players2["Fator1"] <- F1$fator1
players2["Fator2"] <- F2$fator2
players2["Fator3"] <- F3$fator3

#Criando ranking pela soma ponderada dos fatores por sua variância compartilhada
#calculando a variância compartilhada
var_compartilhada <- (afpc_players$sdev ^ 2/sum((afpc_players$sdev ^ 2)))
var_compartilhada

players2 %>% 
  mutate(pontuacao = Fator1 * var_compartilhada[1] +
           Fator2 * var_compartilhada[2] + 
           Fator3 * var_compartilhada[3]) -> players2
           

#Visualizando ranking final
players2 [,10:13] %>% 
  arrange(desc(pontuacao)) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "striped",
                full_width = T,
                font_size = 12)
