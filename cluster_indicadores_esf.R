#Configurações
options(scipen=100)
options(digits=8)

#Bibliotecas usadas no script
library(reshape2)
library(readr)
library(tidyr)
library(caret)
library(ggplot2)
library(corrgram)
library(devtools)
library(plot3D)
library(plot3Drgl)
library(corrplot)
library(cluster)
library(purrr)
library(factoextra)
library(NbClust)
library(ape)

#Leitura do arquivo CSV atribuindo a um dataframe
indicadores_esf <- read_csv("indicadores_esf.csv")
#remove a primeira colune "Distrito"
indicadores_esf <- indicadores_esf[, -c(1)]
#Cria uma coluna com mesclando as colunas "Unidade" e "Área"
indicadores_esf$UNIDADE_AREA <- paste0(indicadores_esf$UNIDADE, " - Area ", indicadores_esf$AREA)
#Remove as duas primeiras coluna, "Unidade" e "Área"
indicadores_esf <- indicadores_esf[, -c(1,2)]

# pré processamento dados
#função para encontrar o "centro" e escalar cada coluna
banco_scale <- scale(indicadores_esf[,-11], center = TRUE, scale = TRUE) #armazena em um novo banco

#banco apenas com a coluna "Unidade - Área"
banco_areas <- indicadores_esf[, 11]

#banco com os dados escalados por "Unidade - Área"
banco_final <- cbind.data.frame(banco_areas, banco_scale) %>% as.data.frame()

#com o pacote corrgram
corrgram(banco_final[, -c(1,12)], order=NULL, lower.panel=panel.shade) #correlograma

#com o pacote corrplot
#cria uma matriz de correlação
correlacao <- cor(banco_final[, -c(1,11)]) 

corrplot(correlacao) #correlograma

#substitui os valores "NA" por "0"
banco_scale[is.na(banco_scale)] <- 0


##########################################################
#Clusterizando com kmeans
##########################################################
##Analisando melhor k
###Initialise ratio_ss 
ratio_ss <- rep(0,7)

#usando o banco_scale para calcular o K, pois o banco_scale possui apenas valores numéricos
###For-loop para calcular ratio
for (k in 1:7) {
  
  # Apply k-means to school_result: school_km
  cluster <- kmeans(banco_scale, centers = k, nstart = 20)
  
  # Save the ratio between of WSS to TSS in kth element of ratio_ss
  ratio_ss[k] <- cluster$tot.withinss/cluster$totss
  
}

###Fazendo scree plot type "b" and xlab "k"
plot(ratio_ss, type = "b", xlab = "k")

########################################################

'
#código para determinar o número de clusters para K-Means
#3 resultados diferentes para o número de clusters
#http://www.sthda.com/english/articles/29-cluster-validation-essentials/96-determining-the-optimal-number-of-clusters-3-must-know-methods/
fviz_nbclust(banco_scale, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

fviz_nbclust(banco_scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

fviz_nbclust(banco_scale, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
'

################################################################################################################################################
#CLUSTER K-MEANS

#substitui os valores "NA" por "0"
banco_final[is.na(banco_final)] <- 0

#Clusterizando com k = 2 e 20 inícios, retirada a coluna de UNIDADE para o cálculo
cluster_k <- kmeans(banco_final[,-1], centers = 2, nstart = 20)

#cola a coluna com o número do cluster no banco_final
banco_final <- cbind(banco_final, cluster_k$cluster)

#cola os bancos "indicadores_esf" e a coluna cluster de "banco_final"
banco_areas_cluster <- cbind.data.frame(indicadores_esf, cluster = banco_final$`cluster_k$cluster`) %>% as.data.frame() #transforma em dataframe

#Clusterizando com k =3 e 20 inícios, retirada a coluna de UNIDADE para o cálculo
cluster_k3 <- kmeans(banco_final[,-1], centers = 3, nstart = 20)

#cola a coluna com o número do cluster no banco_final_k3
banco_final_k3 <- cbind(banco_final, cluster_k3$cluster)

#cola os bancos "indicadores_esf" e a coluna cluster de "banco_final_k3"
banco_areas_cluster_k3 <- cbind.data.frame(indicadores_esf, cluster=banco_final_k3$`cluster_k3$cluster`) %>% as.data.frame() #transforma em dataframe
#transforma a coluna "UNIDADE_AREA" em fator
banco_areas_cluster_k3$UNIDADE_AREA <- as.factor(banco_areas_cluster_k3$UNIDADE_AREA)
################################################################################################################################################

################################################################################################################################################
#função para romover os caracteres especiais
rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}
################################################################################################################################################

#aplica a fnção no banco_areas_cluster_k3 para a coluna "UNIDADE_AREA"
banco_areas_cluster_k3$UNIDADE_AREA <- rm_accent(banco_areas_cluster_k3$UNIDADE_AREA)

#variáveis com dados de 3 colunas
#Usuarios Diferentes - Medico enfermeiro
x <- as.double(banco_areas_cluster_k3$USU_MED_ENF) 
#Usuarios Diferentes - Total  
y <- as.double(banco_areas_cluster_k3$USU_TODOS_SERVICOS)
#Procedimentos Complementares  
z <- as.double(banco_areas_cluster_k3$PROCS_COMPL_MED)

#plot do gráfico em "Cubo" / configuração do gráfico
scatter3D(x, y, z, 
          colvar = NULL, col = banco_areas_cluster_k3$cluster, pch = 20, cex = 1.5, phi = 0, theta = 0, ticktype = "detailed", bty = "g", xlab = "Usuarios Diferentes - Medico enfermeiro",
          ylab = "Usuarios Diferentes - Total", zlab = "Procedimentos Complementares")
#inserindo as legendas
text3D(x, y, z,
       labels = banco_areas_cluster_k3$UNIDADE_AREA,
       add = TRUE, colkey = FALSE, cex = 0.5)

#torna o gráfico interatvo
plotrgl()

########################################################################################################################################
#CLUSTER HIERARQUICO

#banco com os dados escalados por "Unidade - Área"
banco_final_hc <- cbind.data.frame(banco_areas, banco_scale) %>% as.data.frame()

#troca valores de "NA" para "0"
banco_final_hc[is.na(banco_final_hc)] <- 0

#encontrar número de clusters
fviz_nbclust(banco_scale, hcut, method = c("silhouette", "wss", "gap_stat"))
#encontrar número de clusters
fviz_nbclust(banco_scale, hcut, method = "gap_stat")
fviz_nbclust(banco_scale, hcut, method = "wss")
fviz_nbclust(banco_scale, hcut, method = "silhouette")

#Encontrar o número de clusters
gap_stat1 <- clusGap(banco_scale, FUN = hcut, K.max = 10, B = 10)
#> Clustering k = 1,2,..., K.max (= 10): .. done
#> Bootstrapping, b = 1,2,..., B (= 10)  [one "." per sample]:
#> .......... 10 
fviz_gap_stat(gap_stat1)

#Encontrar o número de clusters
gap_stat2 <- clusGap(banco_final_hc[, -1], FUN = hcut, K.max = 10, B = 10)
#> Clustering k = 1,2,..., K.max (= 10): .. done
#> Bootstrapping, b = 1,2,..., B (= 10)  [one "." per sample]:
#> .......... 10 
fviz_gap_stat(gap_stat2)

#encontrar número de clusters
fviz_nbclust(banco_final_hc[, -1], hcut, method = "gap_stat")
fviz_nbclust(banco_final_hc[, -1], hcut, method = "wss")
fviz_nbclust(banco_final_hc[, -1], hcut, method = "silhouette")

banco_final_hc <- as.data.frame(banco_final_hc[, -1], row.names = banco_final_hc[, 1])

########################################################################################################################################

#####################EUCLIDIANA - COMPLETE##################################
# Dissimilarity matrix
dmc <- dist(banco_final_hc[, -1], method = "euclidean")

#clustering e plots
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(dmc, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 4, border = 2:10)

clust1 <- cutree(hc1, k = 4)
fviz_cluster(list(data = banco_final_hc, cluster = clust1))

#####################EUCLIDIANA - AVERAGE##################################
# Dissimilarity matrix
dma <- dist(banco_final_hc[, -1], method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc2 <- hclust(dma, method = "average" )
# Plot the obtained dendrogram
plot(hc2, cex = 0.6, hang = -1)
rect.hclust(hc2, k = 3, border = 2:10)

clust2 <- cutree(hc2, k = 3)
fviz_cluster(list(data = banco_final_hc, cluster = clust2))

#####################EUCLIDIANA - SIMPLE##################################
# Dissimilarity matrix
dms <- dist(banco_final_hc[, -1], method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc3 <- hclust(dms, method = "single" )
# Plot the obtained dendrogram
plot(hc3, cex = 0.6, hang = -1)
rect.hclust(hc3, k = 4, border = 2:10)

clust3 <- cutree(hc3, k = 4)
fviz_cluster(list(data = banco_final_hc, cluster = clust3))

####################AGNES############################
# Compute with agnes (make sure you have the package cluster)
hc4 <- agnes(banco_final_hc, method = "complete")
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
# Agglomerative coefficient
hc4$ac
rect.hclust(hc4, k = 8, border = 2:10)

clust4 <- cutree(hc4, k = 8)
fviz_cluster(list(data = banco_final_hc, cluster = clust4))

hc5 <- agnes(banco_final_hc, method = "ward")
pltree(hc5, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
rect.hclust(hc5, k = 8, border = 2:10)

clust5 <- cutree(hc5, k = 8)
fviz_cluster(list(data = banco_final_hc, cluster = clust5))

#################DIANA##############################
# compute divisive hierarchical clustering
hc6 <- diana(banco_final_hc[, -1])
# Divise coefficient
hc6$dc
# plot dendrogram
pltree(hc6, cex = 0.6, hang = -1, main = "Dendrogram of diana")
rect.hclust(hc5, k = 8, border = 2:10)

clust6 <- cutree(hc6, k = 8)
fviz_cluster(list(data = banco_final_hc, cluster = clust6))