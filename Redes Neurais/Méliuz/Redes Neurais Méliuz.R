install.packages("igraph")
library(igraph)
install.packages("writexl")
library(writexl)

# 2020

# Definindo y
y = c(CASH2020$Último, CASH2020$Máxima, CASH2020$Abertura, CASH2020$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(CASH2020)-1)) {
  de = which((y <= CASH2020$Máxima[i]) & (y >= CASH2020$Mínima[i]))
  para = which((y <= CASH2020$Máxima[i+1]) & (y >= CASH2020$Mínima[i+1]))
  
  a[de, para] = a[de, para] + 1
}

colnames(a) = y
rownames(a) = y

# Criando o grafo
g = graph_from_adjacency_matrix(a, mode="directed", weighted=TRUE)
g = as.undirected(g)

# Clustering com Louvain
cl = cluster_louvain(g, weights=E(g)$weight)

# Definindo as cores dos vértices conforme os clusters
colors = rainbow(max(cl$membership))
V(g)$color = colors[cl$membership]

# Plotando o grafo
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA)

# Adicionando legenda
par(xpd=TRUE)  # Permite desenhar fora da área do gráfico
legend(
  "bottom",  # Posiciona a legenda na parte inferior
  legend=paste("Cluster", 1:max(cl$membership)),
  col=colors,
  pch=16,
  title="Clusters",
  inset=-0.2,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.1  # Aumenta o tamanho da legenda
)

# Análise dos clusters
r = c()
for (j in 1:max(cl$membership)) {
  r = c(r, min(y[cl$membership == j]), max(y[cl$membership == j]), median(y[cl$membership == j]))
  r = c(r, sum(a[as.character(y[cl$membership == j]), as.character(y[cl$membership == j])]))
}

r = matrix(r, ncol=4, byrow=TRUE)
r = as.data.frame(r)
names(r) = c("min", "max", "med", "poder_atracao")

View(r)
write_xlsx(r, path = "clusters_Meliuz_2020.xlsx")


# 2021

# Definindo y
y = c(CASH2021$Último, CASH2021$Máxima, CASH2021$Abertura, CASH2021$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(CASH2021)-1)) {
  de = which((y <= CASH2021$Máxima[i]) & (y >= CASH2021$Mínima[i]))
  para = which((y <= CASH2021$Máxima[i+1]) & (y >= CASH2021$Mínima[i+1]))
  
  a[de, para] = a[de, para] + 1
}

colnames(a) = y
rownames(a) = y

# Criando o grafo
g = graph_from_adjacency_matrix(a, mode="directed", weighted=TRUE)
g = as.undirected(g)

# Clustering com Louvain
cl = cluster_louvain(g, weights=E(g)$weight)

# Definindo as cores dos vértices conforme os clusters
colors = rainbow(max(cl$membership))
V(g)$color = colors[cl$membership]

# Plotando o grafo
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA)

# Adicionando legenda
par(xpd=TRUE)  # Permite desenhar fora da área do gráfico
legend(
  "bottom",  # Posiciona a legenda na parte inferior
  legend=paste("Cluster", 1:max(cl$membership)),
  col=colors,
  pch=16,
  title="Clusters",
  inset=-0.2,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.1  # Aumenta o tamanho da legenda
)

# Análise dos clusters
r = c()
for (j in 1:max(cl$membership)) {
  r = c(r, min(y[cl$membership == j]), max(y[cl$membership == j]), median(y[cl$membership == j]))
  r = c(r, sum(a[as.character(y[cl$membership == j]), as.character(y[cl$membership == j])]))
}

r = matrix(r, ncol=4, byrow=TRUE)
r = as.data.frame(r)
names(r) = c("min", "max", "med", "poder_atracao")

View(r)
write_xlsx(r, path = "clusters_Meliuz_2021.xlsx")


# 2022

# Definindo y
y = c(CASH2022$Último, CASH2022$Máxima, CASH2022$Abertura, CASH2022$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(CASH2022)-1)) {
  de = which((y <= CASH2022$Máxima[i]) & (y >= CASH2022$Mínima[i]))
  para = which((y <= CASH2022$Máxima[i+1]) & (y >= CASH2022$Mínima[i+1]))
  
  a[de, para] = a[de, para] + 1
}

colnames(a) = y
rownames(a) = y

# Criando o grafo
g = graph_from_adjacency_matrix(a, mode="directed", weighted=TRUE)
g = as.undirected(g)

# Clustering com Louvain
cl = cluster_louvain(g, weights=E(g)$weight)

# Definindo as cores dos vértices conforme os clusters
colors = rainbow(max(cl$membership))
V(g)$color = colors[cl$membership]

# Plotando o grafo
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA)

# Adicionando legenda
par(xpd=TRUE)  # Permite desenhar fora da área do gráfico
legend(
  "bottom",  # Posiciona a legenda na parte inferior
  legend=paste("Cluster", 1:max(cl$membership)),
  col=colors,
  pch=16,
  title="Clusters",
  inset=-0.2,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.1  # Aumenta o tamanho da legenda
)

# Análise dos clusters
r = c()
for (j in 1:max(cl$membership)) {
  r = c(r, min(y[cl$membership == j]), max(y[cl$membership == j]), median(y[cl$membership == j]))
  r = c(r, sum(a[as.character(y[cl$membership == j]), as.character(y[cl$membership == j])]))
}

r = matrix(r, ncol=4, byrow=TRUE)
r = as.data.frame(r)
names(r) = c("min", "max", "med", "poder_atracao")

View(r)
write_xlsx(r, path = "clusters_Meliuz_2022.xlsx")


# 2023

# Definindo y
y = c(CASH2023$Último, CASH2023$Máxima, CASH2023$Abertura, CASH2023$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(CASH2023)-1)) {
  de = which((y <= CASH2023$Máxima[i]) & (y >= CASH2023$Mínima[i]))
  para = which((y <= CASH2023$Máxima[i+1]) & (y >= CASH2023$Mínima[i+1]))
  
  a[de, para] = a[de, para] + 1
}

colnames(a) = y
rownames(a) = y

# Criando o grafo
g = graph_from_adjacency_matrix(a, mode="directed", weighted=TRUE)
g = as.undirected(g)

# Clustering com Louvain
cl = cluster_louvain(g, weights=E(g)$weight)

# Definindo as cores dos vértices conforme os clusters
colors = rainbow(max(cl$membership))
V(g)$color = colors[cl$membership]

# Plotando o grafo
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA)

# Adicionando legenda
par(xpd=TRUE)  # Permite desenhar fora da área do gráfico
legend(
  "bottom",  # Posiciona a legenda na parte inferior
  legend=paste("Cluster", 1:max(cl$membership)),
  col=colors,
  pch=16,
  title="Clusters",
  inset=-0.2,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.1  # Aumenta o tamanho da legenda
)

# Análise dos clusters
r = c()
for (j in 1:max(cl$membership)) {
  r = c(r, min(y[cl$membership == j]), max(y[cl$membership == j]), median(y[cl$membership == j]))
  r = c(r, sum(a[as.character(y[cl$membership == j]), as.character(y[cl$membership == j])]))
}

r = matrix(r, ncol=4, byrow=TRUE)
r = as.data.frame(r)
names(r) = c("min", "max", "med", "poder_atracao")

View(r)
write_xlsx(r, path = "clusters_Meliuz_2023.xlsx")