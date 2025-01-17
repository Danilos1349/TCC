install.packages("igraph")
library(igraph)
install.packages("writexl")
library(writexl)

# 2020

# Definindo y
y = c(LWSA2020$Último, LWSA2020$Máxima, LWSA2020$Abertura, LWSA2020$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(LWSA2020)-1)) {
  de = which((y <= LWSA2020$Máxima[i]) & (y >= LWSA2020$Mínima[i]))
  para = which((y <= LWSA2020$Máxima[i+1]) & (y >= LWSA2020$Mínima[i+1]))
  
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
write_xlsx(r, path = "clusters_Locaweb_2020.xlsx")


# 2021

# Definindo y
y = c(LWSA2021$Último, LWSA2021$Máxima, LWSA2021$Abertura, LWSA2021$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(LWSA2021)-1)) {
  de = which((y <= LWSA2021$Máxima[i]) & (y >= LWSA2021$Mínima[i]))
  para = which((y <= LWSA2021$Máxima[i+1]) & (y >= LWSA2021$Mínima[i+1]))
  
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
write_xlsx(r, path = "clusters_Locaweb_2021.xlsx")

# 2022

# Definindo y
y = c(LWSA2022$Último, LWSA2022$Máxima, LWSA2022$Abertura, LWSA2022$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(LWSA2022)-1)) {
  de = which((y <= LWSA2022$Máxima[i]) & (y >= LWSA2022$Mínima[i]))
  para = which((y <= LWSA2022$Máxima[i+1]) & (y >= LWSA2022$Mínima[i+1]))
  
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
write_xlsx(r, path = "clusters_Locaweb_2022.xlsx")

# 2023

# Definindo y
y = c(LWSA2023$Último, LWSA2023$Máxima, LWSA2023$Abertura, LWSA2023$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(LWSA2023)-1)) {
  de = which((y <= LWSA2023$Máxima[i]) & (y >= LWSA2023$Mínima[i]))
  para = which((y <= LWSA2023$Máxima[i+1]) & (y >= LWSA2023$Mínima[i+1]))
  
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
write_xlsx(r, path = "clusters_Locaweb_2023.xlsx")