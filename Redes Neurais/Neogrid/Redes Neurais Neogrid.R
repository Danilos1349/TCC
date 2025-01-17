install.packages("igraph")
library(igraph)
install.packages("writexl")
library(writexl)

# 2020

# Definindo y
y = c(NGRD2020$Último, NGRD2020$Máxima, NGRD2020$Abertura, NGRD2020$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(NGRD2020)-1)) {
  de = which((y <= NGRD2020$Máxima[i]) & (y >= NGRD2020$Mínima[i]))
  para = which((y <= NGRD2020$Máxima[i+1]) & (y >= NGRD2020$Mínima[i+1]))
  
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

# Adicionando legenda abaixo do gráfico
par(xpd=TRUE)  # Permite desenhar fora da área do gráfico
legend(
  "bottom",  # Posiciona a legenda na parte inferior
  legend=paste("Cluster", 1:max(cl$membership)),
  col=colors,
  pch=16,
  title="Clusters",
  inset=-0.1,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.3  # Aumenta o tamanho da legenda
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
write_xlsx(r, path = "clusters_Neogrid_2020.xlsx")


# 2021

# Definindo y
y = c(NGRD2021$Último, NGRD2021$Máxima, NGRD2021$Abertura, NGRD2021$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(NGRD2021)-1)) {
  de = which((y <= NGRD2021$Máxima[i]) & (y >= NGRD2021$Mínima[i]))
  para = which((y <= NGRD2021$Máxima[i+1]) & (y >= NGRD2021$Mínima[i+1]))
  
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
  inset=-0.1,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.3  # Aumenta o tamanho da legenda
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
write_xlsx(r, path = "clusters_Neogrid_2021.xlsx")


# 2022

# Definindo y
y = c(NGRD2022$Último, NGRD2022$Máxima, NGRD2022$Abertura, NGRD2022$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(NGRD2022)-1)) {
  de = which((y <= NGRD2022$Máxima[i]) & (y >= NGRD2022$Mínima[i]))
  para = which((y <= NGRD2022$Máxima[i+1]) & (y >= NGRD2022$Mínima[i+1]))
  
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
  inset=-0.15,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.3  # Aumenta o tamanho da legenda
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
write_xlsx(r, path = "clusters_Neogrid_2022.xlsx")


# 2023

# Definindo y
y = c(NGRD2023$Último, NGRD2023$Máxima, NGRD2023$Abertura, NGRD2023$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(NGRD2023)-1)) {
  de = which((y <= NGRD2023$Máxima[i]) & (y >= NGRD2023$Mínima[i]))
  para = which((y <= NGRD2023$Máxima[i+1]) & (y >= NGRD2023$Mínima[i+1]))
  
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
  inset=-0.15,  # Ajusta a distância da legenda em relação ao gráfico
  cex=1.3  # Aumenta o tamanho da legenda
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
write_xlsx(r, path = "clusters_Neogrid_2023.xlsx")