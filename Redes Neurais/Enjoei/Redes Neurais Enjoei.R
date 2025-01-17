install.packages("igraph")
library(igraph)

# 2020

# Definindo y
y = c(ENJU2020$Último, ENJU2020$Máxima, ENJU2020$Abertura, ENJU2020$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(ENJU2020)-1)) {
  de = which((y <= ENJU2020$Máxima[i]) & (y >= ENJU2020$Mínima[i]))
  para = which((y <= ENJU2020$Máxima[i+1]) & (y >= ENJU2020$Mínima[i+1]))
  
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
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA, main="Rede Neural Enjoei 2020")

# Adicionando legenda
legend(x=1.1, y=0.5, legend=paste("Cluster", 1:max(cl$membership)), col=colors, pch=16, title="Clusters", xpd=TRUE)

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


# 2021

# Definindo y
y = c(ENJU2021$Último, ENJU2021$Máxima, ENJU2021$Abertura, ENJU2021$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(ENJU2021)-1)) {
  de = which((y <= ENJU2021$Máxima[i]) & (y >= ENJU2021$Mínima[i]))
  para = which((y <= ENJU2021$Máxima[i+1]) & (y >= ENJU2021$Mínima[i+1]))
  
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
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA, main="Rede Neural Enjoei 2021")

# Adicionando legenda
legend(x=1.1, y=0.5, legend=paste("Cluster", 1:max(cl$membership)), col=colors, pch=16, title="Clusters", xpd=TRUE)

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


# 2022

# Definindo y
y = c(ENJU2022$Último, ENJU2022$Máxima, ENJU2022$Abertura, ENJU2022$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(ENJU2022)-1)) {
  de = which((y <= ENJU2022$Máxima[i]) & (y >= ENJU2022$Mínima[i]))
  para = which((y <= ENJU2022$Máxima[i+1]) & (y >= ENJU2022$Mínima[i+1]))
  
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
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA, main="Rede Neural Enjoei 2022")

# Adicionando legenda
legend(x=1.1, y=0.5, legend=paste("Cluster", 1:max(cl$membership)), col=colors, pch=16, title="Clusters", xpd=TRUE)

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


# 2023

# Definindo y
y = c(ENJU2023$Último, ENJU2023$Máxima, ENJU2023$Abertura, ENJU2023$Mínima)
y = intersect(y, y)

# Inicializando a matriz
a = matrix(0, ncol=length(y), nrow=length(y))

# Preenchendo a matriz
for(i in 1:(nrow(ENJU2023)-1)) {
  de = which((y <= ENJU2023$Máxima[i]) & (y >= ENJU2023$Mínima[i]))
  para = which((y <= ENJU2023$Máxima[i+1]) & (y >= ENJU2023$Mínima[i+1]))
  
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
plot(g, vertex.color=V(g)$color, vertex.size=5, vertex.label=NA, main="Rede Neural Enjoei 2023")

# Adicionando legenda
legend(x=1.1, y=0.5, legend=paste("Cluster", 1:max(cl$membership)), col=colors, pch=16, title="Clusters", xpd=TRUE)

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

