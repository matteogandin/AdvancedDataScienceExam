# Import packages
library(dplyr)
library(tidygraph)
# library(networkD3)
library(ggplot2)
library(ggraph)
library(igraph)
library(mapview)
library(RColorBrewer)

# IMPORT DATASET
stazioni <- read.csv("Dataset/london_transport_datasets_London_stations.csv", sep = "|")
View(stazioni)

stazioni <- stazioni %>%
  mutate(Zone = as.character(Zone))

linee = read.csv("Dataset/london_transport_datasets_London_tube_lines.csv", sep = "|")
# linee <- linee %>%
#   select(From_Station_Id, To_Station_Id)

# linee <- linee %>%
#   mutate(link_width = 1)

names(linee)
linee <- linee %>%
  select(From_Station_Id, To_Station_Id, From_Station, To_Station, Tube_Line)
View(linee)


# INIZIO ANALISI SUL GRAFO

# GRAFO GENERALE
graph <- graph_from_data_frame(linee, directed = T, vertices = stazioni)
lo <- layout_nicely(graph)
V(graph)$color = "black"
V(graph)$size = 3
V(graph)$label = NA
E(graph)$arrow.size = 0.1
plot(graph, layout = lo)
graph

# MAPPA DELLE STAZIONI
stazioni_from_graph = as_data_frame(graph, what = c("vertices"))

View(stazioni_from_graph)
mapview(stazioni_from_graph, xcol = "Longitude", ycol = "Latitude", crs = 4269, 
        label = stazioni$Zone, zcol="Zone", seq = stazioni$Zone)

# Numero di nodi del grafo -> 653 nodi
vcount(graph)
# Numero di archi del grafo -> 976 archi
ecount(graph)

vertex_attr(graph)
edge_attr(graph)
# Quante e quali sono le zone e quanti nodi (stazioni) ci sono per ogni zona
stazioni %>% distinct(Zone)  # Ci sono 9 zone numerate da 1 a 9

# Zone        tot_stazioni
# <chr>           <int>
# 1 1               73
# 2 2              143
# 3 3              157
# 4 4              110
# 5 5               68
# 6 6               73
# 7 7               12
# 8 8                4
# 9 9               13
stazioni %>%
  group_by(Zone) %>%
  summarise(tot_stazioni = n())

# Grafo con suddivisione per zona
tg <- graph %>% as_tbl_graph()
tg
numero_zone = length(unique(V(graph)$Zone))
numero_zone

cols_f <- colorRampPalette(RColorBrewer::brewer.pal(9, 'Spectral'))
colori_per_zona = cols_f(numero_zone)
colori_per_zona
ggraph(tg, layout = lo) +
  geom_edge_link(
    colour = "Grey",
    end_cap = circle(2.5, 'mm'),
    arrow = arrow(length =
                    unit(1,
                         'mm'),
                  type = 'open'),
    
    # strength = 0.1,
    show.legend = F
  ) +
  geom_node_point(size = 3, aes(colour = Zone)) +
  scale_colour_manual(values = colori_per_zona) +
  theme_graph(background = "White") +
  labs(color = "Zone")


# 2a. Controllo se la rete è connessa
is_connected(graph)  # La rete NON è connessa

# 2b. Ottengo le componenti connesse della rete
componenti = components(graph)
componenti # Ci sono 4 componenti connesse di cui 3 formate solo da un nodo quindi ci sono delle stazioni non raggiungibili

# Seleziono i nodi che appartengono alla componente gigante
nodi_GC = which(componenti$membership == which.max(componenti$csize))
nodi_GC

# Coloro di rosso i nodi della componente gigante
V(graph)[nodi_GC]$color = "red"
plot(graph, layout = lo)

# 653:100 = 392:x

# 2d. Componenti fortemente connesse
scc = components(graph, mode = "strong")
scc # La SCC di dimensione maggiore conta 392 nodi (392/653 = 60%). Tutte le altre sono SCC di dimensione 1
nodi_SCC = which(scc$membership == which.max(scc$csize))
nodi_SCC

V(graph)$color = "white"
V(graph)[nodi_SCC]$color = "red"
plot(graph, layout = lo)

# 2e. Percolazione
percolate = function(g, size, d){
  giant = vector()
  
  c = components(g)
  giant[1] = max(c$csize)
  
  names(d) = 1:length(d)
  d = sort(d, decreasing=TRUE)
  vital = as.integer(names(d[1:size]))
  
  for (i in 1:size) {
    c = components(delete_vertices(g, vital[1:i]))
    giant[i+1] = max(c$csize)
  }
  
  return(giant)
}

size = vcount(graph)/2

rand = percolate(graph, size, sample(x = vcount(graph), size = size))
deg = percolate(graph, size, degree(graph))
bet = percolate(graph, size, d = betweenness(graph))
plot(0:size, deg, type = "l", col=1, 
     xlab="Numero di nodi rimossi", 
     ylab="Dimensione della componente gigante")
lines(0:size, bet, col=2)
lines(0:size, rand, col=3)
lines(0:size, rep(vcount(graph)/2, size+1), lty=2)
legend(x = "topright", 
       legend = c("grado", "betweenness", "casuale"), lty = 1, col = 1:3)

# Analizzando il grafico la dimensione della componente gigante diminuisce rapidamente quindi il modello della rete
# è ad attaccamento preferenziale


# Distanza media
# distanze <- distances(graph)
# distanze
# distanze[is.infinite(distanze)] <- NA # Metto NA per i nodi non connessi
# View(distanze)
# 
# dm = mean(distanze, na.rm = T)
# dm
# 
# tabella_distanze = as.data.frame(table(distanze))
# tabella_distanze$distanze <- as.numeric(tabella_distanze$distanze)
# tabella_distanze <- tabella_distanze[tabella_distanze$distanze > 0, ]  #elimina nodi distanza 0 (se stessi)
# tabella_distanze
# 
# vline_df <- data.frame(xintercept = dm, label = "Distanza media")
# ggplot(tabella_distanze, aes(x = distanze, y = Freq)) +
#   geom_bar(stat = "identity", fill = "dodgerblue") +
#   # geom_vline(xintercept = dm, color = "red", size = 1.2) +
#   geom_vline(data = vline_df, aes(xintercept = xintercept, color = label), size = 1.2, linetype = "dashed") +
#   scale_y_continuous(n.breaks = 10) +
#   scale_x_continuous(n.breaks = 20) +
#   xlab("Distanza") +
#   ylab("Frequenza") + 
#   ggtitle("Frequenza dei cammini minimi") +
#   scale_color_manual(name = "", values = c("Distanza media" = "red"))+
#   theme_minimal()
# mean_distance(graph)
# 
# tab_dist = distance_table(graph, directed = T)
# 
# paths = tab_dist$res
# names(paths) = 1:length(paths)
# barplot(paths, 
#         xlab="Distanza", 
#         ylab="Frequenza",
#         col = "dodgerblue",
#         main = "Frequenza dei cammini minimi"
# )
# abline(v = mean_distance(graph), col = "red", lwd = 4, lty = 2)
# legend("topright",
#        legend = "Distanza media",
#        col = "red",
#        lty = 2,
#        lwd = 2) 
distanza_media = mean_distance(graph)

tab_dist = distance_table(graph, directed = T)

paths = tab_dist$res
names(paths) = 1:length(paths)

grafico <- barplot(paths, 
                   xlab="Distanza", 
                   ylab="Frequenza",
                   col = "dodgerblue",
                   main = "Frequenza dei cammini minimi"
)
posizione_media <- sum(grafico * paths) / sum(paths)
abline(v = posizione_media, col = "red", lwd = 4, lty = 2)
legend("topright",
       legend = "Distanza media",
       col = "red",
       lty = 2,
       lwd = 2)

# Geodetica più lunga (diametro)
# diameter(graph)  # Il cammino minimo maggiore è lungo 54
# 
# # Mostro il diametro nel grafo
# diametro = get_diameter(graph)
# diametro
# V(graph)$color = "white"
# E(graph)$color = "grey"
# E(graph, path=diametro)$color = "red"
# V(graph)[diametro]$color = "red"
# V(graph)[diametro[[1]]]$color = "blue"
# V(graph)[diametro[[length(diametro)]]]$color = "green"
# plot(graph, layout = lo)
# legend(x = "topright", 
#             legend = c("nodo iniziale", "nodo finale"), pch = 16,col = c("blue", "green"))


diameter(graph, directed = T) # Il cammino minimo maggiore è lungo 54

# Mostro il diametro nel grafo
diametro = get_diameter(graph)

V(graph)$color = "white"
E(graph)$color = "grey"
E(graph, path=diametro)$color = "red"
V(graph)[diametro]$color = "red"
V(graph)[diametro[[1]]]$color = "blue"
V(graph)[diametro[[length(diametro)]]]$color = "green"
plot(graph, layout = lo)
legend(x = "topright", 
       legend = c("nodo iniziale", "nodo finale"), pch = 16,col = c("blue", "green"))


stazioni %>%
  filter(Id == diametro[[1]]$name)

stazioni %>%
  filter(Id == diametro[[length(diametro)]]$name)

# FACCIO LA MAPPA DI TUTTE LE STAZIONI ATTRAVERSATE DALLA GEODETICA
stazioni_diametro = stazioni %>%
  filter(Id %in% diametro$name)

mapview(stazioni_diametro, xcol = "Longitude", ycol = "Latitude", crs = 4269)



# GRADI DEI NODI DELLA RETE
V(graph)$degree <- degree(graph)
V(graph)$in_degree <- degree(graph, mode = "in")
V(graph)$out_degree <- degree(graph, mode = "out")

grado_massimo = max(V(graph)$degree)
grado_massimo
grado_medio = mean(V(graph)$degree)
grado_medio

dt = as_data_frame(graph, what = "vertices")

dt %>%
  filter(degree == grado_massimo)

max(V(graph)$in_degree)
dt %>%
  filter(in_degree == max(V(graph)$in_degree))

# La stazione 527 è quella con maggior numero di archi in entrata.
# staz_da_cercare contiene le stazioni i cui archi entrano nella stazione 527
staz_da_cercare <- linee %>%
  filter(To_Station_Id == 527) %>%
  select(From_Station_Id)
staz_da_cercare

# Nella stazioni 527 entrano archi da stazioni che appartengono alla stessa zona o a zone più centrali (zona 1 e 2)
inner_join(stazioni, staz_da_cercare, by = c("Id" = "From_Station_Id"))


# Vediamo in quali zone vanno gli archi uscenti dalla stazioni 527
# staz_da_cercare contiene le stazioni che hanno un arco in entrata proveniente dal nodo 527
staz_da_cercare <- linee %>%
  filter(From_Station_Id == 527) %>%
  select(To_Station_Id)
staz_da_cercare

# Dalla stazione 527 escono due archi verso una zona più periferica (zona 4), 3 archi vanno verso una zona più centrale (zona 2) e i restanti vanno verso stazioni appartenenti alla stessa zona
inner_join(stazioni, staz_da_cercare, by = c("Id" = "To_Station_Id"))

distribuzione_gradi = degree_distribution(graph, cumulative = FALSE)
plot <- barplot(distribuzione_gradi,
                xlab="Grado", 
                ylab="Frequenza",
                col = "dodgerblue",
                main = "Distribuzione dei gradi",
                names.arg = 0:grado_massimo,
                cex.names = 0.8
)

# DISTRIBUZIONE DEI GRADI PER ZONA
ggplot(
  data = dt %>%
    group_by(Zone) %>%
    summarise(degree_medio = (mean(degree))),
  aes(x = Zone, y = degree_medio)
) +
  geom_bar(stat = "Identity", fill = "dodgerblue") +
  geom_text(aes(label = round(degree_medio, 2)), vjust = -0.5) +
  xlab("Zone") +
  ylab("Degree medio") + 
  ggtitle("Degree medio per zona")

ggplot(
  data = dt %>%
    group_by(Zone) %>%
    summarise(degree_medio = (mean(degree)/vcount(graph)) * n()),
  aes(x = Zone, y = degree_medio)
) +
  geom_bar(stat = "Identity", fill = "dodgerblue") +
  geom_text(aes(label = round(degree_medio, 2)), vjust = -0.5) +
  xlab("Zone") +
  ylab("Degree medio") + 
  ggtitle("Degree medio per zona rispetto al numero di nodi della zona")

mapview(dt, xcol = "Longitude", ycol = "Latitude", crs = 4269, 
        label = stazioni$Zone, zcol="Zone", seq = stazioni$Zone, cex = "degree")




## RILEVAMENTO DI COMUNITÀ
# c = cluster_leading_eigen(graph)
# modularity(c)
# 
# length(c)
# 
# sizes(c)
# 
# membership(c)
# 
# crossing(c, graph)
# 
# plot(c, graph, layout = lo)
# 
# plot(graph, vertex.color = membership(c), layout = lo)





