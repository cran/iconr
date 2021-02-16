## ---- include = FALSE---------------------------------------------------------
library(knitr)
library(igraph)
library(dplyr)
library(kableExtra)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">",
  fig.pos = 'H'
)
# ibahernando.path <- paste0(getwd(),"/img/ibahernando_256colours.png")
# brozas.path <- paste0(getwd(),"/img/brozas_256colours.png")
# solanas.path <- paste0(getwd(),"/img/solana_detail_256colours.png")
# solanas.vor.path <- paste0(getwd(),"/img/solana_voronoi_256colours.png")
# ibahernando.path <- "img/ibahernando_256colours.png"
# brozas.path <- "img/brozas_256colours.png"
# solanas.path <- "img/solana_detail_256colours.png"
# solanas.vor.path <- "img/solana_voronoi_256colours.png"
ibahernando.path <- "../man/figures/ibahernando_256colours.png"
brozas.path <- "../man/figures/brozas_256colours.png"
solanas.path <- "../man/figures/solana_detail_256colours.png"
solanas.vor.path <- "../man/figures/solana_voronoi_256colours.png"

## ----down,eval=FALSE, echo=TRUE-----------------------------------------------
#  devtools::install_github("zoometh/iconr", build_vignettes=TRUE)

## ----load, echo=TRUE----------------------------------------------------------
library(iconr)

## ----ls_ext_data--------------------------------------------------------------
dataDir <- system.file("extdata", package = "iconr")
input.files <- list.files(dataDir)
cat(input.files, sep="\n")

## ----paths.imgs, echo=TRUE----------------------------------------------------
imgs_path <- paste0(dataDir, "/imgs.csv")
imgs <- read.table(imgs_path, sep=";", stringsAsFactors = FALSE)

## ----paths, echo=TRUE---------------------------------------------------------
nodes_path <- paste0(dataDir, "/nodes.shp")
nodes.shp <- rgdal::readOGR(dsn = nodes_path, verbose = FALSE)
nodes <- as.data.frame(nodes.shp)
edges_path <- paste0(dataDir, "/edges.shp")
edges.shp <- rgdal::readOGR(dsn = edges_path, verbose = FALSE)
edges <- as.data.frame(edges.shp)

## ----paths.1, echo=TRUE-------------------------------------------------------
nodes_path <- paste0(dataDir, "/nodes.tsv")
nodes <- read.table(nodes_path, sep="\t", stringsAsFactors = FALSE)
edges_path <- paste0(dataDir, "/edges.tsv")
edges <- read.table(edges_path, sep="\t", stringsAsFactors = FALSE)

## ----graph.clss---------------------------------------------------------------
lgrph <- list_dec(imgs, nodes, edges)
g <- lgrph[[1]]
as.character(class(g))

## ----igraph.1, warning=FALSE, fig.align="center", fig.width=6.5, fig.asp=0.58----
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))           
par(mar=c(1, 0, 2, 0), mfrow=c(1, 2), cex.main = 0.9, font.main = 1)
coords <- layout.fruchterman.reingold(lgrph[[1]])
plot(g,
     vertex.size = 15,
     vertex.frame.color="white",
     vertex.label.family = "sans",
     vertex.label.cex = .8,
     main = "Graph drawing based on x, y coordinates"
)
plot(g,
     layout = layout.fruchterman.reingold(g),
     vertex.size = 5 + degree(g)*10,
     vertex.frame.color="white",
     vertex.label.family = "sans",
     vertex.label.cex = .8,
     main = "Force-directed graph drawing,\nwith degree-dependent node size."
)
mtext(g$decor, cex = 1, side = 1, line = -1, outer = TRUE)

## ----imgs,fig.width=6, fig.height=6, fig.align="center",warning=FALSE, fig.cap="\\label{fig:figs}imgs.tsv"----
imgs_path <- paste0(dataDir, "/imgs.tsv")
imgs <- read.table(imgs_path, sep="\t", stringsAsFactors = FALSE)
knitr::kable(imgs, "html") %>% 
  kableExtra::kable_styling(full_width = FALSE, position = "center", font_size=12)

## ----xy_coords,out.width="50%", fig.align="center",echo=FALSE,warning=FALSE----
df.equi <- data.frame(
  "Device/Package" = c("*R graphics*", "*R raster*", "*R magick*", "***GIS interface***"),
  "Unit of measure" = c("number of pixels", "number of pixels", "number of pixels", "**number of pixels**"),
  "Origin" = c("bottom-left corner", "top-left corner", "top-left corner", "**top-left corner**"),
  "x-axis orientation" = c("rightward", "downward", "rightward", "**rightward**"),
  "y-axis orientation" = c("upward", "rightward", "downward", "**upward**"),
  check.names = FALSE)
knitr::kable(df.equi) %>%
  kable_styling(full_width = F)

## ----drawing, out.width="50%", fig.width=6, fig.asp=750/666, fig.align="center", warning=FALSE, echo=TRUE, message=FALSE, fig.cap="\\label{fig:figs} `iconr` (GIS) coordinate convention: decoration `Cerro_Muriano.Cerro_Muriano_1.jpg` with the coordinates of its corners."----
library(magick)
library(graphics)
dataDir <- system.file("extdata", package = "iconr")
imgs_path <- paste0(dataDir, "/imgs.csv")
imgs <- read.table(imgs_path, sep=";", stringsAsFactors = FALSE)
cm1 <- image_read(paste0(dataDir, "/", imgs$img[1]))
W <- image_info(cm1)$width
H <- image_info(cm1)$height
oldpar <- par(no.readonly = TRUE)   
on.exit(par(oldpar))            
par(mar = c(0, 0, 0, 0))
plot(cm1)
box(lwd = 2)
text(0, H, paste0(0, ",", 0), cex = 2, adj = c(0, 1.1))
text(W, H, paste0(W, ",", 0), cex = 2, adj = c(1, 1.1))
text(0, 0, paste0(0, ",", -H), cex = 2, adj = c(0, -0.2))
text(W, 0, paste0(W, ",", -H), cex = 2, adj = c(1, -0.2))

## ----nodes.df, warning=FALSE,fig.align="center",warning=FALSE-----------------
nds.df <- read_nds(site = "Cerro Muriano", decor = "Cerro Muriano 1", dir = dataDir) 
knitr::kable(nds.df, "html") %>% 
  kableExtra::kable_styling(full_width = FALSE, position = "center", font_size=12)

## ----edges.df, warning=FALSE--------------------------------------------------
edges <- read.table(edges_path, sep = "\t", stringsAsFactors = FALSE)
knitr::kable(head(edges), "html", align = "llccc") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center",
                            font_size=12) %>%
  gsub("\\+", "$+$", .)

## ----edges.df.1, warning=FALSE------------------------------------------------
eds.df <- read_eds(site = "Cerro Muriano", decor = "Cerro Muriano 1", dir = dataDir) 
knitr::kable(head(eds.df), "html", align = "llcccrrrr") %>% 
  kableExtra::kable_styling(full_width = FALSE, position = "center", font_size=12) %>%
  gsub("\\+", "$+$", .)

## ----count4, warning=FALSE----------------------------------------------------
named_elements(lgrph[[1]], focus = "edges", nd.var="type")[1]      

## ----count1, warning=FALSE----------------------------------------------------
named_elements(lgrph[[4]], focus = "edges", nd.var="type")

## ----count.all, warning=FALSE-------------------------------------------------
all.edges <- unlist(lapply(lgrph, named_elements, 
                           focus = "edges", nd.var="type", disamb.marker=""))
edges.count <- as.data.frame(table(all.edges))
edges.order <- order(edges.count$Freq, decreasing = TRUE)
edges.count <- edges.count[edges.order, ] 
knitr::kable(head(edges.count), row.names = FALSE) %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center", font_size=12)

## ----graph.attribute.plot.type, out.width="60%", fig.width=6, fig.asp=750/666, fig.align="center", warning=FALSE, fig.cap="Zarza De Montanchez stelae (decoration 4) showing *normal* and *attribute*  edges"----
site <- "Zarza de Montanchez" 
decor <- "Zarza De Montanchez"
nds.df <- read_nds(site, decor, dataDir)
eds.df <- read_eds(site, decor, dataDir)
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              ed.lwd = 1, ed.color = c("darkorange"),
              lbl.size = 0.7)

## ----count_att, warning=FALSE-------------------------------------------------
sort(named_elements(lgrph[[4]], focus = "edges", nd.var = "type"))

## ----graph.overlap.plot.type, out.width="100%", fig.width=12, fig.asp=0.55, fig.align="center", warning=FALSE, fig.cap="Ibahernando stelae (decoration 5) showing *diachronic* and *normal* edges"----
site <- "Ibahernando"
decor <- "Ibahernando"
nds.df <- read_nds(site, decor, dataDir)
eds.df <- read_eds(site, decor, dataDir)
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))          
par(mfrow = c(1, 2))
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              lbl.size = 0.7)
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              nd.var = 'type',
              lbl.size = 0.6)

## ----count3, warning=FALSE----------------------------------------------------
named_elements(lgrph[[5]], focus = "edges", nd.var = "type")

## ----ls_functions-------------------------------------------------------------
cat(ls("package:iconr"), sep="\n")

## ----img.graph.read, echo=TRUE------------------------------------------------
site <- "Cerro Muriano"
decor <- "Cerro Muriano 1"
nds.df <- read_nds(site, decor, dataDir)
eds.df <- read_eds(site, decor, dataDir)

## ----img.graph.plot.type, out.width="60%", fig.width=6, fig.asp=750/666, fig.align="center", warning=FALSE, fig.cap="Cerro Muriano 1 stelae (decoration 1) with the type of each GU"----
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              nd.var = 'type',
              lbl.size = 0.55)

## ----img.graph.plot.id, out.width="60%", fig.width=6, fig.asp=750/666, fig.align="center", warning=FALSE, fig.cap="Cerro Muriano 1 stelae (decoration 1) with the maximum length (in cm) of each GU"----
nds.df$long_cm <- paste0(c(47, 9, 47, 18, 7, 3, 13), "cm")
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              nd.var = 'long_cm',
              nd.color = "brown",
              lbl.color = "brown", lbl.size = 0.7,
              ed.color = "brown")

## ----compare.nodes, results='asis', warning=FALSE-----------------------------
imgs_path <- paste0(dataDir, "/imgs.tsv")
nodes_path <- paste0(dataDir, "/nodes.tsv")
edges_path <- paste0(dataDir, "/edges.tsv")
imgs <- read.table(imgs_path, sep="\t", stringsAsFactors = FALSE)
nodes <- read.table(nodes_path, sep="\t", stringsAsFactors = FALSE)
edges <- read.table(edges_path, sep="\t", stringsAsFactors = FALSE)
lgrph <- list_dec(imgs, nodes, edges)
df.same_nodes <- same_elements(lgrph,
                               focus = "nodes",
                               nd.var = "type")
diag(df.same_nodes) <- cell_spec(diag(df.same_nodes),
                                 font_size = 9)
knitr::kable(df.same_nodes, row.names = TRUE, escape = FALSE, table.attr = "style='width:30%;'",
             caption = "Count of common nodes between decorations") %>%
  column_spec(1, bold=TRUE) %>%
  kableExtra::kable_styling(position = "center", font_size = 12)

## ----compare.2.nodes, fig.show = TRUE, out.width="100%", fig.width=12, fig.asp=0.52, fig.align="center", warning=FALSE----
dec.to.compare <- c(2, 3, 4)
g.compar <- list_compar(lgrph, nd.var = "type")
plot_compar(listg = g.compar, 
            dec2comp = dec.to.compare,
            focus = "nodes",
            nd.size = c(0.5, 1.5),
            dir = dataDir) 

## ----compare.edges, warning=FALSE---------------------------------------------
df.same_edges <- same_elements(lgrph, nd.var = "type", focus = "edges")
diag(df.same_edges) <- cell_spec(diag(df.same_edges),
                                 font_size = 9)
knitr::kable(df.same_edges, row.names = TRUE, escape = F, table.attr = "style='width:30%;'",
             caption = "Count of common edges between decorations") %>%
  column_spec(1, bold=TRUE) %>%
  kableExtra::kable_styling(position = "center", font_size = 12)

## ----compare.2.edges, out.width="100%", fig.width=12, fig.asp=0.52, fig.align="center", warning=FALSE----
dec.to.compare <- c(2, 3, 4)
g.compar <- list_compar(lgrph, nd.var = "type")
plot_compar(listg = g.compar, 
            dec2comp = dec.to.compare,
            focus = "edges",
            nd.size = c(0.5, 1.7),
            dir = dataDir)

## ----ibahernando, out.width="60%", fig.width=6, fig.asp=750/666, fig.align="center", warning=FALSE, fig.cap="Ibahernando stelae (decoration 5) with *diachronic* and *normal* edges, node 1 overlaps node 2 and node 3"----
site <- "Ibahernando" 
decor <- "Ibahernando"
nds.df <- read_nds(site, decor, dataDir)
eds.df <- read_eds(site, decor, dataDir)
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              lbl.size = 0.7)

## ----rm.writing, out.width="100%", fig.width=12, fig.asp=0.55, fig.align="center", warning=FALSE, fig.cap="Ibahernando stelae before and after the selection of node 4 (sword) graph component"----
site <- decor <- "Ibahernando"
selected.nd <- 4
nds.df <- read_nds(site, decor, dataDir)
eds.df <- read_eds(site, decor, dataDir)
l_dec_df <- contemp_nds(nds.df, eds.df, selected.nd)
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))
par(mfrow=c(1, 2))
plot_dec_grph(nds.df, eds.df, imgs,
              site, decor, dataDir,
              nd.var = "type",
              lbl.color = "brown", lbl.size = 0.6)
plot_dec_grph(l_dec_df$nodes, l_dec_df$edges, imgs,
              site, decor, dataDir,
              nd.var = "type",
              lbl.color = "brown", lbl.size = 0.6)

## ----ibahernando.lat, out.width="60%", fig.width=6, fig.asp=750/666, fig.align="center", warning=FALSE, fig.cap="Ibahernando stelae after the selection of node 1 (writing) graph component"----
selected.nd <- 1
nds.df <- read_nds(site, decor, dir = dataDir)
eds.df <- read_eds(site, decor, dir = dataDir)
l_dec_df <- contemp_nds(nds.df, eds.df, selected.nd)
plot_dec_grph(l_dec_df$nodes, l_dec_df$edges, imgs,
              site, decor, dir = dataDir,
              nd.var = "type",
              lbl.size = 0.6, lbl.color = "brown")

## ----clust.comp, warning=FALSE, fig.align="center", fig.width=7, fig.height=5----
oldpar <- par(no.readonly = TRUE)
on.exit(par(oldpar))           
par(mfrow=c(1, 2))
df.same_edges <- same_elements(lgrph, "type", "edges")
df.same_nodes<- same_elements(lgrph, "type", "nodes")
dist.nodes <- dist(as.matrix(df.same_nodes), method = "euclidean")
dist.edges <- dist(as.matrix(df.same_edges), method = "euclidean")
hc.nds <- hclust(dist.nodes, method = "ward.D")
hc.eds <- hclust(dist.edges, method = "ward.D") 
plot(hc.nds, main = "Common nodes", cex = .8)
plot(hc.eds, main = "Common edges", cex = .8)

## ----hclust.compar, warning=FALSE, fig.align="center", fig.width=7------------
suppressPackageStartupMessages(library(dendextend))
suppressPackageStartupMessages(library(dplyr))
oldpar <- par(no.readonly = TRUE)   
on.exit(par(oldpar))           
par(mfrow=c(1, 2))
dend.nds <- as.dendrogram(hc.nds)
dend.eds <- as.dendrogram(hc.eds)
dendlist(dend.nds, dend.eds) %>%
  untangle(method = "step1side") %>% 
  tanglegram(columns_width = c(6, 1, 6),
             main_left = "Common nodes",
             main_right = "Common edges",
             lab.cex = 1.3,
             cex_main = 1.5,
             highlight_branches_lwd = F) 

## ----compare.c.edges, out.width="100%", fig.width=12, fig.asp=0.52, fig.align="center", warning=FALSE----
dec.to.compare <- c(3, 5)
g.compar <- list_compar(lgrph, nd.var = "type")
plot_compar(listg = g.compar, 
            dec2comp = dec.to.compare,
            focus = "nodes",
            nd.size = c(0.5, 1.7),
            dir = dataDir)
plot_compar(listg = g.compar, 
            dec2comp = dec.to.compare,
            focus = "edges",
            nd.size = c(0.5, 1.7),
            dir = dataDir)

