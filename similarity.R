###########################################
#Arquivo: similarity.R
#Autor: Lucas Bicalho
###########################################

data_acid = read.table("data/ACIDENTES.csv", header=T, sep=";")

df = data.frame(dia_semana=weekdays(strptime(data_acid$data, "%d/%m/%Y %H:%M")),
                tipo_acid=data_acid$tipo_acidente,
                mortos=data_acid$mortos,
                distrito=data_acid$distrito)


require(rpart)

mod = rpart(mortos ~ ., data=df,  control = rpart.control(cp = 1e-3))

print(summary(mod))
plot(mod)
text(mod)


ds = weekdays(strptime(data_acid$data, "%d/%m/%Y %H:%M"))
library(data.table)
period = ceiling(hour(strptime(data_acid$data, "%d/%m/%Y %H:%M")))
h = cut(period, breaks = c(0,6,12,18,24), labels = c("Dawn", "Morning", "Afternoon", "Night"))
f = data_acid$feridos

df = data.frame(ds_h = paste0(ds, "_", h, sep=""), feridos=f)

tab = table(df$ds_h, df$feridos)

mult_tab = t(t(tab) * as.numeric(colnames(tab)))

ruas = NULL
for (i in 1:100)
  ruas=rbind(ruas, data.frame(Rua=i, valores=t(rowSums(mult_tab) * runif(12, 1, 5))))
             
# Compute distances and hierarchical clustering
dd <- dist(scale(df), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")

res.dist <- dist(mult_tab, method = "euclidean")
as.matrix(res.dist)[1:6, 1:6]
res.hc <- hclust(d = res.dist, method = "ward.D2")

# cex: label size
library("factoextra")
fviz_dend(res.hc, cex = 0.5)

# Compute cophentic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and
# the original distance
cor(res.dist, res.coph)

res.hc2 <- hclust(res.dist, method = "average")

cor(res.dist, cophenetic(res.hc2))

# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
head(grp, n = 4)
table(grp)
rownames(df)[grp == 1]

fviz_dend(res.hc, cex = 0.5, k = 4, 
          color_labels_by_k = FALSE, rect = TRUE)

# K-means clustering
# +++++++++++++++++++++
km.res <- kmeans(mult_tab, 16052)

# Visualize kmeans clustering
# use repel = TRUE to avoid overplotting
fviz_cluster(km.res, iris[, -5], ellipse.type = "norm")