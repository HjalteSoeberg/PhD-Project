### day1.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Jun 22 2026 (12:39) 
## Version: 
## Last-Updated: Jun 22 2026 (13:45) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(data.table)
library(ggplot2)
load("~/Downloads/species.RData")
df <- setDT(model.data)
plot(pres~prcp, data = df)

m1 <- glm(pres ~ prcp, data = df, family = binomial(link = "logit"))
summary(m1)


m2 <- glm(pres ~ prcp+I(prcp^2), data = df, family = binomial(link = "logit"))
summary(m2)

plot(pres~prcp, data = df)
lines(x = seq(1,10,length = 100), y = predict(m1, newdata = data.frame(prcp = seq(1,10,length = 100)),type = "response"), col = "red")
lines(x = seq(1,10,length = 100), y = predict(m2, newdata = data.frame(prcp = seq(1,10,length = 100)),type = "response"), col = "blue")

#######################
library(ggplot2)
library(jpeg)

img <- readJPEG("~/PhD-Project/Courses/Benoit_ML/Unhealthy_brain.JEPG.jpg")
# 3. Redimensionner en vecteur pour K-means
pixels <- as.vector(img)

# 4. Appliquer K-means avec 3 clusters
set.seed(42)  # pour reproductibilité
kmeans_result <- kmeans(pixels, centers = 3)

# 5. Créer l’image segmentée
segmented_matrix <- matrix(kmeans_result$cluster, nrow = nrow(img), ncol = ncol(img))

# 6. Afficher l’image originale et segmentée
# Pour ggplot2, on doit convertir en data.frame
library(reshape2)
df_original <- melt(img)
df_segmented <- melt(segmented_matrix)

# Tracer les deux images côte à côte
p1 <- ggplot(df_original, aes(x = Var2, y = Var1, fill = value)) +
  geom_raster() +
  scale_fill_gradient(low = "black", high = "white") +
  ggtitle("Image originale") +
  theme_minimal() +
  coord_fixed() +
  scale_y_reverse()

p2 <- ggplot(df_segmented, aes(x = Var2, y = Var1, fill = factor(value))) +
  geom_raster() +
  scale_fill_manual(values = c("black", "gray", "white","red","blue")) +
  ggtitle("Image segmentée (K-means)") +
  theme_minimal() +
  coord_fixed() +
  scale_y_reverse()

## Try with more clusters
set.seed(42)  # pour reproductibilité
kmeans_result <- kmeans(pixels, centers = 5)
# 5. Créer l’image segmentée
segmented_matrix <- matrix(kmeans_result$cluster, nrow = nrow(img), ncol = ncol(img))

# 5. Créer l’image segmentésegmented_matrix <- matrix(kmeans_result$cluster, nrow = nrow(img), ncol = ncol(img))

# 6. Afficher l’image originale et segmentée
# Pour ggplot2, on doit convertir en data.frame
library(reshape2)
df_original <- melt(img)
df_segmented <- melt(segmented_matrix)

p3 <- ggplot(df_segmented, aes(x = Var2, y = Var1, fill = factor(value))) +
  geom_raster() +
 scale_fill_manual(values = c("black", "gray", "white","red","blue"))  +
  ggtitle("Image segmentée (K-means)") +
  theme_minimal() +
  coord_fixed() +
  scale_y_reverse()
p3


######################################################################
### day1.R ends here
