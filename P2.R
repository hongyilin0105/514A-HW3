#library(factoextra)
library(FactoMineR)
#install.packages("FactoMineR")
library(data.table)
library(ggplot2)
library(plot3D)

normalized_ad[,subset:="AD"]
normalized_ctrl[,subset := "ctrl"]
both = rbindlist(list(normalized_ad, normalized_ctrl), use.names = T, fill = T)

both_normalized = cbind(both[,1:2],t(normalize(t(both[,3:8562]), method="standardize", margin = 1L, on.constant = "quiet")))
pca.test = PCA(as.data.frame(both_normalized[,3:8562]), graph = F, ncp = 364, scale.unit = F)

summary(pca.test)
pca.out = data.table(pca.test$eig)
pca.out

pca.test$var$contrib[,1:3]


ggplot(pca.out, aes(x = as.numeric(row.names(pca.out)), y = `cumulative percentage of variance`)) +
  geom_point(col = "cyan3", size = 0.8) + labs(x = "k") + geom_line(aes(y= 80),col = "#FF9999", linetype = 2) +
  geom_line(aes(x = 11), col = "#FF9999", linetype = 2) + geom_point(aes(x=11,y=80.33), size = 2,col = "grey15") +
  annotate("text",x= 50, y = 79, label = "(11, 80.33%)", col = "grey15")+
  labs(title = "Cumulative Information of Varying K in PCA")

ggplot(pca.out, aes(x = as.numeric(row.names(pca.out)), y = eigenvalue)) + geom_bar(stat = "identity", fill = "#FF9999", col = "#FF9999")+
  labs(x = "k", title = "Eigenvalue of Varying K in PCA")

score = data.table(cbind(pca.test$ind$coord[,1:3], subset = both$subset))

score[, 1:3] = lapply(score[,1:3], as.numeric)
score$subset = as.factor(score$subset)
plot3d(x=score$Dim.1, y=score$Dim.2, 
         z=score$Dim.3, phi = 5, theta=20, bty ="g", xlab = "Dimension 1", ylab = "Dimension 2", zlab = "Dimension 3",
         main = "AD vs. Control Group in 3 PCs", col = rainbow(2)[score$subset], colkey = list(side = 4, length = 0.3),
         type = 's', size = 2,
         ticktype = "detailed")

attach(score[subset == "ctrl",])
points3D(x=Dim.1, y=Dim.2, 
       z=Dim.3, phi = 5, theta=65, bty ="g", xlab = "Dimension 1", ylab = "Dimension 2", zlab = "Dimension 3",
       main = "AD vs. Control Group in 3 PCs", col = "green",
       cex = 2, pch =20,
       ticktype = "detailed")
detach(score[subset == "ctrl",])
attach(score[subset == "AD",])
points3D(x=Dim.1, y=Dim.2, 
         z=Dim.3, add = T, phi = 5, theta=65, bty ="g", xlab = "Dimension 1", ylab = "Dimension 2", zlab = "Dimension 3",
         main = "AD vs. Control Group in 3 PCs", col = "red", 
         cex = 2, pch =20,
         ticktype = "detailed")
detach(score[subset == "AD",])

ggplot(score, aes(x = Dim.1, y = Dim.2, col = subset)) + geom_point()+
  labs(title = "Separation of AD & Control Group along 2 PCs") +
  geom_point(aes(x=mean(score[subset=="AD",Dim.1]),y=mean(score[subset=="AD",Dim.2])),col= "#FF9999", size = 5)+
  geom_point(aes(x=mean(score[subset=="ctrl",Dim.1]),y=mean(score[subset=="ctrl",Dim.2])), col= "cyan3",size = 5)
  


