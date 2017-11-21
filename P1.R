library(data.table)
library(akmeans)
library(ggplot2)


#1. Similarity: Dot Product
sqr.sum.root = sqrt(rowSums(normalized_ad[,3:8562]^2))
#Transform the normalized data further into having length = 1
transformed_ad = normalized_ad[,3:8562]/sqr.sum.root
#Santy Check
rowSums(transformed_ad^2)[1:5]

#Construct a function to run Kmeans Clustering on k between min_k and max_k;
#Also return a table of in-cluster and between cluster similarity with corresponding k
#as S and D
kmeans.dot.product = function(norm_data=transformed_ad, min_k, max_k, original_data=normalized_ad[,3:8562]){
  avg.in.sim.list = 0
  avg.btw.sim.list = 0
  for (kc in min_k:max_k) {
    cl = norm.sim.ksc(as.matrix(norm_data), k = kc)
    cl.out = data.table(cbind(cluster= cl$cluster, as.matrix(original_data)))
    # avg in-cluster distance
    avg = 0
    for (i in 1:kc) {
      sum = 0
      count.pairs = 0
      current = as.matrix(cl.out[cluster == i,-1])
      if (nrow(current) == 1) {
        count.pairs = 1
        sum = sum + sum(current^2)
      } else {
        count.pairs = cl$size[i]*(cl$size[i]-1)/2
        for (j in 1:(cl$size[i]-1)){
          for (t in (j+1):(cl$size[i])){
            dist = current[j,] %*% current[t,]
            sum = sum + dist
          }
        }
      }
      avg[i] = sum/count.pairs
    }
    avg.in.sim = mean(avg)
    avg.in.sim.list = rbind(avg.in.sim.list, c(avg.in.sim,kc))
    
    # avg between-cluster distance
    center = list()
    for (i in 1:length(cl$size)) {
      current = data.frame(cl.out[cluster == i,-1])
      center[[i]] = colMeans(current)
    }
    center = t(as.matrix(as.data.table(center)))
    size = nrow(center)
    count.pairs2 = size*(size-1)/2
    sum2 = 0
    for (j in 1:(size-1)){
      for (t in (j+1) : size){
        dist = center[j,] %*% center[t,]
        sum2 = sum2 + dist
      }
    }
    avg.btw.sim  = sum2/count.pairs2
    avg.btw.sim.list = rbind(avg.btw.sim.list, c(avg.btw.sim, kc))
  }
  colnames(avg.in.sim.list) = c("S","k")
  colnames(avg.btw.sim.list) = c("D","k")
  merge(avg.in.sim.list, avg.btw.sim.list, by = "k")
}

#Call on k = 2,3,...100
dot.out = kmeans.dot.product(min_k=2, max_k=100) #about 1.5hr runtime
dot.out$`S/D` = dot.out$S/dot.out$D
write.csv(dot.out[-1,], "kmeans_dot_out.csv", row.names = F)

#reshape the data to feed into ggplot
dot.out.gg = reshape(dot.out[-1,], idvar = "k", times = names(dot.out)[2:4], varying = list(names(dot.out)[2:4]),direction="long")
colnames(dot.out.gg) = c("k","type", "value")
dot.out.gg$type <- factor(dot.out.gg$type, levels = c("S", "D", "S/D"))

#visualize
#all 3 meaures
ggplot(dot.out.gg, aes(x=k, y = value, col = type)) + geom_line()+
  labs(title = "Simiarity from Product-based Kmeans Clustering with Varying K")
dot.out.gg = data.table(dot.out.gg)
#individual measure separately plotted
ggplot(dot.out.gg[type == "S/D",], aes(x=k, y = value)) + geom_line()+ geom_point()+
  ylim(c(-20,0))+ xlim(c(0,25))+
  labs(title = "Negative S/D with absolute value < 20")
ggplot(dot.out.gg[type == "S",], aes(x=k, y = value, col = "pink")) + geom_line()+ geom_point()+
  labs(title = "Avg In-cluster Similarity", y ="S")
ggplot(dot.out.gg[type == "D",], aes(x=k, y = value)) + geom_line(col="seagreen3")+ geom_point(col="seagreen3")+
  labs(title = "Avg Between-cluster Similarity", y ="D")


#2. Similarity: Euclidean
#Construct a function to run Euclidean-based clustering on k in k_list
#similarly, return a table of S, D, k
iter_kmeans = function(data=normalized_ad[,3:8562], k_list){
  datam = as.matrix(data)
  output = 0
  for (k in k_list){
    cl.eu = kmeans(datam, centers = k)
    output = rbind(output, c(mean(sqrt(cl.eu$withinss)), mean(sqrt(cl.eu$betweenss)), k))
  }
  colnames(output) = c("S", "D", "k")
  output[-1,]
}

#Call on k = 2,3, ...100
eu.out = iter_kmeans(k_list = seq(2,100,1))
eu.out = data.frame(eu.out)
eu.out$`S/D` = eu.out$S/eu.out$D
write.csv(eu.out, "kmeans_eu_out.csv", row.names = F)

#reshape the data to feed ggplot
eu.out.gg = reshape(eu.out[-1,], idvar = "k", times = names(eu.out)[c(1:2,4)], varying = list(names(eu.out)[c(1:2,4)]),direction="long")
colnames(eu.out.gg) = c("k","type", "value")
eu.out.gg$type <- factor(eu.out.gg$type, levels = c("S", "D", "S/D"))

#visualize
ggplot(eu.out.gg, aes(x=k, y = value, col = type)) + geom_line()+
  labs(title = "Simiarity from Euclidean-based Kmeans Clustering with Varying K")
eu.out.gg = data.table(eu.out.gg)
ggplot(eu.out.gg[type == "S/D",], aes(x=k, y = value)) + geom_line(col="steelblue3")+ geom_point(col="steelblue3")+
  labs(title = "S/D vs. K") + labs(y = "S/D")

# Visualize both dot product & euclidean-based together
dot.out.gg$fig = "dot product"
eu.out.gg$fig = "euclidean"
all = rbindlist(list(dot.out.gg, eu.out.gg), use.names = T, fill = T)
all$type <- factor(all$type, levels = c("S", "D", "S/D"))
ggplot(all, aes(x=k, y = value, col = type)) + geom_line(size = 0.01) + geom_point(size = 0.1)+
  facet_grid(fig ~., scales = "free_y")+
  labs(title = "Simiarity from Kmeans Clustering with Varying K")
