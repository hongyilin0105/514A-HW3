library(data.table)
library(akmeans)
library(ggplot2)
#set.seed(1)
#cl.test = akmeans(as.matrix(normalized_ad[,3:8562]),d.metric = 2, ths3 = 0.000000000000001, mode = 3, min.k = 2)
#cl.test$size
#cl.test2 = norm.sim.ksc(as.matrix(normalized_ad[,3:8562]), k =4)
#cl.test2$size

#1. Similarity: Dot Product
sqr.sum.root = sqrt(rowSums(normalized_ad[,3:8562]^2))
transformed_ad = normalized_ad[,3:8562]/sqr.sum.root
rowSums(transformed_ad^2)


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

dot.out = kmeans.dot.product(min_k=2, max_k=100) #10:42 - 12:00
dot.out$`S/D` = dot.out$S/dot.out$D
write.csv(dot.out[-1,], "kmeans_dot_out.csv", row.names = F)

dot.out.gg = reshape(dot.out[-1,], idvar = "k", times = names(dot.out)[2:4], varying = list(names(dot.out)[2:4]),direction="long")
colnames(dot.out.gg) = c("k","type", "value")
ggplot(dot.out.gg, aes(x=k, y = value, col = type)) + geom_line()+ylim(c(-1500,500))

#. Similarity: Euclidean
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

eu.out = iter_kmeans(k_list = seq(2,100,1))
eu.out = data.frame(eu.out)
eu.out$`S/D` = eu.out$S/eu.out$D
write.csv(eu.out, "kmeans_eu_out.csv", row.names = F)

eu.out.gg = reshape(eu.out[-1,], idvar = "k", times = names(eu.out)[c(1:2,4)], varying = list(names(eu.out)[c(1:2,4)]),direction="long")
colnames(eu.out.gg) = c("k","type", "value")
ggplot(eu.out.gg, aes(x=k, y = value, col = type)) + geom_line()


# Visualize together
dot.out.gg$fig = "dot product"
eu.out.gg$fig = "euclidean"
all = rbindlist(list(dot.out.gg, eu.out.gg), use.names = T, fill = T)
all$type <- factor(all$type, levels = c("S", "D", "S/D"))
ggplot(all, aes(x=k, y = value, col = type)) + geom_line(size = 0.01) + geom_point()+
  facet_grid(fig ~., scales = "free_y")+
  labs(title = "Simiarity from Kmeans Clustering with Varying K")
