###
### Networks, First Run
###
#We set the working Directory

## Library to produce network graphs
library(igraph) 
library(haven)
library(foreign)
library(lubridate)

setwd("F:\\Dropbox\\calvo-multilevel\\Class 12-15 Networks")
rm(list=ls(all=TRUE))

load("F:/Dropbox/models/twitter/CongresoTimelines/US/USCongressUntil-01012022.RData")
select.names<-names(table(boca.df[,3]))
congress <- data.frame(boca.df[boca.df[,9] %in% select.names, ])

#Get webs
boca2.df<- as.data.frame(boca.df)
test<-strsplit(unlist(boca2.df$web),"/")
boca2.df$web.root<-unlist(lapply(test, function(x) x[3]))
boca2.df$web.root<- sub('.com', "", boca2.df$web.root)
boca2.df$web.root<- sub('www.', "", boca2.df$web.root)
boca2.df$web.root<- gsub("[[:punct:]]", "",boca2.df$web.root)


set.seed(520)

format.str <- "%a %b %d %H:%M:%S %z %Y"
congress$date.rt <-as.POSIXct(strptime(congress[,8], format.str, tz = "GMT"), tz = "GMT")
congress$date.t <-as.POSIXct(strptime(congress[,14], format.str, tz = "GMT"), tz = "GMT")

data<-cbind(c(congress[,3]),c(congress[,9]))
net <- graph.empty()
net <- add.vertices(net, length(unique(c(data))),name=as.character(unique(c(data))))
net <- add.edges(net, t(data))
E(net)$text <- congress$text
E(net)$color <- "SkyBlue2"
V(net)$eig<-evcent(net)$vector
V(net)$ind<- degree(net,mode="in")
V(net)$outd<- degree(net,mode="out")
E(net)$color <- "gray"
E(net)$friends <- congress$friends


###
### Eliminate small communities
###

my.com.fast <- walktrap.community(net)
V(net)$membership <- my.com.fast$membership
sel.com<-names(tail(sort(table(V(net)$membership)),n=4)) 
collect.nodes<- which(V(net)$membership==sel.com[1])
for(i in 2:4){collect.nodes<- c(collect.nodes,which(V(net)$membership==sel.com[i]))}
net <- induced.subgraph(graph=net,vids=collect.nodes)

##
## Estimate Layout
##

l <-layout_with_fr(net, grid = c("nogrid")) 
V(net)$l1<-l[,1]
V(net)$l2<-l[,2]


tiff(filename = paste("Basic.tiff",sep=""), width = 8, height = 8, units = "in", pointsize = 8, compression = c("lzw"),  bg = "white", res = 300)
plot(l, col=V(net)$membership, cex=log(V(net)$ind)/3, pch=16, xlab="", ylab="", yaxt='n', xaxt='n')
dev.off()

l2 <- layout_with_kk(net)
tiff(filename = "Basic-kknet.tiff", width = 16, height = 16, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 300)
plot(l2, col=V(net)$membership, cex=log(V(net)$ind)/3, pch=16, xlab="", ylab="", yaxt='n', xaxt='n')
title("Twitter Network of Politicians, US", cex=1.6)
dev.off()




###
###
### An Example of Clustering
###
###

##Dendogram
wt <- walktrap.community(net)
## Transform the community information into dendogram 
dend <- as.dendrogram(wt, use.modularity=TRUE)

## Plot Dendogram
pdf(file = "Dendogram.pdf", 42, 5, pointsize=5, compress=FALSE)
#tiff(filename = "Dendogram.tiff", width = 22, height = 8, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 300)
plot(dend, nodePar=list(pch=c(NA, 20)))
dev.off()

## Plot Dendogram with triangles
pdf(file = "Dendogram-Triangle.pdf", 42, 5, pointsize=5, compress=FALSE)
#tiff(filename = "Dendogram-triangle.tiff", width = 22, height = 8, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 300)
plot(dend , type="triangle")
dev.off()

##Transform dendogram as Hierarchical Cluster
dend2<-as.hclust(dend)

## Plot with color Names
#tiff(filename = "Dendogram-colors.tiff", width = 22, height = 6, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 300)
labelColors = c("#0508E6","#0003B0", "#FA0A0A","#0091EB", "#FF2400", "#FFA20D", "#AA02BD", "#11BEF7", "#0AFCEC", "#23A0FA", "#E584F0","#04D92F","#04D92F","#F77D11")
# cut dendrogram in 5 clusters
clusMember = cutree(dend2, 14)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
    #attr(n, "edgePar") <- c(a$nodePar, col=labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(dend, colLab)
# make plot
pdf(file = "Dendogram-Color.pdf", 42, 5, pointsize=5, compress=FALSE)
plot(clusDendro, main = "Politician's Dendogram", type = "triangle")
dev.off()



###
###
### With a few more clusters
###
###

dend2<-as.hclust(dend)

#tiff(filename = "Dendogram-colors.tiff", width = 22, height = 6, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 300)
labelColors = c("#0508E6","#0003B0", "#FA0A0A","#0091EB", "#FF2400", "#FA0789", "#AA02BD", "#11BEF7", "#0AFCEC", "#23A0FA", "#DCE800", "#4EA4D9","#FFA20D","#79CAFC", "#0A0A01", "#C0E5FC", "#C0E5FC", "#0A0A01", "#3F9ED9", "#C0E5FC", "#C0E5FC", "#C0E5FC")
# cut dendrogram in 22 clusters
clusMember = cutree(dend2, 22)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
    #attr(n, "edgePar") <- c(a$nodePar, col=labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(dend, colLab)
# make plot
pdf(file = "Dendogram-Color.pdf", 42, 5, pointsize=5, compress=FALSE)
plot(clusDendro, main = "Retweet Network in the US Congress", type = "triangle")
dev.off()


##
## Autority and Hub
##

library(ggplot2)
library(gtable)

cent<-data.frame(bet=betweenness(net),eig=evcent(net)$vector)
# evcent returns lots of data associated with the EC, but we only need the
# leading eigenvector
res<-lm(eig~bet,data=cent)$residuals
cent<-transform(cent,res=res)

Hub<-hub.score(net)$vector
Authority<-authority.score(net)$vector

# We use ggplot2 to make things a
# bit prettier
p<-ggplot(cent,aes(x=log(Hub),y=log(Authority), label=V(net)$name,colour=Hub, size=Authority))+xlab("Hub-Who You Follow")+ylab("Authority-Who Follows You")

# We use the residuals to color and
# shape the points of our plot,
# making it easier to spot outliers.

tiff(filename = "Authority and Hub.tiff", width = 18, height = 9, units = "in", pointsize = 12, compression = c("lzw"), bg = "white", res = 300)
p+geom_text()+ggtitle("Authority vs. Hub")
dev.off()


##
##
## ERGM MODELS
##
##

library("ergm")
library("network")
library("intergraph")

ergmnet <- asNetwork(net)
ergmnet %v% "membership" <- V(net)$membership
ergmnet %v% "name" <- V(net)$name
ergmnet

# Models

model.ergm1 <- ergm(ergmnet ~ edges)
summary(model.ergm1)
model.ergm2 <- ergm(ergmnet ~ edges+nodematch("membership"))
summary(model.ergm2)
model.ergm3 <- ergm(ergmnet ~ edges+nodematch("membership", diff=T))
summary(model.ergm3)
model.ergm4 <- ergm(ergmnet ~ edges+nodemix("membership")+nodefactor("membership"))
summary(model.ergm4)


