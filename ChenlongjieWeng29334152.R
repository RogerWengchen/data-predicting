#Task1 I select the text from ABC News

############################################################################
#Task2 
setwd("C:/Users/WENGCHENLONGJIE/Desktop/FIT3152/Assignment3")
#first import all the package I need for this question
rm(list = ls()) #clean up the workplace at start
library(slam)
library(tm)
library(SnowballC)

#then read the folder which contain all the text file we needed
cname = file.path(".", "text_folder") #
docs = Corpus(DirSource((cname))) #add all the file in to Corpus
print(summary(docs)) #print all the file we have here 


############################################################################
#Task3
#the main parts going with these step
#Tokenise, Convert case, Filtering – including removing stop words, Stemming
#Tokenise change the word to token
toSpace = content_transformer(function(x,pattern) gsub(pattern, " ", x))
docs = tm_map(docs, toSpace, "-") #remove‘-’
docs = tm_map(docs, toSpace, "–") #remove‘-’
docs = tm_map(docs, toSpace, "—") #remove‘-’
docs = tm_map(docs, removeNumbers) # remove numbers
docs = tm_map(docs, removePunctuation) #remove punctuation
docs = tm_map(docs, content_transformer(tolower)) #change words to lower case

#Filter words
# Remove stop words and white space
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stripWhitespace)  

#stem
docs = tm_map(docs, stemDocument, language = "english") #change the word with similar meaning to same token

#create document term matrix 
dtm = DocumentTermMatrix(docs) 

#check word frequencies  
freq = colSums(as.matrix(dtm))  #count same token
length(freq)  #how many token in total
ord = order(freq) #frequency in order
freq[head(ord)] #less
freq[tail(ord)] #most

#remove the word whihc might appear in every document
docs = tm_map(docs, toSpace, "will")
docs = tm_map(docs, toSpace, "year")
docs = tm_map(docs, toSpace, "said")
docs = tm_map(docs, stripWhitespace)  

#create document term matrix 
dtm = DocumentTermMatrix(docs) 

#check word frequencies  
freq = colSums(as.matrix(dtm))  #count same token
length(freq)  #how many token in total
ord = order(freq) #frequency in order
freq[head(ord)] #less
freq[tail(ord)] #most

#frequency of frequencies 
head(table(freq), 10) #make frequency table which appear 1-10 times
tail(table(freq), 10)
dim(dtm) #show the number of files and number of different token
dtms = removeSparseTerms(dtm, 0.75) #here we want only left about 20 tokens 
dim(dtms) #to show how many token after we doing it
#to show the dtms details
inspect(dtms) 
dtms = as.matrix(dtms)
write.csv(dtms, "dtms.csv")


############################################################################
#Task4
#this is the Euclidean distance way to do it.
distmatrix = dist(scale(dtms))
fit = hclust(distmatrix, method = "ward.D")  
tree = as.dendrogram(fit)
tree = color_branches(tree, k = 3) #Color the clusters of clustering results
plot(tree, main = 'Euclidean distance') 

#this is the Cosine Distance way to do it.
library(proxy)
#install.packages("dendextend")
library(dendextend)
dtm_cos = proxy::dist(as.matrix(dtms), method = "cosine")
fit_cos = hclust(dtm_cos) 
tree_cos = as.dendrogram(fit_cos) #Convert fit into a visual tree diagram
tree_cos = color_branches(tree_cos, k = 3) #Color the clusters of clustering results
plot(tree_cos, main='Cosine Distance')

cutfit = cutree(fit, k = 3)
cutfit #use to get more information about Which cluster is each sample assigned to
sort(cutfit)

cutfit_cos = cutree(fit_cos, k = 3) 
cutfit_cos #use to get more information about Which cluster is each sample assigned to
sort(cutfit_cos) # Output the cluster to which each document belongs

library(fpc)
#install.packages("fpc")
#Silhouette Coefficient
silhouette_score <- cluster.stats(distmatrix, cutfit)$avg.silwidth #should from -1 to 1, higher value means better
silhouette_score #0.06029111
silhouette_score_cos <- cluster.stats(distmatrix, cutfit_cos)$avg.silwidth
silhouette_score_cos #-0.03370824

#Calinski-Harabasz Index
ch_index <- cluster.stats(distmatrix, cutfit)$ch #higher is better
ch_index #2.180116
ch_index_cos <- cluster.stats(distmatrix, cutfit_cos)$ch
ch_index_cos #1.143047


############################################################################
#Task5
#first import package we need in this part
library(igraph) 
library(igraphdata)

#these is the steps to make network
# start with original document-term matrix
dtmsx = as.matrix(dtms)
# convert to binary matrix
dtmsx = as.matrix((dtmsx > 0) + 0)
# multiply binary matrix by its transpose
ByAbsMatrix = dtmsx %*% t(dtmsx)
# make leading diagonal zero
diag(ByAbsMatrix) = 0
# create graph object
ByAbs = graph_from_adjacency_matrix(ByAbsMatrix,mode = "undirected", weighted = TRUE)
set.seed(29334152)
plot(ByAbs)

#now start to find the most important document in the network
d = as.table(degree(ByAbs)) #the degree of each document
b = as.table(betweenness(ByAbs)) #the betweenness of each document
c = as.table(closeness(ByAbs)) #the closeness of each document
e = as.table(evcent(ByAbs)$vector) #the eigenvector of each document
stats = as.data.frame(rbind(d,b,c,e))#combine the d,b,c,e together
stats = as.data.frame(t(stats)) #change col to row
colnames(stats) = c("degree", "betweenness", "closeness", "eigenvector") #assign correct name
stats
#sort and explore key nodes
head(stats[order(-stats$betweenness),]) #we only need to know the high most so use head
head(stats[order(-stats$closeness),])
head(stats[order(-stats$eigenvector),])
head(stats[order(-stats$degree),])

V(ByAbs)['Business4.txt']$color = "yellow"
V(ByAbs)['Health2.txt']$color = "red"
V(ByAbs)['Science4.txt']$color = "green"
set.seed(29334152)
plot(ByAbs, vertex.size = degree(ByAbs))

communities <- cluster_fast_greedy(ByAbs)  #community detection
plot(communities, ByAbs, vertex.color = communities$membership, vertex.size = degree(ByAbs))

spectral <- cluster_spinglass(ByAbs) #community detection
membership <- membership(spectral)
plot(ByAbs, vertex.color = membership, vertex.label = V(ByAbs)$name, vertex.size = degree(ByAbs))



############################################################################
#Task6
#these is the steps to make network
# start with original document-term matrix
dtmsx = as.matrix(dtms)
# convert to binary matrix
dtmsx = as.matrix((dtmsx > 0) + 0)
# multiply binary matrix by its transpose
ByTokenMatrix = t(dtmsx) %*% dtmsx
# make leading diagonal zero
diag(ByTokenMatrix) = 0
# create graph object
ByToken = graph_from_adjacency_matrix(ByTokenMatrix,mode = "undirected", weighted = TRUE)
set.seed(29334152)
plot(ByToken)

#now start to find the most important document in the network
dT = as.table(degree(ByToken)) #the degree of each document
bT = as.table(betweenness(ByToken)) #the betweenness of each document
cT = as.table(closeness(ByToken)) #the closeness of each document
eT = as.table(evcent(ByToken)$vector) #the eigenvector of each document
statsT = as.data.frame(rbind(dT,bT,cT,eT))#combine the dT,bT,cT,eT together
statsT = as.data.frame(t(statsT)) #change col to row
colnames(statsT) = c("degree", "betweenness", "closeness", "eigenvector") #assign correct name
statsT
#sort and explore key nodes
head(statsT[order(-statsT$betweenness),]) #we only need to know the high most so use head
head(statsT[order(-statsT$closeness),])
head(statsT[order(-statsT$eigenvector),])
head(statsT[order(-statsT$degree),])

#highlight the most important point
V(ByToken)['nation']$color = "yellow"
V(ByToken)['australian']$color = "red"
V(ByToken)['now']$color = "green"
V(ByToken)['that']$color = "blue"
set.seed(29334152)
plot(ByToken, vertex.size = degree(ByToken))

communities <- cluster_fast_greedy(ByToken)  #community detection
plot(communities, ByToken, vertex.color = communities$membership, vertex.size = degree(ByToken))



############################################################################
#Task7

# start with document term matrix dtms
dtmsa = as.data.frame(dtms) # clone dtms
dtmsa$ABS = rownames(dtmsa) # add row names
dtmsb = data.frame()
for (i in 1:nrow(dtmsa)){
  for (j in 1:(ncol(dtmsa)-1)){
    touse = cbind(dtmsa[i,j], dtmsa[i,ncol(dtmsa)],
                    colnames(dtmsa[j]))
    dtmsb = rbind(dtmsb, touse ) } } # close loops
colnames(dtmsb) = c("weight", "abs", "token")
dtmsc = dtmsb[dtmsb$weight != 0,] # delete 0 weights

# put colunms in order: abs, token, weight
dtmsc = dtmsc[,c(2,3,1)]

# create graph object and declare bipartite
g <- graph.data.frame(dtmsc, directed=FALSE)
bipartite.mapping(g)
V(g)$type <- bipartite_mapping(g)$type
V(g)$color <- ifelse(V(g)$type, "lightblue", "salmon")
V(g)$shape <- ifelse(V(g)$type, "circle", "square")
E(g)$color <- "lightgray"
set.seed(29334152)
plot(g)

communities <- cluster_fast_greedy(g)  #community detection
plot(communities, g, vertex.color = communities$membership, vertex.size = degree(g))

spectral <- cluster_spinglass(g) #community detection
membership <- membership(spectral)
plot(g, vertex.color = membership, vertex.label = V(g)$name, vertex.size = degree(g))



