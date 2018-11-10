#load the data to r-studio
facebook<-read.table("facebook_combined.txt", sep=" ")

nrow(facebook) #88234 rows
a<-facebook[,2] #2nd column of facebook dataframe
b<-facebook[,1] #1st column of facebook dataframe
c<-as.data.frame(cbind(a, b)) #combine the two columns in one new dataframe to create the opposite direction

#change the names
names(facebook) 
names(c)<-c("V1", "V2")

#"merge" the initial dataset facebook with the new one that has the opposite directions
c<-rbind(facebook,c)
nrow(c)#176468
#ensure the uniqueness of all pairs.
nrow(unique(c)) #176468


facebook_sample<-read.table("sample_facebook.txt", sep=" ")

#Question 2:

friends<-function(node, c){
  #friends of node
  target<-c[c$V1==node,2]
  #all users(nodes)
  unique_users<-unique(c$V1)
  #Users that are not friends of #node
  notfriends<-setdiff(unique_users,target)
  #Exclude the target node
  notfriends<-notfriends[!notfriends%in%c(node)]
  library("rlist")
  
  #List of friends of not friends of target (target is not included as friend)
  lista<-list()
  for (i in notfriends){
    lista<-list.append(lista, c[c$V1==i,])}
  
  return (list(target, lista))
}

a107<-friends(107, c)
a1126<-friends(1126, c)
a14<-friends(14, c)
a35<-friends(35, c)
a7<-friends(7, facebook_sample)


fof_fun<-function(target, lista){
  
  #saves the common friends of those nodes that are not friends of the target
  for (i in 1:length(lista)){lista[[i]]<-list(lista[[i]],intersect(target, lista[[i]]$V2))}
  
  #saves the number of common friends of target with the friends of those nodes that are not 
  #friends of the target
  for (i in 1:length(lista)){lista[[i]]<-list(lista[[i]],length(lista[[i]][[2]]))}
  
  #orders the results based on the largest number of mutual friends and keeps the first 11 results
  listb<-head(lista[order(sapply(lista, function(x) x[[2]], simplify=TRUE), decreasing=TRUE)],11)
  
  #formats the results
  cur<-NULL
  for (i in 1:length(listb))
  {cur<-rbind(cur, data.frame(unique(listb[[i]][[1]][[1]]$V1),listb[[i]][[2]]))}
  
  names(cur)<-c("NodeID","Score")
  
  #In the case of ties in friendship score you should output the node with the smallest nodeID.
  if (cur[nrow(cur),2]==cur[nrow(cur)-1,2]){
    if (cur[nrow(cur),1]==max(cur[nrow(cur),1], cur[nrow(cur)-1,1])){
      cur<-cur[-nrow(cur),]
    }
    else{
      cur<-cur[-(nrow(cur)-1),]
    }
  }
  else {
    cur<-cur[-nrow(cur),]
  }
  
  return (cur)
}


fof_fun(a107[[1]], a107[[2]])
fof_fun(a1126[[1]], a1126[[2]])
fof_fun(a14[[1]], a14[[2]])
fof_fun(a35[[1]], a35[[2]])

fof_fun(a7[[1]], a7[[2]])


#Question 3:

jaccard_fun<-function(target, lista){
  
  #saves the common friends of those nodes that are not friends of the target
  for (i in 1:length(lista)){lista[[i]]<-list(lista[[i]],intersect(target, lista[[i]]$V2))}
  
  #The change is here for Jaccard!!! We calculate the division of the length of the common friends of
  #target and the friends of those nodes that are not friends of target 
  #with the length of the union of the not friends of target.
  for (i in 1:length(lista)){
    lista[[i]][[1]]<-list(lista[[i]][[1]],length(lista[[i]][[2]])/length(union(lista[[i]][[1]]$V2,target)))}
  
  #The change is also here for Jaccard!!!
  #Orders the results based on the largest number of mutual friends and keeps the first 11 results
  listb<-head(lista[order(sapply(lista, function(x) x[[1]][[2]], simplify=TRUE), decreasing=TRUE)],11)
  
  #The change is also here for Jaccard!!!-formats the results
  cur<-NULL
  for (i in 1:length(listb))
  {cur<-rbind(cur, data.frame(unique(listb[[i]][[1]][[1]]$V1),listb[[i]][[1]][[2]]))}
  
  names(cur)<-c("NodeID","Score")
  
  if (cur[nrow(cur),2]==cur[nrow(cur)-1,2]){
    if (cur[nrow(cur),1]==max(cur[nrow(cur),1], cur[nrow(cur)-1,1])){
      cur<-cur[-nrow(cur),]
    }
    else{
      cur<-cur[-(nrow(cur)-1),]
    }
  }
  else {
    cur<-cur[-nrow(cur),]
  }
  
  return (cur)
}

jaccard_fun(a107[[1]], a107[[2]])
jaccard_fun(a1126[[1]], a1126[[2]])
jaccard_fun(a14[[1]], a14[[2]])
jaccard_fun(a35[[1]], a35[[2]])
jaccard_fun(a7[[1]], a7[[2]])


#Question 4:

ada<-function(c, target, lista){
  
  #saves the common friends of those nodes that are not friends of the target
  for (i in 1:length(lista)){
    lista[[i]]<-list.append(lista[[i]], list(intersect(target, lista[[i]]$V2)))
  }
  
  
  listq4<-list()
  #For every non friend of target
  for (i in 1:length(lista)){
    sum<-0
    #For every common friend that the target has with the non friend, calculate the number of friends 
    #that it has, take its 1/logarithm and save it.
    for (j in 1:length(unlist(lista[[i]][[3]]))){
      sum<-sum+1/log10(length(c[c$V1==lista[[i]][[3]][[1]][j],2]))}
    listq4<-list.append(listq4, list(unique(lista[[i]][[1]]), sum))
  }
  
  #order the results and keep the first 11
  listb<-head(listq4[order(sapply(listq4, function(x) x[[2]], simplify=TRUE), decreasing=TRUE)],11)
  
  #formats the results
  df<-data.frame()
  for (i in 1:length(listb)){
    df<-rbind(df, t(data.frame(unlist(listb[[i]]))))
  }
  rownames(df)<-NULL
  names(df)<-c("NodeID","Score")
  
  if (df[nrow(df),2]==df[nrow(df)-1,2]){
    if (df[nrow(df),1]==max(df[nrow(df),1], df[nrow(df)-1,1])){
      df<-df[-nrow(df),]
    }
    else{
      df<-df[-(nrow(df)-1),]
    }
  }
  else {
    df<-df[-nrow(df),]
  }
  return(df)
}


ada(c, a107[[1]], a107[[2]])
ada(c, a1126[[1]], a1126[[2]])
ada(c, a14[[1]], a14[[2]])
ada(c, a35[[1]], a35[[2]])
ada(facebook_sample, a7[[1]], a7[[2]])

#Test in sample
ada(7, facebook_sample)


#Question 5:

#A)

#Id that is multiple of 100
filteredId<-unique(c[c$V1%%100==0,1])

#Exclude 0
filteredId<-filteredId[!filteredId%in%0]


fofjacsum<-0
jacadsum<-0
fofadsum<-0

#for each node out of the 40
for (i in filteredId){
  ai<-friends(i, c)
  fof<-fof_fun(ai[[1]], ai[[2]])
  jac<-jaccard_fun(ai[[1]], ai[[2]])
  adam<-ada(c, ai[[1]], ai[[2]])
  
  #similarity percentage for every pair
  q1<-100*length(intersect(fof$NodeID, jac$NodeID))/length(fof$NodeID)
  q2<-100*length(intersect(jac$NodeID, adam$NodeID))/length(jac$NodeID)
  q3<-100*length(intersect(fof$NodeID, adam$NodeID))/length(fof$NodeID)
  
  fofjacsum<-fofjacsum+q1
  jacadsum<-jacadsum+q2
  fofadsum<-fofadsum+q3
  
  print(paste("Node:",i,"Fof-Jaccard:", q1))
  print(paste("Node:",i,"Jaccard-Adamic:",q2))
  print(paste("Node:",i,"Fof-Adamic:", q3))
  
}
#Average similarity between algorithms
print(paste("Average Fof-Adamic:", fofjacsum/40))
print(paste("Average Jaccard-Adamic:", jacadsum/40 ))
print(paste("Average Fof-Adamic:", fofadsum/40))

#B)

count<-0
#vector for friends
llmrp<-c()

listina<-list()
while (count<=100){
  print(count)
  #Randomly choose a real friend connection
  x1 <- sample(0:nrow(c),1)
  x2 <- which(c[x1,]$V1==c$V2 & c[x1,]$V2==c$V1)[1]
  
  while ((x1 %in% llmrp) & (x2 %in% llmrp)){
    x1 <- sample(0:nrow(c),1)
    x2 <- which(c[x1,]$V1==c$V2 & c[x1,]$V2==c$V1)[1]
  }
  
  #add to vector
  llmrp<-c(llmrp,x1,x2)
  
  
  #Two friends F1, F2
  F1<-c[x1,]$V1
  F2<-c[x1,]$V2
  
  #Remove the friendship from the graph(both directions)
  y<-c[-c(x1, x2),]
  
  
  kik<-friends(F1, y)
  
  #call fof
  fo<-fof_fun(kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionfoF2<-which(fo$NodeID==F2)[1]
  
  #call jaccard
  jak<-jaccard_fun(kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionjakF2<-which(jak$NodeID==F2)[1]
  
  #call adam
  ad<-ada(y, kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionadF2<-which(ad$NodeID==F2)[1]
  
  
  kik2<-friends(F2, y)
  #call fof
  fo2<-fof_fun(kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionfo2F1<-which(fo2$NodeID==F1)[1]
  
  #call jaccard
  jak2<-jaccard_fun(kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionjak2F1<-which(jak2$NodeID==F1)[1]
  
  #call adam
  ad2<-ada(y, kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionad2F1<-which(ad2$NodeID==F1)[1]
  
  #if there is a value in every position evaluate all three reccommendation systems
  if (!is.na(positionfoF2) & !is.na(positionjakF2) & !is.na(positionadF2) & !is.na(positionfo2F1) & !is.na(positionjak2F1) & !is.na(positionad2F1)){
    #average two numbers for each similarity function
    avgfof<-(positionfoF2+positionfo2F1)/2
    avgjak<-(positionjakF2+positionjak2F1)/2
    avgad<-(positionadF2+positionad2F1)/2
    #save results to listina
    listina<-list.append(listina, list(c(F1, "-" ,F2), avgfof,avgjak, avgad))
    #increase the counter
    count<-count+1
  } 
  
}

summationfof<-0
summationjac<-0
summationada<-0

#calculate summation of scores per recommendation system
for( i in 1:(length(listina))){
  summationfof<-summationfof+sum(listina[[i]][[2]])
  summationjac<-summationjac+sum(listina[[i]][[3]])
  summationada<-summationada+sum(listina[[i]][[4]])
}

#calculate the average rank per recommendation system
averagefof<-summationfof/100
averagejac<-summationjac/100
averageada<-summationada/100


###########################
#Bonus B.
###########################
hub<-function(c, target, lista){
  
  #saves the common friends of those nodes that are not friends of the target
  for (i in 1:length(lista)){
    lista[[i]]<-list.append(lista[[i]], list(intersect(target, lista[[i]]$V2)))
  }
  # for (i in 1:length(a107[[2]])){
  #   a107[[2]][[i]]<-list.append(a107[[2]][[i]], list(intersect(a107[[1]], a107[[2]][[i]]$V2)))
  # }
  
  # print(lista[[30]])
  listq4<-list()
  #For every non friend of target
  for (i in 1:length(lista)){
    vectorr<-c()
    omg<-length(lista[[i]][[3]][[1]])
    #For every common friend that the target has with the non friend, calculate the number of friends 
    #that it has, take its 1/logarithm and save it.
    for (j in 1:length(unlist(lista[[i]][[3]]))){
      vectorr<-c(vectorr, length(c[c$V1==lista[[i]][[3]][[1]][j],2]))
    }
    minimum<-omg/min(vectorr)
    listq4<-list.append(listq4, list(unique(lista[[i]][[1]]), minimum))
  }
  
  
  #print(listq4[[30]])
  #order the results and keep the first 11
  listb<-head(listq4[order(sapply(listq4, function(x) x[[2]], simplify=TRUE), decreasing=TRUE)],11)
  
  #formats the results
  df<-data.frame()
  for (i in 1:length(listb)){
    df<-rbind(df, t(data.frame(unlist(listb[[i]]))))
  }
  rownames(df)<-NULL
  names(df)<-c("NodeID","Score")
  
  if (df[nrow(df),2]==df[nrow(df)-1,2]){
    if (df[nrow(df),1]==max(df[nrow(df),1], df[nrow(df)-1,1])){
      df<-df[-nrow(df),]
    }
    else{
      df<-df[-(nrow(df)-1),]
    }
  }
  else {
    df<-df[-nrow(df),]
  }
  return(df)
}


count<-0
#vector for friends
llmrp<-c()

listina<-list()
while (count<=100){
  print(count)
  #Randomly choose a real friend connection
  x1 <- sample(0:nrow(c),1)
  x2 <- which(c[x1,]$V1==c$V2 & c[x1,]$V2==c$V1)[1]
  
  while ((x1 %in% llmrp) & (x2 %in% llmrp)){
    x1 <- sample(0:nrow(c),1)
    x2 <- which(c[x1,]$V1==c$V2 & c[x1,]$V2==c$V1)[1]
  }
  
  #add to vector
  llmrp<-c(llmrp,x1,x2)
  
  
  #Two friends F1, F2
  F1<-c[x1,]$V1
  F2<-c[x1,]$V2
  
  #Remove the friendship from the graph(both directions)
  y<-c[-c(x1, x2),]
  
  
  kik<-friends(F1, y)
  
  #call fof
  fo<-fof_fun(kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionfoF2<-which(fo$NodeID==F2)[1]
  
  #call jaccard
  jak<-jaccard_fun(kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionjakF2<-which(jak$NodeID==F2)[1]
  
  #call adam
  ad<-ada(y, kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionadF2<-which(ad$NodeID==F2)[1]
  
  #call hub
  hubu<-hub(y, kik[[1]], kik[[2]])
  #keep the position that F2 appears
  positionhubF2<-which(hubu$NodeID==F2)[1]
  
  
  kik2<-friends(F2, y)
  #call fof
  fo2<-fof_fun(kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionfo2F1<-which(fo2$NodeID==F1)[1]
  
  #call jaccard
  jak2<-jaccard_fun(kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionjak2F1<-which(jak2$NodeID==F1)[1]
  
  #call adam
  ad2<-ada(y, kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionad2F1<-which(ad2$NodeID==F1)[1]
  
  #call hub
  hub2<-hub(y, kik2[[1]], kik2[[2]])
  #keep the position that F1 appears
  positionhub2F1<-which(hub2$NodeID==F1)[1]
  
  #if there is a value in every position evaluate all three reccommendation systems
  if (!is.na(positionhub2F1) & !is.na(positionhubF2) & !is.na(positionfoF2) & !is.na(positionjakF2) & !is.na(positionadF2) & !is.na(positionfo2F1) & !is.na(positionjak2F1) & !is.na(positionad2F1)){
    #average two numbers for each similarity function
    avgfof<-(positionfoF2+positionfo2F1)/2
    avgjak<-(positionjakF2+positionjak2F1)/2
    avgad<-(positionadF2+positionad2F1)/2
    avghub<-(positionhubF2+positionhub2F1)/2
    #save results to listina
    listina<-list.append(listina, list(c(F1, "-" ,F2), avgfof,avgjak, avgad, avghub))
    #increase the counter
    count<-count+1
  } 
  
}

summationfof<-0
summationjac<-0
summationada<-0
summationhub<-0

#calculate summation of scores per recommendation system
for( i in 1:(length(listina))){
  summationfof<-summationfof+sum(listina[[i]][[2]])
  summationjac<-summationjac+sum(listina[[i]][[3]])
  summationada<-summationada+sum(listina[[i]][[4]])
  summationhub<-summationhub+sum(listina[[i]][[5]])
}

#calculate the average rank per recommendation system
averagefof<-summationfof/100
averagejac<-summationjac/100
averageada<-summationada/100
averagehub<-summationhub/100



averagefof
averagejac
averageada
averagehub

bar<-data.frame(2.3,2.22,2.25,2.865)
names(bar)<-c("FoF","Jaccard","Adamic","Hub")
finallly<-t(bar)
test1<-barplot(finallly[,1],col=blues9,main="Average Rank",ylab="Frequency Percentage",xlab="Methods")
text(x=finallly,label=finallly[,1],pos=1,cex=1,col="red")
