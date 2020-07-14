# This script is based off of code provided by Dr. Ami Gates
# you can find her work here -> http://www.mathandstatistics.com/

library(networkD3)
library(arules)
library(rtweet)
library(twitteR)
library(ROAuth)
library(jsonlite)
library(streamR)
library(rjson)
library(tokenizers)
library(tidyverse)
library(plyr)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(stringr)
library(arulesViz)
library(igraph)
library(qdapRegex)
library(arules)

consumer_key = '9NqnCoDxuyLGOq6Z1xdqHt0Xf'
consumer_secret = '4ZPc8xANaF9iVLMRhSitAqcg48G1iG5bSfZlu4hYr7SOkIgNdk'
access_token = '2754695727-FyZrgMQ4KOUhuZfhr15Ke6g74usm7zWF7SaZGgA'
access_secret = 'pgmTJAm0fR86f6jKHsHMDshiAUldw5cuUvJtfLXOiio4d'

requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'


setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
Search<-twitteR::searchTwitter("baseball",n=1000, since="2020-07-12")
Search_DF <- twListToDF(Search)
(Search_DF$text[1])
MLB_data = Search_DF

for (i in 1:nrow(MLB_data)){
  MLB_data$text[i] =  rm_url(MLB_data$text[i], pattern=pastex("@rm_twitter_url", "@rm_url"))
  MLB_data$text[i] =  gsub("@[A-Za-z0-9]+", "", MLB_data$text[i])

}

IgnoreThese=c("and","this", "for","their","the", "is",
            "rt", "it","in", "to", "has", "an","at", "by",
             "of", "well", "as", "I", "come", "as", "a", "if", "some", "also", "with", "that", "just", "from", "have")


Trans <- file("MLB_Transaction_Tweets7", open = "a")
for(i in 1:nrow(Search_DF)){
  Tokens<-tokenizers::tokenize_words(
    MLB_data$text[i],stopwords = IgnoreThese,
    lowercase = TRUE,  strip_punct = TRUE, strip_numeric = TRUE,
    simplify = TRUE)
  cat(unlist(Tokens), "\n", file=Trans, sep=",")
}
close(Trans)


######### Read in the tweet transactions
TweetTrans <- read.transactions("MLB_Transaction_Tweets7",
                                rm.duplicates = FALSE,
                                format = "basket",
                                sep=","
                                ## cols =
)
inspect(TweetTrans)


## Read the transactions data into a dataframe
TweetDF <- read.csv("MLB_Transaction_Tweets7", 
                    header = FALSE, sep = ",")
head(TweetDF)
(str(TweetDF))

## Convert all columns to char 
TweetDF<-TweetDF %>%
  mutate_all(as.character)
(str(TweetDF))
# We can now remove certain words
TweetDF[TweetDF == "t.co"] <- ""
TweetDF[TweetDF == "rt"] <- ""
TweetDF[TweetDF == "http"] <- ""
TweetDF[TweetDF == "https"] <- ""


## Clean with grepl - every row in each column
MyDF<-NULL
MyDF2<-NULL
for (i in 1:ncol(TweetDF)){
  MyList=c() 
  MyList2=c() # each list is a column of logicals ...
  MyList=c(MyList,grepl("[[:digit:]]", TweetDF[[i]]))
  MyDF<-cbind(MyDF,MyList)  ## create a logical DF
  MyList2=c(MyList2,(nchar(TweetDF[[i]])<4 | nchar(TweetDF[[i]])>9))
  MyDF2<-cbind(MyDF2,MyList2) 
  
}

TweetDF[MyDF] <- ""
TweetDF[MyDF2] <- ""



# Now we save the dataframe using the write table command 
write.table(TweetDF, file = "UpdatedTweetFile.csv", col.names = FALSE, 
            row.names = FALSE, sep = ",")
TweetTrans <- read.transactions("UpdatedTweetFile.csv", sep =",", 
                                format("basket"),  rm.duplicates = TRUE)


############ Create the Rules  - Relationships ###########
TweetTrans_rules = arules::apriori(TweetTrans, 
                                   parameter = list(support=.01, conf=.15, minlen=2, maxlen=20))

##  Sort by Conf
SortedRules_conf <- sort(TweetTrans_rules, by="confidence", decreasing=TRUE)
## Sort by Sup
SortedRules_sup <- sort(TweetTrans_rules, by="support", decreasing=TRUE)
## Sort by Lift
SortedRules_lift <- sort(TweetTrans_rules, by="lift", decreasing=TRUE)

TweetTrans_rules_sup <-SortedRules_sup[1:50]
TweetTrans_rules_conf <- SortedRules_conf[1:50]
TweetTrans_rules_lift<- SortedRules_lift[1:50]

igraph_func_sup <- function(TweetTrans_rules){
  Rules_DF2<-DATAFRAME(TweetTrans_rules, separate = TRUE)
  ## Convert to char
  Rules_DF2$LHS<-as.character(Rules_DF2$LHS)
  Rules_DF2$RHS<-as.character(Rules_DF2$RHS)
  
  ## Remove all {}
  Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[{]', replacement='')
  Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[}]', replacement='')

  ## USING SUP
  Rules_S<-Rules_DF2[c(1,2,3)]
  names(Rules_S) <- c("SourceName", "TargetName", "Weight")
  head(Rules_S,30)
  
  edgeList<-Rules_S
  MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
  plot(MyGraph)
  
}
igraph_func_lift <- function(TweetTrans_rules){
  ## Convert the RULES to a DATAFRAME
  Rules_DF2<-DATAFRAME(TweetTrans_rules, separate = TRUE)

  ## Convert to char
  Rules_DF2$LHS<-as.character(Rules_DF2$LHS)
  Rules_DF2$RHS<-as.character(Rules_DF2$RHS)
  
  ## Remove all {}
  Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[{]', replacement='')
  Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[}]', replacement='')
  
  
  ## USING LIFT
  Rules_L<-Rules_DF2[c(1,2,5)]
  names(Rules_L) <- c("SourceName", "TargetName", "Weight")
  
  edgeList<-Rules_L
  # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
  MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
  plot(MyGraph)
  
}
igraph_func_conf <- function(TweetTrans_rules){
  ## Convert the RULES to a DATAFRAME
  Rules_DF2<-DATAFRAME(TweetTrans_rules, separate = TRUE)
  
  ## Convert to char
  Rules_DF2$LHS<-as.character(Rules_DF2$LHS)
  Rules_DF2$RHS<-as.character(Rules_DF2$RHS)
  
  ## Remove all {}
  Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[{]', replacement='')
  Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[}]', replacement='')
  
  
  ## USING CONF
  Rules_C<-Rules_DF2[c(1,2,4)]
  names(Rules_C) <- c("SourceName", "TargetName", "Weight")
  
  edgeList<-Rules_C
  # Create a graph. Use simplyfy to ensure that there are no duplicated edges or self loops
  MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
  plot(MyGraph)
  
}

igraph_func_sup(TweetTrans_rules_sup)
igraph_func_lift(TweetTrans_rules_lift)
igraph_func_conf(TweetTrans_rules_conf)

# NetworkD3
Rules_DF2<-DATAFRAME(TweetTrans_rules, separate = TRUE)
## Convert to char
Rules_DF2$LHS<-as.character(Rules_DF2$LHS)
Rules_DF2$RHS<-as.character(Rules_DF2$RHS)

## Remove all {}
Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[{]', replacement='')
Rules_DF2[] <- lapply(Rules_DF2, gsub, pattern='[}]', replacement='')

# functions to call NetworkD3 for lift, support, and confidence

############################### BUILD THE NODES & EDGES ####################################
(edgeList<-Rules_Sup)
MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))

nodeList <- data.frame(ID = c(0:(igraph::vcount(MyGraph) - 1)), 
                       # because networkD3 library requires IDs to start at 0
                       nName = igraph::V(MyGraph)$name)
## Node Degree
(nodeList <- cbind(nodeList, nodeDegree=igraph::degree(MyGraph, 
                                                       v = igraph::V(MyGraph), mode = "all")))

## Betweenness
BetweenNess <- igraph::betweenness(MyGraph, 
                                   v = igraph::V(MyGraph), 
                                   directed = TRUE) 

(nodeList <- cbind(nodeList, nodeBetweenness=BetweenNess))

## This can change the BetweenNess value if needed
BetweenNess<-BetweenNess/100



## For scaling...divide by 
## RE:https://en.wikipedia.org/wiki/Betweenness_centrality
##/ ((igraph::vcount(MyGraph) - 1) * (igraph::vcount(MyGraph)-2))
## For undirected / 2)
## Min-Max Normalization
##BetweenNess.norm <- (BetweenNess - min(BetweenNess))/(max(BetweenNess) - min(BetweenNess))


## Node Degree


###################################################################################
########## BUILD THE EDGES #####################################################
#############################################################
# Recall that ... 
# edgeList<-Rules_Sup
getNodeID <- function(x){
  which(x == igraph::V(MyGraph)$name) - 1  #IDs start at 0
}
(getNodeID("elephants")) 

edgeList <- plyr::ddply(
  Rules_Sup, .variables = c("SourceName", "TargetName" , "Weight"), 
  function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                          TargetID = getNodeID(x$TargetName)))

head(edgeList)
nrow(edgeList)

##################################################################################
##################   color #################################################
######################################################
COLOR_P <- colorRampPalette(c("#00FF00", "#FF0000"), 
                            bias = nrow(edgeList), space = "rgb", 
                            interpolate = "linear")
COLOR_P
(colCodes <- COLOR_P(length(unique(edgeList$diceSim))))
edges_col <- sapply(edgeList$diceSim, 
                    function(x) colCodes[which(sort(unique(edgeList$diceSim)) == x)])
nrow(edges_col)

## NetworkD3 Object
#https://www.rdocumentation.org/packages/networkD3/versions/0.4/topics/forceNetwork

D3_network_Tweets <- networkD3::forceNetwork(
  Links = edgeList, # data frame that contains info about edges
  Nodes = nodeList, # data frame that contains info about nodes
  Source = "SourceID", # ID of source node 
  Target = "TargetID", # ID of target node
  Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
  NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
  Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
  Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
  height = 700, # Size of the plot (vertical)
  width = 900,  # Size of the plot (horizontal)
  fontSize = 20, # Font size
  linkDistance = networkD3::JS("function(d) { return d.value*10; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
  linkWidth = networkD3::JS("function(d) { return d.value/10; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
  opacity = 0.9, # opacity
  zoom = TRUE, # ability to zoom when click on the node
  opacityNoHover = 0.9, # opacity of labels when static
  linkColour = "red"   ###"edges_col"red"# edge colors
) 

# Plot network
D3_network_Tweets

# Save network as html file
networkD3::saveNetwork(D3_network_Tweets, 
                       "NetD3_DCR2019_worldNewsL.html", selfcontained = TRUE)




NetworkD3_func_lift <- function(Rules_DF2){
  Rules_L<-Rules_DF2[c(1,2,5)]
  names(Rules_L) <- c("SourceName", "TargetName", "Weight")

  (edgeList<-Rules_L)
  MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
  
  nodeList <- data.frame(ID = c(0:(igraph::vcount(MyGraph) - 1)), 
                         # because networkD3 library requires IDs to start at 0
                         nName = igraph::V(MyGraph)$name)
  ## Node Degree
  (nodeList <- cbind(nodeList, nodeDegree=igraph::degree(MyGraph, 
                                                         v = igraph::V(MyGraph), mode = "all")))
  
  ## Betweenness
  BetweenNess <- igraph::betweenness(MyGraph, 
                                     v = igraph::V(MyGraph), 
                                     directed = TRUE) 
  
  (nodeList <- cbind(nodeList, nodeBetweenness=BetweenNess))
  
  ## This can change the BetweenNess value if needed
  BetweenNess<-BetweenNess/100

  getNodeID <- function(x){
    which(x == igraph::V(MyGraph)$name) - 1  #IDs start at 0
  }
  (getNodeID("elephants")) 
  
  edgeList <- plyr::ddply(
    Rules_Sup, .variables = c("SourceName", "TargetName" , "Weight"), 
    function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                            TargetID = getNodeID(x$TargetName)))
  
  head(edgeList)
  nrow(edgeList)
  
  library(networkD3)
  
  D3_network_Tweets <- networkD3::forceNetwork(
    Links = edgeList, # data frame that contains info about edges
    Nodes = nodeList, # data frame that contains info about nodes
    Source = "SourceID", # ID of source node 
    Target = "TargetID", # ID of target node
    Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
    NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
    Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
    Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
    height = 1000, # Size of the plot (vertical)
    width = 1000,  # Size of the plot (horizontal)
    fontSize = 20, # Font size
    linkDistance = networkD3::JS("function(d) { return d.value*10; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
    linkWidth = networkD3::JS("function(d) { return d.value/10; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
    opacity = 0.9, # opacity
    zoom = TRUE, # ability to zoom when click on the node
    opacityNoHover = 0.9, # opacity of labels when static
    linkColour = "red"   ###"edges_col"red"# edge colors
  ) 
  
  # Plot network
  D3_network_Tweets
  
}
NetworkD3_func_sup <- function(Rules_DF2){
  Rules_S<-Rules_DF2[c(1,2,3)]
  names(Rules_S) <- c("SourceName", "TargetName", "Weight")
  
  (edgeList<-Rules_S)
  MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
  
  nodeList <- data.frame(ID = c(0:(igraph::vcount(MyGraph) - 1)), 
                         # because networkD3 library requires IDs to start at 0
                         nName = igraph::V(MyGraph)$name)
  ## Node Degree
  (nodeList <- cbind(nodeList, nodeDegree=igraph::degree(MyGraph, 
                                                         v = igraph::V(MyGraph), mode = "all")))
  
  ## Betweenness
  BetweenNess <- igraph::betweenness(MyGraph, 
                                     v = igraph::V(MyGraph), 
                                     directed = TRUE) 
  
  (nodeList <- cbind(nodeList, nodeBetweenness=BetweenNess))
  
  ## This can change the BetweenNess value if needed
  BetweenNess<-BetweenNess/100
  
  getNodeID <- function(x){
    which(x == igraph::V(MyGraph)$name) - 1  #IDs start at 0
  }
  (getNodeID("elephants")) 
  
  edgeList <- plyr::ddply(
    Rules_Sup, .variables = c("SourceName", "TargetName" , "Weight"), 
    function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                            TargetID = getNodeID(x$TargetName)))
  
  head(edgeList)
  nrow(edgeList)
  
  library(networkD3)
  
  D3_network_Tweets <- networkD3::forceNetwork(
    Links = edgeList, # data frame that contains info about edges
    Nodes = nodeList, # data frame that contains info about nodes
    Source = "SourceID", # ID of source node 
    Target = "TargetID", # ID of target node
    Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
    NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
    Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
    Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
    height = 1000, # Size of the plot (vertical)
    width = 1000,  # Size of the plot (horizontal)
    fontSize = 20, # Font size
    linkDistance = networkD3::JS("function(d) { return d.value*10; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
    linkWidth = networkD3::JS("function(d) { return d.value/10; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
    opacity = 0.9, # opacity
    zoom = TRUE, # ability to zoom when click on the node
    opacityNoHover = 0.9, # opacity of labels when static
    linkColour = "red"   ###"edges_col"red"# edge colors
  ) 
  
  # Plot network
  D3_network_Tweets
  
}
NetworkD3_func_conf <- function(Rules_DF2){
  Rules_C<-Rules_DF2[c(1,2,4)]
  names(Rules_C) <- c("SourceName", "TargetName", "Weight")
  
  (edgeList<-Rules_C)
  MyGraph <- igraph::simplify(igraph::graph.data.frame(edgeList, directed=TRUE))
  
  nodeList <- data.frame(ID = c(0:(igraph::vcount(MyGraph) - 1)), 
                         # because networkD3 library requires IDs to start at 0
                         nName = igraph::V(MyGraph)$name)
  ## Node Degree
  (nodeList <- cbind(nodeList, nodeDegree=igraph::degree(MyGraph, 
                                                         v = igraph::V(MyGraph), mode = "all")))
  
  ## Betweenness
  BetweenNess <- igraph::betweenness(MyGraph, 
                                     v = igraph::V(MyGraph), 
                                     directed = TRUE) 
  
  (nodeList <- cbind(nodeList, nodeBetweenness=BetweenNess))
  
  ## This can change the BetweenNess value if needed
  BetweenNess<-BetweenNess/100
  
  getNodeID <- function(x){
    which(x == igraph::V(MyGraph)$name) - 1  #IDs start at 0
  }
  (getNodeID("elephants")) 
  
  edgeList <- plyr::ddply(
    Rules_Sup, .variables = c("SourceName", "TargetName" , "Weight"), 
    function (x) data.frame(SourceID = getNodeID(x$SourceName), 
                            TargetID = getNodeID(x$TargetName)))
  
  head(edgeList)
  nrow(edgeList)
  
  library(networkD3)
  
  D3_network_Tweets <- networkD3::forceNetwork(
    Links = edgeList, # data frame that contains info about edges
    Nodes = nodeList, # data frame that contains info about nodes
    Source = "SourceID", # ID of source node 
    Target = "TargetID", # ID of target node
    Value = "Weight", # value from the edge list (data frame) that will be used to value/weight relationship amongst nodes
    NodeID = "nName", # value from the node list (data frame) that contains node description we want to use (e.g., node name)
    Nodesize = "nodeBetweenness",  # value from the node list (data frame) that contains value we want to use for a node size
    Group = "nodeDegree",  # value from the node list (data frame) that contains value we want to use for node color
    height = 700, # Size of the plot (vertical)
    width = 500,  # Size of the plot (horizontal)
    fontSize = 20, # Font size
    linkDistance = networkD3::JS("function(d) { return d.value*10; }"), # Function to determine distance between any two nodes, uses variables already defined in forceNetwork function (not variables from a data frame)
    linkWidth = networkD3::JS("function(d) { return d.value/10; }"),# Function to determine link/edge thickness, uses variables already defined in forceNetwork function (not variables from a data frame)
    opacity = 0.9, # opacity
    zoom = TRUE, # ability to zoom when click on the node
    opacityNoHover = 0.9, # opacity of labels when static
    linkColour = "red"   ###"edges_col"red"# edge colors
  ) 
  
  # Plot network
  D3_network_Tweets
  
}

NetworkD3_func_lift(Rules_DF2)
NetworkD3_func_sup(Rules_DF2)
NetworkD3_func_conf(Rules_DF2)
