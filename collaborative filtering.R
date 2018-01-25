############ COLLABORATIVE FILTERING ################
install.packages("NMF")

library('NMF')

x1 = c(5,4,1,1)
x2 = c(4,5,1,1)
x3 = c(1,1,5,5)
x4 = c(1,1,4,5)
x5 = c(1,1,5,4)

R = as.matrix(rbind(x1,x2,x3,x4,x5))
set.seed(1234)

res = nmf(R, 3, "lee")   # lee & seung method

V.hat = fitted(res)
print(V.hat)     # estimated target matrix

w = basis(res)    # W user feature matrix
dim(w)   # n x r (n=5, r=4)
print(w)

h = coef(res)    # H movie feature matrix
dim(h)  # r X p (r=4, p=4)
print(h)

#recommender system via clustering based on vectors in H
movies =  data.frame(t(h))
movies
features = cbind(movies$X1, movies$X2)
features
plot(features)
title("Movie Feature Plot")

#########################################################################################################
### Recommender lab###
#require(devtools)
devtools::source_gist("4676064", filename = "as.data.frame.list.R")
#source_gist("http://gist)

library('recommenderlab')
library('ggplot2')
library('reshape2')

# Read the training file along with header
tr = read.csv("D://machine learning/train_v2.csv", header = T)
head(tr)

# remove the ID column
tr = tr[,-1]
tr[tr$user==1,]

g = acast(tr, user~movie)
# chk the class of g
class(g)

# convert it as a matrix
R = as.matrix(g)

# convert it to a realRatingMatrix data structure
# realratingmatrix is a recommenderlab sparse-matrix like data structure
r = as(R, "realRatingMatrix")

# view r in other possible ways
as(r, "list")  # a list
as(r, "matrix") # a sparse matrix
# i can turn it to dataframe
head(as(r, "dataframe"))


# Create a recommender object(model)
#   They pertain to four different algorithms
#   UBCF : User based collaborative filtering
#   IBCF : Item based collaborative filtering
# Parameter 'method' decides similarity measure
#    Cosine or Jaccard

rec1 = Recommender(r[1:nrow(r)], method = 'UBCF', param=list(normalize = "Z-score",
                                                             method = "Cosine",nn=5))

rec2 = Recommender(r[1:nrow(r)], method = 'UBCF', param=list(normalize = "Z-score",
                                                             method = "Jaccard",nn=5))

rec = Recommender(r[1:nrow(r)], method = "POPULAR")

# Depending upon your selection , examine what you got
print(rec2)
names(getModel(rec2))
getModel(rec2)


#####################################################################################################
###########   MODEL IN ACTION #############


recommended.items.u1022 = predict(rec2, r["1022",], n=5)

# to display them
as(recommended.items.u1022, "list")

# to obtain the top 3
recommended.items.u1022.top3 = bestN(recommended.items.u1022, n=3)

# to display them
as(recommended.items.u1022.top3, "list")

#############################################################################################

############## Create prediction for same user #################
# to predict affinity to all rated items
recom = predict(rec2, r["1022",], type = "ratings")
recom

# Convert all your recommendations to list structure
rec_list = as(recom,"list")
head(summary(rec_list))

# Access this list User 1, item at index 2
rec_list[[1]][2]

# Convert to data frame all recommendations for user 1
u1 = as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
View(u1)
# Create a column by name of id in data frame u1 and populate it with row names
u1$id = row.names(u1)

# Now access movie ratings in column 2 for u1
u1[u1$id==39,1]







###################################### EVALUATION ########################
e = evaluationScheme(r[1:100], method="split", train=0.7, given=15, goodRating=4)
e

algorithms = list(
  "random items" = list(name="Random"),
  "popular items" = list(name="POPULAR"),
  "user-based CF" = list(name="UBCF"),
  "svd" = list(name="SVD")
)

results = evaluate(e, algorithms, n=c(1,3,5,10,15,20,25))
plot(results, annotate = 1:4, legend="topleft")

##################################################################

#test = read.csv("D://machine learning/test_v2.csv", header = T)
#head(test)
