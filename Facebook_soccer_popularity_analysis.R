install.packages("Rfacebook")
library("Rfacebook")
token <- "EAACEdEose0cBAIuJ4RIFuC2S8HWybghTi5ST0gN8ZBvjw5jGBrtta8gjMnWoA3ZASc5EFzOsgEveJPeZBS8rZCQvbWhOg0nNKIzXvCB1xeKrjWnk8ZAT11tsMJPh5sZAc8D4wzXwqb5QZAOZA1GvKPdGh6irjlYWJZAhmRISAUH4aIAZDZD"


me <- getUsers("me", token=token)


me$name
?getPage
page_ronaldo <- getPage("Cristiano", token, n = 200)
page_messi <- getPage("LeoMessi", token, n = 200)

?getPost
?getUsers
post_id <- head(page$id, n = 1)
post <- getPost(post_id, token, n = 1000, likes = TRUE, comments = TRUE)
post_1_ronaldo <- getPost(head(page_ronaldo$id, n = 1), token, n = 1000, likes = TRUE, comments = TRUE)
post_1_messi <- getPost(head(page_messi$id, n = 1), token, n = 1000, likes = TRUE, comments = TRUE)
post[[1]]
a<- post_1_ronaldo[[3]]
post_1_messi[[1]]

a_subset <- a[,"from_id"]
ronaldo_comments_user_details <- getUsers(a_subset, token=token)


# Now, you have facebook connected to your R session for the next 2 hours.
?getFriends
my_friends <- getFriends(token, simplify=TRUE)
my_friends_info <- getUsers(my_friends$id, token=token, private_info=TRUE)

table(my_friends_info$location)

California_resident <- my_friends_info[grep("California", my_friends_info$location), ]$name


# 
# install.packages("httpuv")
# library(Rfacebook)
# library(httpuv)
# #fbOAuth(app_id, app_secret, extended_permissions = TRUE)
# fb_oauth=fbOAuth("709956495823279", "f5a0098433e6f6eb3b0a77acf5f9eedc", extended_permissions = TRUE)
# 
# #fb_oauth <- fbOAuth(app_id="123456789", app_secret="1A2B3C4D")
# 
# setwd("C:/Users/Abheek/study materials/Abheek_Assignment/R Final Modules/Text Mining and sentiment analysis/Facebook analysis")
# save(fb_oauth, file="fb_oauth")
# getwd()
# dir()
# 
# library(Rfacebook)
# library(httpuv)
# library(RColorBrewer)
# 
# load("fb_oauth")
# me <- getUsers("me", token=fb_oauth)
# #getFQL(query, token)
# my_friends <- getFriends(token=fb_oauth, simplify=F)
# str(my_friends)

my_friends_info <- getUsers(my_friends$id, token=token, private_info=TRUE)

table(my_friends_info$location)
table(my_friends_info$relationship_status)
pie(table(my_friends_info$relationship_status),col=brewer.pal(5, "Set1"))

table(my_friends_info$location)
pie(table(my_friends_info$location),col=brewer.pal(20, "Greens"))
pie(table(my_friends_info$locale),col=brewer.pal(4, "Blues"))
pie(table(my_friends_info$gender),col=brewer.pal(3, "Oranges"))

load("fb_oauth")

mat <- getNetwork(my_friends$id, token=token, format="adj.matrix")

library(igraph)

network <- graph.adjacency(mat, mode="undirected")
getwd()

pdf("network_plot.pdf")

plot(network ,vertex.size=5, 
     vertex.label=NA, 
     vertex.label.cex=0.45,
     edge.arrow.size=1,
     edge.curved=TRUE,)

dev.off()
