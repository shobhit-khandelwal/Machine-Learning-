movies <- read.table("u.item", sep = "|", header = FALSE, stringsAsFactors = FALSE, quote="")
movies <- movies[,c(1,2)]
names(movies) <- c("movieid","movie")

rank   <- read.table("u.data", sep = "\t", header = FALSE, stringsAsFactors = FALSE, col.names = c("userid","movieid","rating","ts"))

critics <- merge(movies, rank, by = "movieid")
critics$movie <- NULL
critics$ts <- NULL
names(critics) <- c("movieid","person","rank")
N<-length(critics[,1])
M<-length(movies[,1])

# build dissimilarity matrices manually, using critics
# this can take a little bit of time, so the resulting 
# matrices are attached in a separate data file
dissim_gower<-matrix(NA,M,M)
dissim_eucld<-matrix(NA,M,M)
c<-0
for (i in 1:(M-1))
{
  for (j in (i+1):M) 
  {
    
    idx_i<-critics[,1]==i # select movie i
    idx_j<-critics[,1]==j # select movie j
    sel_cr<-rbind(critics[idx_i,2:3],critics[idx_j,2:3]) # critics that rated either one of two movies and their ratings
    sel_ord<-sel_cr[order(sel_cr[,1]),] # sort selected critics by person id
    same_c<-diff(c(0,sel_ord[,1]))==0 # find critics that rated both movies
    L<-sum(same_c)
    if (L>0)
    {
      same_c_prev<-c(same_c[2:length(same_c)],FALSE)
      dissim_gower[i,j]<-sum(sel_ord[same_c,2]!=sel_ord[same_c_prev,2])/L
      dissim_eucld[i,j]<-sqrt(sum((sel_ord[same_c,2]-sel_ord[same_c_prev,2])^2))
    }
    
    c<-c+1
    if (c %% 10000 ==0)
    {
      cat("Progress: ",as.character(round(c/(M*(M-1)/2)*10000)/100)," percent\n") 
    }
  }
}

# dissimilarities have zero on the diagonal

for (i in 1:M)
{
  dissim_gower[i,i]<-0
  dissim_eucld[i,i]<-0
}

# dissimilarities are symmetric

for (i in 2:M)
{
  for (j in 1:(i-1)) 
  {
    dissim_gower[i,j]<-dissim_gower[j,i]
    dissim_eucld[i,j]<-dissim_eucld[j,i]
  }
}

# > sum(is.na(dissim_gower))/1682/1682
# [1] 0.3043451
# 30% of dissimilarities could not be computed (no overlap between users)
# replace NA dissimilarities with averages
dissim_gower[is.na(dissim_gower)]<-mean(dissim_gower, na.rm=T)
dissim_eucld[is.na(dissim_eucld)]<-mean(dissim_eucld, na.rm=T)

K_clust<-5
# assign movies to K_clust clusters using k-mediods
med_g<-pam(dissim_gower,k=K_clust,diss=T)
clust_g<-med_g$clustering
med_e<-pam(dissim_eucld,k=K_clust,diss=T)
clust_e<-med_e$clustering
# compute multidimensional scaling of movies onto R^3
proj_g<-cmdscale(dissim_gower,3)
proj_e<-cmdscale(dissim_eucld,3)

cols<-451:471 # some colors for plotting

# a trivial function that plots points by cluster - will be used multiple times
plot_2dc <- function(coords,clust,cols,K){
  k<-1
  idx<-clust==k
  plot(coords[idx,],
       xlim=c(min(coords[,1]),max(coords[,1])),
       ylim=c(min(coords[,2]),max(coords[,2])),
       xlab="",
       ylab="",
       col=cols[k])
  for (k in 2:K)
  {
    idx<-clust==k
    points(coords[idx,],col=cols[k])
  }
}

plot_2dc(proj_g[,1:2],clust_g,cols,K_clust)
plot_2dc(proj_e[,1:2],clust_g,cols,K_clust)
plot_2dc(proj_g[,2:3],clust_g,cols,K_clust)
plot_2dc(proj_e[,2:3],clust_g,cols,K_clust)


for (k in 1:K_clust)
{
  idx<-clust==k
  scatterplot3d(coords[idx,],
                xlim=c(min(coords[,1]),max(coords[,1])),
                ylim=c(min(coords[,2]),max(coords[,2])),
                zlim=c(min(coords[,3]),max(coords[,3])),
                color=cols[k])
}



#Question  1


xcan<-t(read.table("xcancer.dat"))
ycan<-t(read.table("ycancer.dat"))

xcan<-apply(xcan,2,function(x) pmax(pmin(x,16000),20))
gen_min<-apply(xcan,2,function(x) min(x))
gen_max<-apply(xcan,2,function(x) max(x))

gen_idx<-(gen_max/gen_min>5)&(gen_max-gen_min>500)

xcan<-xcan[,gen_idx]
xcan<-apply(xcan,2,function(x) (x-mean(x))/sd(x))

N<-length(ycan)


M<-length(xcan[1,])

K<-14


# define a Z_mtx function for using in comparingnclustering assignments
Z_mtx <- function(array){
  N<-length(array)
  Z<-matrix(0,nrow=N,ncol=N)
  for (i in 1:(N-1))
  {
    for(j in (i+1):N)
    {
      Z[i,j]=array[i]==array[j]
    }
  }
  return(Z)
}

Y<-Z_mtx(ycan)
I<-matrix(1,N,N)



# compute 14 centers based on true labels 
# test if they are stable (apply kmeans using these centers)

centers<-matrix(0,K,M)
for (k in 1:K)
{
  centers[k,]<-apply(xcan[ycan==k,],2,mean)
}
kres_stb<-kmeans(xcan, centers, iter.max=10)

klab_stb<-kres_stb$cluster

Y2<-Z_mtx(klab_stb)

rho_stb<-sum(Y*(I-Y2)+(I-Y)*Y2)


# run kmeans 10 times using random starting points
# and find the "best" one
rho_min<-rho_stb # initialize minimal rho with some point
kres_min<-kres_stb
for (trial in 1:10)
{
  kres<-kmeans(xcan, 14, iter.max = 10, nstart = 14)
  klab<-kres$cluster
  Z<-Z_mtx(klab)
  rho<-sum(Y*(I-Z)+(I-Y)*Z)
  
  if(rho<rho_min)
  {
    rho_min<-rho
    kres_min<-kres
  }
}


# run kernel kmeans 5 times
rhok_min<-Inf #initialize the best rho error for kernel kmeans
for (trial in 1:5)
{
  kkres<-kkmeans(xcan, 14, kernel = "rbfdot") # run kernel kmeans
  kklab<-kkres@.Data
  Z<-Z_mtx(kklab)
  rho<-sum(Y*(I-Z)+(I-Y)*Z)  
  if(rho<rhok_min)
  {
    rhok_min<-rho
    kkres_min<-kkres
  }
}

# results:
rho_min
# [1] 3008
rhok_min
# [1] 2697
# conclusion: kernel kmeans has better performance on this data
