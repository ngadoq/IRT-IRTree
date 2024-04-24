
###########################################################################
## Item response tree modeling 
## See item_response_trees.pdf for four tree diagrams 
###########################################################################

library(flirt)


#################################
## Verbal aggression data 
## Three-point scale (Figure 1)
################################

data(verb3)

# expand the data based on the tree structure 
# use dendrify2 function 
mapping <- cbind(c(0, 1, 1), c(NA, 0, 1))
wide <- dendrify2(verb3, mapping, wide=T)         


# multidimensional model 
model01 <- flirt(data=wide[,-1], loading=list(on=T, inside=F), 
          mul=list(on=T, dim_info=list(dim1=1:24, dim2=c(25:48) )) , 
          control=list(nq=5) )


# standardized loading and covariance matrix              
est_alp <- model01@pars[1:48,1]  # vector of alpha estimates 
est_cov <- model01@pars[99:101,1]  # vector of covariance matrix estimates 
cov_matrix <- matrix(c(est_cov[1]^2,est_cov[3],est_cov[3] ,est_cov[2]^2),2,2, byrow=F) # covariance matrix 
dim_info <- list(dim1=1:24, dim2=c(25:48) ) # list 

test0 <- std_coef(est=est_alp, dim_info=dim_info, cov_matrix= cov_matrix) 

# correlation  
test0$cor_mat


##################################
## science  data 
## Four-point scale (Figure 2)
##################################

data(science)

## choose items in dim 1 (1,3,4,7)
science1 <- science[, c(1,3,4,7)]


## Tree 2 : Linear tree 
mapping3 <- as.matrix(cbind(c(0, 1, 1, 1), c(NA,0, 1, 1), c(NA, NA, 0,1)))
wide3 <- dendrify2(science1 , mapping3, wide=T)           
          
          
# multidimensional model 
model21 <- flirt(data=wide3[,-1], loading=list(on=T, inside=F), 
          mul=list(on=T, dim_info=list(dim1=1:4, dim2=5:8, dim3=9:12 )) , 
          control=list(nq=5) )

# standardized loading and covariance matrix              
est_alp <- model21@pars[1:12,1]  # vector of alpha estimates 
est_cov <- model21@pars[28:33,1]  # vector of covariance matrix estimates 
cov_matrix <- matrix(c(est_cov[1]^2,est_cov[4],est_cov[5],est_cov[4], est_cov[2]^2, 
est_cov[6], est_cov[5], est_cov[6], est_cov[3]^2),3,3, byrow=F) # covariance matrix 
dim_info <- list(dim1=1:4, dim2=5:8, dim3=9:12) # list 

test1 <- std_coef(est=est_alp, dim_info=dim_info, cov_matrix= cov_matrix) 

# correlation  
test1$cor_mat
 
# comment: 
# here the negative correlation with dimension 2 is due to the negative loadings for dimension 2 
# we interpret the correlation to be positive 
 
 
###############################################
## charity data with missing data  
## ordinal tree (Figure 3)
###############################################

data(charity)

# recode missing values to -1 (or something else)

charity[is.na(charity)] <- -1


mapping4 <- as.matrix(cbind(c(0, 1, 1, 1,1), c(NA, 1,2, 3, 4)))
wide4 <- dendrify2(charity, mapping4, wide=T)  


# recode the second set of sub-items (node 2 item 1 to 5) to 0,1,2,3

wide4[,7:11] <- wide4[,7:11] -1 

   
# multidimensional model 
#(dimension 1: binary data, dimension 2: ordinal data with graded response model)
model31 <- flirt(data=wide4[,-1], loading=list(on=T, inside=F), 
          mul=list(on=T, dim_info=list(dim1=1:5, dim2=6:10 )) , 
          control=list(nq=5,  link="adjacent") )
# covairance           
est <- model31@pars[,1] 

cov_mat <- matrix(c(est[33]^2,est[35], est[35], est[34]^2), 2,2)      
# correlation 
std_cov(cov_mat, dim_info=list(dim1=1:5, dim2=6:10 ))



###############################################
## charity data with missing data  
## binary tree (Figure 4)
###############################################

mapping5 <- as.matrix(cbind(c(0, 1, 1, 1,1), c(NA, 0,0, 1, 1), c(NA, 1, 0,NA,NA), 
    c(NA, NA, NA, 0,1)))
wide5 <- dendrify2(charity, mapping5, wide=T) 


# multidimensional model 
model41 <- flirt(data=wide5[,-1], loading=list(on=T, inside=F), 
          mul=list(on=T, dim_info=list(dim1=1:5, dim2=6:10, dim3=11:15, dim4=16:20 )) , 
          control=list(nq=5) )


est_alp <- model41@pars[1:20,1]  # vector of alpha estimates 
est_cov <- model41@pars[45:54,1]  # vector of covariance matrix estimates 
cov_matrix <- matrix(c(est_cov[1]^2,est_cov[5],est_cov[6],est_cov[7], est_cov[5], est_cov[2]^2, 
est_cov[8], est_cov[9], est_cov[6], est_cov[8],est_cov[3]^2,est_cov[10],est_cov[7],est_cov[9],est_cov[10],
est_cov[4]^2),4,4, byrow=F) # covariance matrix 
dim_info <- list(dim1=1:5, dim2=6:10, dim3=11:15, dim4=16:20 ) # list 

test2 <- std_coef(est=est_alp, dim_info=dim_info, cov_matrix= cov_matrix) 
# correlation  
test2$cor_mat
