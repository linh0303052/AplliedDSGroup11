rm(list=ls())
library(data.table)


#Load Base Datasets
train <- read.csv( 'data/input/train.csv' )
test  <- read.csv( 'data/input/test.csv' )
table(train$TARGET)
dim(train)
dim(test)


#AUC function
AUC <-function (actual, predicted) {
  r <- as.numeric(rank(predicted))
  n_pos <- as.numeric(sum(actual == 1))
  n_neg <- as.numeric(length(actual) - n_pos)
  auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1)/2)/(n_pos *  n_neg)
  auc
}



#Load Marios Models
tmp1 <- fread('train_final.csv')
tmp2 <- fread('test_final.csv')
tmp1$ID <- train$ID
tmp2$ID <- test$ID
model1 <- rbind( tmp1, tmp2  )


#Merge all models by ID
raw <- data.table( ID=c(train$ID, test$ID)  )
raw <- merge(    raw, model1, by="ID", sort=F )


#Split Train and Test
tr <- data.table( ID=train$ID )
tr <- raw[ raw$ID %in% train$ID  ]
ts <- raw[ raw$ID %in% test$ID  ]
tr[, ID:=NULL ]
ts[, ID:=NULL ]
target <- train$TARGET


#Rank Train and Test
for( i in 1:ncol(tr) ){
  tr[[i]] <- rank(tr[[i]], ties.method = "average")
  ts[[i]] <- rank(ts[[i]], ties.method = "average")
}



#turn Matrix
tr <- as.matrix(tr)
ts <- as.matrix(ts)



#Override Age < 23 as ZERO
tr[ train$var15<23 ,  ] <- 0
ts[ test$var15<23 ,  ] <- 0




#Optim transform function
fn.optim.sub <- function( mat, pars ){
  as.numeric( rowSums( mat * matrix( pars, nrow=nrow(mat) , ncol=ncol(mat), byrow=T ) ) )
}


#Optim evaluation maximization function
fn.optim <- function( pars ){
  AUC( target , fn.optim.sub( tr , pars ) )
}



#Bag optim 3 times using random initial Weigths
set.seed(2)
initial_w <- rep(1/ncol(tr),ncol(tr) ) + runif( ncol(tr) ,-0.005,0.005 )
opt1 <- optim( par=initial_w , fn.optim, control = list(maxit=3333, trace=T, fnscale = -1)   )

set.seed(3)
initial_w <- rep(1/ncol(tr),ncol(tr) ) + runif( ncol(tr) ,-0.005,0.005 )
opt2 <- optim( par=initial_w , fn.optim, control = list(maxit=3333, trace=T, fnscale = -1)   )

set.seed(1234)
initial_w <- rep(1/ncol(tr),ncol(tr) )
opt3 <- optim( par=initial_w , fn.optim, control = list(maxit=3333, trace=T, fnscale = -1)   )




#Show AUC
AUC( target , fn.optim.sub( tr , opt1$par ) )
print( data.frame( colnames(tr) , opt1$par ) )

AUC( target , fn.optim.sub( tr , opt2$par ) )
print( data.frame( colnames(tr) , opt2$par ) )

AUC( target , fn.optim.sub( tr , opt3$par ) )
print( data.frame( colnames(tr) , opt3$par ) )

tmp <-       rank( fn.optim.sub( tr, opt1$par ) )
tmp <- tmp + rank( fn.optim.sub( tr, opt2$par ) )
tmp <- tmp + rank( fn.optim.sub( tr, opt3$par ) )
print( AUC( target , tmp ) )




#Calcule predictions of TestSet
tmp <-       rank( fn.optim.sub( ts, opt1$par ) )
tmp <- tmp + rank( fn.optim.sub( ts, opt2$par ) )
tmp <- tmp + rank( fn.optim.sub( ts, opt3$par ) )




#Build Submission File
sub  <- data.frame( ID=test$ID, TARGET = tmp/max(tmp) )
hist(sub$TARGET,1000)
summary( sub$TARGET  )
write.table( sub, 'data/final.submission.1.csv', row.names=F, quote=F, sep=','  )

