# install.packages("stringr", repos='http://cran.us.r-project.org')
args<-commandArgs(TRUE)
library("stringr")

if(length(args) <= 0){
	print("Nao foi possivel executar o script")
	stop("modo de uso: \n\t\t RScript nameFile.R sequencia1 sequencia2\n", call.=FALSE)
}

#match <- 2
#mismatch <- -1
#d <- -2

match <- 5
mismatch <- -2
d <- -6

scoreAlinhamento <- 0

#seq1 <- "AATCCCAG"
#seq2 <- "ACTAACG"

seq1 <- args[1]
seq2 <- args[2]

#acrescenta '0' na sequencia
paste("0", seq1, sep="")
paste("0", seq2, sep="")

i <- nchar(seq1)
j <- nchar(seq2) 

#inicializa a matriz com 0
matz <- matrix(0:0, nrow = nchar(seq1)+1, ncol = nchar(seq2)+1)

tamSeq1 <- nchar(seq1)
tamSeq2 <- nchar(seq2)

for (i in 1:tamSeq1){
  matz[i+1,1] <- d*i
}

for (i in 1:tamSeq2){
  matz[1,i+1] <- d*i
}

for (i in 1:tamSeq1+1){
  for (j in 1:tamSeq2+1){
    if(str_sub(seq1,i+1,i+1) == str_sub(seq2,j+1,j+1)){
      eq <- match
    }
    else{
      eq <- mismatch
    }
    
    s1 <- matz[i-1,j-1] + eq
    s2 <- matz[i-1,j] + d
    s3 <- matz[i, j-1] + d
    
    matz[i,j] <- max(s1, s2, s3)
  }
}

## ate aqui está correto (montagem de matriz)

seq1Result <- ""
seq2Result <- ""
i <- nchar(seq1)
j <- nchar(seq2) 


while (i > 1 && j >1){
  sc <- matz[i-1,j-1]
  cat("F1: ", str_sub(seq1,i,i), " F2: ", str_sub(seq2,j,j), "\n")
  if (str_sub(seq1,i,i) == str_sub(seq2,j,j)){
    sc <- sc + match
  } else{
    sc <- sc + mismatch
  }
  
  if(sc == matz[i-1,j-1]){
    seq1Result <- c(str_sub(seq1,i,i), seq1Result)
    seq2Result <- c(str_sub(seq2,j,j), seq2Result)
    i <- i -1
    j <- j-1
    next
  }
  
  if ((matz[i-1,j] + d) == matz[i,j]) {
    seq1Result <- c(str_sub(seq1,i,i), seq1Result)
    seq2Result <- c("-", seq2Result)
    i <- i-1
    next
  }
  
  if ((matz[i,j-1] + d) == matz[i,j]) {
    seq1Result <- c("-", seq1Result)
    seq2Result <- c(str_sub(seq2,j,j), seq2Result)
    j <- j-1
    next
  }
}

cat ("Seq1: ", seq1Result, "\n")
cat ("Seq2: ", seq2Result, "\n\n")

scoreAlinhamento <- matz[nchar(seq1)+1,nchar(seq2)+1]
print(paste('resultado: ', scoreAlinhamento))


