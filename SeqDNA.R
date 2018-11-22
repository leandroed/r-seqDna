library(stringr)
match <- 2
mismatch <- -1
d <- -2
scoreAlinhamento <- 0

seq1 <- "AATCCCAG"
seq2 <- "ACTAACG"

matz <- matrix(0:0, nrow = nchar(seq1)+1, ncol = nchar(seq2)+1)

for (i in 1:nchar(seq1)+1){
  matz[i,1] <- d*i
}
for (i in 1:nchar(seq2)+1){
  matz[1,i] <- d*i
}

for (i in 1:nchar(seq1)+1){
  for (j in 1:nchar(seq2)+1){
    if(str_sub(seq1,i,i) == str_sub(seq2,j,j)){
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

scoreAlinhamento <- matz[nchar(seq1)+1,nchar(seq2)+1]
print(paste('resultado: ', scoreAlinhamento))
