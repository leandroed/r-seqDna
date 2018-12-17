install.packages("stringr", repos='http://cran.us.r-project.org')
library("stringr")

args<-commandArgs(TRUE)
arq1 <- args[1]
arq2 <- args[2]

if(length(args) <= 0){
  print("Erro ao executar arquivos. Tente novamente!")
  stop("modo de uso: \n\t\t RScript nameFile.R sequencia1.txt sequencia2.txt\n", call.=FALSE)
}

primeiro_arquivo <- readLines(arq1)
segundo_arquivo <- readLines(arq2)

seq1 <- primeiro_arquivo[2:length(primeiro_arquivo)]
seq2 <- segundo_arquivo[2:length(segundo_arquivo)]

seq1 <- paste(seq1, collapse = '')
seq2 <- paste(seq2, collapse = '')

match <- 1
mismatch <- -1
d <- -1

i <- nchar(seq1)
j <- nchar(seq2) 

#preenche todas as posições da matriz com 'NA'
matz <- matrix(NA, nrow = nchar(seq1)+1, ncol = nchar(seq2)+1)

tamSeq1 <- nchar(seq1)
tamSeq2 <- nchar(seq2)

matz[1,1] <- 0

for (i in 1:tamSeq1){
  matz[i+1,1] <- d*i
}

for (i in 1:tamSeq2){
  matz[1,i+1] <- d*i
}

#monta matriz
for (i in 1:tamSeq1+1){
  for (j in 1:tamSeq2+1){
    if(str_sub(seq1,i-1,i-1) == str_sub(seq2,j-1,j-1)){
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

#alinhamento
seq1Result <- ""
seq2Result <- ""

i <- nchar(seq1)+1
j <- nchar(seq2)+1

while (i > 1 && j > 1){
  #melhor foi seq1[i] com seq2[i] 
  sc <- matz[i-1,j-1]
  
  if (str_sub(seq1,i-1,i-1) == str_sub(seq2,j-1,j-1)){
    sc <- sc + match
  } else{
    sc <- sc + mismatch
  }
  
  if(sc == matz[i,j]){
    seq1Result <- c(str_sub(seq1,i-1,i-1), seq1Result)
    seq2Result <- c(str_sub(seq2,j-1,j-1), seq2Result)
    i <- i -1
    j <- j-1
    next
  }
  
  #melhor foi seq1[i] com -
  if ((matz[i-1,j] + d) == matz[i,j]) {
    seq1Result <- c(str_sub(seq1,i-1,i-1), seq1Result)
    seq2Result <- c("-", seq2Result)
    i <- i-1
    next
  }
  
  #melhor foi seq2[i] com -
  if ((matz[i,j-1] + d) == matz[i,j]) {
    seq1Result <- c("-", seq1Result)
    seq2Result <- c(str_sub(seq2,j-1,j-1), seq2Result)
    j <- j-1
    next
  }
}

print("************ Entrada alinhamento ************")
cat ("Sequencia 1: ", seq1, "\n")
cat ("Sequencia 2: ", seq2, "\n\n")

print("************ Sequencias alinhadas ************")
cat ("Sequencia 1:\n", paste(seq1Result, collapse=''), "\n")
cat ("Sequencia 2:\n", paste(seq2Result, collapse=''),"\n\n")

print(paste('Score: ', matz[nchar(seq1)+1,nchar(seq2)+1]))

print(matz)
print("Foi gerado o arquivo 'alinhamento.txt' com a sequencia alinhada.")

##escrita em arquivo
arq_sequencia1 <- c("Sequencia de entrada 1: ", seq1)
arq_sequencia1 <- paste(arq_sequencia1, collapse = '')

arq_sequencia2 <- c("Sequencia de entrada 2: ", seq2)
arq_sequencia2 <- paste(arq_sequencia2, collapse = '')

arq_sequencia_saida1 <- c("Alinhamento 1: ", paste(seq1Result, collapse=''))
arq_sequencia_saida1 <- paste(arq_sequencia_saida1, collapse = '')

arq_sequencia_saida2 <- c("Alinhamento 2: ", paste(seq2Result, collapse=''))
arq_sequencia_saida2 <- paste(arq_sequencia_saida2, collapse = '')

arq_score <- c("Score: ", matz[nchar(seq1)+1,nchar(seq2)+1])
arq_score <- paste(arq_score, collapse = '')

saida_arquivo = c(
  "************ Entrada do alinhamento ************",
  arq_sequencia1,
  arq_sequencia2,
  "************ Saida do alinhamento ************",
  arq_sequencia_saida1,
  arq_sequencia_saida2,
  arq_score
)

saida_arquivo <- paste(saida_arquivo, "\n")

write.table(saida_arquivo, file = "alinhamento.txt", row.names = FALSE)


#Rodar: Rscript SeqDNA.R arq1.txt arq2.txt
