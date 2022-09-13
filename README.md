#Função que vai caulcular a média e a media de 20 alunos-NOTAS

#PAra calcular a media
notas <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  soma_notas <- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_notas <- soma_notas/20
  cat("A média das nptas é: ", media_notas, "|")
  
  #Para caulcular a mediana
  lista_notas = c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)#Criando um vetor
  tamanho_lista = length(lista_notas)# para saber quantos elementos tem dentro da lista_notas
  
  #Ordenando a lista
  notas_ordenadas <- sort(lista_notas)#Ordenando
  
  #Calcular a mediana.
  if(tamanho_lista%%2==0){
    nota_mediana <- mean(notas_ordenadas[(tamanho_lista/2)+0:1])
    cat("A mediana é: ", nota_mediana, "|")
  }else{
    nota_mediana <- notas_ordenadas[(tamanho_lista+1)/2]
    cat("A mediana é: ", nota_mediana, "|")
  }
  #PAra 50% menores e maiores notas - média
  menores_notas_media = mean(notas_ordenadas[0:(tamanho_lista/2)])
  maiores_notas_media = mean(notas_ordenadas[((tamanho_lista/2) + 1): tamanho_lista])
  
  cat("A média das 50% menores notas é: ", menores_notas_media, "|")
  cat("A média das 50% maiores notas é: ", maiores_notas_media, "|")
}

#Testando
notas(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)

#Idade dos alunos

#PAra calcular a Idade
idade <- function(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t){
  soma_idade <- a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t
  media_idade <- soma_idade/20
  cat("A média das notas é: ", media_idade, "|")
  
  #Para caulcular a mediana
  lista_idade = c(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)#Criando um vetor
  tamanho_idade = length(lista_idade)# para saber quantos elementos tem dentro da lista_notas
  
  #Ordenando a lista
  idade_ordenadas <- sort(lista_idade)#Ordenando
  
  #Calcular a mediana.
  if(tamanho_idade%%2==0){
    idade_mediana <- mean(idade_ordenadas[(tamanho_idade/2)+0:1])
    cat("A mediana é: ", idade_mediana, "|")
  }else{
    idade_mediana <- idade_ordenadas[(tamanho_idade+1)/2]
    cat("A mediana é: ", idade_mediana, "|")
  }
  #PAra 50% menores e maiores notas - média
  menores_idade_media = mean(idade_ordenadas[0:(tamanho_idade/2)])
  maiores_idade_media = mean(idade_ordenadas[((tamanho_idade/2) + 1): tamanho_idade])
  
  cat("A média das 50% menores idades é: ", menores_idade_media, "|")
  cat("A média das 50% maiores idades é: ", maiores_idade_media, "|")
}

#Testando
idade(18,17,17,17,17,18,15,12,14,14,10,10,10,10,14,16,17,17,17,18)

#Trazendo os vetores para fora das funções.(Intalamos a biblioteca dplyr)

notas <- c(8,7,7,7,7,8,5,2,4,4,10,10,10,10,4,6,7,7,7,8)
idade <- c(18,17,17,17,17,18,15,12,14,14,10,10,10,10,14,16,17,17,17,18)

df <- data.frame(notas, idade)

#Ordenando

library(dplyr)
df_ordenado_idades <- df[order(df$idade), ]# O síbulo $ é para pegar a coluna, nesse caso coluna idade

#Obtendo a média das notas para 50% menores idades

df_menor <- head(df_ordenado_idades, nrow(df_ordenado_idades)/2)#A função head() serve para pegar as primeiras linhas
#A função row() determinar quantas linhas mostrar. 

#Obtendo a média das notas para 50% menores
df_menor <- head(df_ordenado_idades, nrow(df_ordenado_idades)/2)
media_menor_idade_notas <- mean(df_menor$notas)

#Obtendo a média das notas para 50% maiores
df_maior <- tail(df_ordenado_idades, nrow(df_ordenado_idades)/2)#Tail() peagar o conteúdo final da tabela
media_maior_idade_notas <- mean(df_maior$notas)

#Há relação entre a idade e a nota? 
cat("A diferença entre as maiores notas e as menores notas é de:", ((7.3/6.5)-1)*100)
cat("A diferença entre as maiores idades e as menores idade é de:", ((17.3/12.5)-1)*100)

#Coeficiente de variação. função SD para calcular o desvio padrão.

cv_notas <- (sd(notas) / mean(notas)) * 100
cv_idades <- (sd(idade)/ mean(idade)) * 100
cat("O coeficiente de variação das notas é: ", cv_notas, "|")
cat("O coeficiente de variação das idades é: ", cv_idades )


#Mediana avançada
cor(idade, notas, method = "pearson")

Foi notado que tem relação entre a idade e a nota. 
