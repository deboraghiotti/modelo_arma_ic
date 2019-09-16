source("calculo-residuos.r")
#Modelo Arma(p,q)
#p -> lag 1
#q -> lag 2
#n -> tamanho da seria sintetica

armaModel = function(p,q,n) {

  L <- 50 
  #Leitura do arquivo que contem os dados
  #Padr?o: arquivo.txt contendo o m?s e a vaz?o mensal
  
  sistema <<- Sys.info ( )['sysname']
  
  if (sistema == "Linux")
    dados = tk_choose.files ( )
  if (sistema == "Windows")
    dados = choose.files ( )
  
  serie_historica = read.table(dados,head=T,sep = ";" , dec = ",")
  #print(serie_historica)
  #serie_historica[,1] : data da vazao (varia de acordo com o arquivo)
  #serie_historica[,2] : vazao
  
  #Somando os meses de um mesmo ano, para ter as vazões anuais
  serie_matriz <- matrix(serie_historica[,2],nrow=12)
  serieAnual <- apply(serie_matriz,2,sum)
  
  #Padronizando a serie historica (aplicar ln)
  serieAnual <- log(serieAnual)
  
  #Calculos de media, devio padrao e variancia da serie anual
  #mediaSA -> media da serie anual
  #dpSA -> desvio padrao da serie anual
  #varSA ->variancia da serie anual
  
  mediaSA = mean(serieAnual)
  dpSA = sd(serieAnual)
  varSA = var(serieAnual)
  
  #Normalizando a serie historica
  serieAnual <- (serieAnual - mediaSA)/dpSA
  
  #Calculo dos coeficientes phi e teta
  #phi: coeficientes peri?dicos autorregressivos 
  #teta: coeficientes peri?dicos m?dia m?vel

  #Para o calculo dos coeficientes foi utilizado a fun??o arima do pr?pio r

   parametros <- arima(serieAnual, order= c(p,0,q))
   coeficientes <- parametros$coef
   print(coeficientes)
   phi <- coeficientes[1:p]
   #print(phi)
   teta <- coeficientes[(p+1):(q+p)]
   #print(teta)
   
  #Calculo dos residuos para a geracao de e
  #residuos <- calculo_residuos(p,q,phi,teta,serieAnual) 
  residuos <- parametros$residuals
  print(residuos)
  
  dpResiduos <- sd(residuos)
 
  #ruido ? o ruido branco -> vari?vel normal e independente com m?dia zero
  #Para esse primeiro teste est? sendo utilizada a fun??o rnorm que gera x variaveis
  #aleatorias de media 0 e sd = 1
  
  ruido <- rnorm(n+L)
  ruido <- ruido*dpResiduos
  #print(ruido)

  #Primeiro teste modelo arma 1.0 

  y <- vector()

  for(t in 1:(n+L)){
    y[t] <- ruido[t]

    #Somatorio -> ar
    for(i in 1:p){
      if((t-i) < 1)
        break
      else
      y[t] <- y[t] + phi[i]*y[t-i]
    }

    #somatoria ->ma
    for(j in 1:q){
      if((t-j) < 1)
        break
      else
        y[t] <- y[t] - teta[j]*ruido[t-j]

    }

  }
  
  #Desnormalizando a serie
  
  y <- y*dpSA + mediaSA
  
  y <- exp(y)
  
  #Retirando os 50 primeiros anos
  
  serie_sintetica <- y[(L+1):(n+L)]
  years <- 1:n
  data <- data.frame(serie_sintetica,years)
  
  return(serie_sintetica)

}



  