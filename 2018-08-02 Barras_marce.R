library(tidyverse)


datos <- read.csv("Datos/2018-08-02 Barras_marce.csv", header=T, sep=";")

datos$TIEMPO<-as.factor(datos$TIEMPO)
datos$TTO<-as.factor(datos$TTO)




for (i in 3:ncol(datos)) {
  
  plot<-ggplot(datos, aes(x=TTO, y=datos[,i], fill=TIEMPO)) +
    geom_bar(stat="identity", position="dodge") + ggtitle(paste(colnames(datos[i])))+
    scale_fill_brewer(palette="Dark2")+ 
    theme_bw() +labs(x="Tratamiento", y="Intesidad (ABC)") 
  
  print(plot)
}



ggplot(datos, aes(x=TTO, y=V21, fill=TIEMPO)) +
  geom_bar(stat="identity", position="dodge") + ggtitle('VV21')+
  scale_fill_brewer(palette="Dark2")+ 
  theme_bw() +labs(x="Tratamiento", y="Intesidad (ABC)") 

