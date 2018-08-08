library(tidyverse)
library(car)
library(MASS)
library(agricolae)
library(ggsci)

datos<-read.csv('Datos/2018-08-02 DatosAguacate1.csv', sep=';')

## CIAT #######
datCIAT<-filter(datos, Patogeno=='CIAT')
    ## Area ####
    #### Análisis
      ## ANOVA #####
      modArea<-lm(Area~Tratamiento + Tiempo, data = datCIAT)
        ## Supuestos
          shapiro.test(modArea$residuals)
          bartlett.test(Area~Tratamiento,datCIAT)
          
      anova(modArea)
      
      HSD.test(modArea, 'Tratamiento', console = T)
    
    datCIAT<-datos %>%
              filter(Patogeno=='CIAT') %>%
                  group_by(Tratamiento) %>%
                    summarise(PrArea=mean(Area), sdArea=sd(Area))
    
    
      ggplot(datCIAT, aes(Tratamiento, PrArea, fill=Tratamiento)) +
        geom_bar(stat='identity') + 
        geom_errorbar(aes(ymax = PrArea + sdArea, ymin=PrArea - sdArea,  width=0.25)) + 
        scale_fill_jama() + theme_bw() + 
        labs(x='Tratamiento', y='Área bajo la curva') + 
        theme(legend.position="none")

  ### Inh########  