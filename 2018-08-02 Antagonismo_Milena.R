library(tidyverse)
library(car)
library(MASS)
library(agricolae)
library(ggsci)

datos<-read.csv('Datos/2018-08-02 DatosAguacate1.csv', sep=';')

## CIAT #######
    datCIAT<-filter(datos, Patogeno=='CIAT')
        ## Area 
        #### Análisis
          ## ANOVA
          modArea<-lm(Area~Tratamiento + Tiempo, data = datCIAT)
            ## Supuestos
              shapiro.test(modArea$residuals)
              bartlett.test(Area~Tratamiento,datCIAT)
              plot(modArea)
          anova(modArea)
          
          HSD.test(modArea, 'Tratamiento', console = T)
          
          with(datCIAT,friedman(Tiempo,Tratamiento,Area ,console = T))
        
        ## In 
          #### Análisis
          ## ANOVA 
          modInh<-lm(Inh~Tratamiento + Tiempo, data = datCIAT)
          ## Supuestos
          shapiro.test(modInh$residuals)
          bartlett.test(Inh~Tratamiento,datCIAT)
          plot(modArea)
          anova(modArea)
          
          HSD.test(modArea, 'Tratamiento', console = T)
          
          with(datCIAT,friedman(Tiempo,Tratamiento,Area ,console = T))
          with(datCIAT,kruskal(Area,Tratamiento ,console = T))
          
              # Etquetas 
              labls<-c(
                'Control',
                expression(paste(italic(T. ~asperellum),~T20)),
                expression(paste(italic(T. ~asperellum),~T38)),
                expression(paste(italic(T. ~harzianum),~T35)),
                expression(paste(italic(T. ~harzianum),~T44)),
                expression(paste(italic(Trichoderma),~sp.~T261)),
                expression(paste(italic(Trichoderma),~sp.~T34))
              )
          
          datCIAT2<-datos %>%
                  filter(Patogeno=='CIAT') %>%
                      group_by(Tratamiento) %>%
                        summarise(PrArea=mean(Area),
                                  PrAV=mean(AreaVolatiles),
                                  sdArea=sd(Area),
                                  sdAV=sd(AreaVolatiles))  
                        
        
        
         ## Barplot 
        
        ggplot(datCIAT2, aes(Tratamiento, PrArea, fill=Tratamiento)) +
            geom_bar(stat='identity') + 
            geom_errorbar(aes(ymax = PrArea + sdArea, ymin=PrArea - sdArea,  width=0.25)) + 
            scale_fill_jama() + theme_bw() + 
            labs(x='Tratamiento' ,y='Área bajo la curva') + 
            theme(legend.position="none") + 
            scale_x_discrete(labels = labls)
          
         ## Boxplot 
          ggplot(datos, aes(Tratamiento, log(Area))) + 
            geom_boxplot() + geom_jitter(aes(colour=Tiempo, alpha=0.002)) +
            theme_bw()+ 
            scale_x_discrete(labels = labls)
          
    
        ### Porcentaje de inhibi
          datInP<-datos %>% 
            filter(Tratamiento != 'Control') 
            
          modInP<-lm(PorInh~Tratamiento+Tiempo, data = datInP)     
          ## Supuestos
          shapiro.test(modInP$residuals)
          bartlett.test(PorInh~Tratamiento,datInP)
          plot(modInP)
          anova(modInP)
          
          HSD.test(modInP, 'Tratamiento', console = T)
          
    # Volatiles   
        # Area 
          
          modAV<-lm(AreaVolatiles~Tratamiento+Tiempo, data =datCIAT)
          ## Supuestos
          shapiro.test(modAV$residuals)
          bartlett.test(AreaVolatiles~Tratamiento,datCIAT)
          plot(modAV)
          anova(modAV)
          HSD.test(modAV, 'Tratamiento', console = T)
          
        # InhVolatiles
          
          modInhV<-lm(InhVolatiles~Tratamiento+Tiempo, data =datCIAT)
          ## Supuestos
          shapiro.test(modInhV$residuals)
          bartlett.test(AreaVolatiles~Tratamiento,datCIAT)
          plot(modInhV)
          anova(modInhV)
          HSD.test(modInhV, 'Tratamiento', console = T)  
          ## Barplot 
          
          ggplot(datCIAT2, aes(Tratamiento, PrAV, fill=Tratamiento)) +
            geom_bar(stat='identity') + 
            geom_errorbar(aes(ymax = PrAV + sdAV, ymin=PrAV - sdAV,  width=0.25)) + 
            scale_fill_jama() + theme_bw() + 
            labs(x='Tratamiento' ,y='Área bajo la curva') + 
            theme(legend.position="none") + 
            scale_x_discrete(labels = labls)
          
          ## Boxplot 
          ggplot(datos, aes(Tratamiento, log(AreaVolatiles))) + 
            geom_boxplot() + geom_jitter(aes(colour=Tiempo, alpha=0.002)) +
            theme_bw()+ 
            scale_x_discrete(labels = labls)
          
          
## P #######
          datP<-filter(datos, Patogeno=='P')
          ## Area 
          #### Análisis
          ## ANOVA
          modAreaP<-lm(Area~Tratamiento + Tiempo, data = datP)
          ## Supuestos
          shapiro.test(modAreaP$residuals)
          bartlett.test(Area~Tratamiento,datP)
          plot(modAreaP)
          anova(modAreaP)
          
          HSD.test(modAreaP, 'Tratamiento', console = T)
          
          with(datP,friedman(Tiempo,Tratamiento,Area ,console = T))
          
        ## In 
          #### Análisis
          ## ANOVA 
          modInhP<-lm(Inh~Tratamiento + Tiempo, data = datP)
          ## Supuestos
          shapiro.test(modInhP$residuals)
          bartlett.test(Inh~Tratamiento,datP)
          plot(modInhP)
          anova(modInhP)
          
          HSD.test(modInhP, 'Tratamiento', console = T)
          
          with(datP,friedman(Tiempo,Tratamiento,Inh ,console = T))
          with(datP,kruskal(Inh,Tratamiento ,console = T))
          
          # Etquetas 
          labls<-c(
            'Control',
            expression(paste(italic(T. ~asperellum),~T20)),
            expression(paste(italic(T. ~asperellum),~T38)),
            expression(paste(italic(T. ~harzianum),~T35)),
            expression(paste(italic(T. ~harzianum),~T44)),
            expression(paste(italic(Trichoderma),~sp.~T261)),
            expression(paste(italic(Trichoderma),~sp.~T34))
          )
          
          datP2<-datos %>%
            filter(Patogeno=='P') %>%
            group_by(Tratamiento) %>%
            summarise(PrArea=mean(Area),
                      PrAV=mean(AreaVolatiles),
                      sdArea=sd(Area),
                      sdAV=sd(AreaVolatiles))  
          
          
          
          ## Barplot 
          
          ggplot(datP2, aes(Tratamiento, PrArea, fill=Tratamiento)) +
            geom_bar(stat='identity') + 
            geom_errorbar(aes(ymax = PrArea + sdArea, ymin=PrArea - sdArea,  width=0.25)) + 
            scale_fill_jama() + theme_bw() + 
            labs(x='Tratamiento' ,y='Área bajo la curva') + 
            theme(legend.position="none") + 
            scale_x_discrete(labels = labls)
          
          ## Boxplot 
          ggplot(datP, aes(Tratamiento, log(Area))) + 
            geom_boxplot() + geom_jitter(aes(colour=Tiempo, alpha=0.002)) +
            theme_bw()+ 
            scale_x_discrete(labels = labls)
          
          
          ### Porcentaje de inhibi
          datInP<-datP %>% 
            filter(Tratamiento != 'Control') 
          
          modInP<-lm(PorInh~Tratamiento+Tiempo, data = datInP)     
          ## Supuestos
          shapiro.test(modInP$residuals)
          bartlett.test(PorInh~Tratamiento,datInP)
          plot(modInP)
          anova(modInP)
          
          HSD.test(modInP, 'Tratamiento', console = T)
          
          # Volatiles   
          # Area 
          
          modAV<-lm(AreaVolatiles~Tratamiento+Tiempo, data =datP)
          ## Supuestos
          shapiro.test(modAV$residuals)
          bartlett.test(AreaVolatiles~Tratamiento,datP)
          plot(modAV)
          anova(modAV)
          HSD.test(modAV, 'Tratamiento', console = T)
          
          # InhVolatiles
          
          modInhV<-lm(InhVolatiles~Tratamiento+Tiempo, data =datP)
          ## Supuestos
          shapiro.test(modInhV$residuals)
          bartlett.test(AreaVolatiles~Tratamiento,datP)
          plot(modInhV)
          anova(modInhV)
          HSD.test(modInhV, 'Tratamiento', console = T)  
          ## Barplot 
          
          ggplot(datP2, aes(Tratamiento, PrAV, fill=Tratamiento)) +
            geom_bar(stat='identity') + 
            geom_errorbar(aes(ymax = PrAV + sdAV, ymin=PrAV - sdAV,  width=0.25)) + 
            scale_fill_jama() + theme_bw() + 
            labs(x='Tratamiento' ,y='Área bajo la curva') + 
            theme(legend.position="none") + 
            scale_x_discrete(labels = labls)
          
          ## Boxplot 
          ggplot(datP, aes(Tratamiento, log(AreaVolatiles))) + 
            geom_boxplot() + geom_jitter(aes(colour=Tiempo, alpha=0.002)) +
            theme_bw()+ 
            scale_x_discrete(labels = labls)
  
### Crecimiento en CMC ################################ 
          
  datCMC<-read.csv('Datos/CrecimientoCMC.csv', header = T, sep=';')
    
    # PDA
    datPDA<-datCMC %>% filter(Medio=='PDA')       
    
    modPDA<-lm(ABC~Cepa+Tiempo, data=datPDA)
      # Supuestos
        shapiro.test(modPDA$residuals)
        plot(modPDA)
        bartlett.test(ABC~Cepa, data=datPDA)
        
        anova(modPDA)    
        HSD.test(modPDA, 'Cepa', console = T)  
        
  # CMC
        datCMCs<-datCMC %>% filter(Medio=='CMC')       
        
        modCMC<-lm(log(ABC)~Cepa+Tiempo, data=datCMCs)
        # Supuestos
        shapiro.test(modCMC$residuals)
        plot(modCMC)
        bartlett.test(log(ABC)~Cepa, data=datCMCs)
        
        anova(modPDA)    
        HSD.test(modCMC, 'Cepa', console = T)  
        
        
        
        resCMC<-datCMC %>%
          group_by( Cepa,Medio) %>%
            summarise(PrArea=mean(ABC),
                    sdArea=sd(ABC)) 
        
        
        labls2<-c(
          expression(paste(italic(T. ~asperellum),~T20)),
          expression(paste(italic(T. ~asperellum),~T38)),
          expression(paste(italic(T. ~harzianum),~T35)),
          expression(paste(italic(T. ~harzianum),~T44)),
          expression(paste(italic(Trichoderma),~sp.~T261)),
          expression(paste(italic(Trichoderma),~sp.~T34))
        )
        
        ggplot(resCMC, aes(Cepa, PrArea, fill=Cepa)) +
          geom_bar(stat='identity') +
          geom_errorbar(aes(ymax = PrArea + sdArea, ymin=PrArea - sdArea,  width=0.25)) + 
          scale_fill_jama() + theme_bw() + 
          labs(x='Cepa' ,y='Área bajo la curva') + 
          theme(legend.position="none") + 
          scale_x_discrete(labels = labls2) +facet_grid(~Medio)
        