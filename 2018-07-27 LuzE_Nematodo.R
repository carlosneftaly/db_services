#' ---
#' title: "Análisis nemátodos - Luz E."
#' author: "Carlos N. Lozano "
#' date: "27, Julio de 2018"
#' ---


### Paquetes ######

library(tidyverse)
library(car)


datos<-read.csv('Datos/2018-07-27 LuzE_Nematodo.csv', sep=';')


## Raíces funcionales 
ggplot(datos, aes(Muestreo, rf, size=Precipitacion) ) + 
  geom_point() + facet_grid(~Suelo) +
   geom_smooth()


ggplot(datos, aes(Precipitacion, rf )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Número de raíces funcionales') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) + 
  scale_y_continuous(breaks = seq(0, 150, by = 20))






modA<-glm((rf)~Precipitacion, data=subset(datos, Suelo=='A'),family = poisson() )
summary(modA)

modS<-glm(rf~Precipitacion+Suelo,family = poisson(), data=datos )
summary(modS)


with(datos, tapply(rf, Suelo, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


#### Raíces no funcionales 


ggplot(datos, aes(Precipitacion, rnf )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Número de raíces NO funcionales') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) + 
  scale_y_continuous(breaks = seq(0, 150, by = 20))






modrnf<-glm((rnf)~Precipitacion, data=subset(datos, Suelo=='A'),family = poisson() )
summary(modrnf)

modSn<-glm(rnf~Precipitacion+Suelo,family = poisson(), data=datos )
summary(modSn)



####### NEcrosis 



ggplot(datos, aes(Precipitacion, neecrosisp )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Raíces Necrosadas') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) 



modNeA<-glm((neecrosisp)~Precipitacion, data=subset(datos, Suelo=='A'),family = poisson() )
summary(modNeA)


summary(glm.nb(neecrosisp ~ Precipitacion, 
                     data=subset(datos, Suelo=='A')))


modNeB<-glm((neecrosisp)~Precipitacion, data=subset(datos, Suelo=='B'),family = poisson() )
summary(modNeB)

summary(glm.nb(neecrosisp ~ Precipitacion, 
               data=subset(datos, Suelo=='B')))

modNeC<-glm((neecrosisp)~Precipitacion, data=subset(datos, Suelo=='C'),family = poisson() )
summary(modNeC)

summary(glm.nb(neecrosisp ~ Precipitacion, 
               data=subset(datos, Suelo=='C')))
modNeD<-glm((neecrosisp)~Precipitacion, data=subset(datos, Suelo=='D'),family = poisson() )
summary(modNeD)

summary(glm.nb(neecrosisp ~ Precipitacion, 
               data=subset(datos, Suelo=='D')))



########## Rad #######


ggplot(datos, aes(Precipitacion, Radophulus )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Radophulus similis') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) 


modRadA<-glm((Radophulus)~Precipitacion, data=subset(datos, Suelo=='A'),family = poisson() )
summary(modRadA)
summary(m4 <- glm.nb(Radophulus + 1 ~ Precipitacion, 
                     data=subset(datos, Suelo=='A')))

modRadB<-glm((Radophulus)~Precipitacion, data=subset(datos, Suelo=='B'),family = poisson() )
summary(modRadB)


summary(m3 <- glm.nb(Radophulus ~ Precipitacion, 
                     data=subset(datos, Suelo=='B')))

modRadC<-glm((Radophulus)~Precipitacion, data=subset(datos, Suelo=='C'),family = poisson() )
summary(modRadC)

summary(m2 <- glm.nb(Radophulus ~ Precipitacion, 
                     data=subset(datos, Suelo=='C')))


modRadD<-glm((Radophulus)~Precipitacion, data=subset(datos, Suelo=='D'),family = poisson() )
summary(modRadD)

summary(m1 <- glm.nb(Radophulus ~ Precipitacion, 
                     data=subset(datos, Suelo=='D')))




########### Helicoty  ############### 

ggplot(datos, aes(Precipitacion, Helicoty )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Helicoty sp.') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) 


    modHeA<-glm((Helicoty)~Precipitacion, data=subset(datos, Suelo=='A'),family = poisson() )
    summary(modHeA)
    
    
    modHeB<-glm((Helicoty)~Precipitacion, data=subset(datos, Suelo=='B'),family = poisson() )
    summary(modHeB)
    
    
    modHeC<-glm((Helicoty)~Precipitacion, data=subset(datos, Suelo=='C'),family = poisson() )
    summary(modHeC)
    
    modHeD<-glm((Helicoty)~Precipitacion, data=subset(datos, Suelo=='D'),family = poisson() )
    summary(modHeD)
    
    
#### Meloidogyne ################ 


ggplot(datos, aes(Precipitacion, Meloidogy )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Meloidogyne sp.') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) 


    


########## Total FIto ############### 

ggplot(datos, aes(Precipitacion, Total_Fito )) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth(method = 'glm') + theme_bw() + 
  labs(x='Precipicitación (mm)', y='Número total de nemátodos') + 
  theme(axis.title = element_text(size=16),
        strip.text =element_text(size=16),
        axis.text = element_text(size=14)) + 
  scale_x_continuous(breaks = seq(0, 260, by = 50)) 



    
    modFTA<-glm((Total_Fito)~Precipitacion, data=subset(datos, Suelo=='A'),family = poisson() )
    summary(modFTA)
    
    
    modFTB<-glm((Total_Fito)~Precipitacion, data=subset(datos, Suelo=='B'),family = poisson() )
    summary(modFTB)
    
    
    modFTC<-glm((Total_Fito)~Precipitacion, data=subset(datos, Suelo=='C'),family = poisson() )
    summary(modFTC)
    
    modFTD<-glm((Total_Fito)~Precipitacion, data=subset(datos, Suelo=='D'),family = poisson() )
    summary(modFTD)



















ggplot(datos, aes(x = Precipitacion, y = rf, colour = Suelo)) +
  geom_point(aes(y = rf), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1)

ggplot(datos, aes(Muestreo, Radophulus, size=Precipitacion, colour=as.factor(Año)) ) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth()


ggplot(datos, aes(Precipitacion, Radophulus) ) + 
  geom_point() + facet_grid(~Suelo) +
  geom_smooth() + geom_jitter()


ggplot(datos, aes(Muestreo, Radophulus, size=Precipitacion) ) + 
  geom_point() +facet_grid(~Suelo)+
  geom_smooth() + geom_jitter()


ggplot(datos, aes(Muestreo, Total_Fito, colour=Suelo) ) + 
  geom_point() + 
  geom_smooth() + geom_jitter()



ggplot(datos, aes(Muestreo, Radophulus/Total_Fito) ) + 
  geom_point() +facet_grid(~Suelo)+
  geom_smooth() + geom_jitter()


ggplot(datos, aes(as.factor(Muestreo), Helicoty/Total_Fito) ) + 
  geom_boxplot() +facet_grid(~Suelo)+
  geom_smooth() + geom_jitter()

ggplot(datos, aes(as.factor(Muestreo), Precipitacion, colour=Suelo) ) + 
  geom_point() + geom_line(aes(as.factor(Muestreo),  Helicoty/Total_Fito ))+ facet_grid(~Año, scale='free_x') + 
  geom_smooth() + geom_jitter()


###### 

res1<-datos%>%group_by(Muestreo, Suelo) %>% 
  summarise(PrepM=mean(Precipitacion, na.rm=T),sdPrep=sd(Precipitacion, na.rm=T), Mrado=mean(Radophulus, na.rm=T))


ggplot(res1 ) + 
  geom_bar(aes(as.factor(Muestreo), PrepM),stat = 'identity') +
             geom_line(aes(as.factor(Muestreo),Mrado)) + 
  scale_y_continuous(sec.axis = res1$Mrado)




+facet_grid(~Suelo)+
  geom_smooth() + geom_jitter()
