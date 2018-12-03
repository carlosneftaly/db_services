library(tidyverse)
library(car)
library(agricolae)
library(ggsci)

# Datos 

datObj1<-read.csv('Datos/Yeison_obj1.csv', header = T, sep = ';')

res<-datObj1 %>% group_by(Tiempo,Tratamiento) %>%
     summarise(Biom=mean(Biomasa), desv=sd(Biomasa)/n(), nt=n())


## Días 

dat4 <- datObj1 %>% filter(Tiempo==4)

boxplot(dat4$Biomasa~dat4$Tratamiento)
mod4 <- lm(dat4$Biomasa~dat4$Tratamiento)
shapiro.test(mod4$residuals)
leveneTest(dat4$Biomasa~dat4$Tratamiento)
anova(mod4)


dat8 <- datObj1 %>% filter(Tiempo==8)

boxplot(log(dat8$Biomasa)~dat8$Tratamiento)
mod8 <- lm(log(dat8$Biomasa)~dat8$Tratamiento)
shapiro.test(mod8$residuals)
leveneTest(log(dat8$Biomasa)~dat8$Tratamiento)
anova(mod8)


HSD.test(mod8, 'dat8$Tratamiento', console=T)

# Barplot 

sig <- c('a', 'a', 'a', 'B', 'A', 'AB')
names(sig)<-sig
 resBp <- datObj1 %>% filter(Tiempo == 4 | Tiempo==8) %>%
   group_by(Tiempo, Tratamiento) %>%
   summarise(Biom=mean(Biomasa), desv=sd(Biomasa)/n(), nt=n()) 
 
 reBpf <- cbind(as.data.frame(resBp), sig)


ggplot(reBpf, aes(x=factor(Tiempo), y=Biom, fill=Tratamiento)) + 
  geom_bar(stat = 'identity',  position='dodge') + 
  geom_errorbar(alpha=0.4, aes(ymin=Biom-desv, ymax=Biom+desv),
                colour='black', width=0.2, position=position_dodge(.9)) + 
  xlab("Tiempo (días)") +
  ylab('Biomasa (mg/L)') + 
  theme_bw()  +  
  theme(axis.text.x = element_text(size=16),
      axis.text.y = element_text(size=16), 
      strip.text.x = element_text(size = 19), 
      text = element_text(size=22))  + 
  scale_fill_aaas() + geom_text(aes(label=sig), 
                                position=position_dodge(width=0.9), 
                                vjust=-3.2)


pd <- position_dodge(0.08)

ggplot(res, aes(x=Tiempo, y=Biom, colour=Tratamiento)) + 
  geom_errorbar(alpha=0.4, aes(ymin=Biom-desv, ymax=Biom+desv),
                colour='black', width=0.2) + 
  geom_line(position=pd,size=1.5, alpha=.6) +
  geom_point(position=pd, size=1.5, shape=21, fill="white") +
  xlab("Tiempo (días)") +
  ylab('Biomasa (mg/L)') + 
  theme_bw()  +  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16), strip.text.x = element_text(size = 19), text = element_text(size=22)) + 
  scale_x_continuous(breaks=c(0:10)) +  scale_color_aaas(name='Tratamiento') 





### obj1.1

dat1.1 <- read.csv('Datos/Yeison_obj1.1.csv', header = T, sep=';')

modMiu<- lm(dat1.1$mu~dat1.1$Tratamiento)
shapiro.test(modMiu$residuals)
leveneTest(dat1.1$mu~dat1.1$Tratamiento)

anova(modMiu)



modP<- lm(dat1.1$Proteina~dat1.1$Tratamiento)
shapiro.test(modP$residuals)
leveneTest(dat1.1$Proteina~dat1.1$Tratamiento)

anova(modP)
HSD.test(modP, 'dat1.1$Tratamiento', console = T)


modCa<- lm(dat1.1$Carbohidratos~dat1.1$Tratamiento)
shapiro.test(modCa$residuals)
leveneTest(dat1.1$Carbohidratos~dat1.1$Tratamiento)

anova(modCa)


############# Objetivo 2 ######################

datObj2<-read.csv('Datos/Yeison_obj2.csv', header = T, sep = ';')

res2<-datObj2 %>% group_by(Dia,Tratamiento) %>%
  summarise(Biom=mean(Biomasa), desv=sd(Biomasa)/n(), nt=n())


## Días 

dat4.2 <- datObj2 %>% filter(Dia==4)

boxplot(dat4.2$Biomasa~dat4.2$Tratamiento)
mod4.2 <- lm(dat4.2$Biomasa~dat4.2$Tratamiento)
shapiro.test(mod4.2$residuals)
leveneTest(dat4.2$Biomasa~dat4.2$Tratamiento)
anova(mod4.2)

LSD.test(mod4.2, 'dat4.2$Tratamiento', console=T)


dat8.2 <- datObj2 %>% filter(Dia==8)

boxplot((dat8.2$Biomasa)~dat8.2$Tratamiento)
mod8 <- lm(dat8.2$Biomasa~dat8.2$Tratamiento)
shapiro.test(dat8.2$residuals)
leveneTest((dat8$Biomasa)~dat8$Tratamiento)
anova(mod8)




# Barplot 

sig <- c('b', 'a', 'a', 'a', 'A', 'A', 'A', 'A')
names(sig)<-sig
resBp2 <- datObj2 %>% filter(Dia == 4 | Dia==8) %>%
  group_by(Dia, Tratamiento) %>%
  summarise(Biom=mean(Biomasa), desv=sd(Biomasa)/n(), nt=n()) 

reBpf <- cbind(as.data.frame(resBp2), sig)


ggplot(reBpf, aes(x=factor(Dia), y=Biom, fill=Tratamiento)) + 
  geom_bar(stat = 'identity',  position='dodge') + 
  geom_errorbar(alpha=0.4, aes(ymin=Biom-desv, ymax=Biom+desv),
                colour='black', width=0.2, position=position_dodge(.9)) + 
  xlab("Tiempo (días)") +
  ylab('Biomasa (mg/L)') + 
  theme_bw()  +  
  theme(axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16), 
        strip.text.x = element_text(size = 19), 
        text = element_text(size=22))  + 
  scale_fill_aaas() + geom_text(aes(label=sig), 
                                position=position_dodge(width=0.9), 
                                vjust=-3.2)


pd <- position_dodge(0.08)

ggplot(res2, aes(x=Dia, y=Biom, colour=Tratamiento)) + 
  geom_errorbar(alpha=0.4, aes(ymin=Biom-desv, ymax=Biom+desv),
                colour='black', width=0.2) + 
  geom_line(position=pd,size=1.5, alpha=.6) +
  geom_point(position=pd, size=1.5, shape=21, fill="white") +
  xlab("Tiempo (días)") +
  ylab('Biomasa (mg/L)') + 
  theme_bw()  +  theme(axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16), strip.text.x = element_text(size = 19), text = element_text(size=22)) + 
  scale_x_continuous(breaks=c(0:10)) +  scale_color_aaas(name='Tratamiento') 


### obj2.1

dat2.1 <- read.csv('Datos/Yeison_obj2.2.csv', header = T, sep=';')

modMiu<- lm(dat2.1$Tasa~dat2.1$Tratamiento)
shapiro.test(modMiu$residuals)
leveneTest(dat2.1$Tasa~dat2.1$Tratamiento)

anova(modMiu)



modP<- lm(dat2.1$Proteina~dat2.1$Tratamiento)
shapiro.test(dat2.1$residuals)
leveneTest(dat2.1$Proteina~dat2.1$Tratamiento)

anova(modP)
HSD.test(modP, 'dat2.1$Tratamiento', console = T)


modCa<- lm(dat2.1$Carbohidratos~dat2.1$Tratamiento)
shapiro.test(modCa$residuals)
leveneTest(dat2.1$Carbohidratos~dat2.1$Tratamiento)

anova(modCa)



############## objetivo 3 ################### 
# Datos 

datObj3<-read.csv('Datos/Yeison_obj3.csv', header = T, sep = ';')

res3<-datObj3 %>% group_by(Tiempo,Tratamiento) %>%
  summarise(Biom=mean(Biomasa), desv=sd(Biomasa)/n(), nt=n())




dat10 <- datObj3 %>% filter(Tiempo==10)

boxplot(dat10$Biomasa~dat10$Tratamiento)
mod8 <- lm(dat10$Biomasa~dat10$Tratamiento)
shapiro.test(dat10$residuals)
leveneTest(log(dat10$Biomasa)~dat10$Tratamiento)
t.test( dat10$Biomasa~dat10$Tratamiento, var.equal = T)




# Barplot 

sig <- c('a', 'a', 'a', 'B', 'A', 'AB')
names(sig)<-sig
resBp <- datObj1 %>% filter(Tiempo == 4 | Tiempo==8) %>%
  group_by(Tiempo, Tratamiento) %>%
  summarise(Biom=mean(Biomasa), desv=sd(Biomasa)/n(), nt=n()) 

reBpf <- cbind(as.data.frame(resBp), sig)



pd <- position_dodge(0.08)

ggplot(res3, aes(x=Tiempo, y=Biom, colour=Tratamiento)) + 
  geom_errorbar(alpha=0.4, aes(ymin=Biom-desv, ymax=Biom+desv),
                colour='black', width=0.2) + 
  geom_line(position=pd,size=1.5, alpha=.6) +
  geom_point(position=pd, size=1.5, shape=21, fill="white") +
  xlab("Tiempo (días)") +
  ylab('Biomasa (mg/L)') + 
  theme_bw()  +  theme(axis.text.x = element_text(size=16),
                       axis.text.y = element_text(size=16), strip.text.x = element_text(size = 19), text = element_text(size=22)) + 
  scale_x_continuous(breaks=c(0:10)) +  scale_color_aaas(name='Tratamiento') 

### obj 3.1 



dat3.1 <- read.csv('Datos/Yeison_obj3.1.csv', header = T, sep=';')

modMiu<- lm(dat3.1$Tasa~dat3.1$Tratamiento)
shapiro.test(modMiu$residuals)
leveneTest(dat3.1$Tasa~dat3.1$Tratamiento)

anova(modMiu)



modP<- lm(dat3.1$Proteina~dat3.1$Tratamiento)
shapiro.test(modP$residuals)
leveneTest(dat3.1$Proteina~dat3.1$Tratamiento)

anova(modP)



modCa<- lm(dat3.1$Carbohidratos~dat3.1$Tratamiento)
shapiro.test(modCa$residuals)
leveneTest(dat3.1$Carbohidratos~dat3.1$Tratamiento)

anova(modCa)

############## Regresion ################## 

regL <- read.csv('Datos/reg.csv', header = T, sep = ';')

ggplot(regL, aes(Luz, pH, colour=Tratamiento)) + geom_point()
