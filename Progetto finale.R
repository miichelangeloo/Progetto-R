library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(reshape2)
library(gganimate)
library(cowplot)
library(stringr)

#setwd("/Users/leonardozoffi/Desktop/Università/Data Science/Progetto R/Tutto per progetto")
SpLC17<- read.csv("SpLC/SpLC2017.csv", sep=";", dec=",", header = TRUE )
SpLC18<- read.csv("SpLC/SpLC2018.csv", sep=";",dec=",", header = TRUE )
SpLC19<- read.csv("SpLC/SpLC2019.csv", sep=";", dec=",",header = TRUE )
SpLC20<- read.csv("SpLC/SpLC2020.csv", sep=";",dec=",", header = TRUE )
SpLC21<- read.csv("SpLC/SpLC2021.csv", sep=";", dec=",",header = TRUE )
SpLCA17<- read.csv("SpLC/SpLC2017A.csv", sep=";", dec=",",header = TRUE )
SpLCA18<- read.csv("SpLC/SpLC2018A.csv", sep=";",dec=",", header = TRUE )
SpLCA19<- read.csv("SpLC/SpLC2019A.csv", sep=";", dec=",",header = TRUE )
SpLCA20<- read.csv("SpLC/SpLC2020A.csv", sep=";", dec=",",header = TRUE )
SpLCA21<- read.csv("SpLC/SpLC2021A.csv", sep=";",dec=",", header = TRUE )

SpLE17<- read.csv("SpLE/SpLE2017.csv", sep=";", dec=",", header = TRUE )
SpLE18<- read.csv("SpLE/SpLE2018.csv", sep=";",dec=",", header = TRUE )
SpLE19<- read.csv("SpLE/SpLE2019.csv", sep=";", dec=",",header = TRUE )
SpLE20<- read.csv("SpLE/SpLE2020.csv", sep=";",dec=",", header = TRUE )
SpLE21<- read.csv("SpLE/SpLE2021.csv", sep=";", dec=",",header = TRUE )

QA2017 <- read.csv("QA/QA2016.csv", sep=";", dec=",", header = TRUE)
QA2018 <- read.csv("QA/QA2017.csv", sep=";", dec=",", header = TRUE)
QA2019 <- read.csv("QA/QA2018.csv", sep=";", dec=",", header = TRUE)
QA2020 <- read.csv("QA/QA2019.csv", sep=";", dec=",", header = TRUE)
QA2021 <- read.csv("QA/QA2020.csv", sep=";", dec=",", header = TRUE)

PaAAlE2017 <- read.csv("PaAAlE/PaAAlE2017.csv", sep=";", header = TRUE, na.strings=c("","NA"))
PaAAlE2018 <- read.csv("PaAAlE/PaAAlE2018.csv", sep=";", header = TRUE)
PaAAlE2019 <- read.csv("PaAAlE/PaAAlE2019.csv", sep=";", header = TRUE)
PaAAlE2020 <- read.csv("PaAAlE/PaAAlE2020.csv", sep=";", header = TRUE, na.strings=c("","NA"))

POAR <- read.csv("POAR/POAR.csv", sep=";", header = TRUE)

MCs17<- read.csv("MCs/MCs2017.csv", sep=";", dec=",", header = FALSE )
MCs18<- read.csv("MCs/MCs2018.csv", sep=";",dec=",", header = FALSE )
MCs19<- read.csv("MCs/MCs2019.csv", sep=";", dec=",",header = FALSE )
MCs20<- read.csv("MCs/MCs2020.csv", sep=";",dec=",", header = FALSE )
MCs21<- read.csv("MCs/MCs2021.csv", sep=";", dec=",",header = FALSE)

MCs17 <- MCs17[-c(6,7),]
MCs18 <- MCs18[-c(6,7),]
MCs19 <- MCs19[-c(6,7),]
MCs20 <- MCs20[-c(6,7),]
MCs21 <- MCs21[-c(6,7),]

MCs17["Anno"] <- 2017
MCs18["Anno"] <- 2018
MCs19["Anno"] <- 2019
MCs20["Anno"] <- 2020
MCs21["Anno"] <- 2021

MCs1721 <- bind_rows(MCs17, MCs18, MCs19, MCs20, MCs21)

colnames(MCs1721)[1] <- "Regione"
colnames(MCs1721)[2] <- "Difficolta.collegamento.mezzi"
colnames(MCs1721)[3] <- "Cattive.condizioni.strade"

regMCs1721<-map(unique(MCs1721$Regione), ~MCs1721%>%
                 filter(Regione==.x ) %>%
                  plot_ly(x = ~Anno, y = ~Difficolta.collegamento.mezzi, type = 'scatter', mode = 'lines+markers')%>%
                  layout(title=.x))

regMCs1721[[6]]


PaAAlE2017 <- PaAAlE2017[c(1,2,13)]
PaAAlE2018 <- PaAAlE2018[c(1,2,12)] 
PaAAlE2019 <- PaAAlE2019[c(1,2,12)]
PaAAlE2020 <- PaAAlE2020[c(1,2,13)]

str(PaAAlE2018)

PaAAlE2017["Anno"] <- 2017
PaAAlE2018["Anno"] <- 2018
PaAAlE2019["Anno"] <- 2019
PaAAlE2020["Anno"] <- 2020

PaAAle1720 <- bind_rows(PaAAlE2017, PaAAlE2018, PaAAlE2019, PaAAlE2020)




regPa1720 <- map(unique (PaAAle1720$REGIONE), ~PaAAle1720 %>%
             filter(REGIONE==.x ) %>%
               group_by(ALIMENTAZIONE)%>%
              plot_ly(x = ~Anno, y = ~TOTALE, type = 'scatter', mode = 'lines+markers', color=~ALIMENTAZIONE)%>%
               layout(title=.x)
             
             )
               

ComReg <- read.csv("ComReg.csv", sep=";", header = TRUE)
colnames(ComReg)[1]="Comune"
colnames(ComReg)[2]="Regione"

QA2017 <- merge(QA2017, ComReg, by="Comune")
QA2018 <- merge(QA2018, ComReg, by="Comune")
QA2019 <- merge(QA2019, ComReg, by="Comune")
QA2020 <- merge(QA2020, ComReg, by="Comune")
QA2021 <- merge(QA2021, ComReg, by="Comune")
 

QA2017M <- select(QA2017, -1)%>%
  group_by(Regione)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))

QA2018M <- select(QA2018, -1)%>%
  group_by(Regione)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))

QA2019M <- select(QA2019, -1)%>%
  group_by(Regione)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))

QA2020M <- select(QA2020, -1)%>%
  group_by(Regione)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))

QA2021M <- select(QA2021, -1)%>%
  group_by(Regione)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))



QA2017M["Anno"] <- 2017
QA2018M["Anno"] <- 2018
QA2019M["Anno"] <- 2019
QA2020M["Anno"] <- 2020
QA2021M["Anno"] <- 2021


QA1721M <- bind_rows(QA2017M, QA2018M, QA2019M, QA2020M, QA2021M)



QA1721I <- select(QA1721M, -1)%>%
  group_by(Anno)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))
  
QA1721I <- melt(QA1721I[-5], id="Anno")

QAI1721Plot <- QA1721I %>%
  plot_ly(x = QA1721I$Anno, y = QA1721I$value, type = 'scatter', mode = 'lines+markers', color= QA1721I$variable)%>%
  layout(title="Italia")
  


QA2017M <- melt(QA2017M[1:4])
QA2018M <- melt(QA2018M[1:4])
QA2019M <- melt(QA2019M[1:4])
QA2020M <- melt(QA2020M[1:4])
QA2021M <- melt(QA2021M[1:4])

QA2017M["Anno"] <- 2017
QA2018M["Anno"] <- 2018
QA2019M["Anno"] <- 2019
QA2020M["Anno"] <- 2020
QA2021M["Anno"] <- 2021

QA1721M <- bind_rows(QA2017M, QA2018M, QA2019M, QA2020M, QA2021M)



QARegPlot<-map(unique(QA1721M$Regione), ~QA1721M%>%
                 filter(Regione==.x ) %>%
                 plot_ly(x = ~Anno, y = ~value, type = 'scatter', mode = 'lines+markers', color= ~variable)%>%
                 layout(title=.x))
                 
QARegPlot[[20]]


                 

SpLE17<- SpLE17[1:11,1:7]
SpLE18<- SpLE18[1:11,1:7]
SpLE19<- SpLE19[1:11,1:7]
SpLE20<- SpLE20[1:11,1:7]
SpLE21<-SpLE21[1:11,1:7]

SpLE17<- t(SpLE17)
colnames(SpLE17)<-SpLE17[1,]
SpLE17<-SpLE17[-1,]

SpLE18<- t(SpLE18)
colnames(SpLE18)<-SpLE18[1,]
SpLE18<-SpLE18[-1,]

SpLE19<- t(SpLE19)
colnames(SpLE19)<-SpLE19[1,]
SpLE19<-SpLE19[-1,]

SpLE20<- t(SpLE20)
colnames(SpLE20)<-SpLE20[1,]
SpLE20<-SpLE20[-1,]

SpLE21<- t(SpLE21)
colnames(SpLE21)<-SpLE21[1,]
SpLE21<-SpLE21[-1,]


SpLE17.long<- melt(SpLE17)
SpLE18.long<- melt(SpLE18) 
SpLE19.long<- melt(SpLE19)
SpLE20.long<- melt(SpLE20)
SpLE21.long<- melt(SpLE21)




SpLE17.long["Anno"] <- 2017
SpLE18.long["Anno"] <- 2018
SpLE19.long["Anno"] <- 2019
SpLE20.long["Anno"] <- 2020
SpLE21.long["Anno"] <- 2021

SpLE1721 <- bind_rows(SpLE17.long, SpLE18.long, SpLE19.long, SpLE20.long, SpLE21.long)

SpLE1721$value <- gsub(",",".",SpLE1721$value)


SpLE1721[1] <- lapply(SpLE1721[1], as.character)
SpLE1721[2] <- lapply(SpLE1721[2], as.character)
SpLE1721[3] <- lapply(SpLE1721[3], as.numeric)


str(SpLE1721)




colnames(SpLE1721)[1] <- "X"
colnames(SpLE1721)[2] <- "variable"


str(SpLE1721)

etPlots<-map(unique(SpLE1721$X), ~SpLE1721%>%
                  filter(X==.x ) %>%
                  plot_ly( labels = ~variable, values = ~value, type = 'pie', frame= ~Anno)%>%
                  layout(title = .x ,
                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))

SpLC17 <- SpLC17[-c(7,6), 1:(length(SpLC17)-2)]
SpLC18 <- SpLC18[-c(7,6), 1:(length(SpLC18)-2)]
SpLC19 <- SpLC19[-c(7,6), 1:(length(SpLC19)-2)]
SpLC20<- SpLC20[-c(7,6), 1:(length(SpLC20)-2)]
SpLC21<- SpLC21[-c(7,6), 1:(length(SpLC21)-2)]


SpLC17A.long<-melt(SpLC17[,1:3])
SpLC17B.long<-melt(SpLC17[,-c(2,3)])

SpLC18A.long<-melt(SpLC18[,1:3])
SpLC18B.long<-melt(SpLC18[,-c(2,3)])

SpLC19A.long<-melt(SpLC19[,1:3])
SpLC19B.long<-melt(SpLC19[,-c(2,3)])

SpLC20A.long<-melt(SpLC20[,1:3])
SpLC20B.long<-melt(SpLC20[,-c(2,3)])

SpLC21A.long<-melt(SpLC21[,1:3])
SpLC21B.long<-melt(SpLC21[,-c(2,3)])


SpLC17A.long["Anno"] <- 2017
SpLC18A.long["Anno"] <- 2018
SpLC19A.long["Anno"] <- 2019
SpLC20A.long["Anno"] <- 2020
SpLC21A.long["Anno"] <- 2021

SpLC17B.long["Anno"] <- 2017
SpLC18B.long["Anno"] <- 2018
SpLC19B.long["Anno"] <- 2019
SpLC20B.long["Anno"] <- 2020
SpLC21B.long["Anno"] <- 2021



SpLC1721A <- bind_rows(SpLC17A.long, SpLC18A.long, SpLC19A.long, SpLC20A.long, SpLC21A.long)

SpLC1721B <- bind_rows(SpLC17B.long, SpLC18B.long, SpLC19B.long, SpLC20B.long, SpLC21B.long)



                   

regPlotsA<-map(unique(SpLC1721A$X), ~SpLC1721A%>%
                filter(X==.x ) %>%
                plot_ly(labels = ~variable, values = ~value, type = 'pie', frame=~Anno)%>%
                layout(title = .x ,
                       xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
                 animation_opts(
                   redraw = FALSE, frame =3000, transition= 2900
                 )
)




regPlotsB<-map(unique(SpLC1721B$X), ~SpLC1721B%>%
  filter(X==.x ) %>%
   plot_ly(labels = ~variable, values = ~value, type = 'pie', frame=~Anno)%>%
   layout(title = .x ,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))%>%
  animation_opts(
   redraw = FALSE, frame =3000, transition= 2900
  )
  

ISpLC17B <- data.frame(sapply(SpLC17[, 4:ncol(SpLC17)], FUN=mean))
ISpLC17B["Anno"] <- 2017
ISpLC17B["variable"] <- rownames(ISpLC17B)
colnames(ISpLC17B)[1] <- "value"

ISpLC18B <- data.frame(sapply(SpLC18[, 4:ncol(SpLC18)], FUN=mean))
ISpLC18B["Anno"] <- 2018
ISpLC18B["variable"] <- rownames(ISpLC18B)
colnames(ISpLC18B)[1] <- "value"

ISpLC19B <- data.frame(sapply(SpLC19[, 4:ncol(SpLC19)], FUN=mean))
ISpLC19B["Anno"] <- 2019
colnames(ISpLC19B)[1] <- "value"
ISpLC19B["variable"] <- rownames(ISpLC19B)

ISpLC20B <- data.frame(sapply(SpLC20[, 4:ncol(SpLC20)], FUN=mean))
ISpLC20B["Anno"] <- 2020
colnames(ISpLC20B)[1] <- "value"
ISpLC20B["variable"] <- rownames(ISpLC20B)

ISpLC21B <- data.frame(sapply(SpLC21[, 4:ncol(SpLC21)], FUN=mean))
ISpLC21B["Anno"] <- 2021
colnames(ISpLC21B)[1] <- "value"
ISpLC21B["variable"] <- rownames(ISpLC21B)

ISpLC1721B <- bind_rows(ISpLC17B, ISpLC18B, ISpLC19B, ISpLC20B,ISpLC21B)


ISpLC1721A <- bind_rows(SpLC17A.long, SpLC18A.long, SpLC19A.long, SpLC20A.long, SpLC21A.long)
ISpLC1721A <- ISpLC1721A[-1]%>%
  group_by(variable, Anno)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))
  


IPlotB<- plot_ly(data=ISpLC1721B, labels = ~variable, values = ~value, type = 'pie', frame=~Anno)%>%
        layout(title = "Italia",
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

IPlotA<- plot_ly(data=ISpLC1721A, labels = ~variable, values = ~value, type = 'pie', frame=~Anno)%>%
  layout(title = "Italia",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
