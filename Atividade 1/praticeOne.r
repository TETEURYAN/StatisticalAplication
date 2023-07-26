url <- ("C:/Users/mathe/Downloads/mtcars.csv")
data <- read.csv(url,  sep = ';', dec = ',', encoding = 'UFT-8', header = TRUE)

View(data)

#1

mpg <- data$mpg

minimo <- min(mpg)
minimo

maximo <- max(mpg)
maximo

media <- mean(mpg)
media

mediana <- median(mpg)
mediana

dp <- sd(mpg)
dp

quartis <- quantile(mpg, probs = c(0.25, 0.5, 0.75, 1 ))
quartis

hist(quartis)
boxplot(mpg)

#2

disp <- data$disp
hp <- data$hp

mec <- list(disp, hp)
boxplot(mec, names = c("disp", "hp"))

#C

qsec <- data$qsec

quantis <- quantile(qsec)
quantis

freq_ab <- table(cut(qsec, breaks = quantis, right = FALSE))
freq_ab

freq_rel <- prop.table(freq_ab)
freq_rel

freqabac <- cumsum(freq_ab)
freqabac

freqrac <- cumsum(freq_rel)
freqrac

tb <- cbind(freq_ab, freqabac, freq_rel = round(freq_rel*100, digits = 2), freqrac = round(freqrac*100, digits = 2))
tb

hist(qsec, breaks = quantis)

#D

dist <- hist(hp)
gh <- dist$breaks

freq <- table(cut(hp, breaks = gh, right = FALSE) )
freq

freqabac <- cumsum(freq)
freqabac

freqrel <- prop.table(freq)
freqrel

freqreac <- cumsum(freqrel)
tables <- cbind(freq, freqabac, freqrel = round(freqrel * 100, digits = 2), freqreac = round(freqreac * 100, digits = 2))
tables

#E 

#motor <- data$motor
#marcha <- data$marcha

matriz <-  with(data, table(motor, marcha))
matriz

addmargins(matriz)
matriz

par(mfrow = c(1,2))

barplot(matriz, beside = TRUE, legend = TRUE)

inversa <- with(data, table(marcha, motor))
barplot(inversa, beside = TRUE, legend = TRUE)
inversa 

#F
m <- with(data, table(marcha, hp))
m

b
