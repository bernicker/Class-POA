
install.packages("readxl")
install.packages("data.table")
install.packages("plyr")
install.packages("xlsx")
install.packages("MASS")
install.packages("dplyr")

library(readxl)
library(data.table)
library(plyr)
library(xlsx)
library(MASS)
library(dplyr)

UNIRITTER = read_excel ("C:/Users/luis.bernicker/Desktop/TRABALHO SAUDE/FINAL SAUDE.xlsx", sheet = 1, col_names = TRUE)

setDT(UNIRITTER)
UNIRITTER[, Count := .N, by = list(UNIRITTER$Matrcula, UNIRITTER$CURSO_FIM) ]

#########################################

UNIRITTER %>%  
  group_by(UNIRITTER$Matrcula) %>%
  summarise(number = n()) %>%
  mutate()

#########################################


UNIRITTER$new <- paste(UNIRITTER$Curso, UNIRITTER$DISCIPLINA, sep = "-")

UNIRITTER$formando <- UNIRITTER$DISCIPLINAS_FALTANTES - UNIRITTER$Count

UNIRITTER$formandofim <- ifelse(UNIRITTER$formando==0,1,0)

str(UNIRITTER)

UNIRITTER1 <- merge(aggregate(UNIRITTER$QUANT_ALUNOS,by=list(GRUPODESINERGIA=UNIRITTER$GRUPODESINERGIA, Turno=UNIRITTER$Turno, CAMPUS=UNIRITTER$CAMPUS, CURSO=UNIRITTER$CURSO_FIM), FUN=sum), UNIRITTER)

names(UNIRITTER1)[names(UNIRITTER1) == 'x'] <- 'ALUNOS'


UNIRITTER2 <- merge(aggregate(UNIRITTER1$formandofim,by=list(GRUPODESINERGIA=UNIRITTER1$GRUPODESINERGIA, Turno=UNIRITTER1$Turno, CAMPUS=UNIRITTER1$CAMPUS, CURSO=UNIRITTER1$CURSO_FIM), FUN=sum), UNIRITTER1)

names(UNIRITTER2)[names(UNIRITTER2) == 'x'] <- 'FORMANDO_FIMMM'


setDT(UNIRITTER2)
UNIRITTER2[, P := 1:.N, by = list(GRUPODESINERGIA, Turno, CAMPUS, CURSO_FIM)]

UNIRITTER2 <- UNIRITTER2[!(UNIRITTER2$P > 1),]


UNIRITTER3<- merge(aggregate(UNIRITTER2$new,by=list(GRUPODESINERGIA=UNIRITTER2$GRUPODESINERGIA, Turno=UNIRITTER2$Turno, CAMPUS=UNIRITTER2$CAMPUS), paste, collapse=""), UNIRITTER2)

names(UNIRITTER3)[names(UNIRITTER3) == 'x'] <- 'SINERGIA'


UNIRITTER4 <- merge(aggregate(UNIRITTER3$ALUNOS,by=list(GRUPODESINERGIA=UNIRITTER3$GRUPODESINERGIA, Turno=UNIRITTER3$Turno, CAMPUS=UNIRITTER3$CAMPUS), FUN=sum), UNIRITTER3)

names(UNIRITTER4)[names(UNIRITTER4) == 'x'] <- 'ALUNOS_FINAL'


UNIRITTER5 <- merge(aggregate(UNIRITTER4$FORMANDO_FIMMM,by=list(GRUPODESINERGIA=UNIRITTER4$GRUPODESINERGIA, Turno=UNIRITTER4$Turno, CAMPUS=UNIRITTER4$CAMPUS), FUN=sum), UNIRITTER4)

names(UNIRITTER5)[names(UNIRITTER5) == 'x'] <- 'FORMANDO_FINAL'



write.xlsx(UNIRITTER5, "C:/Users/luis.bernicker/Desktop/TRABALHO SAUDE/OFERTA_UNIRTTER4.xlsx")
