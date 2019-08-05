# UTF-8 encoding #


#clear environment
rm(list = ls())

if(!require(pacman))(install.packages("pacman")) #this package manage your packages
pacman::p_load(lavaan, semPlot, OpenMx, tidyverse, knitr, kableExtra, GGally, openxlsx)


data.file <- "E:/Google Drive/R Zone/Correlação - Trilha  - Análise Fatorial Confirmaória/dados_5_lista_2018_Trilha.xlsx"

# import data
# FMI = half sibs family (Família de Meio Irmãos) in popcorn
FMI <- read.xlsx(data.file, sheet = 1, colNames = TRUE)
str(FMI)

#colnames(FMI) <- c('FAMILY', 'BLOC', 'PLANT.HEIGHT', 'EXPANSION.CAPACITY', 'YIELD', 'resistance.E.turcicum')

# check data
summary(FMI[,3:ncol(FMI)])
sum(is.na(FMI))


attach(FMI)

#choose a model with features
model <-'EXT ~ AP + RG + CE'
fit <- cfa(model, data = FMI)

#    The cfa() function is a dedicated function for fitting 
#    confirmatory factor analysis models. The first argument 
#    is the user-specified model. The second argument is the 
#    dataset that contains the observed variables. Once the 
#    model has been fitted, the summary() function provides 
#    a nice summary of the fitted model.

summary(fit, 
        fit.measures = TRUE, 
        standardized = T,    # important to scale problem between features
        rsquare = T)

# get a causal diagram
semPaths(fit, 'std', 
         what = "col", 
         layout= "tree", 
         edge.label.cex=.8, 
         tyle = "lisrel", 
         residuals = F)

# get a bicolor matrix of correlation
ggcorr(FMI[-c(1, 2)], # Variaveis de FMI menos coluna tratamento e bloco
       nbreaks = 10,  # numero de intervalos entre >-1 e <1
       label = T,     # imprime as correlacoes na matriz
       label_round = 4, # numero de casas decimais da correlacao
       label_size = 5, # tamanho dos numeros das correlacoes
       geom = "tile", # Accepts either "tile", "circle", "text" or "blank".
       low = "firebrick2", high = "dodgerblue3", # cores (http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf)
       name = "Correlation Scale", # titulo da escala de cores
       legend.size = 13, # tamanho dos caracteres da legenda
       legend.position = "right", # local da legenda the position of legends ("none", "left", "right", "bottom", "top", or two-element numeric vector)
       hjust = 0.5) +  # posicao dos nomes das colunas (variaveis)
  ggtitle(label = "Correlation Plot in FMI popcorn") + # titulo da figura
  theme(plot.title = element_text(hjust = .75)) # ajuste da posicao do titulo
