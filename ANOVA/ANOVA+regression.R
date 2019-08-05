################################################################################################
# A simple experiment testing four doses of ABA hormone in embriogenic callus of Carica papaya #
################################################################################################

rm(list = ls())

path <- "C:/Users/Caio/Desktop/Análises Terceiros/ANOVA Reg - Lucas Xavier/"
data.file <- "19-06-10 - Resultado ABA Lucas Xavier.xlsx"

pacman::p_load(openxlsx, magrittr, ExpDes, reshape, ggplot2)

ABA.data <- read.xlsx(paste(path, data.file, sep = ''), rows = c(2:11),
                      cols = c(5:9), colNames = T)

# reshape the data.frame to long format

reshaped.ABA <- reshape(data = ABA.data, 
                        varying = colnames(ABA.data)[2:5],
                        direction = 'long',
                        v.names = 'Massa',
                        times = names(ABA.data)[2:5],
                        timevar = "Concentração.ABA",
                        idvar = 'Repetição')

### Here, I replace the missing data with 0, simply because eventually a callus 
###     plate is lost in this experiment. Not considering the lost board makes 
###     me think of an average that other experiments probably won't reach.

reshaped.ABA[is.na(reshaped.ABA)] <- 0

# mean comparison test
sink(paste(path, "teste_de_médias.txt", sep = ''))
crd(as.factor(reshaped.ABA$Concentração.ABA), reshaped.ABA$Massa, 
    quali=T, nl=F, mcomp = 'tukey')
sink()

# regression
sink(paste(path, "reg.txt", sep = ''))
crd(rep(c(0, 0.5, 5, 10), each = 9), reshaped.ABA$Massa, 
    quali=F, nl=F, mcomp = 'tukey')
sink()

reshaped.ABA %<>%
        transform(dose = rep(c(0, 0.5, 5, 10), each = 9))

coeficientes <- reshaped.ABA %>%
        lm(Massa~dose + I(dose^2), .) # quadratic model by good adjust on reg.txt

equacao <- sprintf('y = %.3f + %.3fx - %.3fx², R² = %.2f', 
                   coeficientes[[1]][1], 
                   coeficientes[[1]][2], 
                   abs(coeficientes[[1]][3]), 0.923384) # R² obtained on reg.txt

length((reshaped.ABA$Massa))

range.massa <- data.frame(dose = seq(from = range(reshaped.ABA$Massa)[1], 
                          to = range(reshaped.ABA$Massa)[2], 
                          length.out = 36))

err <- predict(coeficientes, newdata = range.massa, se.fit = T)

range.massa$lci <- err$fit - 1.96 * err$se.fit
range.massa$fit <- err$fit
range.massa$uci <- err$fit + 1.96 * err$se.fit


tiff(paste(path, "reg.tiff", sep = ''), width = 1200, height = 680, bg = "transparent", 
     compression = "lzw", res = "300ppi")
ggplot(range.massa, aes(x = dose, y = fit)) +
        geom_line() +
                geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity") +
        geom_point(data = reshaped.ABA, aes(x = sort(reshaped.ABA$Massa), y = range.massa$fit)) +
        geom_text(aes(x = 5.5, y = 5, label = equacao), hjust=0, vjust=1) +
        labs(x = "Doses de ABA", y = "Massa do Calo", 
             title = substitute(paste("Massa de Calos embriogênicos de mamão (", italic("Carica papaya"), ") em função de doses de ABA")), 
             subtitle = "Testadas quatro doses de ABA (0; 0,5; 5; 10)", 
             caption = "A linha azul indica os valores ajustados pela regressão.\n A área cinza são os intervalos de confiança ajustados.\n Os pontos são os dados observados.")
dev.off()
