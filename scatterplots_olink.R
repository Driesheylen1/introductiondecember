library(graphics)
scatter = scatterplots_input_test
  plot(scatter$`I_AM _CSAS2_1_` ~ scatter$`sepsis_CSAS2 27`,
       pch = 19, xlim = c(-1,15), ylim = c(-1,15), 
       col = 'Red', xlab = 'Sepsis_CSAS2 27', 
       ylab =  'I_AM _CSAS2_1_', main = 'OLINK controls' )

abline(a = 0, b = 1)


grid (lwd = 3, col = "light cyan", equilogs = T)


?plot
?grid
