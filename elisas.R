library(readxl)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(reshape2)

# This file is released open-source under the BSD 3 Clause Licence
# Author: Jack F. Murphy <jack@mrph.dev>

############# USER DEFINED ENTRIES #####################################
NUM_OF_REPLICATES <- 6
NUM_OF_GROUPS <- 6

data <- read_excel('<file-path>.xlsx', sheet='<sheet-name>')
##########################################################################

baseline <- data$curve[8]
data <- data[-c(8), ]
data[c(1, 3:7)] <- data[c(1,3:7)] - baseline

model <- lm(curve ~ 0 + conc, data = data)
# contrations are stored in dataframe 'concentration'
concentration =  data[1:NUM_OF_REPLICATES, 3:(NUM_OF_GROUPS+2)] / model$coefficients # if force fit through origin
# concentration =  (data[1:NUM_OF_REPLICATES, 3:(NUM_OF_GROUPS+2)] - model$coefficients[1]) / model$coefficients[2] # if not forces through origin
melted <- melt(concentration)

# Plot the standard curve and linear fit
plot(curve ~ conc, data=data)
abline(model)
r2 = summary(model)$adj.r.squared
mylabel = bquote(italic(Adj.~R)^2 == .(format(r2, digits = 7)))
text(x = 150, y = .1, labels = mylabel)

# Plot the concentrations
graph <- ggbarplot(melted, x='variable', y='value', add=c('mean_sd', 'jitter'), fill='variable') +
  xlab('') + ylab('Concentration (pg/ml)') 
  
plot(graph)

