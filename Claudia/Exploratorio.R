library(xtable)
library(tidyverse)
library(caret)
library(reticulate)
library(formatR)

train = read.csv("train.csv", header = TRUE)
unique_m = read.csv("unique_m.csv", header = TRUE)
train$critical_temp = NULL
unique_m$material = NULL
df = data.frame(train, unique_m)
attach(df)
variables = names(df)
cantvariables = length(variables)
variables_baja_var = nearZeroVar(df, names = TRUE)
for(i in 1:(length(variables_baja_var))){
    a = paste("df$",variables_baja_var[i],"=NULL",sep = "")
    eval(parse( text=a ))
}
variables = names(df)
cantvariables = length(variables)
tol = 0.05
cor_baja = c()
for(i in 1:(cantvariables-1)){
    correlacion = cor(df[,i], df$critical_temp)
    if(abs(correlacion) < tol){
        cor_baja = c(cor_baja, i)
    }
}
variables_cor_baja = c()
for(i in 1:(length(cor_baja))){
    variables_cor_baja = c(variables_cor_baja, variables[cor_baja[i]])
}
for(i in 1:(length(variables_cor_baja))){
    a = paste("df$",variables_cor_baja[i],"=NULL",sep = "")
    eval(parse( text=a ))
}
variables = names(df)
cantvariables = length(variables)
tol = 0.8
cor_alta = c()
for( i in 1:(cantvariables-1)){
    if(i != cantvariables-1){
        for(j in (i+1):(cantvariables-1)){
            correlacion = cor(df[,i], df[,j])
            if(abs(correlacion) > tol){
                cor_alta = c(cor_alta,j)
            }
        }
    }
}
cor_alta = sort(cor_alta[!duplicated(cor_alta)])
variables_cor_alta = c()
for(i in 1:(length(cor_alta))){
    variables_cor_alta = c(variables_cor_alta, variables[cor_alta[i]])
}
for(i in 1:(length(variables_cor_alta))){
    a = paste("df$",variables_cor_alta[i],"=NULL",sep = "")
    eval(parse( text=a ))
}
corrplot1 = cor(df)
attach(df)
variables = names(df)
cantvariables = length(variables)
df = df[!duplicated(df),]
rm(list=ls()[-7])

correlaciones = cor(df)[,34][1:33]
cor_ord = sort(abs(correlaciones))

#FORWARD

modfor = step(lm(critical_temp ~ 1, data = df), direction = "forward",
              scope = ~ std_ThermalConductivity + number_of_elements + range_fie +
                  O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
                  wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
                  mean_FusionHeat + wtd_mean_ThermalConductivity +
                  mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
                  wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
                  wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
                  wtd_entropy_ThermalConductivity + mean_atomic_mass +
                  wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
                  entropy_ThermalConductivity)

summary(modfor)
#R^2 = 0.669
plot(df$critical_temp, modfor$fitted.values)

#BACKWARD

modback = step(lm(critical_temp ~ std_ThermalConductivity + number_of_elements + range_fie +
                      O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
                      wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
                      mean_FusionHeat + wtd_mean_ThermalConductivity +
                      mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
                      wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
                      wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
                      wtd_entropy_ThermalConductivity + mean_atomic_mass +
                      wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
                      entropy_ThermalConductivity, data = df),
               direction = "backward")

summary(modback)
#R^2 = 0.669
plot(df$critical_temp, modback$fitted.values)

#AMBOS

modambos = step(lm(critical_temp ~ std_ThermalConductivity + number_of_elements + range_fie +
                       O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
                       wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
                       mean_FusionHeat + wtd_mean_ThermalConductivity +
                       mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
                       wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
                       wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
                       wtd_entropy_ThermalConductivity + mean_atomic_mass +
                       wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
                       entropy_ThermalConductivity, data = df),
                direction = "both")

summary(modambos)
#R^2 = 0.669
plot(df$critical_temp, modambos$fitted.values)

#DROP1

modover = lm(data = df, critical_temp ~ std_ThermalConductivity + number_of_elements + range_fie +
                 O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
                 wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
                 mean_FusionHeat + wtd_mean_ThermalConductivity +
                 mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
                 wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
                 wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
                 wtd_entropy_ThermalConductivity + mean_atomic_mass +
                 wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
                 entropy_ThermalConductivity)

drop1(modover, test = "Chisq")

#Quitar range_ElectronAffinity, no es sigificativa

modover = update(modover, ~ std_ThermalConductivity + number_of_elements + range_fie +
                     O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
                     wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
                     mean_FusionHeat + wtd_mean_ThermalConductivity +
                     mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
                     wtd_range_atomic_mass + Ca + wtd_range_fie +
                     wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
                     wtd_entropy_ThermalConductivity + mean_atomic_mass +
                     wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
                     entropy_ThermalConductivity)

drop1(modover, test = "Chisq")

#Quitar range_fie, no es significativa

modover = update(modover, ~ std_ThermalConductivity + number_of_elements +
                     O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
                     wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
                     mean_FusionHeat + wtd_mean_ThermalConductivity +
                     mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
                     wtd_range_atomic_mass + Ca + wtd_range_fie +
                     wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
                     wtd_entropy_ThermalConductivity + mean_atomic_mass +
                     wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
                     entropy_ThermalConductivity)

summary(modover)
#R^2 = 0.669. Tenemos el mismo coeficiente de determinación, pero
#lo logramos con dos variables menos que los modelos anteriores.

#ADD1

mod0 = lm(critical_temp ~ 1, data = df)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos std_ThermalConductivity ya que es la con AIC más bajo,
#entre todas las variables significativas

mod0 = update(mod0, ~ std_ThermalConductivity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos Ba, variable co AIC más bajo

mod0 = update(mod0, ~ std_ThermalConductivity + Ba)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos number_of_elements (AIC más bajo)

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos Ca, AIC más bajo

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_mean_ElectronAffinity, AIC más bajo

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos range_fie, AIC más bajo

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos range_Valence, AIC más bajo

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos range_atomic_mass

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_mean_TermalConductivity

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos gmean_ThermalConductivity

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos mean_ThermalConductivity

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos Oxígeno

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_range_atomic_mass

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos range_FusionHeat

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos mean_FusionHeat, AIC bajo + considerar que es más significativa

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos mean_density

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos mean_atomic_mass

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_range_valence

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos mean_fie

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_entropy_ThermalConductivity

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos entropy_ThermalConductivity

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos mean_atomic_radius

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity + mean_atomic_radius)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_range_atomic_radius

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity + mean_atomic_radius +
                  wtd_range_atomic_radius)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_range_ElectronAffinity

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity + mean_atomic_radius +
                  wtd_range_atomic_radius + wtd_range_ElectronAffinity)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_mean_fie

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity + mean_atomic_radius +
                  wtd_range_atomic_radius + wtd_range_ElectronAffinity +
                  wtd_mean_fie)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_entropy_fie

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity + mean_atomic_radius +
                  wtd_range_atomic_radius + wtd_range_ElectronAffinity +
                  wtd_mean_fie + wtd_entropy_fie)

add1(mod0, ~ std_ThermalConductivity + number_of_elements + range_fie +
         O + Ba + Cu + range_atomic_mass + wtd_range_Valence +
         wtd_mean_fie + wtd_entropy_fie + gmean_ThermalConductivity +
         mean_FusionHeat + wtd_mean_ThermalConductivity +
         mean_ThermalConductivity + mean_Density + wtd_range_atomic_radius +
         wtd_range_atomic_mass + Ca + wtd_range_fie + range_ElectronAffinity +
         wtd_range_ElectronAffinity + range_Valence + range_FusionHeat +
         wtd_entropy_ThermalConductivity + mean_atomic_mass +
         wtd_mean_ElectronAffinity + mean_atomic_radius + mean_fie +
         entropy_ThermalConductivity, test = "Chisq")

#Agregamos wtd_range_fie

mod0 = update(mod0, ~ std_ThermalConductivity + Ba + number_of_elements + Ca +
                  wtd_mean_ElectronAffinity + range_fie + range_Valence +
                  range_atomic_mass + wtd_mean_ThermalConductivity +
                  gmean_ThermalConductivity + mean_ThermalConductivity + O +
                  wtd_range_atomic_mass + range_FusionHeat + mean_FusionHeat +
                  mean_Density + mean_atomic_mass + wtd_range_Valence +
                  mean_fie + wtd_entropy_ThermalConductivity +
                  entropy_ThermalConductivity + mean_atomic_radius +
                  wtd_range_atomic_radius + wtd_range_ElectronAffinity +
                  wtd_mean_fie + wtd_entropy_fie + wtd_range_fie)

summary(mod0)

#R^2 = 0.6689
