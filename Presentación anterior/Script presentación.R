library(xtable)
library(tidyverse)
library(caret)
library(GGally)
library(ggcorrplot)
library(reticulate)
library(cowplot)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# Limpieza
train = read.csv("train.csv", header = TRUE)
unique_m = read.csv("unique_m.csv", header = TRUE)
train$critical_temp = NULL
unique_m$material = NULL # Variable cualitativa nominal
df = data.frame(train, unique_m)
variables = names(df)
cantvariables = length(variables)
attach(df)

# sumario limpieza
sumario = df %>% summarise(Minimo = min(critical_temp),
                           Primer_Cuartil = quantile(critical_temp)[2],
                           Mediana = quantile(critical_temp)[3],
                           Tercer_Cuartil = quantile(critical_temp)[4],
                           Maximo = max(critical_temp),
                           Promedio = mean(critical_temp),
                           Varianza = var(critical_temp)) %>% t() %>%
    xtable()
names(sumario) = c("critical_temp")
rownames(sumario) = c("Mínimo","Primer Cuartil","Mediana","Segundo Cuartil","Máximo", "Promedio", "Varianza")


# Graficos variable respuesta

plot1 = ggplot(data = df) +
    geom_boxplot(mapping = aes(y = critical_temp), fill="dodgerblue3", outlier.color = "dodgerblue3") +
    geom_density(mapping = aes(y = critical_temp ), fill = "blue", alpha = .3, col = NA)
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
    xlab("Boxplot")



ggplot(data = df, mapping = aes(critical_temp)) +
    geom_line(alpha = .3, col = NA) +
    geom_histogram(fill="dodgerblue3") +
    theme_minimal() +
    ylab("Cantidad")


ggplot(data = df) +
    geom_histogram(breaks= quantile(critical_temp,seq(0,1,by=0.1)),
                   aes(x=critical_temp,
                       y=density(critical_temp))) +
    geom_density(aes(x=critical_temp,
                     y=density(critical_temp)))

ggplot(df, aes(critical_temp)) +
    geom_density(aes(y = ..count.. * 12), # multiply count by bins
                 fill = "dodgerblue1", alpha = .5, col = NA) +
    geom_histogram(binwidth = 12, alpha = .7, fill = "dodgerblue3") +
    ylab("Cuenta")

# Limpieza

train = read.csv("train.csv", header = TRUE)
unique_m = read.csv("unique_m.csv", header = TRUE)
train$critical_temp = NULL
unique_m$material = NULL # Variable cualitativa nominal
df = data.frame(train, unique_m)

attach(df)
variables = names(df)
cantvariables = length(variables)

print(paste("Hay ", cantvariables," variables", sep=""))

# Datos que varían muy poco
variables_baja_var = nearZeroVar(df, names = TRUE) # Explicar

for(i in 1:(length(variables_baja_var))){
    a = paste("df$",variables_baja_var[i],"=NULL",sep = "")
    eval(parse( text=a ))
}
variables = names(df)
cantvariables = length(variables)

print(paste("Quedan ", cantvariables," variables", sep=""))

# Eliminar variables con correlación muy baja respecto a critical_temp (|cor|<0.05)

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

print(paste("Quedan ", cantvariables," variables", sep=""))

# Eliminar variables muy correlacionadas entre si (|cor|>0.8)

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

print(paste("Quedaron ", cantvariables," variables", sep=""))

# Datos duplicados

df = df[!duplicated(df),]

# Sumario limpieza

titulos1 = c("Variables", "Datos")


tabla1 = matrix(c(21263, 21207,169, 34), byrow = TRUE, ncol = 2)
colnames(tabla1) = c("Con limpieza", "Sin limpieza")
rownames(tabla1) = c("Datos", "Variables")

# Visualización correlaciones

matriz = as.data.frame(cor(df))


colnames(matriz) = abc

ggcorrplot(matriz, hc.order = TRUE, type = "lower", outline.col = "white", tl.cex = 0, colors = c("darkorange", "white", "deepskyblue"))




corr_simple <- function(data=df,sig=0.5){
    #convert data to numeric in order to run correlations
    #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
    df_cor <- data %>% mutate_if(is.character, as.factor)
    df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
    #run a correlation and drop the insignificant ones
    corr <- cor(df_cor)
    #prepare to drop duplicates and correlations of 1
    corr[lower.tri(corr,diag=TRUE)] <- NA
    #drop perfect correlations
    corr[corr == 1] <- NA
    #turn into a 3-column table
    corr <- as.data.frame(as.table(corr))
    #remove the NA values from above
    corr <- na.omit(corr)
    #select significant values
    corr <- subset(corr, abs(Freq) > sig)
    #sort by highest correlation
    corr <- corr[order(-abs(corr$Freq)),]
    #print table
    #print(corr)
    #turn corr back into matrix in order to plot with corrplot
    mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")

    #plot correlations visually
    corrplot(mtx_corr, is.corr=FALSE, tl.col="black",addgrid.col="lightskyblue1", na.label=" ",tl.pos = "n",col=colorRampPalette(c("darkorange","white","deepskyblue"))(200))
}

columns = c("critical_temp", "std_ThermalConductivity")

corr_simple(df, 0.4)

colors <- c("critical_temp" = "deepskyblue3", "std_ThermalConductivity" = "darkorange")

ggplot(data = df) +
    geom_density(aes(x = critical_temp, fill = "critical_temp"), color = "red",alpha = 0.3) +
    geom_density(aes(x = std_ThermalConductivity, fill = "std_ThermalConductivity"),color = "blue",alpha = 0.3) +
    ylab("Densidad") +
    xlab("Variables")


# Exploratorio

mayor_correlacion = matrix(NA, ncol = 3, nrow = 6)
for(i in 1:6){
    ndf = df[df$range_Valence == i,]
    vcor = tail(sort(abs(na.omit(cor(ndf)[,168][1:167]))),1)
    for(j in 1:(length(vcor))){
        mayor_correlacion[i, ] = c(as.numeric(i), names(vcor), as.numeric(vcor))
    }
}
mayor_correlacion

modelos = list()
reales = c(); simulados = c()
for(i in 1:6){
    ndf = df[df$range_Valence == i,]
    a = paste("modelos$rv",mayor_correlacion[i,1],"= lm(ndf$critical_temp~ndf$",mayor_correlacion[i,2],")",sep = "")
    eval(parse( text=a ))
    reales = c(reales,ndf$critical_temp)
    a = paste("simulados = c(simulados,modelos$rv",i,"$fitted.values)",sep = "")
    eval(parse( text=a ))
}

plot(density(simulados), col="red")
lines(density(reales),col="blue")



cor(reales,simulados)

modelo = lm(critical_temp ~ std_ThermalConductivity, data = df)

plot(density(df$critical_temp))
lines(density(modelo$fitted.values))

# Gráficos
p1 = ggplot(mapping = aes(x = C, y = critical_temp),data = df[df$number_of_elements == 1,] ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(method = "lm", se = FALSE,color = "deepskyblue4")

p2 = ggplot(mapping = aes(x = std_FusionHeat, y = critical_temp),data = df[df$number_of_elements == 2,] ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(method = "lm", se = FALSE,color = "deepskyblue4")

p3 = ggplot(mapping = aes(x = mean_atomic_mass, y = critical_temp),data = df[df$number_of_elements == 3,] ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(method = "lm", se = FALSE,color = "deepskyblue4")

p4 = ggplot(mapping = aes(x = Ba, y = critical_temp), data = df[df$number_of_elements == 4,] ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(method = "lm", se = FALSE,color = "deepskyblue4")

p5 = ggplot(mapping = aes(x = Ba, y = critical_temp),data = df[df$number_of_elements == 5,] ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(method = "lm",se = FALSE, color = "deepskyblue4")

p6 = ggplot(data = df[df$number_of_elements == 6,],mapping = aes(x = mean_ThermalConductivity, y = critical_temp) ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(method = "lm",se = FALSE, color = "deepskyblue4")

p7 = ggplot(data = df[df$number_of_elements == 7,],mapping = aes(x = Ca, y = critical_temp) ) +
    geom_point(color = "dodgerblue3") +
    geom_smooth(se = FALSE, method = "lm", color = "deepskyblue4")

p8 = ggplot(mapping = aes(x = std_Density, y = critical_temp),data = df[df$number_of_elements == 8,] ) +
    geom_point( color = "dodgerblue3") +
    geom_smooth(se = FALSE,method = "lm", color = "deepskyblue4")

p9 = ggplot(mapping = aes(x = wtd_entropy_ThermalConductivity, y = critical_temp),data = df[df$number_of_elements == 9,] ) +
    geom_point(color = "dodgerblue3") +
    geom_smooth(se = FALSE,method = "lm", color = "deepskyblue4")

title <- ggdraw() +
    draw_label(
        "Mayor correlacion segun number of elements",
        fontface = 'bold',
        x = 0,
        hjust = 0
    ) +
    theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
    )

plot_row = plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 5)
plot_grid(
    title, plot_row,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.1, 1)
)
