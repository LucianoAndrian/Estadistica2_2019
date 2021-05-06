rm(list=ls())
library(ggplot2)
datos<-read.table("datos_Ej3.txt")
plot(datos$x,datos$y,type = "p")
#cor(datos$x,datos$y,method = "pearson")
#ajuste<-lm(datos$x~datos$y)
#abline(ajuste)
pearson<-cor.test(datos$x,datos$y,method = "pearson",conf.level = 0.95)
kendall<-cor.test(datos$x,datos$y,method = "kendall",conf.level = 0.95)

####################################################################################
regre <- function(df){
  m <- lm(y ~ x, df);
  eq <- substitute(y == b %.% italic(x) + a *","~~r^2~"="~r2, 
                   list(a = round(coef(m)[[1]],digits=2), 
                        b = round(coef(m)[[2]], digits = 4), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

ggplot(as.data.frame(datos), aes(x=x, y=y)) + geom_point(color = "black", size = 2) +
    ylab("Y")+ xlab("X")+ theme_bw() + 
    stat_smooth(method = "lm", formula = y~x, se = TRUE, level= 0.95) +
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(x = 0.5, y = 0.5, label = regre(as.data.frame(datos)), parse = TRUE,size=5)  

##########################################################################################
#ggsave("linear_reg.JPG",plot=grid.arrange(Vlag[[1]],Vlag[[2]],Vlag[[3]],ncol=2),width = 30,height = 15 ,units = "cm")
