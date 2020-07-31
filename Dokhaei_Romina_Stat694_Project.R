

library(config)
library(configr)
library(ggplot2)

# Merge multiple sections, default is all sections 
# You can use this to reduce one layer in configuration file
json.config.all <- eval.config.merge(file = "config.json")


#read config parametrs
name_dataset=unlist(json.config.all["name_dataset"][1])
x_var=unlist(json.config.all["x_var"][1])
y_var=unlist(json.config.all["y_var"][1])
cov=unlist(json.config.all["covariate"][1])
cov_name=unlist(json.config.all["covariate_name"][1])
result_file=unlist(json.config.all["name_file"][1])
result_plot=unlist(json.config.all["name_pic"][1])

#read data
data=read.csv(name_dataset)

#run regression model
reg_mod=lm(data=data,data[,c(y_var)]~data[,c(x_var)])

#save regression summary as a text file in the result folder
sink(result_file)
print(summary(lm(cars$speed ~ cars$dist)))
sink()

#make regression plot
l <- list(a=format(coef(reg_mod)[1], digits=2), 
          b=format(coef(reg_mod)[2], digits=2), 
          r2=format(summary(reg_mod)$r.squared, digits=2), 
          p=format(summary(reg_mod)$coefficients[2, 4], digits=2))
d<-as.data.frame(l)
d[] <- lapply(d, function(x) as.numeric(as.character(x)))
eq <- substitute(italic(y)==a+b*italic(x) ~','~italic(R)^2 ~'='~ r2,d)

p <- ggplot(data, aes(data[,c(x_var)], data[,c(y_var)] )) +
  geom_point()+stat_smooth(method='lm')+geom_text(aes(x =min(data[,c(x_var)]+((range(data[,c(x_var)])[2]-range(data[,c(x_var)])[1])/4)), y = max(data[,c(y_var)]), label = as.character(as.expression(eq))), parse = TRUE)+
  xlab(x_var)+ylab(y_var)


# save regression plot
ggsave(plot =p,result_plot,width=16,height=14,units='cm')