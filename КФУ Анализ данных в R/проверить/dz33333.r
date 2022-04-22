setwd("C:/Users/pc/Desktop/univer/3kurs/r_course/Data")
read.csv2("bloodwork_sheet.csv",stringsAsFactors=TRUE,sep = ",",na.strings=c("","NA"))->data

setwd("C:/Users/pc/Desktop/univer/3kurs/r_course/Data")
data <- read.csv("data__Kopia.csv", header=TRUE, sep=";", dec=",");
data_ready <- na.omit(data);

norms=read.csv("norms.csv",header=TRUE, sep=";", dec=",");

#если столбец гендера не на своем месте
gender_index = grep("gender", colnames(data_ready))

#сначала для межучин, потом для женщин
#по результатам выдаем "low", "norm", "high"
for (analisys_name in names(data_ready)[4:10]){
  data_ready[data_ready[gender_index]=="male", analisys_name] =
    cut( data_ready[data_ready[gender_index]=="male", analisys_name] ,
         breaks=c(0, as.numeric(norms[1,analisys_name]), as.numeric(norms[3,analisys_name]), Inf),
         labels=c("low", "norm","high"));
  data_ready[data_ready[gender_index]=="female", analisys_name] =
    cut( data_ready[data_ready[gender_index]=="female", analisys_name] ,
         breaks=c(0,as.numeric(norms[2,analisys_name]), as.numeric(norms[4,analisys_name]), Inf),
         labels=c("low", "norm", "high"));
  data_ready[, analisys_name] =
    cut( data_ready[, analisys_name], breaks=c(0, 1, 2, Inf), labels=c("low", "norm","high"))
}
norms = c()
#количество нормальных анализов
for(r in 1:nrow(data_ready)){
  norms = c(norms,norm_count(data_ready[r,], "norm"))  
}
data_ready = cbind(data_ready, norms)

#график "ненормальности анализов" в соответствии с типом заболевания
barplot(tapply(data_ready$norms!=7, data_ready$Type, FUN=sum))
barplot(tapply(data_ready$norms==7, data_ready$Type, FUN=sum))

#строим для анализов, которые ниже нормы(для анализов выше нормы аналогично)
lows = c()
for(r in 1:nrow(data_ready)){
  lows = c(lows,norm_count(data_ready[r,], "low"))  
}
data_ready = cbind(data_ready, lows)
barplot(tapply(data_ready$Type=="Healthy", data_ready$lows, FUN=sum), 
        legend.text = "Сколько анализов ниже при")

barplot(tapply(data_ready$Type=="Healthy", data_ready$counts, FUN=sum), 
        legend.text = "Сколько анализов не в норме при")

#запись в файл
write.csv(data_ready, "C:/Users/pc/Desktop/univer/3kurs/r_course/Data/out_norms.csv", row.names = FALSE)

norm_count <- function(vector, line_str) {
  temp_C <- length(vector[vector[] == line_str])
  return(temp_C)
}