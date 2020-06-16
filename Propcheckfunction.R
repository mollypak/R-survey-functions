propschecks<-function(data, design,variab, outcome){
  uni<-sort(unique(data[,variab]))
  for(i in 1:length(uni)){
    print.noquote(paste(variab, "=",uni[i]))
    print.noquote(paste("Denominators:",table(data[,variab])[i]))
    if(table(data[,variab])[i]<30) print.noquote(paste('demoninator less than 30 for',variab,"=",uni[i],"!SUPRESS!"))
    print.noquote(paste(round(svyciprop(formula(paste("~",outcome)), subset(design,get(variab)==uni[i]), na.rm=TRUE, method="beta"),3)*100,
                 "(",round(attributes(svyciprop(formula(paste("~",outcome)), subset(design,get(variab)==uni[i]), na.rm=TRUE, method="beta"))$ci[1],3)*100,"-",
                 round(attributes(svyciprop(formula(paste("~",outcome)), subset(design,get(variab)==uni[i]), na.rm=TRUE, method="beta"))$ci[2],3)*100,")"))
 
       val<-svyciprop(formula(paste("~",outcome)), subset(design,get(variab)==uni[i]), na.rm=TRUE, method="beta")
   upp<- attributes(svyciprop(formula(paste("~",outcome)), subset(design,get(variab)==uni[i]), na.rm=TRUE, method="beta"))$ci[2]
  lowe<-attributes(svyciprop(formula(paste("~",outcome)), subset(design,get(variab)==uni[i]), na.rm=TRUE, method="beta"))$ci[1]
  wid<-upp-lowe
  if(wid>0.30) print.noquote(paste('absolute CI width >0.30 for',variab,"=",uni[i],"!SUPRESS!,absolute CI width=",wid))
  if((wid/val)>1.30) print.noquote(paste('relative CI width >1.30 for',variab,"=",uni[i],"!SUPRESS!,absolute CI width=",wid/val))
  if(degf(design)<8) print.noquote("Degrees of freesdom less than 8")
  if(wid<0.05 &(degf(design)<8|table(data_full[,variab])[i]==0)) print.noquote(paste('absolute width <0.05 for',variab,"=",uni[i],"and either df<8 or 0 events, !FLAGGED!"))
  
  
   }
  
}


propschecks_mi<-function(data, design,variab, outcome){
  uni<-sort(unique(data[,variab]))
  for(i in 1:length(uni)){
    print.noquote(paste(variab, "=",uni[i]))
    print.noquote(paste("Denominators:",table(data[,variab])[i]))
    if(table(data[,variab])[i]<30) print.noquote(paste('demoninator less than 30 for',variab,"=",uni[i],"!SUPRESS!"))
      print(paste(as.numeric(MIcombine(with(subset(design,get(variab)==uni[i]), exp=svymean(~iggpos, na.rm=TRUE)))[1]),
                                   " (",kg(~iggpos,subset(design,get(variab)==uni[i]))[1],", ",
                                   kg(~iggpos,subset(design,get(variab)==uni[i]))[2],")", sep = ""))
         

      
    val<-MIcombine(with(subset(design,get(variab)==uni[i]), exp=svymean(~iggpos, na.rm=TRUE)))$coefficients[1]
    upp<- kg(~iggpos,subset(design,get(variab)==uni[i]))[2]
    lowe<-kg(~iggpos,subset(design,get(variab)==uni[i]))[1]
    wid<-upp-lowe
    if(wid>0.30) print.noquote(paste('absolute CI width >0.30 for',variab,"=",uni[i],"!SUPRESS!,absolute CI width=",wid))
    if((wid/val)>1.30) print.noquote(paste('relative CI width >1.30 for',variab,"=",uni[i],"!SUPRESS!,absolute CI width=",wid/val))
    
    
  }
  
}





