#library(devtools)
#library(roxygen2)

####################################################################################################
#####################################   Row Functions for n groups       ###########################
####################################################################################################

#########################################Categorical Variables######################################

###################
# No. (percentage)#
###################
#' A row function
#'
#' Creates no. and percentage for each level of the variable, 
#' with a p-value from Chi-square test or Fisher's exact test. 
#' @param Variable An R object. Categorical variable of interest.   
#' @param Group An R object. Group variable.
#' @param Margin a vector giving the desired margin to calculate percentages in the table for each group.
#' 1 indicates rows, 2 indicates columns. 
#' @details Can handle >1 group; number of columns will adjust automatically. 
#' @details Test based on expected cell value <5. Will only return row for last level of the factor.
#' @export
#' @examples x<-rep(c('Red','Blue'),c(10,40))  
#' @examples y<-rep(c('A','B'), 25) 
#' @examples prop_row(x,y)
#' prop_row()
prop_row<-function(y,x,margin,single) {
  
temp<-table(y,x)
temp.prop<-round(prop.table(temp,margin)*100,0)

if (length(levels(x))>1){
    temp.expect<-sum((suppressWarnings(chisq.test(temp))$expected<5)*1)
}

else{temp.expect=NULL}

if(length(levels(x))>1){
    
    if(temp.expect==0){
      p<-round(chisq.test(temp)$p.value,4)
      }
  
    else{
      p<-round(fisher.test(temp)$p.value,4)
      }
}

else{
  p<-NULL
}

temp1<-paste0(table(y), ' (',round(prop.table(table(y))*100,0), '%)')


if(length(levels(x))>1){
 
  temp2<-c(Variable="",c(rep("",ncol(temp)+1)),Pval=p)
  
  if(length(levels(y))<3 & single==T){
    
    t<-data.frame(rbind(temp2,c(Variable="",temp1[2],paste0(temp[2,]," (",temp.prop[2,],"%)"),Pval=p)))[2,]
  }
  
  else{
    for(i in (1:nrow(temp))){
      temp2<-rbind(temp2,c(Variable=rownames(temp)[i],temp1[i],paste0(temp[i,]," (",temp.prop[i,],"%)"),Pval=" "))
      } 
    t<-data.frame(temp2,row.names=NULL)
  }
  
t$Pval<-as.character(t$Pval)
t$Pval[t$Pval==0]<-"<0.0001"
}

else{
  
  temp2<-c(Variable="",V2="")
  
  if(length(levels(y))<3 & single==T){
    t<-suppressWarnings(data.frame(Variable="",V2=temp1[2]))
    }
  
  else{t<-suppressWarnings(data.frame(rbind(temp2,cbind(Variable=rownames(temp),V2=temp1))))
      }
}
 
return(t)  

}

####################################################################################################
#########################################Continuous Variables#######################################

##############
# Mean +/- SD#
##############
#' A row function
#'
#' This function creates mean and SD, p-value from t-test or ANOVA
#' @param Variable of interest and group variable 
#' @keywords Continuous  
#' @export
#' @examples
#' mean_row()
mean_row<-function(y,x,ron){
  
  mean.all<-round(mean(y,na.rm=T),ron)
  sd.all<-round(sd(y,na.rm=T),ron)
  
  mean.y<-round(tapply(y,x,mean,na.rm=T),ron)
  sd.y<-round(tapply(y,x,sd,na.rm=T),ron)

if(length(levels(x))>1){
    
    if (length(mean.y) == 2){
    p<-round(t.test(y~as.factor(x))$p.value,4)} 
    
    else{p<-round(summary(aov(y~as.factor(x)))[[1]][[1,"Pr(>F)"]],4)}
  }

else{p<-NULL}
    
  
if(length(levels(x))>1){
  m<-matrix(0,ncol=length(mean.y)+3,nrow=1)
  t<-data.frame(rbind(m,c(Variable="", paste0(mean.all,"\u00B1",sd.all), paste0(mean.y,"\u00B1",sd.y),Pval=p)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
}

else{
   m<-matrix(0,ncol=2,nrow=1)
   t<-data.frame(rbind(m,c(Variable="", paste0(mean.all,"\u00B1",sd.all))))[-1,]
}
  
  return(t)
}

###############
# Median (IQR)#
############### 
#' A row function
#'
#' This function creates median and IQR, p-value from Wilcox rank-sum or Kruskal-Wallis
#' @param Variable of interest and group variable 
#' @keywords Continuous, non-normal  
#' @export
#' @examples
#' median_row()
median_row<-function(y,x,ron){
  
  med.all<-round(median(y,na.rm=T),ron)
  q1.all<-round(quantile(y,.25,na.rm=T),ron)
  q2.all<-round(quantile(y,.75,na.rm=T),ron)
  
  med.y<-round(tapply(y,x,median,na.rm=T),ron)
  q1<-round(tapply(y,x,function(x) quantile(x,.25,na.rm=T)),ron)
  q2<-round(tapply(y,x,function(x) quantile(x,.75,na.rm=T)),ron)
  
  if(length(levels(x))>1){
    
    if (length(med.y) == 2){
      p<-round(wilcox.test(y~x)$p.value,4)} 
    
    else{p<-round(kruskal.test(y~x)$p.value,4)}
  }
  
  else{p<-NULL}
  
  if(length(levels(x))>1){
  
  m<-matrix(0,ncol=length(med.y)+3,nrow=1)
  t<-data.frame(rbind(m,c(Variable="",paste0(med.all," (",q1.all,", ",q2.all,")"),
                          paste0(med.y," (",q1,", ",q2,")"),Pval=p)))[-1,]
  
  t$Pval<-as.character(t$Pval)
  t$Pval[t$Pval==0]<-"<0.0001"
  }
  
  else{
    m<-matrix(0,ncol=2,nrow=1)
    t<-data.frame(rbind(m,c(Variable="",paste0(med.all," (",q1.all,", ",q2.all,")"))))[-1,]
  }
  
  return(t)
}


#######################################################################################################
#These functions work regardless of number of columns in table (groups in dataset)#

#' A function to create column names with sample sizes
#' @param Grouping variable 
#' @keywords Column labels with sample size 
#' @export
#' @examples
#' column_label()
column_label<-function(x){
  if(length(levels(x))>1){
    c("Characteristic", paste0("All (n=",sum(table(x)),")"),
    paste0(levels(x)," (n=",table(x),")"),
    "P Value")}
  else{c("Characteristic", paste0("All (n=",sum(table(x)),")"))}
}
###################################################################################
#' A function to determine which row function to employ
#' @param Variable of interest and group variable 
#' @keywords Categorical, continuous variables 
#' @export
#' @examples
#' fx_rows()
fx_rows<-function(variable,group,margin,single,ron,summary.stat){
  
    if(is.numeric(variable)==T & summary.stat=='mean'){
    
      temp<-mean_row(variable,group,ron)
    }
    
    else if(is.numeric(variable)==T & summary.stat=='median'){
      temp<-median_row(variable,group,ron)
    }
   
   else if(is.numeric(variable)==T & summary.stat=='both')
     {
     if(abs(moments::skewness(variable,na.rm=T))<=3){
       temp<-mean_row(variable,group,ron)
     }
     
     else if(abs(moments::skewness(variable,na.rm=T))>3){
       temp<-median_row(variable,group,ron)
     }
   }
  
  else{
    temp<-prop_row(variable,group,margin,single)
  }
  
  return(temp)
}
##################################################################################
#' A function to generate a * to add to the label for non-normally distributed continuous variables
#' @param Variable of interest
#' @keywords * indication
#' @export
#' @examples
#' fx_symbol()
fx_symbol<-function(y,x){
  
  if(length(levels(x))>1){
  
  temp<-table(y,x)
  temp.expect<-sum((suppressWarnings(chisq.test(temp))$expected<5)*1)
  
  if(is.numeric(y)==F & temp.expect!=0){
    symbol<-"*"
    }
    
  else{
    symbol<-""
    }
  }
  
  else{
    symbol<-""
  }
  return(symbol)
}
##################################################################################################
#' A function to create a summary table for 1 or more groups. 
#' @param data a data frame containing the variables listed in variables 
#' and the group variable.  
#' @param variables a vector or scalar of variables of interest, in quotation marks.      
#' @param group an R object. Grouping variable can have >=1 level(s).  
#' @param margin a number, 1 or 2, giving the desired margin to calculate percentages over.
#' 1 indicates rows, 2 indicates columns. 
#' @param single a logical indicating whether categorical variables with 2 levels should be
#' presented as a single row. This row will correspond to the second level listed for the factor. 
#' @param ron a number giving the desired decimal places in rounding for continuous variables. 
#' Defaults to 0 unless otherwise indicated.
#' Note: p value rounding set to 4 digits. 
#' @param col.names a logical value indicating whether the column labels with group name and sample size are desired. 
#' @param summary.stat a character string specifying the summary statistic for continuous variables, 
#' must be one of "both" (default), "median" or "mean". 
#' @keywords Table 1
#' @details Can handle >=1 group; number of columns will adjust automatically. T-test or ANOVA performed 
#' for continuous variables with skewness <=3. Chi-squared or Fisher's Exact (for expected cell value <5) performed 
#' for categorical variables. Fisher's exact test is indicated by * next to variable name. One summary column in the case of a single group.    
#' @export
#' @examples iris$sepal_di<-as.factor((iris$Sepal.Length<5)*1)
#' #set factor levels
#' levels(iris$sepal_di)=c("<5",'>=5')
#' levels(iris$Species)=c('Setosa','Versicolor','Virginica')
#' #Set Labels
#' label(iris$sepal_di)='Sepal Length'
#' label(iris$Sepal.Length)="Sepal Length"
#' label(iris$Sepal.Width)='Sepal Width'
#' label(iris$Petal.Length)='Petal Length'
#' label(iris$Petal.Width)='Petal Width'
#' tab1<-final_table(iris,c('Sepal.Length','Petal.Length','Sepal.Width','Petal.Width','sepal_di'),iris$Species,1,T,2)
final_table<-function(data,variables,group,margin=2,single=F,ron, col.names=T, summary.stat='both'){
  
  #create table
  temp<-data.frame(do.call(rbind,lapply(data[variables],fx_rows,group,margin,single,ron,summary.stat)),row.names=NULL)
  temp$Variable<-as.character(temp$Variable)
  temp$Variable[temp$Variable==""]<-paste0(Hmisc::label(data[variables]),lapply(data[(variables)],fx_symbol,group))
 
  #remove numbering from factor levels
  temp$Variable<-gsub(".*\\^", "", temp$Variable)
  
  #set column names with sample sizes
  if (col.names==T){
  colnames(temp)<-column_label(group)
  }
  
  return(temp)
}

########################################################################################################
#Proportion of missing data
#' A function to quantify missing data
#'
#' This function creates no. and % of missing data
#' @param Variable of interest and group variable 
#' @keywords Continuous, non-normal  
#' @export
#' @examples
#' missing_row()
missing_row<-function(y,x){
  
  miss.all<-paste0(length(x[is.na(x)==T])," (",round((length(x[is.na(x)==T])/length(x))*100),'%',")")
  
  miss.groups<-tapply(y,x, function(x) paste0(length(x[is.na(x)==T])," (",round((length(x[is.na(x)==T])/length(x))*100),'%',")"))
  
  m<-matrix(0,ncol=length(miss.groups)+2,nrow=1)
  
  #put row together
  t<-data.frame(rbind(m,c(Variable="",All=miss.all,miss.groups)))
  
  return(t[-1,])
}

##################################################################################################
#' A function to create a summary of missing data for 1 or more groups. 
#' @param data a data frame containing the variables listed in variables 
#' and the group variable.  
#' @param variables a vector or scalar of variables of interest, in quotation marks.      
#' @param group an R object. Grouping variable can have >=1 level(s).  
#' @param col.names a logical value indicating whether the column labels with group name and sample size are desired.
#' @keywords Missing data, summary table
#' @details Can handle >=1 group; number of columns will adjust automatically. 
#' @export
#' @examples 
#' iris$sepal_di<-as.factor((iris$Sepal.Length<5)*1)
#' #set factor levels
#' levels(iris$sepal_di)=c("<5",'>=5')
#' levels(iris$Species)=c('Setosa','Versicolor','Virginica')
#' #Set Labels
#' label(iris$sepal_di)='Sepal Length'
#' label(iris$Sepal.Length)="Sepal Length"
#' label(iris$Sepal.Width)='Sepal Width'
#' label(iris$Petal.Length)='Petal Length'
#' label(iris$Petal.Width)='Petal Width'
#' tab_miss<-missing_table(iris,c('Sepal.Length','Petal.Length','Sepal.Width','Petal.Width','sepal_di'),iris$Species)
missing_table<-function(data,variables,group,col.names=T){
  
  #create table
  temp<-data.frame(do.call(rbind,lapply(data[variables],missing_row,group)),row.names=NULL)
  temp$Variable<-as.character(temp$Variable)
  temp$Variable<-Hmisc::label(data[variables])
  
  #set column names with sample sizes
  if (col.names==T){
    colnames(temp)<-column_label(group)[-length(column_label(group))]
  }
  
  return(temp)
}
###############################################################################
#' A function to describe the distribution of a variable 
#' Tabulates and displays mean, geometric mean, median, proportion missing, 
#' histogram and histogram of the log outcome
#' @param Variable of interest and group variable 
#' @keywords mean, median, distribution  
#' @export
#' @examples
#' dist_check()
dist_check<-function(var1){
  par(mfrow=c(2,2))
  hist(var1)
  hist(log(var1))
  avg<-mean(var1,na.rm=T)
  avg_exp<-exp(mean(log(var1),na.rm=T))
  med<-median(var1,na.rm=T)
  miss<-length(var1[is.na(var1)==T])/length(var1)
  m<-min(var1,na.rm=T)
  M<-max(var1,na.rm=T)
  
  return(list(mean=avg,geom_mean=avg_exp,median=med,missing=miss,minimum=m,maximum=M))
}

####################################################
########      Regression Models     #############
####################################################
label_fx<-function(x){
  
  if(is.numeric(x)==T) {
    lab<-label(x)
  }
  else{lab<-paste(label(x),'-',names(table(x))[-1])
  }
}

####################################################
########      Linear regression      #############
####################################################
#' A function to create a summary of a linear regression model.
#' @param y outcome variable of interest to be pasted in formula, in quotation marks.  
#' @param x a single explanatory variable, or multiple variables separated by +, in quotation marks.  
#' @param data a data frame containing the variables listed in x and y.  
#' @keywords Linear regression, summary table
#' @details Table with estimates, 95% CIs and p-values.  
#' @export
#' @examples 
#' iris$sepal_di<-as.factor((iris$Sepal.Length<5)*1)
#' #set factor levels
#' levels(iris$sepal_di)=c("<5",'>=5')
#' levels(iris$Species)=c('Setosa','Versicolor','Virginica')
#' #Set Labels
#' label(iris$Species)='Species'
#' label(iris$sepal_di)='Sepal Length'
#' label(iris$Sepal.Length)="Sepal Length"
#' label(iris$Sepal.Width)='Sepal Width'
#' label(iris$Petal.Length)='Petal Length'
#' label(iris$Petal.Width)='Petal Width'
#' mod_tab<-fx_model_linear('Sepal.Length',c('Species','Petal.Length'),iris)

fx_model_linear<-function(y,x,data){
  
  f<-formula(paste0(y,"~",paste0(x,collapse = '+')))
  model<-lm(f,data)
  
  ret<-data.frame(Independent_variable=unlist(lapply(data[x],label_fx)),
                  Estimate=paste0(round(coef(model)[-1],2)," (",
                                  round(confint(model)[,1],2)[-1],", ",
                                  round(confint(model)[,2],2)[-1],")"),
                  Pvalue=round(summary(model)$coef[,4],4)[-1],
                  row.names=NULL)
  
  ret$Pvalue[ret$Pvalue==0]<-'<0.0001'
  colnames(ret)<-c("Predictor","Estimate (95% CI)","P Value")
  return(ret)
  
}

####################################################
########      Logistic regression      #############
####################################################
#' A function to create a summary of a logistic regression model.
#' @param y outcome variable of interest to be pasted in formula, in quotation marks.  
#' @param x a single explanatory variable, or multiple variables separated by +, in quotation marks.  
#' @param data a data frame containing the variables listed in x and y.  
#' @keywords Linear regression, summary table
#' @details Table with estimates, 95% CIs and p-values.  
#' @export
#' @examples 
#' iris$sepal_di<-as.factor((iris$Sepal.Length<5)*1)
#' #set factor levels
#' levels(iris$sepal_di)=c("<5",'>=5')
#' levels(iris$Species)=c('Setosa','Versicolor','Virginica')
#' #Set Labels
#' label(iris$Species)='Species'
#' label(iris$sepal_di)='Sepal Length'
#' label(iris$Sepal.Length)="Sepal Length"
#' label(iris$Sepal.Width)='Sepal Width'
#' label(iris$Petal.Length)='Petal Length'
#' label(iris$Petal.Width)='Petal Width'
#' mod_tab<-fx_model_logistic('sepal_di',c('Petal.Length','Petal.Width'),iris)
fx_model_logistic<-function(y,x,data){
  
  f<-formula(paste0(y,"~",paste0(x,collapse = '+')))
  model<-glm(f,data,family='binomial')
  
  ret<-data.frame(Independent_variable=unlist(lapply(data[x],label_fx)),
                  OR=paste0(round(exp(coef(model)[-1]),2)," (",
                            round(exp(confint(model)[,1]),2)[-1],", ",
                            round(exp(confint(model)[,2]),2)[-1],")"),
                  Pvalue=round(summary(model)$coef[,4],4)[-1],
                  row.names=NULL)
  
  ret$Pvalue[ret$Pvalue==0]<-'<0.0001'
  colnames(ret)<-c("Parameter","OR (95% CI)","P Value")
  return(ret)
}

# #change documentation
# setwd('..')
# document()
