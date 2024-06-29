#' Descriptive Summary for a numerical vector.
#' @param x must be numeric.
#' @return
#' N = Number of observations in \code{x}.
#'
#' Total= Sum of \code{x}.
#'
#' MEAN= Mean of \code{x}.
#'
#' SD= Standard deviation of \code{x}.
#'
#' MEDIAN= Median of \code{x}.
#'
#' MIN= Minimum of \code{x}.
#'
#' MAX= Maximum of \code{x}.
#'
#' IQR= Interquartile range (Q1,Q3) of \code{x}.i.e., 1st quantile and the 3rd quantile.
#'
#' Note: All the summary statistics will be calculated after removing the \code{NA} values.
#'
#' @export
#'
#' @examples
#'
#' ##-- For Simple vector --##
#'
#' # Create a vector:
#' x=c(1:10)
#'
#' # Generate Summary Statistics:
#'   descriptive(x)
#'
#' ##-- For data frame --##
#'
#' ## IRIS data:
#'
#' # descriptive summary of  Sepal Length, Sepal Width, Petal Length and, Petal Width
#'
#' rbind(with(iris,descriptive(x= Sepal.Length)),
#'       with(iris,descriptive(x= Sepal.Width)),
#'       with(iris,descriptive(x= Petal.Length)),
#'       with(iris,descriptive(x= Petal.Width)))
#'

descriptive=function(x){data.frame(Var= deparse(substitute(x)),
                                     N=length(na.omit(x)),
                                     TOTAL=round(sum(x,na.rm = T),2),
                                     MEAN=round(mean(x,na.rm=T),2),
                                     SD=round(sd(x,na.rm=T),2),
                                     MEDIAN=round(median(x,na.rm=T),2),
                                     MIN=round(min(x,na.rm = T),2),
                                     MAX=round(max(x,na.rm = T),2),
                                     IQR=paste("(",sprintf("%0.2f",summary(x)[2]),",",sprintf("%0.2f",summary(x)[5]),")",sep=""))}



#' Group wise descriptive summary of a numeric vector.
#'
#' To generate summary statistics for a numerical variable '\code{x}' at different levels of a particular factor variable '\code{group}' in a \code{data.frame}.
#'
#'
#' @param x must be numeric.
#' @param group must be factor.
#' @param xname Name of numeric variable. Default is the name of x.
#'
#'
#' @return
#' N = Number of observations in \code{x}.
#'
#' Total= Sum of \code{x}.
#'
#' MEAN= Mean of \code{x}.
#'
#' SD= Standard deviation of \code{x}.
#'
#' MEDIAN= Median of \code{x}.
#'
#' MIN= Minimum of \code{x}.
#'
#' MAX= Maximum of \code{x}.
#'
#' IQR= Interquartile range (Q1,Q3) of \code{x}.i.e., 1st quantile and the 3rd quantile.
#'
#' Note: All the summary statistics will be calculated after removing the \code{NA} values.
#'
#' @export
#'
#' @examples
#'
#' ## IRIS data:
#'
#' # descriptive summary of  Sepal Length, Sepal Width, Petal Length and, Petal Width
#'
#' rbind(with(iris,descriptive_groupby(x= Sepal.Length, group= Species)),
#'       with(iris,descriptive_groupby(x= Sepal.Width, group= Species)),
#'       with(iris,descriptive_groupby(x= Petal.Length, group= Species)),
#'       with(iris,descriptive_groupby(x= Petal.Width, group= Species)))
#'
descriptive_groupby= function(x,group,xname= deparse(substitute(x)) ){
  descp=function(x){data.frame(N=length(na.omit(x)),TOTAL=round(sum(x,na.rm = T),2),MEAN=round(mean(x,na.rm=T),2),SD=round(sd(x,na.rm=T),2),MEDIAN=round(median(x,na.rm=T),2),MIN=round(min(x,na.rm = T),2),MAX=round(max(x,na.rm = T),2),IQR=paste("(",sprintf("%0.2f",summary(x)[2]),",",sprintf("%0.2f",summary(x)[5]),")",sep=""))}
  tab= do.call(rbind,tapply(x, group, descp))
  tab= data.frame(Var=c(xname,rep("",nlevels(group)-1)),
                  Group= levels(group),
                  tab)
  row.names(tab)=NULL
  return(tab)

}


#' Column-wise count of missing values in a data frame.
#'
#' @param data data.frame
#'
#' @return
#' data.frame
#'
#' @export
#'
#' @examples
#'
#' ## airquality Data from R:
#'
#'missing.count(airquality)
#'
missing.count=function(data){
  na.fun=function(x){
    return(sum(is.na(x)))
  }
  tab=data.frame(apply(data,2,na.fun))
  tab=data.frame(Var=row.names(tab),
                 No.of.missings= tab$apply.data..2..na.fun.,row.names = NULL)

  ifelse(sum(tab$No.of.missings)>0,
         return(tab[which(tab$No.of.missings!=0),]),
         print("There are no missing values."))
}


#' Association of a numeric variable between multiple category (>2) of a factor variable.
#'
#' It compares the distributions of a numeric variable at different levels of a factor variable and also checks their association
#' by performing One-way ANOVA if the distribution of the normal or by performing Kruskal Wallis test if it is skewed and summarize
#' the summary in a data frame format. By default it performs both.
#'
#'
#' @param x must be numeric vector.
#' @param group must be factor with greater than 2 levels.
#' @param xname Name of numeric variable. Default is the name of x.
#'
#' @return
#' N = Number of observations in \code{x}.
#'
#' Total= Sum of \code{x}.
#'
#' MEAN= Mean of \code{x}.
#'
#' SD= Standard deviation of \code{x}.
#'
#' MEDIAN= Median of \code{x}.
#'
#' IQR= Interquartile range (Q1,Q3) of \code{x}.i.e., 1st quantile and the 3rd quantile.
#'
#' shapiro.test.PValue= P-value of the Shapiro test.
#'
#' Test.Stat = 1 way ANOVA F statistics.
#'
#' Par.Pval= Parametric test's P-value,i.e., 1 way ANOVA test's P-value.
#'
#' NP.Pval= Non parametric test P-value,i.e., Kruskal Wallis test's P-value.
#'
#' Note: (1) All the summary statistics will be calculated after removing the \code{NA} values.
#'
#'       (2) If shapiro.test$p-value >0.05 i.e not significant => the data is Normal otherwise,
#'           <0.05 i.e., significant then it is skewed
#'
#' @export
#'
#' @examples
#'
#' ## IRIS data:
#'
#' # descriptive summary of  Sepal Length, Sepal Width, Petal Length and, Petal Width
#'
#' rbind(with(iris,indp_K_Populations(x= Sepal.Length, group= Species)),
#'       with(iris,indp_K_Populations(x= Sepal.Width, group= Species)),
#'       with(iris,indp_K_Populations(x= Petal.Length, group= Species)),
#'       with(iris,indp_K_Populations(x= Petal.Width, group= Species)))
#'
indp_K_Populations = function(x,group,xname=deparse(substitute(x))){
  descrip=function(x){c(N=length(na.omit(x)),Mean=round(mean(x,na.rm=T),2),SD=round(sd(x,na.rm=T),2),Median=round(median(x,na.rm=T),2),I.Q.R=paste("(",sprintf("%0.2f",summary(x)[2]),",",sprintf("%0.2f",summary(x)[5]),")",sep=""))}
  tab=do.call(rbind,tapply(x,group,descrip))
  tab=data.frame(var=c(xname,rep("",nlevels(group)-1)),
                 Group=c(deparse(substitute(group)),rep("",nlevels(group)-1)),
                 Levels=levels(group),
                 tab,
                 shapiro.test.PValue=round(as.numeric(as.data.frame(do.call(rbind,tapply(x,group,shapiro.test)))$p.value),3),
                 Test.Stat=c(paste("F-test:",round(summary(aov(x~group))[[1]][1,4],3),sep=" "),rep("",nlevels(group)-1)), ##-- 1 way ANOVA F value
                 Par.Pval=c(ifelse(sprintf("%0.3f",summary(aov(x~group))[[1]][1,5])=="0.000","<0.001",sprintf("%0.3f",summary(aov(x~group))[[1]][1,5])),rep("",nlevels(group)-1)),                    ##-- 1 way ANOVA P value
                 NP.Pval=c(ifelse(sprintf("%0.3f",kruskal.test(x~group)$p.value)=="0.000","<0.001",sprintf("%0.3f",kruskal.test(x~group)$p.value)),rep("",nlevels(group)-1)))                   ##-- Kruskal Wallis test
  rownames(tab)=NULL
  #names(tab)[2]= deparse(substitute(group))
  tab
}


#' One sample pare-wise comparison test
#'
#'
#' This function helps to compare the distribution of two samples when each observation
#' in one sample can be paired with an observation in the other sample i.e., a measurement
#' is taken on a subject before and after some treatment or,measurement is taken under two
#' different conditions – e.g. the response time of a patient is measured on two different drugs.
#'
#'
#' @param x.before measurement of the random variable at the 1st time point or at the 1st condition.
#' @param x.after measurement of the random variable at the 2nd time point or at the 2nd condition.
#' @param var Name of numeric variable.
#'
#' @return
#' N = Number of observations in \code{x}.
#'
#' Total= Sum of \code{x}.
#'
#' MEAN= Mean of \code{x}.
#'
#' SD= Standard deviation of \code{x}.
#'
#' MEDIAN= Median of \code{x}.
#'
#' IQR= Interquartile range (Q1,Q3) of \code{x}.i.e., 1st quantile and the 3rd quantile.
#'
#' Mean.diff= Mean difference between \code{x.before} and \code{x.after}.
#'
#' Conf.Int = 95% confidence interval of the mean difference. derived from the paired t-test.
#'
#' Par.Pval= Parametric test's P-value,i.e., paired t-test's P-value.
#'
#' NP.Pval= Non parametric test P-value,i.e., Wilcoxon Signed Rank test's P-value.
#'
#' Note: All the summary statistics will be calculated after removing the \code{NA} values.
#'
#'
#' @export
#'
#' @examples
#'
#' ## sleep data:
#'
#' paired_one_Populations(x.before = sleep$extra[sleep$group==1],
#'                        x.after = sleep$extra[sleep$group==2],
#'                        var = "extra")
#'
paired_one_Populations = function(x.before,x.after,var){
  descrip=function(x){c(N=length(na.omit(x)),Mean=round(mean(x,na.rm=T),2),SD=round(sd(x,na.rm=T),2),Median=round(median(x,na.rm=T),2),I.Q.R=paste("(",sprintf("%0.2f",summary(x)[2]),",",sprintf("%0.2f",summary(x)[5]),")",sep=""))}
  tab=rbind(descrip(x.before),descrip(x.after))
  tab=data.frame(Var=c(var,""),
                 Period=c("Before","After"),
                 tab,
                 Mean.diff= c(round(t.test(x.before,x.after,paired=T)$estimate,3),""),
                 Conf.Int= c(paste("(",round(t.test(x.before,x.after,paired=T)$conf.int[1],3),",",round(t.test(x.before,x.after,paired=T)$conf.int[2],3),")",sep = ""),""),
                 Par.Pval=c(sprintf("%0.3f",t.test(x.before,x.after,paired=T)$p.value),""),
                 NP.Pval=c(sprintf("%0.3f",wilcox.test(x.before,x.after,paired = T,exact=FALSE)$p.value),""))
  rownames(tab)=NULL
  tab
}


#' One sample pare-wise comparison test
#'
#'
#'
#'
#' @param x must be numeric.
#' @param group must be factor with greater than 2 levels.
#' @param dunn.method adjusts the p-value for multiple comparisons using the
#'        Bonferroni, Šidák, Holm, Holm-Šidák, Hochberg, Benjamini-Hochberg,
#'        or Benjamini-Yekutieli adjustment (see details: ?FSA::dunnTest). default
#'        option that is used here is "bh"=>Benjamini-Hochberg
#'
#' @return
#' N = Number of observations in \code{x}.
#'
#' Total= Sum of \code{x}.
#'
#' MEAN= Mean of \code{x}.
#'
#' SD= Standard deviation of \code{x}.
#'
#' MEDIAN= Median of \code{x}.
#'
#' IQR= Interquartile range (Q1,Q3) of \code{x}.i.e., 1st quantile and the 3rd quantile.
#'
#' Mean.diff= Mean difference between \code{x.before} and \code{x.after}.
#'
#' DunnTest_P.adj = Dunn test's adjusted P-value by using bh method.
#'
#' DunnTest_P.unadj= Dunn test's adjusted P-value to determine statistical significance for each pairwise comparison.
#'
#'
#' @note
#' Terms like "unadjusted p-value" and "adjusted p-value" are frequently used in statistical hypothesis testing,
#' especially when performing multiple comparisons.
#'
#' The p-value derived from a statistical test without multiple comparison adjustment is known as the "unadjusted p-value."
#' Under the presumption that the null hypothesis is true, it expresses the likelihood of getting a test statistic that is as
#' extreme as or more extreme than the one that was observed.
#'
#' For several comparisons done at the same time, the adjusted p-value (also known as the corrected p-value) is utilized.
#' The probability of incorrectly rejecting the null hypothesis (Type I error) rises with more comparisons. In order to
#' account for this, adjusted p-values regulate the total Type I error rate (which includes the false discovery rate and
#' family-wise error rate).
#'
#' Now, in terms of the Dunn test—a non-parametric substitute for the one-way ANOVA—it is utilized for pairwise comparisons
#' after the Kruskal-Wallis test. The p-value that is acquired straight out of the Dunn test, without any multiple comparison
#' correction, is known as the unadjusted p-value.
#'
#' Here’s what the unadjusted p-value means in the context of the Dunn test:
#'
#' (1) Each pairwise comparison will produce an uncorrected p-value of its own if you use the Dunn test to perform numerous pairwise
#' comparisons after discovering a significant result in the Kruskal-Wallis test (which determines whether the distributions of
#' various groups differ).
#'
#' (2) A pairwise difference can be deemed statistically significant if its unadjusted p-value is compared to your selected significance
#' threshold, which is often 0.05. You reject the null hypothesis and determine that there is a significant difference between the
#' groups under comparison if the unadjusted p-value is smaller than your significance level.
#'
#' (3) sConducting numerous comparisons raises the likelihood of making a Type I error since each pairwise comparison has its own
#' unadjusted p-value. Therefore, in order to account for this inflation in the Type I error rate overall, some researchers
#' decide to modify these p-values.
#'
#'
#'
#' @export
#'
#'
#' @import
#' FSA
pairwise_Comparison_for_K_Populations = function(x,group,dunn.method="bh"){

  A=data.frame(FSA::dunnTest(x~group,method=dunn.method)$res)
  B= data.frame(TukeyHSD(aov(x~group))$group)
  m=matrix(unlist(strsplit(row.names(B),"-")),ncol=2,byrow=T)
  B=data.frame(Comparison=paste(paste0(m[,2]," "),paste0(" ",m[,1]),sep = "-"),B)
  row.names(B)=NULL

  tab=data.frame(Var=c(deparse(substitute(x)),rep("",dim(B)[1]-1)),

                 Comparison=B$Comparison,
                 Mean_Diff=paste0(round(B$diff,2)," (",round(B$lwr,2),",",round(B$upr,2),")"),
                 TukeyHSD_PValue=sprintf("%0.3f",B$p.adj),
                 DunnTest_P.adj=sprintf("%0.3f",A$P.adj[match(B$Comparison,A$Comparison)]),
                 DunnTest_P.unadj=sprintf("%0.3f",A$P.unadj[match(B$Comparison,A$Comparison)]))


  result=list(TukeyHSD=B,DunnTest=A,Summarytab=tab)
  return(result)
}


#' Frequency table of a categorical variable.
#'
#' Creates frequency frequencies and percentages of the different levels of a factor variable.
#'
#'
#' @param x must be a factor
#' @param xname Name of the variable. Default will be the x name.
#' @param conf.Int. Logical argument to calculate the confidence interval of the percentages. Default is \code{FALSE}.
#' @param conf.level integer argument, the level of confidence. Default is 0.95,i.e., 95% level of confidence.
#'
#'
#' @export

freq_table=function(x,xname=deparse(substitute(x)),conf.Int.=FALSE,conf.level=0.95){
  if(conf.Int.==FALSE){
    tabl=data.frame(var=c(xname,rep("",nlevels(x)-1)),table(x),Per=round(100*prop.table(table(x)),2))[,-4]
    names(tabl)=c("Var","Group","Freq","Per(%)")
    return(tabl)
  }
  else{
    require(DescTools)
    tabl=data.frame(var=c(xname,rep("",nlevels(x)-1)),table(x),Per=round(100*prop.table(table(x)),2))[,-4]
    names(tabl)=c("Var","Group","Freq","Per(%)")
    Confi= MultinomCI(tabl$`Per(%)`,conf.level=conf.level,method="sisonglaz") # Confidence Interval for Multinomial Distribution
    tabl$`95% C.I`= paste0("(",round(Confi[,2]*100,2),",",round(Confi[,3]*100,2),")")
    return(tabl)
  }
}


#' Group-wise frequency table of a categorical variable
#'
#'
#'
#' @export
freq_table_group_by=function(Exposure,margin=1,Outcome,conf.Int.=FALSE,conf.level=0.95){
  if(conf.Int.==FALSE){
    xname=deparse(substitute(Exposure))
    tab=table(Exposure,Outcome)
    dat= data.frame(matrix(paste0(tab,"(",round(prop.table(table(Exposure,Outcome),margin = margin)*100,2),"%)"),dim(tab)))
    names(dat)= levels(Outcome)
    dat=data.frame(Var=c(xname,rep("",nlevels(Exposure)-1)),
                   Levels= levels(Exposure),
                   dat)
    return(dat)
  }
  else{
    require(DescTools)
    xname=deparse(substitute(Exposure))
    tab=table(Exposure,Outcome)
    dat= data.frame(matrix(paste0(tab,"(",round(prop.table(table(Exposure,Outcome),margin = margin)*100,2),"%)"),dim(tab)))
    names(dat)= levels(Outcome)
    if(margin==1){
      Conf1= t(round(MultinomCI(tab[1,],conf.level=conf.level,method="sisonglaz")*100,2))
      Conf2= t(round(MultinomCI(tab[2,],conf.level=conf.level,method="sisonglaz")*100,2))
    }
    else{
      Conf1= t(round(MultinomCI(tab[,1],conf.level=conf.level,method="sisonglaz")*100,2))
      Conf2= t(round(MultinomCI(tab[,2],conf.level=conf.level,method="sisonglaz")*100,2))
    }

    dat=data.frame(Var=c(xname,rep("",nlevels(Exposure)-1)),
                   Levels= levels(Exposure),
                   dat,
                   CI1=c(paste0("(",Conf1[-1,1][1],",",Conf1[-1,1][2],")"),paste0("(",Conf2[-1,1][1],",",Conf2[-1,1][2],")")),
                   CI2=c(paste0("(",Conf1[-1,2][1],",",Conf1[-1,2][2],")"),paste0("(",Conf2[-1,2][1],",",Conf2[-1,2][2],")")))


    names(dat)=c(names(dat)[1:4],paste0("CI(%)for ",levels(Outcome)[1]),paste0("CI(%)for ",levels(Outcome)[2]))
    return(dat)
  }

}



#' Compare a numaric variable between two levels of a factor variable
#'
#'
#'
#' @export
indep_two_Populations = function(x,group,basegroup=c(1,2),xname=deparse(substitute(x)),plot=FALSE,shp=FALSE){
  
  descrip=function(x){c(N=length(na.omit(x)),Mean=round(mean(x,na.rm=T),2),S.D=round(sd(x,na.rm=T),2),Median=round(median(x,na.rm=T),2),MIN=round(min(x,na.rm = T),2),MAX=round(max(x,na.rm = T),2),I.Q.R=paste("(",sprintf("%0.2f",summary(x)[2]),",",sprintf("%0.2f",summary(x)[5]),")",sep=""))}
  tab=do.call(rbind,tapply(x,group,descrip))
  
  ## if shapiro.test$p-value >0.05 i.e not significant => the data is Normal otherwise, <0.05 i.e., significant then it is skewed
  
  if(plot==TRUE){
    def.par <- par(no.readonly = TRUE) ## Default layout
    layout( matrix(c(1,2,3,3), nrow=2, byrow=TRUE) )
    hist(x[which(group==levels(group)[1])],main=levels(group)[1],col=adjustcolor("red", alpha.f = 0.40),xlab=deparse(substitute(x)))
    abline(v=mean(x[which(group==levels(group)[1])],na.rm=T),col="red",lty=2,lwd=2)
    legend("topright",legend = c("Mean"),lty=2,lwd=2,col="red",cex=0.8,bty = "n")
    hist(x[which(group==levels(group)[2])],main=levels(group)[2],col=adjustcolor("blue", alpha.f = 0.40),xlab=deparse(substitute(x)))
    abline(v=mean(x[which(group==levels(group)[2])],na.rm=T),col="red",lty=2,lwd=2)
    #legend("topright",legend = c("Mean"),lty=2,lwd=2,col="red",cex=0.8,bty = "n")
    boxplot(x~group,col=adjustcolor(c("red","blue"), alpha.f = 0.40))
    par(def.par)
  }
  
  if(sum(is.na(tab[,3]))>0){
    tab=data.frame(Var=c(xname,rep("",nlevels(group)-1)),
                   Group=c(deparse(substitute(group)),rep("",nlevels(group)-1)),
                   Levels=levels(group),
                   tab,
                   Shapiro.test.Pvale="",
                   CI="",
                   P.ValueP= "",
                   P.valueNP="")
    rownames(tab)=NULL
    return(tab)
  }
  else{
    ##---- Normality Test Shapiro-test ----##
    if(shp==T){
      shp= c(round(shapiro.test(subset(x,group==levels(group)[1]))$p.value,3),round(shapiro.test(subset(x,group==levels(group)[2]))$p.value,3))
      
      tab=data.frame(Var=c(xname,rep("",nlevels(group)-1)),
                     Group=c(deparse(substitute(group)),rep("",nlevels(group)-1)),
                     Levels=levels(group),
                     tab,
                     Shapiro.test.Pvale=shp,
                     Mean.Diff= c(ifelse(basegroup==1,c(round(t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$estimate[1]-t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$estimate[2],3)),
                                         c(round(t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$estimate[1]-t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$estimate[2],3))),""),
                     #Test_Stat=c(paste("t-stat:",round(t.test(x~group,mu=0,var.eq=F,paired=F)$statistic,3),sep=" "),rep("",nlevels(group)-1)), #----- t- test test statistic
                     #Conf_Int= c(paste("(",round(t.test(x~group,mu=0,var.eq=F,paired=F)$conf.int[1],3),",",round(t.test(x~group,mu=0,var.eq=F,paired=F)$conf.int[2],3),")",sep = ""),rep("",nlevels(group)-1)),  #----- Confidence Interval
                     CI= c(ifelse(basegroup==1,c(paste0("(",sprintf("%0.2f",t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$conf.int[1]),",",sprintf("%0.2f",t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$conf.int[2]),")"),rep("",nlevels(group)-1)),
                                  c(paste0("(",sprintf("%0.2f",t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$conf.int[1]),",",sprintf("%0.2f",t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$conf.int[2]),")"),rep("",nlevels(group)-1))),""),
                     P.ValueP= c(ifelse(sprintf("%0.3f",t.test(x~group,mu=0,var.eq=F,paired=F)$p.value)=="0.000","<0.001",sprintf("%0.3f",t.test(x~group,mu=0,var.eq=F,paired=F)$p.value)),rep("",nlevels(group)-1)),              #----- t- test P-value
                     P.valueNP=c(ifelse(sprintf("%0.3f",wilcox.test(x~group,exact=FALSE)$p.value)=="0.000","<0.001",sprintf("%0.3f",wilcox.test(x~group,exact=FALSE)$p.value)),rep("",nlevels(group)-1)))                             #----- Mann Whitney U test P-Value
      rownames(tab)=NULL
      return(tab)
    }else{
      tab=data.frame(Var=c(xname,rep("",nlevels(group)-1)),
                     Group=c(deparse(substitute(group)),rep("",nlevels(group)-1)),
                     Levels=levels(group),
                     tab,
                     Mean.Diff= c(ifelse(basegroup==1,c(round(t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$estimate[1]-t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$estimate[2],3)),
                                         c(round(t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$estimate[1]-t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$estimate[2],3))),""),
                     #Test_Stat=c(paste("t-stat:",round(t.test(x~group,mu=0,var.eq=F,paired=F)$statistic,3),sep=" "),rep("",nlevels(group)-1)), #----- t- test test statistic
                     #Conf_Int= c(paste("(",round(t.test(x~group,mu=0,var.eq=F,paired=F)$conf.int[1],3),",",round(t.test(x~group,mu=0,var.eq=F,paired=F)$conf.int[2],3),")",sep = ""),rep("",nlevels(group)-1)),  #----- Confidence Interval
                     CI= c(ifelse(basegroup==1,c(paste0("(",sprintf("%0.2f",t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$conf.int[1]),",",sprintf("%0.2f",t.test(x[which(group==levels(group)[2])],x[which(group==levels(group)[1])],mu=0,var.eq=F,paired=F)$conf.int[2]),")"),rep("",nlevels(group)-1)),
                                  c(paste0("(",sprintf("%0.2f",t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$conf.int[1]),",",sprintf("%0.2f",t.test(x[which(group==levels(group)[1])],x[which(group==levels(group)[2])],mu=0,var.eq=F,paired=F)$conf.int[2]),")"),rep("",nlevels(group)-1))),""),
                     P.ValueP= c(ifelse(sprintf("%0.3f",t.test(x~group,mu=0,var.eq=F,paired=F)$p.value)=="0.000","<0.001",sprintf("%0.3f",t.test(x~group,mu=0,var.eq=F,paired=F)$p.value)),rep("",nlevels(group)-1)),              #----- t- test P-value
                     P.valueNP=c(ifelse(sprintf("%0.3f",wilcox.test(x~group,exact=FALSE)$p.value)=="0.000","<0.001",sprintf("%0.3f",wilcox.test(x~group,exact=FALSE)$p.value)),rep("",nlevels(group)-1)))                             #----- Mann Whitney U test P-Value
      rownames(tab)=NULL
      return(tab)
    }
  }
  
}


#' Association between two categorical variables
#'
#'
#'
#' @export
mytab_two_two_Contingency=function(x,y,margin,xlev=deparse(substitute(x)),Simulate.P.Val=FALSE,yets.correction=FALSE,plot=FALSE,exact.test=FALSE){
  tab=table(x,y)                         ##--- simple Contingency table
  tab_p=round(prop.table(tab,margin = margin)*100,2)   ##--- Contingency table with Percentage
  tab_out=data.frame(var=c(xlev,rep("",nlevels(x)-1)),level=levels(x),matrix(paste(tab,"(",tab_p,")",sep = ""),nrow(tab),ncol(tab)))
  names(tab_out)[-c(1,2)]=colnames(tab)

  if(exact.test==TRUE){

    tab_out=data.frame(tab_out,
                       Chi.square=c(ifelse(0%in%apply(tab,2,sum),"",sprintf("%0.3f",chisq.test(x,y,correct = yets.correction)$statistic)),rep("",nlevels(x)-1)),
                       Pval.chisq=c(ifelse(0%in%apply(tab,2,sum),"",ifelse(sprintf("%0.3f",chisq.test(x,y,correct = yets.correction)$p.value)=="0.000","<0.001",sprintf("%0.3f",chisq.test(x,y,correct = yets.correction)$p.value))),rep("",nlevels(x)-1)), ##---- Chi-square Test P-Value
                       PValue.Fisher= c(ifelse(0%in%apply(tab,2,sum),"",ifelse(sprintf("%0.3f",fisher.test(x,y,simulate.p.value = Simulate.P.Val)$p.value)=="0.000","<0.001",sprintf("%0.3f",fisher.test(x,y,simulate.p.value = Simulate.P.Val)$p.value))),rep("",nlevels(x)-1)))     ##---- Fisher's Exact Test P-Value for Non-Parametric
  }else{

    tab_out=data.frame(tab_out,
                       Chi.square=c(ifelse(0%in%apply(tab,2,sum),"",sprintf("%0.3f",chisq.test(x,y,correct = yets.correction)$statistic)),rep("",nlevels(x)-1)),
                       Pval.chisq=c(ifelse(0%in%apply(tab,2,sum),"",ifelse(sprintf("%0.3f",chisq.test(x,y,correct = yets.correction)$p.value)=="0.000","<0.001",sprintf("%0.3f",chisq.test(x,y,correct = yets.correction)$p.value))),rep("",nlevels(x)-1))) ##---- Chi-square Test P-Value
  }
    if(plot==TRUE){
      tab=table(x,y)
      Matrix.t=t(tab)
      barplot(Matrix.t,
              beside = TRUE,
              legend = TRUE,
              #ylim = c(0, 8),   ### y-axis: used to prevent legend overlapping bars
              cex.names = 0.8,  ### Text size for bars
              cex.axis = 0.8,   ### Text size for axis
              args.legend = list(x   = "topright",   ### Legend location
                                 cex = 0.8,          ### Legend text size
                                 bty = "n"),
              col=hcl.colors(nlevels(x), "Set 2"),
              xlab=deparse(substitute(x)))
    }

    return(tab_out)
  }



