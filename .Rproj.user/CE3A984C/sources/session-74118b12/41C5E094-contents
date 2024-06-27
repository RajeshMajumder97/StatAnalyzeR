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
#' rbind(with(iris,tab_indp_K_Populations(x= Sepal.Length, group= Species)),
#'       with(iris,tab_indp_K_Populations(x= Sepal.Width, group= Species)),
#'       with(iris,tab_indp_K_Populations(x= Petal.Length, group= Species)),
#'       with(iris,tab_indp_K_Populations(x= Petal.Width, group= Species)))
#'
tab_indp_K_Populations = function(x,group,xname=deparse(substitute(x))){
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


