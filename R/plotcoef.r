#' @title plotcoef

#' this function plot the results of logit analyses
#' @usage plotcoef(df,nb=11,title="")
#' @examples library(logitloop)
#' @examples library(dplyr)
#' @examples data(kras)
#' @examples kras%>%select(-os.months)->expression
#' @examples df<-logitloop(expression,outcome="os.status")
#' @examples df
#' @examples plotcoef(df,nb=11,title="")




plotcoef<-function(df,nb=11,title="Univariate Logit analysis",size=18){

		## load necessary packages
		  	if(!require(ggplot2)){
    	install.packages("ggplot2")
    	library(ggplot2)}
		df<-head(df,n=nb)
		
		
	
		## perform the barplot
		p=ggplot(data=df,aes(x=reorder(predictors,NLP),y=beta,fill=significance))+geom_bar(stat="identity")+
			coord_flip()+
			theme_minimal()+
			xlab("Covariates")+
			ylab("Coefficient Logit regression / (p.values)")+
			scale_fill_manual(values=c("lightskyblue1","plum2"))+
			geom_text(aes(label=round(p.values,5)),hjust=0, vjust=0.5,color="navy",position= position_dodge(0),size=5,angle=0)+
			ggtitle(title) +theme(text = element_text(size = size))+theme(legend.position="bottom")

		return(p)
}



