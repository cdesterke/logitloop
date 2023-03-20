#' @title logitloop

#' this function makes univariate logit analyses
#' @usage logitloop(expression,outcome="os.status")
#' @examples library(logitloop)
#' @examples library(dplyr)
#' @examples data(kras)
#' @examples kras%>%select(-os.months)->expression
#' @examples df<-logitloop(expression,outcome="os.status")
#' @examples df



logitloop<-function(data,outcome){

suppressWarnings({
	## install survival needed package if necessary

	if(!require(dplyr)){
    		install.packages("dplyr")
    		library(dplyr)}

		## vector of the predictors
		predictors=setdiff(names(data),outcome)

		## loop on predictors and extract beta coeffficients
		beta<-lapply(setdiff(names(data),outcome),
       		function(var) {
	           		formula    <- as.formula(paste(outcome," ~", var))
           			res.logist <- glm(formula, data = data, family = binomial)
	     			beta=summary(res.logist)$coef[2,1]
            		beta
       		})

		## loop on predictors and extract p-values
		pvals<-lapply(setdiff(names(data),outcome),
       		function(var) {
				formula    <- as.formula(paste(outcome," ~", var))
           			res.logist <- glm(formula, data = data, family = binomial)
	     			pvals=summary(res.logist)$coef[2,4]
          			pvals
       		})

		##unlist the outputs
		unpvals<-unlist(pvals)
		unbeta<-unlist(beta)

		## build the data.frame
		df<-as.data.frame(cbind(predictors,beta=unbeta,p.values=unpvals))

		df$beta<-as.numeric(df$beta)
		df$p.values<-as.numeric(df$p.values)
		## odds ratios
		df$odds.ratios=exp(df$beta)

		## order p.values
		df<-df[order(df$p.values,decreasing=F),]

		## compute adjusted pvalues
		df$adjpvals<-p.adjust(df$p.values,method="fdr")


		## compute NLP
		df$NLP= -log(df$p.values,10)
		## compute significance
		df%>%mutate(significance=ifelse(p.values<=0.05,"YES","no"))->df
		## compute prognosis
		df%>%mutate(risk=case_when(odds.ratios == 1 ~ "nothing",
						odds.ratios>1 ~ "POSITIVE",
						odds.ratios<1 ~ "negative"))->df



		## return the dataframe
		return(df)
	})
}





