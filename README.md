# logitloop
R-package to perform iterative logit analyses accross columns of a table


## code to install multirocauc package
```r
library(devtools)
install_github("cdesterke/logitloop")
```


## code for preprocessing data

```r
## preprocess remove missing data
library(logitloop)
library(dplyr)
data(kras)
kras%>%select(-os.months)->expression
df<-logitloop(expression,outcome="os.status")
df

```
![res](https://github.com/cdesterke/logitloop/blob/main/df.png)


## plot the coefficient results
```r
plotcoef(df,nb=11,title="")

```
![res](https://github.com/cdesterke/logitloop/blob/main/beta.png)

