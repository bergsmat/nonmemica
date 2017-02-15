## ----eval=FALSE----------------------------------------------------------
#  install.packages(repos=NULL,'../nonmemica_0.1.zip') # supply your path and version
#  library(nonmemica)

## ----echo=FALSE, results='hide'------------------------------------------
library(nonmemica)

## ----eval=FALSE----------------------------------------------------------
#  as.partab(1001, project='../model')

## ----eval=FALSE----------------------------------------------------------
#  options(project = '../model')
#  as.partab(1001)

## ----eval=FALSE----------------------------------------------------------
#  methods(format)

## ----eval=FALSE----------------------------------------------------------
#  as.partab(1001)

## ----eval=FALSE----------------------------------------------------------
#  1001 %>% as.partab

## ----eval=FALSE----------------------------------------------------------
#  2 %>% sqrt %>% signif(3)

## ----eval=FALSE----------------------------------------------------------
#  signif(sqrt(2),digits=3)

## ----eval=FALSE----------------------------------------------------------
#  as.partab(1001, xmlfile='..model/1001.xml')

## ----eval=FALSE----------------------------------------------------------
#  as.partab(1001, bootcsv='..model/1001/bootstrap_results.csv')

## ----eval=FALSE----------------------------------------------------------
#  as.partab(1001, ctlfile = '../models/1001.ctl',fields = c('symbol','label','unit')).

## ------------------------------------------------------------------------
library(magrittr)
library(nonmemica)
options(project = system.file('project/model',package='nonmemica'))
1001 %>% as.definitions %>% head

## ----eval = FALSE--------------------------------------------------------
#  1001 %>% as.definitions(write=T)
#  1001 %>% as.partab(ctlfile = NULL)

## ------------------------------------------------------------------------
as.partab(1001)

## ------------------------------------------------------------------------
x <- as.partab(1001)

## ----eval=FALSE----------------------------------------------------------
#  as.partab(1001, verbose=TRUE)

## ------------------------------------------------------------------------
1001 %>% as.partab(
  format = F, 
  ci = F, 
  relative = F, 
  digits = NULL
)

## ------------------------------------------------------------------------
library(knitr)
1001 %>% as.partab %>% kable

## ------------------------------------------------------------------------
library(pander)
1001 %>% as.partab %>% pander(justify='right')

## ------------------------------------------------------------------------
library(ReporteRs)
1001 %>% as.partab %>% as.docx %>% as.file('1001.docx')
x <- as.flextable(as.partab(1001))
x

