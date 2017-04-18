#' Create and Evaluate NONMEM Models in a Project Context
#' 
#' NONMEM (Icon plc.) is software for nonlinear mixed effects modeling.
#' The foundational interface is a text file (control stream, typ. *.mod 
#' or *.ctl) that specifies model input, structure, and output. There are
#' many add-on interfaces for NONMEM (see references for a few examples).
#' However, much day-to-day modeling, even for R users, involves substantial
#' manual interventions.
#' 
#' Nonmemica streamlines interactions with NONMEM. It adopts some
#' established conventions and techniques (e.g. from PsN and metrumrg), but 
#' introduces others that may be useful.
#' 
#' Of course, NONMEM itself is licensed software that must be installed
#' independently.  Nonmemica is largely indifferent to how NONMEM is 
#' installed or invoked. However, several features depend on the *.xml 
#' output that NONMEM creates. Also, the best-supported directory structure 
#' is that which has numbers for model names, with all model-specific files 
#' in a like-named subdirectory of a "project" directory. An example is 
#' given below.
#' 
#' Nonmemica adopts three control stream encoding conventions that merit
#' special mention. First, the problem statement is encoded in the form
#' //like/x//but/y// where x is a reference model name and y is a feature
#' difference from the reference model (see likebut() ).  This allows any 
#' given model to be described by chaining together its legacy of features
#' (see runlog() ), which generally works better than trying to exhaustively
#'  describe it in the model name.
#' 
#' Second, Nonmemica only needs a single output table ($TABLE record). 
#' Be sure to use ONEHEADER but avoid FIRSTONLY.  Nonmemica will integrate
#' model inputs and outputs, regardless of table counts, into one data.frame
#' (see superset() ).
#' 
#' Third, Nonmemica supports integrated metadata. With respect to model 
#' inputs, use package spec to store column metadata in a companion file.
#' For model outputs (tabled items) supply column metadata directly in 
#' the control stream (or a *.def file; see example and help).
#' 
#' @references \url{https://en.wikipedia.org/wiki/NONMEM}
#' @references \url{http://www.iconplc.com/innovation/nonmem/}
#' @references \url{https://uupharmacometrics.github.io/PsN/}
#' @references \url{http://xpose.sourceforge.net/}
#' @references \url{http://www.pirana-software.com/}
#' @references \url{http://wfn.sourceforge.net/}
#' @references \url{https://github.com/MikeKSmith/rspeaksnonmem}
#' @references \url{https://r-forge.r-project.org/R/?group_id=1215}
#' 
#' @examples
# Create a working project.
source <- system.file(package = 'nonmemica','project/model')
target <- tempdir()
source
target
file.copy(source,target,recursive = TRUE)
options(project = target)

# Load some packages
library(magrittr)
library(fold)
library(metaplot)
library(wrangle)
# library(dplyr, warn.conflicts = F)
# library(tidyr, warn.conflicts = F)
# library(nonmemica) # was 0.4.1, now 0.5.2
# library(csv)
# library(knitr)
# (pander)
# library(lattice)
# library(origami)
# library(spec)
#trellis.par.set(background = list(col = "transparent"))

# Identify features of a model.
1001 %>% modelpath
1001 %>% modeldir
1001 %>% modelfile
1001 %>% modelpath('xml')
1001 %>% datafile
1001 %>% specfile
1001 %>% specfile %>% read.spec
1001 %>% specfile %>% read.spec %>% as.folded
1001 %>% as.model
1001 %>% as.model %>% comments
1001 %>% definitions
1001 %>% runlog(T)
1001 %>% runlog
1001 %>% partab
1001 %>% num_parameters
1001 %>% nms_canonical
1001 %>% nms_psn
1001 %>% nms_nonmem
1001 %>% parameters
1001 %>% errors
1001 %>% as.model %>% initial
1001 %>% as.model %>% lower
1001 %>% as.model %>% upper
1001 %>% as.model %>% fixed
1001 %>% meta %>% class
1001 %>% meta

# Derive datasets.
1001 %>% superset %>% head
1001 %>% superset %>% group_by(ID,TIME) %>% status
1001 %>% metasuperset(c('ID','TIME')) %>% head
1001 %>% fold(ID,TIME,subset='MDV==0') %>% head

# Make diagnostic plots.
1001 %>% fold(ID,TIME,subset='MDV == 0') %>% metaplot(CWRESI, ref=0)
1001 %>% fold(ID,TIME,subset='MDV == 0') %>% metaplot(CWRESI, TAD, SEX, yref=0, alpha = 0.1, ysmooth = TRUE)
1001 %>% fold(ID,TIME,subset='MDV == 0') %>% metaplot(ETA1, SEX, ref=0)
1001 %>% fold(ID,TIME,subset='MDV == 0') %>% metaplot(SEX, ETA1, ref=0)
1001 %>% fold(ID,TIME,subset='MDV == 0') %>% metaplot(ETA1, ETA2, ETA3)

# Derive models.
1001 %>% likebut('duplicated',y = 1002, overwrite=TRUE )
1001 %>% tweak

#' @docType package
#' @name nonmemica
NULL