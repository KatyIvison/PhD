#modelling using Albizia lebbeck
library(biomod2)
library(dismo)
library(rgdal)
library(ggplot2)
library(mapproj)
library(raster)
library(gridExtra)
library(ggpubr)
library(sp)


albleb<-read.csv('Albizia lebbeck.csv')
albleb<-albleb[1:1023,]
albleb<-albleb[,2:7]
albleb$PresAbs=1
write.csv(albleb,file="Albizia lebbeck individual.csv")
allmap<-getData("worldclim", res = 5, var = "bio")

DataSpecies<-albleb 
head(DataSpecies)
myRespName<-"AlbiziaLebbeck"
myResp<-DataSpecies[,7]
myRespXY<-DataSpecies[,3:2]
myExpl<-allmap#from ch 4 worksheet

myBiomodData<-BIOMOD_FormatingData(resp.var=myResp,
                                   expl.var=myExpl,
                                   resp.xy=myRespXY,
                                   resp.name=myRespName,
                                   PA.nb.rep=10, #no. pa repeats
                                   PA.nb.absences=10000,#no. pa/repeat
                                   PA.strategy='random')
myBiomodData
plot(myBiomodData)

# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions()

# 3. Computing the models
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = ('MARS'),#models go here
  models.options = myBiomodOption,
  NbRunEval=1,##this makes 1 run
  DataSplit=80,
  Prevalence=0.5,
  VarImport=3,
  models.eval.meth = c('TSS','ROC'),
  SaveObj = TRUE,
  rescal.all.models = TRUE,
  do.full.models = FALSE,
  modeling.id = paste(myRespName,"FirstModeling",sep=""))
myBiomodModelOut

# get all models evaluation
myBiomodModelEval <- get_evaluations(myBiomodModelOut)
# print the dimnames of this object
dimnames(myBiomodModelEval)


# let's print the TSS scores of RGLM
myBiomodModelEval["TSS","Testing.data","MARS",,]

#ROC scores of all selected models
myBiomodModelEval["ROC","Testing.data",,,]

# print variable importances
get_variables_importance(myBiomodModelOut)

#ensemble modelling
myBiomodEM <- BIOMOD_EnsembleModeling(
  modeling.output = myBiomodModelOut,
  chosen.models = 'all',
  em.by='all',
  eval.metric = c('TSS'),
  eval.metric.quality.threshold = c(0.7),
  prob.mean = T,
  prob.cv = T,
  prob.ci = T,
  prob.ci.alpha = 0.05,
  prob.median = T,
  committee.averaging = T,
  prob.mean.weight = T,
  prob.mean.weight.decay = 'proportional' )

# print summary
myBiomodEM
# get evaluation scores
get_evaluations(myBiomodEM)

# projection over the globe under current conditions
myBiomodProj <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExpl,
  proj.name = 'current',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = F,
  output.format = '.grd')

# summary of crated oject
myBiomodProj

# files created on hard drive
list.files("AlbiziaLebbeck/proj_current/")

# make some plots
plot(myBiomodProj)


###future predictions
myExplFuture<-getData("CMIP5", res = 5, var = "bio",model="NO",rcp=45,year=70)
#make sure labels are the same as current raster
names(myExplFuture)<-c('bio1','bio2','bio3','bio4','bio5','bio6','bio7','bio8','bio9','bio10','bio11','bio12','bio13','bio14','bio15','bio16','bio17','bio18','bio19')
myBiomodProjFuture <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = myExplFuture,
  proj.name = 'future',
  selected.models = 'all',
  binary.meth = 'TSS',
  compress = 'xz',
  clamping.mask = T,
  output.format = '.grd')

# make some plots, sub-selected by str.grep argument model CTA
plot(myBiomodProjFuture)

#ensemble projections when using multiple models
myBiomodEF <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProj)
myBiomodEF
# reduce layer names for plotting convegences
plot(myBiomodEF)

multiple.plot(Data=c(myBiomodProj,myBiomodProjFuture),coor=myRespXY)
as.grob(myBiomodProj)




