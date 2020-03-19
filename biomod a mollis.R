library(biomod2)
library(dismo)
library(rgdal)
library(ggplot2)
library(mapproj)
library(raster)
library(gridExtra)
library(ggpubr)
library(sp)
library(rgbif)

amobj<-print(occ_data(scientificName="Alchemilla mollis",limit=100000))
amobj<-data.frame(amobj)
amobj<-subset(amobj,select = c("scientificName","decimalLatitude","decimalLongitude","basisOfRecord","countryCode","country"))
amobj<-subset(amobj,subset=basisOfRecord%in% c("HUMAN_OBSERVATION","OBSERVATION","LITERATURE","MATERIAL_SAMPLE"))
amobj<-amobj[(!(amobj$decimalLatitude=="0")&!(amobj$decimalLongitude=="0")),]
amobj<-na.omit(amobj)
amobj<-unique(amobj)

allmap<-getData("worldclim", res = 5, var = "bio")

DataSpecies<-amobj
DataSpecies$PresAbs=1
head(DataSpecies)
myRespName<-"AlchemillaMollis"
myResp<-DataSpecies[,7]
myRespXY<-DataSpecies[,3:2]
myExpl<-allmap

myBiomodData<-BIOMOD_FormatingData(resp.var=myResp,
                                   expl.var=myExpl,
                                   resp.xy=myRespXY,
                                   resp.name=myRespName,
                                   PA.nb.rep=1, #no. pa repeats
                                   PA.nb.absences=10000,#no. pa/repeat
                                   PA.strategy='random')
myBiomodData
plot(myBiomodData)

# 2. Defining Models Options using default options.
myBiomodOption <- BIOMOD_ModelingOptions(MAXENT.Phillips=list(path_to_maxent.jar='Y:/prs/Durham PhD/GloNAF/Data for R/GloNAF GBIF/Biomod practice/'))
#add something to direct maxent to java file?
myBiomodOption


# 3. Computing the models
myBiomodModelOut <- BIOMOD_Modeling(
  myBiomodData,
  models = c('GLM','GAM','RF','GBM'),#these are some models
  models.options = myBiomodOption,
  NbRunEval=1,##how many runs
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


# let's print the TSS scores of algorithms
myBiomodModelEval["TSS","Testing.data","GLM",,]

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
list.files("ArabisCaucasica/proj_current/")

plot(myBiomodProj)
# make some plots sub-selected by str.grep argument
plot(myBiomodProj, str.grep = 'MARS')
#make plots of Random forest model
plot(myBiomodProj, str.grep = 'RF')
plot(myBiomodProj, str.grep = 'SRE')

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

plot(myBiomodProjFuture)
# make some plots, sub-selected by str.grep argument model CTA
plot(myBiomodProjFuture, str.grep = 'CTA')
plot(myBiomodProj, str.grep = 'MARS')

#ensemble projections
myBiomodEF <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProj)
myBiomodEF
# reduce layer names for plotting convegences
plot(myBiomodEF)

myBiomodEFfuture <- BIOMOD_EnsembleForecasting(
  EM.output = myBiomodEM,
  projection.output = myBiomodProjFuture)