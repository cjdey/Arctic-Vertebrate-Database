require(lme4)
require(lmerTest)
require(tidyr)
require(dplyr)
require(tibble)
require(readr)
require(glmmTMB) 
require(MASS)
require(pscl)
###################


AVD <- read_csv("Arctic_Vertebrate_Database_v3.csv")
AVD$group <- as.factor(AVD$group)
AVD$IUCN_status <- as.factor(AVD$IUCN_status)

#create new variable of whether a species has been evaluated
AVD <- mutate(AVD, evaluated = ifelse ((is.na(IUCN_status) | IUCN_status == "DD"), 0,1)) #include data deficient species as unevaluated
AVD <- mutate(AVD, evaluated_3 =ifelse  (is.na(IUCN_status), "NE", ifelse(IUCN_status == "DD", "DD", "EV"))) #set data deficient as own level

#create new variable of the total publications from 1987-2016
AVD <- AVD %>% mutate(totalpubs= Reduce("+",.[24:53]))

#################
#Descriptive stats
##################

median(AVD$totalpubs)
range(AVD$totalpubs)
length(which(AVD$totalpubs < 10)) / length(AVD$totalpubs)
length(which(AVD$totalpubs < 1)) / length(AVD$totalpubs)

median(AVD$knowntraits)
range(AVD$knowntraits)
length(which(AVD$knowntraits < 2)) / length(AVD$knowntraits)

length(which(AVD$evaluated ==1)) / length(AVD$evaluated)

#Removing the amphibians (3 species)
AVD <- AVD %>% filter(group != "Amphibians") 

################
#Taxonmic differences 
################

#######
#publications ~ group
#######

#Vanilla glm
ML_NB<-glm.nb(totalpubs~group, data=AVD)

#Zero-inflated models
ZN1<-zeroinfl(totalpubs~group | 1, data=AVD, dist="negbin") 
ZN2<-zeroinfl(totalpubs~group | group, data=AVD, dist="negbin")

#Hurdle models
HN1<-hurdle(totalpubs ~ group  | 1, data = AVD, dist = "negbin")
HN2<-hurdle(totalpubs ~ group  | group, data = AVD, dist = "negbin")

fm <- list("ML" = ML_NB, "ZI-NB" = ZN1, "ZI-NB2" = ZN2, "H-NB" = HN1, "H-NB2" = HN2)
rbind(logLik = sapply(fm, function(x) round(logLik(x), digits = 0)),
      AIC = sapply(fm, function(x) round(AIC(x), digits = 0)),
      Df = sapply(fm, function(x) attr(logLik(x), "df")))

#Change the reference levels for the factors to improve interpretability
sum1.1<-AVD %>% group_by(group) %>% filter(totalpubs > 0) %>%
  summarise( research =mean(totalpubs)) %>% arrange(research)

AVD$group2<-factor(AVD$group, levels=c(paste(sum1.1$group[1:6]))) #for the negative binom component

sum1.2<-AVD %>% group_by(group) %>% mutate(totalpubs = ifelse (totalpubs > 0, 0,1)) %>%
  summarise(percent_zero = mean(totalpubs)) %>% arrange(-percent_zero)

AVD$group3<-factor(AVD$group, levels=c(paste(sum1.2$group[1:6]))) #for the hurdle component

summary(HN2<-hurdle(totalpubs ~ group2  | group3, data = AVD, dist = "negbin"))


#######
#known traits ~ group
#######

#set factor order
sum2 <- AVD %>% group_by(group) %>% 
  summarise(mean_known = mean(knowntraits)) %>%  arrange(mean_known)

AVD$group <- factor(AVD$group, levels=c(paste(sum2$group[1:6])))

#binomial GLM
summary(M2 <- glm(cbind(knowntraits, 6-knowntraits)~group, family="binomial",data=AVD))
confint(M2)

#######
#IUCN evaluation ~ group
#######

#set factor order
sum3<-AVD %>% group_by(group) %>% 
  summarise(percent_eval = mean(evaluated)) %>%  arrange(percent_eval)

AVD$group <- factor(AVD$group, levels=c(paste(sum3$group[1:6])))

#binomial GLM
summary(M3 <- glm(evaluated~group, family ="binomial", data=AVD))
confint(M3)



##############
##Research effort
##############

ts <- AVD %>% 
  gather(starts_with('WOS_'), key="year", value="papers" )

ts$year<-as.numeric(paste(substring(ts$year, 5)))

aa <- read_csv("all_assessments.csv") #data of all evaluations for each species

##Correct differences in status between older assessments
aa$status <- recode(aa$status, 
                    'LR/cd' = "NT", 'LR/lc' = "LC",'LR/nt' = "NT", 'E' = "EN",
                    'V' = "VU", 'DD' = "X", 'K' = "X",'NE' = "X", 'NR' = "X",
                    'R' = "X", 'T' = "X")

aa$status[which(aa$status== "X")] <- NA
aa <- aa %>% filter(!is.na(status)) 

##add current status (i.e. most recent IUCN eval) 
cstat<-c()

for (i in 1:nrow(ts)) {
  sp <- ts$latin_name[i]
  y <- ts$year[i]
  x <- aa[which(aa$latin_name == sp),]
  cstat[i]<- ifelse(nrow(x) == 0,"NE",
                    ifelse(!any(x$year < y), "NE", 
                           x$status[which(x$year == max(x$year[which((x$year-y) < 0)]))]))   
} 

#create a two level variable from the IUCN status
ts$current_status <- recode(cstat, 'VU' = 'Evaluated', 'CR' = 'Evaluated',
                   'EN' = 'Evaluated', 'EW' = 'Evaluated',
                   'LC' = 'Evaluated', 'NT' = 'Evaluated',
                   'NE' = 'Not Evaluated')

#set order of factor levels
ts$current_status <- factor(ts$current_status, levels=c( "Not Evaluated", "Evaluated"))

sum4<-ts %>% group_by(group) %>% 
  summarise( p =median(papers)) %>% arrange(p)

ts$group <- factor(ts$group, levels=paste(sum4$group[c(1:6)]))

#rescale year to improve interpretability of model estimates
ts<-filter(ts, year < 2017)
ts$year<-scale(ts$year, center=T, scale=2*sd(ts$year)) 

M5 <- glmmTMB(papers ~
                year * current_status +
                  group * year +
                           (year|latin_name),
                family=nbinom2,  data=ts)   


##############
##Predictors of IUCN Evaluation
##############

AVD<-ungroup(AVD)
AVDsub<- dplyr::select(AVD, group, evaluated, totalpubs, knowntraits, body_size_g, trophic_level)
AVDsub<-na.omit(AVDsub)
AVDsub<-mutate(AVDsub,
               log10_publications = log10(totalpubs + 0.1),
               log10_bodysize = log10(body_size_g) )

M4<-glmer(evaluated ~ 
            log10_publications + knowntraits + log10_bodysize + trophic_level +
            (1|group), family="binomial", data = AVDsub)


