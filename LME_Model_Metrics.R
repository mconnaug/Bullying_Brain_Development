# Load the gamm4 library
{
library(gamm4)
library(mediation)
library(tidyverse)
library(lmerTest)
library(knitr)
library(lavaan)
library(psych)
library(MBESS)
library(greybox)
library(lme4)
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggseg)
library(effects)
library(rstatix)
library(broom)
library(ggpubr)
library(effectsize)
library(ggeffects)
library(plyr)
library(methods)
library(stargazer)
library(sjPlot)
library(nlme)
library(data.table)
library(MuMIn)
library(lmerTest)
library(lmtest)
library(officer)
library(knitr)
library(readxl)
library(RColorBrewer)
library(dvmisc)
library(qwraps2)
library(AICcmodavg)
library(simr)
library(emmeans)
library(stats)
library(mutoss)
library(powerlmm)


}
#Load Data
{
# read in excel file     
IMAGEN_bully <- read_excel("/Users/michaelconnaughton/Desktop/IMAGEN/MASTER_FILE/IMAGEN_MASTER.xlsx", na = ".")
# read in csv file - read.csv() or read.csv2() functions. The former function is used if the separator is a ,, the latter if ; is used to separate the values in your data file.
IMAGEN_bully <- as.data.frame(IMAGEN_bully)


# convert to r dataframe
IMAGEN_bully <- as.data.frame(IMAGEN_bully)

# Find the index of the row with the lowest age
index_of_lowest_age <- which.min(IMAGEN_bully$age_years)

# Remove the row with the lowest age
IMAGEN_bully <- IMAGEN_bully[-index_of_lowest_age, ]

# Verify the row removal (optional)
print(data)

# List of failed QC IDs to be removed (include all variations of the IDs)
outliers <- c("10400245_1", "12645188_1", "12953830_1", "20015942_1", 
              "22888407_1", "26135259_1", "28852607_1", "29257136_1", 
              "30276332_1", "30383377_1", "32506627_1", "35587656_1", 
              "38976393_1", "41748802_1", "47975799_1", "54552397_1", 
              "64999269_1", "82654000_1", "83308215_1", "90579038_1", 
              "93239552_1", "95136410_1", "98720163_1", "99550415_1",
              "3328367_2", "20418061_2", "6272781_3", "22053782_2", 
              "10646873_3", "61507009_2", "13361778_3", "68802565_2", 
              "20015942_3", "68849025_2", "25193415_3", "30276332_2", 
              "30276332_3", "87961112_2", "38916315_3", "35587656_2", 
              "80627914_3", "75438006_2", "95958758_3", "97329782_2", 
              "97555742_2", "3504454_2", "67766736_2", "73628015_2", 
              "18632052_2", "69895644_2")

# Removed failed QC IDs from data frame
IMAGEN_bully <- IMAGEN_bully %>% 
  filter(!(ID %in% outliers))


# mutate factors

IMAGEN_bully <- IMAGEN_bully%>%
  mutate(sex = factor(sex,levels = c(1,2),labels = c("female", "male")))

IMAGEN_bully <- IMAGEN_bully%>%
 mutate(scan_site = factor(scan_site,levels = c(1,2,3,4,5,6,7,8),labels = c("London", "Northamptom", "Dublin", "Berlin", "Hamberg", "Mannheim", "Paris", "Dresden")))


IMAGEN_bully <- IMAGEN_bully%>%
  mutate(p_bully = factor(p_bully,levels = c(0,1),labels = c("No", "Yes")))


IMAGEN_bully <- IMAGEN_bully%>%
  mutate(timepoint = factor(timepoint,levels = c(1,2,3),labels = c("1", "2", "3")))
summary(IMAGEN_bully$lh_cuneus_volume)

IMAGEN_bully <- IMAGEN_bully%>%
  mutate(p_perpetrator = factor(p_perpetrator,levels = c(0,1),labels = c("No", "Yes")))

IMAGEN_bully <- IMAGEN_bully%>%
  mutate(has_a_good_friend = factor(has_a_good_friend,levels = c(0,1,2),labels = c("Not True", "Partially True", "Certainly True")))

IMAGEN_bully <- IMAGEN_bully%>%
  mutate(attached_to_parent = factor(attached_to_parent,levels = c(0,1,2),labels = c("Not True", "Partially True", "Certainly True")))



#Exclude entries with bully status of "2"
#IMAGEN_bully <- IMAGEN_bully %>% 
#filter(bully_status != 2)

#code factor variables
contrasts(IMAGEN_bully$sex) <- c(-.5, .5)

contrasts(IMAGEN_bully$p_bully) <- c(-.5, .5)

#contrasts(IMAGEN_bully$p_perpetrator) <- c(-.5, .5)

#diff_LMM <- na.omit(diff_LMM, diff_local_eff_log_c)
summary(IMAGEN_bully)

# Define your dependent variable

IMAGEN_bully$EstimatedTotalIntraCranialVol<- as.numeric(IMAGEN_bully$EstimatedTotalIntraCranialVol)
IMAGEN_bully$age<- as.numeric(IMAGEN_bully$age)
IMAGEN_bully$bully_victim<- as.numeric(IMAGEN_bully$bully_victim)
IMAGEN_bully$SES<- as.numeric(IMAGEN_bully$SES)
IMAGEN_bully$age_baseline<- as.numeric(IMAGEN_bully$age_baseline)
IMAGEN_bully$mode_c_pds<- as.numeric(IMAGEN_bully$mode_c_pds)
IMAGEN_bully$age_years<- as.numeric(IMAGEN_bully$age_years)
IMAGEN_bully$WHO_Total<- as.numeric(IMAGEN_bully$WHO_Total)
IMAGEN_bully$LEQ<- as.numeric(IMAGEN_bully$LEQ)
IMAGEN_bully$MatrixReasoning_Baseline<- as.numeric(IMAGEN_bully$MatrixReasoning_Baseline)
IMAGEN_bully$bmi_Baseline<- as.numeric(IMAGEN_bully$bmi_Baseline)

# Scale Variables
IMAGEN_bully$age <- scale(IMAGEN_bully$age)
IMAGEN_bully$bully_victim <- scale(IMAGEN_bully$bully_victim)
IMAGEN_bully$SES <- scale(IMAGEN_bully$SES)
IMAGEN_bully$mode_c_pds <- scale(IMAGEN_bully$mode_c_pds)
IMAGEN_bully$age_baseline <- scale(IMAGEN_bully$age_baseline)
IMAGEN_bully$EstimatedTotalIntraCranialVol <- scale(IMAGEN_bully$EstimatedTotalIntraCranialVol)
IMAGEN_bully$WHO_Total <- scale(IMAGEN_bully$WHO_Total)
IMAGEN_bully$LEQ <- scale(IMAGEN_bully$LEQ)
IMAGEN_bully$MatrixReasoning_Baseline <- scale(IMAGEN_bully$MatrixReasoning_Baseline)
IMAGEN_bully$bmi_Baseline <- scale(IMAGEN_bully$bmi_Baseline)


# Scale Variables

# Create the interaction term and scale it
#IMAGEN_bully$bully_victim_age_interaction <- scale(IMAGEN_bully$age * IMAGEN_bully$bully_victim)


# Remove Missing Data
missing_bully_victim <- is.na(IMAGEN_bully$bully_victim)
missing_scan_site <- is.na(IMAGEN_bully$scan_site)

# Remove observations with missing bully_victim values
IMAGEN_bully <- IMAGEN_bully[complete.cases(IMAGEN_bully$bully_victim), ]
IMAGEN_bully <- IMAGEN_bully[complete.cases(IMAGEN_bully$scan_site), ]

sum(is.na(IMAGEN_bully$bully_victim))
sum(is.na(IMAGEN_bully$scan_site))

DVlist <- c(
  "lh_bankssts_volume", "lh_caudalanteriorcingulate_volume", "lh_caudalmiddlefrontal_volume", 
  "lh_cuneus_volume", "lh_entorhinal_volume", "lh_fusiform_volume", 
  "lh_inferiorparietal_volume", "lh_inferiortemporal_volume", "lh_isthmuscingulate_volume", 
  "lh_lateraloccipital_volume", "lh_lateralorbitofrontal_volume", "lh_lingual_volume", 
  "lh_medialorbitofrontal_volume", "lh_middletemporal_volume", "lh_parahippocampal_volume", 
  "lh_paracentral_volume", "lh_parsopercularis_volume", "lh_parsorbitalis_volume", 
  "lh_parstriangularis_volume", "lh_pericalcarine_volume", "lh_postcentral_volume", 
  "lh_posteriorcingulate_volume", "lh_precentral_volume", "lh_precuneus_volume", 
  "lh_rostralanteriorcingulate_volume", "lh_rostralmiddlefrontal_volume", "lh_superiorfrontal_volume", 
  "lh_superiorparietal_volume", "lh_superiortemporal_volume", "lh_supramarginal_volume", 
  "lh_frontalpole_volume", "lh_temporalpole_volume", "lh_transversetemporal_volume", 
  "lh_insula_volume", "rh_bankssts_volume", "rh_caudalanteriorcingulate_volume", 
  "rh_caudalmiddlefrontal_volume", "rh_cuneus_volume", "rh_entorhinal_volume", 
  "rh_fusiform_volume", "rh_inferiorparietal_volume", "rh_inferiortemporal_volume", 
  "rh_isthmuscingulate_volume", "rh_lateraloccipital_volume", "rh_lateralorbitofrontal_volume",
  "rh_lingual_volume", "rh_medialorbitofrontal_volume", "rh_middletemporal_volume", 
  "rh_parahippocampal_volume", "rh_paracentral_volume", "rh_parsopercularis_volume", 
  "rh_parsorbitalis_volume", "rh_parstriangularis_volume", "rh_pericalcarine_volume", 
  "rh_postcentral_volume", "rh_posteriorcingulate_volume", "rh_precentral_volume", 
  "rh_precuneus_volume", "rh_rostralanteriorcingulate_volume", "rh_rostralmiddlefrontal_volume", 
  "rh_superiorfrontal_volume", "rh_superiorparietal_volume", "rh_superiortemporal_volume", 
  "rh_supramarginal_volume", "rh_frontalpole_volume", "rh_temporalpole_volume", 
  "rh_transversetemporal_volume", "rh_insula_volume", "Left_Cerebellum_Cortex",
  "Left_Thalamus_Proper", "Left_Caudate", "Left_Putamen", 
  "Left_Pallidum", "Brain_Stem", "Left_Hippocampus", 
  "Left_Amygdala","Left_Accumbens_area", "Left_VentralDC", 
  "Right_Cerebellum_Cortex", "Right_Thalamus_Proper", 
  "Right_Caudate", "Right_Putamen", "Right_Pallidum", 
  "Right_Hippocampus", "Right_Amygdala", "Right_Accumbens_area", 
  "Right_VentralDC",	"TotalGrayVol")	


for (dv in DVlist) {
  IMAGEN_bully[[dv]] <- scale(IMAGEN_bully[[dv]])
}

# Initialize a list to store outlier information
outlier_info <- list()

# Loop through each DV
for (dv in DVlist) {
  # Convert to numeric
  IMAGEN_bully[[dv]] <- as.numeric(IMAGEN_bully[[dv]])
  
  # Calculate mean and standard deviation
  mean_DV <- mean(IMAGEN_bully[[dv]], na.rm = TRUE)
  sd_DV <- sd(IMAGEN_bully[[dv]], na.rm = TRUE)
  
  # Define cutoffs for outliers
  lower_bound <- mean_DV - 3 * sd_DV
  upper_bound <- mean_DV + 3 * sd_DV
  
  # Identify outliers
  outliers <- which(IMAGEN_bully[[dv]] < lower_bound | IMAGEN_bully[[dv]] > upper_bound)
  
  # Store outlier information
  outlier_info[[dv]] <- IMAGEN_bully[outliers, dv]
  
  # Replace outliers with NA
  IMAGEN_bully[[dv]][outliers] <- NA
}

}
# The outlier_info list now contains the outliers for each DV

# frames
{
AIC_fit_1 <- c();
AIC_fit_2 <- c();
AIC_Q_RFX_1 <- c();
AIC_Q_RFX_2 <- c();
AIC_L_RFX_1 <- c();
AIC_L_RFX_2 <- c();
AIC_FFX_null_Q <- c();
AIC_FFX_simple_Q <- c();
AIC_FFX_complex_Q <- c();
AIC_FFX_null_L <- c();
AIC_FFX_simple_L <- c();
AIC_FFX_complex_L <- c();


LL_fit_1 <- c();
LL_fit_2 <- c();
LL_Q_RFX_1 <- c();
LL_Q_RFX_2 <- c();
LL_L_RFX_1 <- c();
LL_L_RFX_2 <- c();
LL_FFX_null_Q <- c();
LL_FFX_simple_Q <- c();
LL_FFX_complex_Q <- c();
LL_FFX_null_L <- c();
LL_FFX_simple_L <- c();
LL_FFX_complex_L <- c();

BIC_fit_1 <- c(); 
BIC_fit_2 <- c(); 
BIC_Q_RFX_1 <- c(); 
BIC_Q_RFX_2 <- c(); 
BIC_L_RFX_1 <- c();
BIC_L_RFX_2 <- c();
BIC_FFX_null_Q <- c();
BIC_FFX_simple_Q <- c();
BIC_FFX_complex_Q <- c();
BIC_FFX_null_L <- c();
BIC_FFX_simple_L <- c();
BIC_FFX_complex_L <- c();

LL.Fit.pmat <- c();
dif.1.2_Fit <- c();
dif_Q_RFX <- c();

dif.0.2_FFX_Q <- c();
dif.2.2B_FFX_Q <- c();
dif.0.2B_FFX_Q <- c();
  

dif.0.2_FFX_L<- c();
dif.2.2B_FFX_L <- c();
dif.0.2B_FFX_L <- c();

LL.FFX_Q.pmat <- c();
LLcompare_Q_FFX <- c();
AICcompare_Q_FFX <- c();
BICcompare_Q_FFX <- c();
LL.FFX_L.pmat <- c();
LLcompare_L_FFX <- c();
AICcompare_L_FFX <- c();
BICcompare_L_FFX <- c();



LL_Q_RFX.pmat <- c();
LL_L_RFX.pmat <- c();
LL_simple_Q_FFX.pmat <- c();
LL_complex_Q_FFX.pmat <- c();
LL_simple_L_FFX.pmat <- c();
LL_complex_L_FFX.pmat <- c();

AICccompare_fit <- c();
BICccompare_fit <- c();
LLccompare_fit <- c();

AICccompare_Q_RFX <- c();
BICccompare_Q_RFX <- c();
LLccompare_Q_RFX <- c();

AICccompare_L_RFX <- c();
BICccompare_L_RFX <- c();
LLccompare_L_RFX <- c();


AICccompare_Q_FFX <- c();
BICccompare_Q_FFX <- c();
LLccompare_Q_FFX <- c();

AICccompare_L_FFX <- c();
BICccompare_L_FFX <- c();
LLccompare_L_FFX <- c();

}

#QUAD_LINEAR
{
# Linear MODEL 1
Linear.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim * age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_fit_1 <- append(LL_fit_1, logLik(Linear.outcomes[[outcome]], REML=T))
  AIC_fit_1 <- append(AIC_fit_1, AICc(Linear.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_fit_1 <- append(BIC_fit_1, BICc(Linear.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
}


# Quadratic MODEL 1
Quad.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_fit_2 <- append(LL_fit_2, logLik(Quad.outcomes[[outcome]], REML=T))
  AIC_fit_2 <- append(AIC_fit_2, AICc(Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_fit_2 <- append(BIC_fit_2, BICc(Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
}





# observe all intercept AICc in a table
AICcompare_fit <- data.frame(DVlist, AIC_fit_1, AIC_fit_2)
BICcompare_fit <- data.frame(DVlist, BIC_fit_1, BIC_fit_2)
LLcompare_fit <- data.frame(DVlist, LL_fit_1, LL_fit_2)

# LL.FFX.pmat <- c()
for (outcome in 1:88) {
  # extract p-values only
  # do the interaction effect and the PE variable add anything to the null prediction model?
 dif.1.2_Fit <- anova(Linear.outcomes[[outcome]], Quad.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
 LL.Fit.pmat <- append(LL.Fit.pmat,dif.1.2_Fit[2,1])
}


# create a list of FFX model comparisons (M0_M2 = null compared to full, M0_M2B = null compared to reduced model, M2_M2B = full compared to reduced)
compared_Fit <- c("Linear_Quadratic")


# IS create a dataframe with outcome scales as rows, and 3 columns showing model comparisons

# Create a dataframe with the p-values for different variables
LL_Fit.pvals <- data.frame(
  compared_Fit,
  "lh_bankssts_volume" = LL.Fit.pmat[1],
  "lh_caudalanteriorcingulate_volume" = LL.Fit.pmat[2],
  "lh_caudalmiddlefrontal_volume" = LL.Fit.pmat[3],
  "lh_cuneus_volume" = LL.Fit.pmat[4],
  "lh_entorhinal_volume" = LL.Fit.pmat[5],
  "lh_fusiform_volume" = LL.Fit.pmat[6],
  "lh_inferiorparietal_volume" = LL.Fit.pmat[7],
  "lh_inferiortemporal_volume" = LL.Fit.pmat[8],
  "lh_isthmuscingulate_volume" = LL.Fit.pmat[9],
  "lh_lateraloccipital_volume" = LL.Fit.pmat[10],
  "lh_lateralorbitofrontal_volume" = LL.Fit.pmat[11],
  "lh_lingual_volume" = LL.Fit.pmat[12],
  "lh_medialorbitofrontal_volume" = LL.Fit.pmat[13],
  "lh_middletemporal_volume" = LL.Fit.pmat[14],
  "lh_parahippocampal_volume" = LL.Fit.pmat[15],
  "lh_paracentral_volume" = LL.Fit.pmat[16],
  "lh_parsopercularis_volume" = LL.Fit.pmat[17],
  "lh_parsorbitalis_volume" = LL.Fit.pmat[18],
  "lh_parstriangularis_volume" = LL.Fit.pmat[19],
  "lh_pericalcarine_volume" = LL.Fit.pmat[20],
  "lh_postcentral_volume" = LL.Fit.pmat[21],
  "lh_posteriorcingulate_volume" = LL.Fit.pmat[22],
  "lh_precentral_volume" = LL.Fit.pmat[23],
  "lh_precuneus_volume" = LL.Fit.pmat[24],
  "lh_rostralanteriorcingulate_volume" = LL.Fit.pmat[25],
  "lh_rostralmiddlefrontal_volume" = LL.Fit.pmat[26],
  "lh_superiorfrontal_volume" = LL.Fit.pmat[27],
  "lh_superiorparietal_volume" = LL.Fit.pmat[28],
  "lh_superiortemporal_volume" = LL.Fit.pmat[29],
  "lh_supramarginal_volume" = LL.Fit.pmat[30],
  "lh_frontalpole_volume" = LL.Fit.pmat[31],
  "lh_temporalpole_volume" = LL.Fit.pmat[32],
  "lh_transversetemporal_volume" = LL.Fit.pmat[33],
  "lh_insula_volume" = LL.Fit.pmat[34],
  "rh_bankssts_volume" = LL.Fit.pmat[35],
  "rh_caudalanteriorcingulate_volume" = LL.Fit.pmat[36],
  "rh_caudalmiddlefrontal_volume" = LL.Fit.pmat[37],
  "rh_cuneus_volume" = LL.Fit.pmat[38],
  "rh_entorhinal_volume" = LL.Fit.pmat[39],
  "rh_fusiform_volume" = LL.Fit.pmat[40],
  "rh_inferiorparietal_volume" = LL.Fit.pmat[41],
  "rh_inferiortemporal_volume" = LL.Fit.pmat[42],
  "rh_isthmuscingulate_volume" = LL.Fit.pmat[43],
  "rh_lateraloccipital_volume" = LL.Fit.pmat[44],
  "rh_lateralorbitofrontal_volume" = LL.Fit.pmat[45],
  "rh_lingual_volume" = LL.Fit.pmat[46],
  "rh_medialorbitofrontal_volume" = LL.Fit.pmat[47],
  "rh_middletemporal_volume" = LL.Fit.pmat[48],
  "rh_parahippocampal_volume" = LL.Fit.pmat[49],
  "rh_paracentral_volume" = LL.Fit.pmat[50],
  "rh_parsopercularis_volume" = LL.Fit.pmat[51],
  "rh_parsorbitalis_volume" = LL.Fit.pmat[52],
  "rh_parstriangularis_volume" = LL.Fit.pmat[53],
  "rh_pericalcarine_volume" = LL.Fit.pmat[54],
  "rh_postcentral_volume" = LL.Fit.pmat[55],
  "rh_posteriorcingulate_volume" = LL.Fit.pmat[56],
  "rh_precentral_volume" = LL.Fit.pmat[57],
  "rh_precuneus_volume" = LL.Fit.pmat[58],
  "rh_rostralanteriorcingulate_volume" = LL.Fit.pmat[59],
  "rh_rostralmiddlefrontal_volume" = LL.Fit.pmat[60],
  "rh_superiorfrontal_volume" = LL.Fit.pmat[61],
  "rh_superiorparietal_volume" = LL.Fit.pmat[62],
  "rh_superiortemporal_volume" = LL.Fit.pmat[63],
  "rh_supramarginal_volume" = LL.Fit.pmat[64],
  "rh_frontalpole_volume" = LL.Fit.pmat[65],
  "rh_temporalpole_volume" = LL.Fit.pmat[66],
  "rh_transversetemporal_volume" = LL.Fit.pmat[67],
  "rh_insula_volume" = LL.Fit.pmat[68],
  "Left_Cerebellum_Cortex" = LL.Fit.pmat[69],
  "Left_Thalamus_Proper" = LL.Fit.pmat[70],
  "Left_Caudate" = LL.Fit.pmat[71],
  "Left_Putamen" = LL.Fit.pmat[72],
  "Left_Pallidum" = LL.Fit.pmat[73],
  "Brain_Stem" = LL.Fit.pmat[74],
  "Left_Hippocampus" = LL.Fit.pmat[75],
  "Left_Amygdala" = LL.Fit.pmat[76],
  "Left_Accumbens_area" = LL.Fit.pmat[77],
  "Left_VentralDC" = LL.Fit.pmat[78],
  "Right_Cerebellum_Cortex" = LL.Fit.pmat[79],
  "Right_Thalamus_Proper" = LL.Fit.pmat[80],
  "Right_Caudate" = LL.Fit.pmat[81],
  "Right_Putamen" = LL.Fit.pmat[82],
  "Right_Pallidum" = LL.Fit.pmat[83],
  "Right_Hippocampus" = LL.Fit.pmat[84],
  "Right_Amygdala" = LL.Fit.pmat[85],
  "Right_Accumbens_area" = LL.Fit.pmat[86],
  "Right_VentralDC" = LL.Fit.pmat[87],
  "TotalGrayVol" = LL.Fit.pmat[88]
)

 


# transpose
LL_Fit.pvals<-t(LL_Fit.pvals)


#save IS output
saveRDS(LL_Fit.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/LL.Fit.pvals.Rds")
saveRDS(LLcompare_fit, file = "/Users/michaelconnaughton/Desktop/IMAGEN/LLcompare_fit.Rds")
saveRDS(AICcompare_fit, file = "/Users/michaelconnaughton/Desktop/IMAGEN/AICccompare_fit.Rds")
saveRDS(BICcompare_fit, file = "/Users/michaelconnaughton/Desktop/IMAGEN/AICccompare_fit.Rds")

write.table(LL_Fit.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/LL.Fit.pval.txt", quote = F, sep=",", col.names = F)
write.table(LLcompare_fit, file = "/Users/michaelconnaughton/Desktop/IMAGEN/LLcompare_fit.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(AICcompare_fit, file = "/Users/michaelconnaughton/Desktop/IMAGEN/AICcompare_fit.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(BICcompare_fit, file = "/Users/michaelconnaughton/Desktop/IMAGEN/BICcompare_fit.txt", quote = F, sep=",", row.names = F, col.names = T)


}

#Quad_MODEL_RFX
{
# Quad_RFX_1

Quad_RFX_1.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})

for (outcome in 1:88) {
  LL_Q_RFX_1 <- append(LL_fit_1, logLik(Quad_RFX_1.outcomes[[outcome]], REML=T))
  AIC_Q_RFX_1 <- append(AIC_fit_1, AICc(Quad_RFX_1.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_Q_RFX_1 <- append(BIC_fit_1, BICc(Quad_RFX_1.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
}


# Quad_RFX_2
Quad_RFX_2.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_Q_RFX_2 <- append(LL_Q_RFX_2, logLik(Quad.outcomes[[outcome]], REML=T))
  AIC_Q_RFX_2 <- append(AIC_Q_RFX_2, AICc(Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_Q_RFX_2 <- append(BIC_Q_RFX_2, BICc(Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
}





# observe all intercept AICc in a table
AICcompare_Q_RFX <- data.frame(DVlist, AIC_Q_RFX_1, AIC_Q_RFX_2)
BICcompare_Q_RFX <- data.frame(DVlist, BIC_Q_RFX_1, BIC_Q_RFX_2)
LLcompare_Q_RFX <- data.frame(DVlist, LL_Q_RFX_1, LL_Q_RFX_2)

# LL.FFX.pmat <- c()
for (outcome in 1:88) {
  # extract p-values only
  # do the interaction effect and the PE variable add anything to the null prediction model?
  dif_Q_RFX <- anova(Quad_RFX_1.outcomes[[outcome]], Quad_RFX_2.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL_Q_RFX.pmat <- append(LL_Q_RFX.pmat,dif_Q_RFX[2,1])
}


# create a list of FFX model comparisons (M0_M2 = null compared to full, M0_M2B = null compared to reduced model, M2_M2B = full compared to reduced)
compared_Q_RFX <- c("Quad_RFX1_Quad_RFX2")


# IS create a dataframe with outcome scales as rows, and 3 columns showing model comparisons

# Create a dataframe with the p-values for different variables
LL_Q_RFX.pvals <- data.frame(
  compared_Q_RFX,
  "lh_bankssts_volume" = LL_Q_RFX.pmat[1],
  "lh_caudalanteriorcingulate_volume" = LL_Q_RFX.pmat[2],
  "lh_caudalmiddlefrontal_volume" = LL_Q_RFX.pmat[3],
  "lh_cuneus_volume" = LL_Q_RFX.pmat[4],
  "lh_entorhinal_volume" = LL_Q_RFX.pmat[5],
  "lh_fusiform_volume" = LL_Q_RFX.pmat[6],
  "lh_inferiorparietal_volume" = LL_Q_RFX.pmat[7],
  "lh_inferiortemporal_volume" = LL_Q_RFX.pmat[8],
  "lh_isthmuscingulate_volume" = LL_Q_RFX.pmat[9],
  "lh_lateraloccipital_volume" = LL_Q_RFX.pmat[10],
  "lh_lateralorbitofrontal_volume" = LL_Q_RFX.pmat[11],
  "lh_lingual_volume" = LL_Q_RFX.pmat[12],
  "lh_medialorbitofrontal_volume" = LL_Q_RFX.pmat[13],
  "lh_middletemporal_volume" = LL_Q_RFX.pmat[14],
  "lh_parahippocampal_volume" = LL_Q_RFX.pmat[15],
  "lh_paracentral_volume" = LL_Q_RFX.pmat[16],
  "lh_parsopercularis_volume" = LL_Q_RFX.pmat[17],
  "lh_parsorbitalis_volume" = LL_Q_RFX.pmat[18],
  "lh_parstriangularis_volume" = LL_Q_RFX.pmat[19],
  "lh_pericalcarine_volume" = LL_Q_RFX.pmat[20],
  "lh_postcentral_volume" = LL_Q_RFX.pmat[21],
  "lh_posteriorcingulate_volume" = LL_Q_RFX.pmat[22],
  "lh_precentral_volume" = LL_Q_RFX.pmat[23],
  "lh_precuneus_volume" = LL_Q_RFX.pmat[24],
  "lh_rostralanteriorcingulate_volume" = LL_Q_RFX.pmat[25],
  "lh_rostralmiddlefrontal_volume" = LL_Q_RFX.pmat[26],
  "lh_superiorfrontal_volume" = LL_Q_RFX.pmat[27],
  "lh_superiorparietal_volume" = LL_Q_RFX.pmat[28],
  "lh_superiortemporal_volume" = LL_Q_RFX.pmat[29],
  "lh_supramarginal_volume" = LL_Q_RFX.pmat[30],
  "lh_frontalpole_volume" = LL_Q_RFX.pmat[31],
  "lh_temporalpole_volume" = LL_Q_RFX.pmat[32],
  "lh_transversetemporal_volume" = LL_Q_RFX.pmat[33],
  "lh_insula_volume" = LL_Q_RFX.pmat[34],
  "rh_bankssts_volume" = LL_Q_RFX.pmat[35],
  "rh_caudalanteriorcingulate_volume" = LL_Q_RFX.pmat[36],
  "rh_caudalmiddlefrontal_volume" = LL_Q_RFX.pmat[37],
  "rh_cuneus_volume" = LL_Q_RFX.pmat[38],
  "rh_entorhinal_volume" = LL_Q_RFX.pmat[39],
  "rh_fusiform_volume" = LL_Q_RFX.pmat[40],
  "rh_inferiorparietal_volume" = LL_Q_RFX.pmat[41],
  "rh_inferiortemporal_volume" = LL_Q_RFX.pmat[42],
  "rh_isthmuscingulate_volume" = LL_Q_RFX.pmat[43],
  "rh_lateraloccipital_volume" = LL_Q_RFX.pmat[44],
  "rh_lateralorbitofrontal_volume" = LL_Q_RFX.pmat[45],
  "rh_lingual_volume" = LL_Q_RFX.pmat[46],
  "rh_medialorbitofrontal_volume" = LL_Q_RFX.pmat[47],
  "rh_middletemporal_volume" = LL_Q_RFX.pmat[48],
  "rh_parahippocampal_volume" = LL_Q_RFX.pmat[49],
  "rh_paracentral_volume" = LL_Q_RFX.pmat[50],
  "rh_parsopercularis_volume" = LL_Q_RFX.pmat[51],
  "rh_parsorbitalis_volume" = LL_Q_RFX.pmat[52],
  "rh_parstriangularis_volume" = LL_Q_RFX.pmat[53],
  "rh_pericalcarine_volume" = LL_Q_RFX.pmat[54],
  "rh_postcentral_volume" = LL_Q_RFX.pmat[55],
  "rh_posteriorcingulate_volume" = LL_Q_RFX.pmat[56],
  "rh_precentral_volume" = LL_Q_RFX.pmat[57],
  "rh_precuneus_volume" = LL_Q_RFX.pmat[58],
  "rh_rostralanteriorcingulate_volume" = LL_Q_RFX.pmat[59],
  "rh_rostralmiddlefrontal_volume" = LL_Q_RFX.pmat[60],
  "rh_superiorfrontal_volume" = LL_Q_RFX.pmat[61],
  "rh_superiorparietal_volume" = LL_Q_RFX.pmat[62],
  "rh_superiortemporal_volume" = LL_Q_RFX.pmat[63],
  "rh_supramarginal_volume" = LL_Q_RFX.pmat[64],
  "rh_frontalpole_volume" = LL_Q_RFX.pmat[65],
  "rh_temporalpole_volume" = LL_Q_RFX.pmat[66],
  "rh_transversetemporal_volume" = LL_Q_RFX.pmat[67],
  "rh_insula_volume" = LL_Q_RFX.pmat[68],
  "Left_Cerebellum_Cortex" = LL_Q_RFX.pmat[69],
  "Left_Thalamus_Proper" = LL_Q_RFX.pmat[70],
  "Left_Caudate" = LL_Q_RFX.pmat[71],
  "Left_Putamen" = LL_Q_RFX.pmat[72],
  "Left_Pallidum" = LL_Q_RFX.pmat[73],
  "Brain_Stem" = LL_Q_RFX.pmat[74],
  "Left_Hippocampus" = LL_Q_RFX.pmat[75],
  "Left_Amygdala" = LL_Q_RFX.pmat[76],
  "Left_Accumbens_area" = LL_Q_RFX.pmat[77],
  "Left_VentralDC" = LL_Q_RFX.pmat[78],
  "Right_Cerebellum_Cortex" = LL_Q_RFX.pmat[79],
  "Right_Thalamus_Proper" = LL_Q_RFX.pmat[80],
  "Right_Caudate" = LL_Q_RFX.pmat[81],
  "Right_Putamen" = LL_Q_RFX.pmat[82],
  "Right_Pallidum" = LL_Q_RFX.pmat[83],
  "Right_Hippocampus" = LL_Q_RFX.pmat[84],
  "Right_Amygdala" = LL_Q_RFX.pmat[85],
  "Right_Accumbens_area" = LL_Q_RFX.pmat[86],
  "Right_VentralDC" = LL_Q_RFX.pmat[87],
  "TotalGrayVol" = LL_Q_RFX.pmat[88]
)




# transpose
LL_Q_RFX.pvals<-t(LL_Q_RFX.pvals)


#save IS output
saveRDS(LL_Q_RFX.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.Q_RFX.pvals.Rds")
saveRDS(LLcompare_Q_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_Q_RFX.Rds")
saveRDS(AICcompare_Q_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICccompare_Q_RFX.Rds")
saveRDS(BICcompare_Q_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICccompare_Q_RFX.Rds")

write.table(LL_Q_RFX.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.Q_RFX.pval.txt", quote = F, sep=",", col.names = F)
write.table(LLcompare_Q_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_Q_RFX.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(AICcompare_Q_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_Q_RFX.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(BICcompare_Q_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_Q_RFX.txt", quote = F, sep=",", row.names = F, col.names = T)

} 

#Linear_MODEL_RFX
{
  # Linear_RFX_1
  
  Linear_RFX_1.outcomes <- lapply(DVlist, function(x) {
    lme4::lmer(substitute(i ~ bully_victim * age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})
  
  for (outcome in 1:88) {
    LL_L_RFX_1 <- append(LL_L_RFX_1, logLik(Linear_RFX_1.outcomes[[outcome]], REML=T))
    AIC_L_RFX_1 <- append(AIC_L_RFX_1, AICc(Linear_RFX_1.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    BIC_L_RFX_1 <- append(BIC_L_RFX_1, BICc(Linear_RFX_1.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  }
  
  
  # Linear_RFX_2
  Linear_RFX_2.outcomes <- lapply(DVlist, function(x) {
    lme4::lmer(substitute(i ~ bully_victim * age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})
  
  
  for (outcome in 1:88) {
    LL_L_RFX_2 <- append(LL_L_RFX_2, logLik(Linear_RFX_2.outcomes[[outcome]], REML=T))
    AIC_L_RFX_2 <- append(AIC_L_RFX_2, AICc(Linear_RFX_2.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    BIC_L_RFX_2 <- append(BIC_L_RFX_2, BICc(Linear_RFX_2.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  }
  
  
  
  
  
  # observe all intercept AICc in a table
  AICcompare_L_RFX <- data.frame(DVlist, AIC_L_RFX_1, AIC_L_RFX_2)
  BICcompare_L_RFX <- data.frame(DVlist, BIC_L_RFX_1, BIC_L_RFX_2)
  LLcompare_L_RFX <- data.frame(DVlist, LL_L_RFX_1, LL_L_RFX_2)
  
  # LL.FFX.pmat <- c()
  for (outcome in 1:88) {
    # extract p-values only
    # do the interaction effect and the PE variable add anything to the null prediction model?
    dif_L_RFX <- anova(Linear_RFX_1.outcomes[[outcome]], Linear_RFX_2.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
    LL_L_RFX.pmat <- append(LL_L_RFX.pmat,dif_L_RFX[2,1])
  }
  
  
  # create a list of FFX model comparisons (M0_M2 = null compared to full, M0_M2B = null compared to reduced model, M2_M2B = full compared to reduced)
  compared_L_RFX <- c("Linear_RFX1_Linear_RFX2")
  
  
  # IS create a dataframe with outcome scales as rows, and 3 columns showing model comparisons
  
  # Create a dataframe with the p-values for different variables
  LL_L_RFX.pvals <- data.frame(
    compared_L_RFX,
    "lh_bankssts_volume" = LL_L_RFX.pmat[1],
    "lh_caudalanteriorcingulate_volume" = LL_L_RFX.pmat[2],
    "lh_caudalmiddlefrontal_volume" = LL_L_RFX.pmat[3],
    "lh_cuneus_volume" = LL_L_RFX.pmat[4],
    "lh_entorhinal_volume" = LL_L_RFX.pmat[5],
    "lh_fusiform_volume" = LL_L_RFX.pmat[6],
    "lh_inferiorparietal_volume" = LL_L_RFX.pmat[7],
    "lh_inferiortemporal_volume" = LL_L_RFX.pmat[8],
    "lh_isthmuscingulate_volume" = LL_L_RFX.pmat[9],
    "lh_lateraloccipital_volume" = LL_L_RFX.pmat[10],
    "lh_lateralorbitofrontal_volume" = LL_L_RFX.pmat[11],
    "lh_lingual_volume" = LL_L_RFX.pmat[12],
    "lh_medialorbitofrontal_volume" = LL_L_RFX.pmat[13],
    "lh_middletemporal_volume" = LL_L_RFX.pmat[14],
    "lh_parahippocampal_volume" = LL_L_RFX.pmat[15],
    "lh_paracentral_volume" = LL_L_RFX.pmat[16],
    "lh_parsopercularis_volume" = LL_L_RFX.pmat[17],
    "lh_parsorbitalis_volume" = LL_L_RFX.pmat[18],
    "lh_parstriangularis_volume" = LL_L_RFX.pmat[19],
    "lh_pericalcarine_volume" = LL_L_RFX.pmat[20],
    "lh_postcentral_volume" = LL_L_RFX.pmat[21],
    "lh_posteriorcingulate_volume" = LL_L_RFX.pmat[22],
    "lh_precentral_volume" = LL_L_RFX.pmat[23],
    "lh_precuneus_volume" = LL_L_RFX.pmat[24],
    "lh_rostralanteriorcingulate_volume" = LL_L_RFX.pmat[25],
    "lh_rostralmiddlefrontal_volume" = LL_L_RFX.pmat[26],
    "lh_superiorfrontal_volume" = LL_L_RFX.pmat[27],
    "lh_superiorparietal_volume" = LL_L_RFX.pmat[28],
    "lh_superiortemporal_volume" = LL_L_RFX.pmat[29],
    "lh_supramarginal_volume" = LL_L_RFX.pmat[30],
    "lh_frontalpole_volume" = LL_L_RFX.pmat[31],
    "lh_temporalpole_volume" = LL_L_RFX.pmat[32],
    "lh_transversetemporal_volume" = LL_L_RFX.pmat[33],
    "lh_insula_volume" = LL_L_RFX.pmat[34],
    "rh_bankssts_volume" = LL_L_RFX.pmat[35],
    "rh_caudalanteriorcingulate_volume" = LL_L_RFX.pmat[36],
    "rh_caudalmiddlefrontal_volume" = LL_L_RFX.pmat[37],
    "rh_cuneus_volume" = LL_L_RFX.pmat[38],
    "rh_entorhinal_volume" = LL_L_RFX.pmat[39],
    "rh_fusiform_volume" = LL_L_RFX.pmat[40],
    "rh_inferiorparietal_volume" = LL_L_RFX.pmat[41],
    "rh_inferiortemporal_volume" = LL_L_RFX.pmat[42],
    "rh_isthmuscingulate_volume" = LL_L_RFX.pmat[43],
    "rh_lateraloccipital_volume" = LL_L_RFX.pmat[44],
    "rh_lateralorbitofrontal_volume" = LL_L_RFX.pmat[45],
    "rh_lingual_volume" = LL_L_RFX.pmat[46],
    "rh_medialorbitofrontal_volume" = LL_L_RFX.pmat[47],
    "rh_middletemporal_volume" = LL_L_RFX.pmat[48],
    "rh_parahippocampal_volume" = LL_L_RFX.pmat[49],
    "rh_paracentral_volume" = LL_L_RFX.pmat[50],
    "rh_parsopercularis_volume" = LL_L_RFX.pmat[51],
    "rh_parsorbitalis_volume" = LL_L_RFX.pmat[52],
    "rh_parstriangularis_volume" = LL_L_RFX.pmat[53],
    "rh_pericalcarine_volume" = LL_L_RFX.pmat[54],
    "rh_postcentral_volume" = LL_L_RFX.pmat[55],
    "rh_posteriorcingulate_volume" = LL_L_RFX.pmat[56],
    "rh_precentral_volume" = LL_L_RFX.pmat[57],
    "rh_precuneus_volume" = LL_L_RFX.pmat[58],
    "rh_rostralanteriorcingulate_volume" = LL_L_RFX.pmat[59],
    "rh_rostralmiddlefrontal_volume" = LL_L_RFX.pmat[60],
    "rh_superiorfrontal_volume" = LL_L_RFX.pmat[61],
    "rh_superiorparietal_volume" = LL_L_RFX.pmat[62],
    "rh_superiortemporal_volume" = LL_L_RFX.pmat[63],
    "rh_supramarginal_volume" = LL_L_RFX.pmat[64],
    "rh_frontalpole_volume" = LL_L_RFX.pmat[65],
    "rh_temporalpole_volume" = LL_L_RFX.pmat[66],
    "rh_transversetemporal_volume" = LL_L_RFX.pmat[67],
    "rh_insula_volume" = LL_L_RFX.pmat[68],
    "Left_Cerebellum_Cortex" = LL_L_RFX.pmat[69],
    "Left_Thalamus_Proper" = LL_L_RFX.pmat[70],
    "Left_Caudate" = LL_L_RFX.pmat[71],
    "Left_Putamen" = LL_L_RFX.pmat[72],
    "Left_Pallidum" = LL_L_RFX.pmat[73],
    "Brain_Stem" = LL_L_RFX.pmat[74],
    "Left_Hippocampus" = LL_L_RFX.pmat[75],
    "Left_Amygdala" = LL_L_RFX.pmat[76],
    "Left_Accumbens_area" = LL_L_RFX.pmat[77],
    "Left_VentralDC" = LL_L_RFX.pmat[78],
    "Right_Cerebellum_Cortex" = LL_L_RFX.pmat[79],
    "Right_Thalamus_Proper" = LL_L_RFX.pmat[80],
    "Right_Caudate" = LL_L_RFX.pmat[81],
    "Right_Putamen" = LL_L_RFX.pmat[82],
    "Right_Pallidum" = LL_L_RFX.pmat[83],
    "Right_Hippocampus" = LL_L_RFX.pmat[84],
    "Right_Amygdala" = LL_L_RFX.pmat[85],
    "Right_Accumbens_area" = LL_L_RFX.pmat[86],
    "Right_VentralDC" = LL_L_RFX.pmat[87],
    "TotalGrayVol" = LL_L_RFX.pmat[88]
  )
  
  
  
  
  # transpose
  LL_L_RFX.pvals<-t(LL_L_RFX.pvals)
  
  
  #save IS output
  saveRDS(LL_L_RFX.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.L_RFX.pvals.Rds")
  saveRDS(LLcompare_L_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_L_RFX.Rds")
  saveRDS(AICcompare_L_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICccompare_L_RFX.Rds")
  saveRDS(BICcompare_L_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICccompare_L_RFX.Rds")
  
  write.table(LL_L_RFX.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.L_RFX.pval.txt", quote = F, sep=",", col.names = F)
  write.table(LLcompare_L_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_L_RFX.txt", quote = F, sep=",", row.names = F, col.names = T)
  write.table(AICcompare_L_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_L_RFX.txt", quote = F, sep=",", row.names = F, col.names = T)
  write.table(BICcompare_L_RFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_L_RFX.txt", quote = F, sep=",", row.names = F, col.names = T)
  
} 

#Quad_MODEL_FFX
{
# Null Quad MODEL
Null_Quad.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ age + I(age^2) +  sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_FFX_null_Q <- append(LL_FFX_null_Q, logLik(Null_Quad.outcomes[[outcome]], REML=T))
  AIC_FFX_null_Q <- append(AIC_FFX_null_Q, AICc(Null_Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_FFX_null_Q <- append(BIC_FFX_null_Q, BICc(Null_Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
}


# Quad_Simple MODEL 1
Quad_simple.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim + age + I(age^2) +  sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_FFX_simple_Q <- append(LL_FFX_simple_Q, logLik(Quad_simple.outcomes[[outcome]], REML=T))
  AIC_FFX_simple_Q <- append(AIC_FFX_simple_Q, AICc(Quad_simple.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_FFX_simple_Q <- append(BIC_FFX_simple_Q, BICc(Null_Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))

}


# Quad MODEL 1
Quad_complex.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_FFX_complex_Q <- append(LL_FFX_complex_Q, logLik(Quad_complex.outcomes[[outcome]], REML=T))
  AIC_FFX_complex_Q <- append(AIC_FFX_complex_Q, AICc(Quad_complex.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_FFX_complex_Q <- append(BIC_FFX_complex_Q, BICc(Null_Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  
  }



# observe all log likelihoods in a table
LLcompare_Q_FFX <- data.frame(DVlist, LL_FFX_null_Q, LL_FFX_simple_Q, LL_FFX_complex_Q)
AICcompare_Q_FFX <- data.frame(DVlist, AIC_FFX_null_Q, AIC_FFX_simple_Q, AIC_FFX_complex_Q)
BICcompare_Q_FFX <- data.frame(DVlist, BIC_FFX_null_Q, BIC_FFX_simple_Q, BIC_FFX_complex_Q)


compared_FFX_IS_Q <- c("Null_Simple", "Null_Complex", "Simple_Complex")


# LL.FFX.pmat <- c()
for (outcome in 1:88) {
  # extract p-values only
  # do the interaction effect and the PE variable add anything to the null prediction model?
  dif.0.2_FFX_Q <- anova(Null_Quad.outcomes[[outcome]], Quad_simple.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.0.2_FFX_Q[2,1])
  dif.0.2B_FFX_Q <- anova(Null_Quad.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.0.2B_FFX_Q[2,1])
  dif.2.2B_FFX_Q <- anova(Quad_simple.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.2.2B_FFX_Q[2,1])
}


LL.FFX_Q.pvals <- data.frame(
  compared_FFX_IS_Q,
  "lh_bankssts_volume" = LL.FFX_Q.pmat[1:3],
  "lh_caudalanteriorcingulate_volume" = LL.FFX_Q.pmat[4:6],
  "lh_caudalmiddlefrontal_volume" = LL.FFX_Q.pmat[7:9],
  "lh_cuneus_volume" = LL.FFX_Q.pmat[10:12],
  "lh_entorhinal_volume" = LL.FFX_Q.pmat[13:15],
  "lh_fusiform_volume" = LL.FFX_Q.pmat[16:18],
  "lh_inferiorparietal_volume" = LL.FFX_Q.pmat[19:21],
  "lh_inferiortemporal_volume" = LL.FFX_Q.pmat[22:24],
  "lh_isthmuscingulate_volume" = LL.FFX_Q.pmat[25:27],
  "lh_lateraloccipital_volume" = LL.FFX_Q.pmat[28:30],
  "lh_lateralorbitofrontal_volume" = LL.FFX_Q.pmat[31:33],
  "lh_lingual_volume" = LL.FFX_Q.pmat[34:36],
  "lh_medialorbitofrontal_volume" = LL.FFX_Q.pmat[37:39],
  "lh_middletemporal_volume" = LL.FFX_Q.pmat[40:42],
  "lh_parahippocampal_volume" = LL.FFX_Q.pmat[43:45],
  "lh_paracentral_volume" = LL.FFX_Q.pmat[46:48],
  "lh_parsopercularis_volume" = LL.FFX_Q.pmat[49:51],
  "lh_parsorbitalis_volume" = LL.FFX_Q.pmat[52:54],
  "lh_parstriangularis_volume" = LL.FFX_Q.pmat[55:57],
  "lh_pericalcarine_volume" = LL.FFX_Q.pmat[58:60],
  "lh_postcentral_volume" = LL.FFX_Q.pmat[61:63],
  "lh_posteriorcingulate_volume" = LL.FFX_Q.pmat[64:66],
  "lh_precentral_volume" = LL.FFX_Q.pmat[67:69],
  "lh_precuneus_volume" = LL.FFX_Q.pmat[70:72],
  "lh_rostralanteriorcingulate_volume" = LL.FFX_Q.pmat[73:75],
  "lh_rostralmiddlefrontal_volume" = LL.FFX_Q.pmat[76:78],
  "lh_superiorfrontal_volume" = LL.FFX_Q.pmat[79:81],
  "lh_superiorparietal_volume" = LL.FFX_Q.pmat[82:84],
  "lh_superiortemporal_volume" = LL.FFX_Q.pmat[85:87],
  "lh_supramarginal_volume" = LL.FFX_Q.pmat[88:90],
  "lh_frontalpole_volume" = LL.FFX_Q.pmat[91:93],
  "lh_temporalpole_volume" = LL.FFX_Q.pmat[94:96],
  "lh_transversetemporal_volume" = LL.FFX_Q.pmat[97:99],
  "lh_insula_volume" = LL.FFX_Q.pmat[100:102],
  "rh_bankssts_volume" = LL.FFX_Q.pmat[103:105],
  "rh_caudalanteriorcingulate_volume" = LL.FFX_Q.pmat[106:108],
  "rh_caudalmiddlefrontal_volume" = LL.FFX_Q.pmat[109:111],
  "rh_cuneus_volume" = LL.FFX_Q.pmat[112:114],
  "rh_entorhinal_volume" = LL.FFX_Q.pmat[115:117],
  "rh_fusiform_volume" = LL.FFX_Q.pmat[118:120],
  "rh_inferiorparietal_volume" = LL.FFX_Q.pmat[121:123],
  "rh_inferiortemporal_volume" = LL.FFX_Q.pmat[124:126],
  "rh_isthmuscingulate_volume" = LL.FFX_Q.pmat[127:129],
  "rh_lateraloccipital_volume" = LL.FFX_Q.pmat[130:132],
  "rh_lateralorbitofrontal_volume" = LL.FFX_Q.pmat[133:135],
  "rh_lingual_volume" = LL.FFX_Q.pmat[136:138],
  "rh_medialorbitofrontal_volume" = LL.FFX_Q.pmat[139:141],
  "rh_middletemporal_volume" = LL.FFX_Q.pmat[142:144],
  "rh_parahippocampal_volume" = LL.FFX_Q.pmat[145:147],
  "rh_paracentral_volume" = LL.FFX_Q.pmat[148:150],
  "rh_parsopercularis_volume" = LL.FFX_Q.pmat[151:153],
  "rh_parsorbitalis_volume" = LL.FFX_Q.pmat[154:156],
  "rh_parstriangularis_volume" = LL.FFX_Q.pmat[157:159],
  "rh_pericalcarine_volume" = LL.FFX_Q.pmat[160:162],
  "rh_postcentral_volume" = LL.FFX_Q.pmat[163:165],
  "rh_posteriorcingulate_volume" = LL.FFX_Q.pmat[166:168],
  "rh_precentral_volume" = LL.FFX_Q.pmat[169:171],
  "rh_precuneus_volume" = LL.FFX_Q.pmat[172:174],
  "rh_rostralanteriorcingulate_volume" = LL.FFX_Q.pmat[175:177],
  "rh_rostralmiddlefrontal_volume" = LL.FFX_Q.pmat[178:180],
  "rh_superiorfrontal_volume" = LL.FFX_Q.pmat[181:183],
  "rh_superiorparietal_volume" = LL.FFX_Q.pmat[184:186],
  "rh_superiortemporal_volume" = LL.FFX_Q.pmat[187:189],
  "rh_supramarginal_volume" = LL.FFX_Q.pmat[190:192],
  "rh_frontalpole_volume" = LL.FFX_Q.pmat[193:195],
  "rh_temporalpole_volume" = LL.FFX_Q.pmat[196:198],
  "rh_transversetemporal_volume" = LL.FFX_Q.pmat[199:201],
  "rh_insula_volume" = LL.FFX_Q.pmat[202:204],
  "Left_Cerebellum_Cortex" = LL.FFX_Q.pmat[205:207],
  "Left_Thalamus_Proper" = LL.FFX_Q.pmat[208:210],
  "Left_Caudate" = LL.FFX_Q.pmat[211:213],
  "Left_Putamen" = LL.FFX_Q.pmat[214:216],
  "Left_Pallidum" = LL.FFX_Q.pmat[217:219],
  "Brain_Stem" = LL.FFX_Q.pmat[220:222],
  "Left_Hippocampus" = LL.FFX_Q.pmat[223:225],
  "Left_Amygdala" = LL.FFX_Q.pmat[226:228],
  "Left_Accumbens_area" = LL.FFX_Q.pmat[229:231],
  "Left_VentralDC" = LL.FFX_Q.pmat[232:234],
  "Right_Cerebellum_Cortex" = LL.FFX_Q.pmat[235:237],
  "Right_Thalamus_Proper" = LL.FFX_Q.pmat[238:240],
  "Right_Caudate" = LL.FFX_Q.pmat[241:243],
  "Right_Putamen" = LL.FFX_Q.pmat[244:246],
  "Right_Pallidum" = LL.FFX_Q.pmat[247:249],
  "Right_Hippocampus" = LL.FFX_Q.pmat[250:252],
  "Right_Amygdala" = LL.FFX_Q.pmat[253:255],
  "Right_Accumbens_area" = LL.FFX_Q.pmat[256:258],
  "Right_VentralDC" = LL.FFX_Q.pmat[259:261],
  "TotalGrayVol" = LL.FFX_Q.pmat[262:264]
)



LL.FFX_Q.pvals<-t(LL.FFX_Q.pvals)

#save IS output
saveRDS(LL.FFX_Q.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.Q_FFX.pvals.Rds")
saveRDS(LLcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_Q_FFX.Rds")
saveRDS(AICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_Q_FFX.Rds")
saveRDS(BICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_Q_FFX.Rds")

write.table(LL.FFX_Q.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.Q_FFX.pval.txt", quote = F, sep=",", col.names = F)
write.table(LLcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_Q_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(AICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_Q_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(BICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_Q_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)



}

#Linear_MODEL_FFX
{
  # Null Linear MODEL
  Null_Linear.outcomes <- lapply(DVlist, function(x) {
    lme4::lmer(substitute(i ~ age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})
  
  
  for (outcome in 1:88) {
    LL_FFX_null_L <- append(LL_FFX_null_L, logLik(Null_Linear.outcomes[[outcome]], REML=T))
    AIC_FFX_null_L <- append(AIC_FFX_null_L, AICc(Null_Linear.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    BIC_FFX_null_L <- append(BIC_FFX_null_L, BICc(Null_Linear.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  }
  
  
  # Linear_Simple MODEL 1
  Linear_simple.outcomes <- lapply(DVlist, function(x) {
    lme4::lmer(substitute(i ~ bully_victim + age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})
  
  
  for (outcome in 1:88) {
    LL_FFX_simple_L <- append(LL_FFX_simple_L, logLik(Linear_simple.outcomes[[outcome]], REML=T))
    AIC_FFX_simple_L <- append(AIC_FFX_simple_L, AICc(Linear_simple.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    BIC_FFX_simple_L <- append(BIC_FFX_simple_L, BICc(Null_Linear.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    
  }
  
  
  # Linear MODEL 1
  Linear_complex.outcomes <- lapply(DVlist, function(x) {
    lme4::lmer(substitute(i ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})
  
  
  for (outcome in 1:88) {
    LL_FFX_complex_L <- append(LL_FFX_complex_L, logLik(Linear_complex.outcomes[[outcome]], REML=T))
    AIC_FFX_complex_L <- append(AIC_FFX_complex_L, AICc(Linear_complex.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    BIC_FFX_complex_L <- append(BIC_FFX_complex_L, BICc(Null_Linear.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
    
  }
  
  
  
  # observe all log likelihoods in a table
  LLcompare_L_FFX <- data.frame(DVlist, LL_FFX_null_L, LL_FFX_simple_L, LL_FFX_complex_L)
  AICcompare_L_FFX <- data.frame(DVlist, AIC_FFX_null_L, AIC_FFX_simple_L, AIC_FFX_complex_L)
  BICcompare_L_FFX <- data.frame(DVlist, BIC_FFX_null_L, BIC_FFX_simple_L, BIC_FFX_complex_L)
  
  
  compared_FFX_L <- c("Null_Simple", "Null_Complex", "Simple_Complex")
  
  
  # LL.FFX.pmat <- c()
  for (outcome in 1:88) {
    # extract p-values only
    # do the interaction effect and the PE variable add anything to the null prediction model?
    dif.0.2_FFX_L <- anova(Null_Linear.outcomes[[outcome]], Linear_simple.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
    LL.FFX_L.pmat <- append(LL.FFX_L.pmat,dif.0.2_FFX_L[2,1])
    dif.0.2B_FFX_L <- anova(Null_Linear.outcomes[[outcome]], Linear_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
    LL.FFX_L.pmat <- append(LL.FFX_L.pmat,dif.0.2B_FFX_L[2,1])
    dif.2.2B_FFX_L <- anova(Linear_simple.outcomes[[outcome]], Linear_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
    LL.FFX_L.pmat <- append(LL.FFX_L.pmat,dif.2.2B_FFX_L[2,1])
  }
  
  
  LL.FFX_L.pvals <- data.frame(
    compared_FFX_L,
    "lh_bankssts_volume" = LL.FFX_L.pmat[1:3],
    "lh_caudalanteriorcingulate_volume" = LL.FFX_L.pmat[4:6],
    "lh_caudalmiddlefrontal_volume" = LL.FFX_L.pmat[7:9],
    "lh_cuneus_volume" = LL.FFX_L.pmat[10:12],
    "lh_entorhinal_volume" = LL.FFX_L.pmat[13:15],
    "lh_fusiform_volume" = LL.FFX_L.pmat[16:18],
    "lh_inferiorparietal_volume" = LL.FFX_L.pmat[19:21],
    "lh_inferiortemporal_volume" = LL.FFX_L.pmat[22:24],
    "lh_isthmuscingulate_volume" = LL.FFX_L.pmat[25:27],
    "lh_lateraloccipital_volume" = LL.FFX_L.pmat[28:30],
    "lh_lateralorbitofrontal_volume" = LL.FFX_L.pmat[31:33],
    "lh_lingual_volume" = LL.FFX_L.pmat[34:36],
    "lh_medialorbitofrontal_volume" = LL.FFX_L.pmat[37:39],
    "lh_middletemporal_volume" = LL.FFX_L.pmat[40:42],
    "lh_parahippocampal_volume" = LL.FFX_L.pmat[43:45],
    "lh_paracentral_volume" = LL.FFX_L.pmat[46:48],
    "lh_parsopercularis_volume" = LL.FFX_L.pmat[49:51],
    "lh_parsorbitalis_volume" = LL.FFX_L.pmat[52:54],
    "lh_parstriangularis_volume" = LL.FFX_L.pmat[55:57],
    "lh_pericalcarine_volume" = LL.FFX_L.pmat[58:60],
    "lh_postcentral_volume" = LL.FFX_L.pmat[61:63],
    "lh_posteriorcingulate_volume" = LL.FFX_L.pmat[64:66],
    "lh_precentral_volume" = LL.FFX_L.pmat[67:69],
    "lh_precuneus_volume" = LL.FFX_L.pmat[70:72],
    "lh_rostralanteriorcingulate_volume" = LL.FFX_L.pmat[73:75],
    "lh_rostralmiddlefrontal_volume" = LL.FFX_L.pmat[76:78],
    "lh_superiorfrontal_volume" = LL.FFX_L.pmat[79:81],
    "lh_superiorparietal_volume" = LL.FFX_L.pmat[82:84],
    "lh_superiortemporal_volume" = LL.FFX_L.pmat[85:87],
    "lh_supramarginal_volume" = LL.FFX_L.pmat[88:90],
    "lh_frontalpole_volume" = LL.FFX_L.pmat[91:93],
    "lh_temporalpole_volume" = LL.FFX_L.pmat[94:96],
    "lh_transversetemporal_volume" = LL.FFX_L.pmat[97:99],
    "lh_insula_volume" = LL.FFX_L.pmat[100:102],
    "rh_bankssts_volume" = LL.FFX_L.pmat[103:105],
    "rh_caudalanteriorcingulate_volume" = LL.FFX_L.pmat[106:108],
    "rh_caudalmiddlefrontal_volume" = LL.FFX_L.pmat[109:111],
    "rh_cuneus_volume" = LL.FFX_L.pmat[112:114],
    "rh_entorhinal_volume" = LL.FFX_L.pmat[115:117],
    "rh_fusiform_volume" = LL.FFX_L.pmat[118:120],
    "rh_inferiorparietal_volume" = LL.FFX_L.pmat[121:123],
    "rh_inferiortemporal_volume" = LL.FFX_L.pmat[124:126],
    "rh_isthmuscingulate_volume" = LL.FFX_L.pmat[127:129],
    "rh_lateraloccipital_volume" = LL.FFX_L.pmat[130:132],
    "rh_lateralorbitofrontal_volume" = LL.FFX_L.pmat[133:135],
    "rh_lingual_volume" = LL.FFX_L.pmat[136:138],
    "rh_medialorbitofrontal_volume" = LL.FFX_L.pmat[139:141],
    "rh_middletemporal_volume" = LL.FFX_L.pmat[142:144],
    "rh_parahippocampal_volume" = LL.FFX_L.pmat[145:147],
    "rh_paracentral_volume" = LL.FFX_L.pmat[148:150],
    "rh_parsopercularis_volume" = LL.FFX_L.pmat[151:153],
    "rh_parsorbitalis_volume" = LL.FFX_L.pmat[154:156],
    "rh_parstriangularis_volume" = LL.FFX_L.pmat[157:159],
    "rh_pericalcarine_volume" = LL.FFX_L.pmat[160:162],
    "rh_postcentral_volume" = LL.FFX_L.pmat[163:165],
    "rh_posteriorcingulate_volume" = LL.FFX_L.pmat[166:168],
    "rh_precentral_volume" = LL.FFX_L.pmat[169:171],
    "rh_precuneus_volume" = LL.FFX_L.pmat[172:174],
    "rh_rostralanteriorcingulate_volume" = LL.FFX_L.pmat[175:177],
    "rh_rostralmiddlefrontal_volume" = LL.FFX_L.pmat[178:180],
    "rh_superiorfrontal_volume" = LL.FFX_L.pmat[181:183],
    "rh_superiorparietal_volume" = LL.FFX_L.pmat[184:186],
    "rh_superiortemporal_volume" = LL.FFX_L.pmat[187:189],
    "rh_supramarginal_volume" = LL.FFX_L.pmat[190:192],
    "rh_frontalpole_volume" = LL.FFX_L.pmat[193:195],
    "rh_temporalpole_volume" = LL.FFX_L.pmat[196:198],
    "rh_transversetemporal_volume" = LL.FFX_L.pmat[199:201],
    "rh_insula_volume" = LL.FFX_L.pmat[202:204],
    "Left_Cerebellum_Cortex" = LL.FFX_L.pmat[205:207],
    "Left_Thalamus_Proper" = LL.FFX_L.pmat[208:210],
    "Left_Caudate" = LL.FFX_L.pmat[211:213],
    "Left_Putamen" = LL.FFX_L.pmat[214:216],
    "Left_Pallidum" = LL.FFX_L.pmat[217:219],
    "Brain_Stem" = LL.FFX_L.pmat[220:222],
    "Left_Hippocampus" = LL.FFX_L.pmat[223:225],
    "Left_Amygdala" = LL.FFX_L.pmat[226:228],
    "Left_Accumbens_area" = LL.FFX_L.pmat[229:231],
    "Left_VentralDC" = LL.FFX_L.pmat[232:234],
    "Right_Cerebellum_Cortex" = LL.FFX_L.pmat[235:237],
    "Right_Thalamus_Proper" = LL.FFX_L.pmat[238:240],
    "Right_Caudate" = LL.FFX_L.pmat[241:243],
    "Right_Putamen" = LL.FFX_L.pmat[244:246],
    "Right_Pallidum" = LL.FFX_L.pmat[247:249],
    "Right_Hippocampus" = LL.FFX_L.pmat[250:252],
    "Right_Amygdala" = LL.FFX_L.pmat[253:255],
    "Right_Accumbens_area" = LL.FFX_L.pmat[256:258],
    "Right_VentralDC" = LL.FFX_L.pmat[259:261],
    "TotalGrayVol" = LL.FFX_L.pmat[262:264]
  )
  
  
  
  LL.FFX_L.pvals<-t(LL.FFX_L.pvals)
  
  #save IS output
  saveRDS(LL.FFX_L.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.FFX_L.pvals.Rds")
  saveRDS(LLcompare_L_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_L_FFX.Rds")
  saveRDS(AICcompare_L_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_L_FFX.Rds")
  saveRDS(BICcompare_L_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_L_FFX.Rds")
  
  write.table(LL.FFX_L.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.FFX_L.pvals.txt", quote = F, sep=",", col.names = F)
  write.table(LLcompare_L_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_L_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
  write.table(AICcompare_L_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_L_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
  write.table(BICcompare_L_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_L_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
  
  
  



# observe all log likelihoods in a table
LLcompare_Quad_model <- data.frame(DVlist, LL_FFX_simple_1, LL_FFX_complex_1)

# observe all AICc in a table
AICccompare_Quad_model <- data.frame(DVlist, AIC_FFX_simple_1, AIC_FFX_complex_1)


compared_FFX_IS_Q <- c("Null_Simple", "Null_Complex", "Simple_Complex")


# LL.FFX.pmat <- c()
for (outcome in 1:88) {
  # extract p-values only
  # do the interaction effect and the PE variable add anything to the null prediction model?
  dif.0.2_IS_Q <- anova(Null_Quad.outcomes[[outcome]], Quad_simple.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_IS_Q.pmat <- append(LL.FFX_IS_Q.pmat,dif.0.2_IS_Q[2,1])
  dif.0.2B_IS_Q <- anova(Null_Quad.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_IS_Q.pmat <- append(LL.FFX_IS_Q.pmat,dif.0.2B_IS_Q[2,1])
  dif.2.2B_IS_Q <- anova(Quad_simple.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_IS_Q.pmat <- append(LL.FFX_IS_Q.pmat,dif.2.2B_IS_Q[2,1])
}
}

#LL.FFX_IS_Q.pvals_DATA_FRAME
{
LL.FFX_IS_Q.pvals <- data.frame(
  compared_FFX_IS_Q,
  "lh_bankssts_volume" = LL.FFX_IS_Q.pmat[1:3],
  "lh_caudalanteriorcingulate_volume" = LL.FFX_IS_Q.pmat[4:6],
  "lh_caudalmiddlefrontal_volume" = LL.FFX_IS_Q.pmat[7:9],
  "lh_cuneus_volume" = LL.FFX_IS_Q.pmat[10:12],
  "lh_entorhinal_volume" = LL.FFX_IS_Q.pmat[13:15],
  "lh_fusiform_volume" = LL.FFX_IS_Q.pmat[16:18],
  "lh_inferiorparietal_volume" = LL.FFX_IS_Q.pmat[19:21],
  "lh_inferiortemporal_volume" = LL.FFX_IS_Q.pmat[22:24],
  "lh_isthmuscingulate_volume" = LL.FFX_IS_Q.pmat[25:27],
  "lh_lateraloccipital_volume" = LL.FFX_IS_Q.pmat[28:30],
  "lh_lateralorbitofrontal_volume" = LL.FFX_IS_Q.pmat[31:33],
  "lh_lingual_volume" = LL.FFX_IS_Q.pmat[34:36],
  "lh_medialorbitofrontal_volume" = LL.FFX_IS_Q.pmat[37:39],
  "lh_middletemporal_volume" = LL.FFX_IS_Q.pmat[40:42],
  "lh_parahippocampal_volume" = LL.FFX_IS_Q.pmat[43:45],
  "lh_paracentral_volume" = LL.FFX_IS_Q.pmat[46:48],
  "lh_parsopercularis_volume" = LL.FFX_IS_Q.pmat[49:51],
  "lh_parsorbitalis_volume" = LL.FFX_IS_Q.pmat[52:54],
  "lh_parstriangularis_volume" = LL.FFX_IS_Q.pmat[55:57],
  "lh_pericalcarine_volume" = LL.FFX_IS_Q.pmat[58:60],
  "lh_postcentral_volume" = LL.FFX_IS_Q.pmat[61:63],
  "lh_posteriorcingulate_volume" = LL.FFX_IS_Q.pmat[64:66],
  "lh_precentral_volume" = LL.FFX_IS_Q.pmat[67:69],
  "lh_precuneus_volume" = LL.FFX_IS_Q.pmat[70:72],
  "lh_rostralanteriorcingulate_volume" = LL.FFX_IS_Q.pmat[73:75],
  "lh_rostralmiddlefrontal_volume" = LL.FFX_IS_Q.pmat[76:78],
  "lh_superiorfrontal_volume" = LL.FFX_IS_Q.pmat[79:81],
  "lh_superiorparietal_volume" = LL.FFX_IS_Q.pmat[82:84],
  "lh_superiortemporal_volume" = LL.FFX_IS_Q.pmat[85:87],
  "lh_supramarginal_volume" = LL.FFX_IS_Q.pmat[88:90],
  "lh_frontalpole_volume" = LL.FFX_IS_Q.pmat[91:93],
  "lh_temporalpole_volume" = LL.FFX_IS_Q.pmat[94:96],
  "lh_transversetemporal_volume" = LL.FFX_IS_Q.pmat[97:99],
  "lh_insula_volume" = LL.FFX_IS_Q.pmat[100:102],
  "rh_bankssts_volume" = LL.FFX_IS_Q.pmat[103:105],
  "rh_caudalanteriorcingulate_volume" = LL.FFX_IS_Q.pmat[106:108],
  "rh_caudalmiddlefrontal_volume" = LL.FFX_IS_Q.pmat[109:111],
  "rh_cuneus_volume" = LL.FFX_IS_Q.pmat[112:114],
  "rh_entorhinal_volume" = LL.FFX_IS_Q.pmat[115:117],
  "rh_fusiform_volume" = LL.FFX_IS_Q.pmat[118:120],
  "rh_inferiorparietal_volume" = LL.FFX_IS_Q.pmat[121:123],
  "rh_inferiortemporal_volume" = LL.FFX_IS_Q.pmat[124:126],
  "rh_isthmuscingulate_volume" = LL.FFX_IS_Q.pmat[127:129],
  "rh_lateraloccipital_volume" = LL.FFX_IS_Q.pmat[130:132],
  "rh_lateralorbitofrontal_volume" = LL.FFX_IS_Q.pmat[133:135],
  "rh_lingual_volume" = LL.FFX_IS_Q.pmat[136:138],
  "rh_medialorbitofrontal_volume" = LL.FFX_IS_Q.pmat[139:141],
  "rh_middletemporal_volume" = LL.FFX_IS_Q.pmat[142:144],
  "rh_parahippocampal_volume" = LL.FFX_IS_Q.pmat[145:147],
  "rh_paracentral_volume" = LL.FFX_IS_Q.pmat[148:150],
  "rh_parsopercularis_volume" = LL.FFX_IS_Q.pmat[151:153],
  "rh_parsorbitalis_volume" = LL.FFX_IS_Q.pmat[154:156],
  "rh_parstriangularis_volume" = LL.FFX_IS_Q.pmat[157:159],
  "rh_pericalcarine_volume" = LL.FFX_IS_Q.pmat[160:162],
  "rh_postcentral_volume" = LL.FFX_IS_Q.pmat[163:165],
  "rh_posteriorcingulate_volume" = LL.FFX_IS_Q.pmat[166:168],
  "rh_precentral_volume" = LL.FFX_IS_Q.pmat[169:171],
  "rh_precuneus_volume" = LL.FFX_IS_Q.pmat[172:174],
  "rh_rostralanteriorcingulate_volume" = LL.FFX_IS_Q.pmat[175:177],
  "rh_rostralmiddlefrontal_volume" = LL.FFX_IS_Q.pmat[178:180],
  "rh_superiorfrontal_volume" = LL.FFX_IS_Q.pmat[181:183],
  "rh_superiorparietal_volume" = LL.FFX_IS_Q.pmat[184:186],
  "rh_superiortemporal_volume" = LL.FFX_IS_Q.pmat[187:189],
  "rh_supramarginal_volume" = LL.FFX_IS_Q.pmat[190:192],
  "rh_frontalpole_volume" = LL.FFX_IS_Q.pmat[193:195],
  "rh_temporalpole_volume" = LL.FFX_IS_Q.pmat[196:198],
  "rh_transversetemporal_volume" = LL.FFX_IS_Q.pmat[199:201],
  "rh_insula_volume" = LL.FFX_IS_Q.pmat[202:204],
  "Left_Cerebellum_Cortex" = LL.FFX_IS_Q.pmat[205:207],
  "Left_Thalamus_Proper" = LL.FFX_IS_Q.pmat[208:210],
  "Left_Caudate" = LL.FFX_IS_Q.pmat[211:213],
  "Left_Putamen" = LL.FFX_IS_Q.pmat[214:216],
  "Left_Pallidum" = LL.FFX_IS_Q.pmat[217:219],
  "Brain_Stem" = LL.FFX_IS_Q.pmat[220:222],
  "Left_Hippocampus" = LL.FFX_IS_Q.pmat[223:225],
  "Left_Amygdala" = LL.FFX_IS_Q.pmat[226:228],
  "Left_Accumbens_area" = LL.FFX_IS_Q.pmat[229:231],
  "Left_VentralDC" = LL.FFX_IS_Q.pmat[232:234],
  "Right_Cerebellum_Cortex" = LL.FFX_IS_Q.pmat[235:237],
  "Right_Thalamus_Proper" = LL.FFX_IS_Q.pmat[238:240],
  "Right_Caudate" = LL.FFX_IS_Q.pmat[241:243],
  "Right_Putamen" = LL.FFX_IS_Q.pmat[244:246],
  "Right_Pallidum" = LL.FFX_IS_Q.pmat[247:249],
  "Right_Hippocampus" = LL.FFX_IS_Q.pmat[250:252],
  "Right_Amygdala" = LL.FFX_IS_Q.pmat[253:255],
  "Right_Accumbens_area" = LL.FFX_IS_Q.pmat[256:258],
  "Right_VentralDC" = LL.FFX_IS_Q.pmat[259:261],
  "TotalGrayVol" = LL.FFX_IS_Q.pmat[262:264]
)
  LL.FFX_IS_Q.pvals<-t(LL.FFX_IS_Q.pvals)
  
}

#Get Model Metrics

IMAGEN_bully$MatrixReasoning_Baseline <- scale(IMAGEN_bully$MatrixReasoning_Baseline)
IMAGEN_bully$bmi_Baseline <- scale(IMAGEN_bully$bmi_Baseline)

{
  Linear_complex_results <- list()
  
  # Loop through each DV
  for (dv in DVlist) {
    # Construct the formula
    formula <- as.formula(paste(dv, "~ bully_victim + age + MatrixReasoning_Baseline + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site)"))
    
    # Fit the model
    model <- lmer(formula, data=IMAGEN_bully, REML=T, control=lmerControl(optimizer="bobyqa"))
    
    # Get summary
    model_summary <- summary(model)
    
    # Extract estimates, standard errors, t-values, and p-values
    coefficients <- data.frame(Effect = rownames(model_summary$coefficients), 
                               Estimate = model_summary$coefficients[, "Estimate"],
                               Std_Error = model_summary$coefficients[, "Std. Error"],
                               T_Value = model_summary$coefficients[, "t value"],
                               P_Value = model_summary$coefficients[, "Pr(>|t|)"])
    
    # Store results
    Linear_complex_results[[dv]] <- coefficients
    
    # Combine results into a single data frame
    final_results <- bind_rows(Linear_complex_results, .id = "DV")
    
    # Export results to a CSV file (can be opened in Word)
    write.csv(final_results, "/Users/michaelconnaughton/Desktop/IMAGEN/sensitivity/Linear_simple_MatrixReasoning_Baseline.csv", row.names = FALSE)
    
  }
  
  
}

#No RFX Models
{
  Left_Thalamus_Proper_null <- lmer(Left_Thalamus_Proper ~ age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data=IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
  Left_Thalamus_Proper_simple <- lmer(Left_Thalamus_Proper ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data=IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
  Left_Thalamus_Proper_complex <- lmer(Left_Thalamus_Proper ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data=IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
  
  lrtest(Left_Thalamus_Proper_simple, Left_Thalamus_Proper_null)
  lrtest(Left_Thalamus_Proper_complex, Left_Thalamus_Proper_null)
  lrtest(Left_Thalamus_Proper_simple, Left_Thalamus_Proper_complex)
  
  BIC(Left_Thalamus_Proper_null)
  BIC(Left_Thalamus_Proper_simple)
  BIC(Left_Thalamus_Proper_complex)
  
  
  AIC(Left_Thalamus_Proper_null)
  AIC(Left_Thalamus_Proper_simple)
  AIC(Left_Thalamus_Proper_complex)
  
  #######
  
  Right_Cerebellum_Cortex_null <- lmer(Right_Cerebellum_Cortex ~ age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data=IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
  Right_Cerebellum_Cortex_simple <- lmer(Right_Cerebellum_Cortex ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data=IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
  Right_Cerebellum_Cortex_complex <- lmer(Right_Cerebellum_Cortex ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data=IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer ="bobyqa"))
  
  lrtest(Right_Cerebellum_Cortex_simple, Right_Cerebellum_Cortex_null)
  lrtest(Right_Cerebellum_Cortex_complex, Right_Cerebellum_Cortex_null)
  lrtest(Right_Cerebellum_Cortex_simple, Right_Cerebellum_Cortex_complex)
  
  BIC(Right_Cerebellum_Cortex_null)
  BIC(Right_Cerebellum_Cortex_simple)
  BIC(Right_Cerebellum_Cortex_complex)
  
  
  AIC(Right_Cerebellum_Cortex_null)
  AIC(Right_Cerebellum_Cortex_simple)
  AIC(Right_Cerebellum_Cortex_complex)
}

#Post Hoc Pval
{
  
  
  # Numbers from the given list
  Quad_complex_bully_age <- c(0.505623701, 2.45e-06, 0.198773759, 4.89e-05, 1.58e-05, 0.265626004, 0.197911997, 0.021335117, 0.537914697, 1.77e-06, 0.277460992, 0.199783815, 4.74e-05, 0.450645531, 0.006041167, 0.471654501, 0.212289872, 0.000591639, 0.715736216, 0.000452553, 0.636675949, 0.118869511, 0.815526581, 0.770498423, 0.03438106, 0.31982337, 0.783639694, 0.212788264, 0.200641608, 0.000805438, 1.41e-06, 0.850931688, 0.621632386, 1.09e-12, 3.39e-05, 5.79e-05, 0.850253806, 1.47e-07, 6.31e-12, 0.071025554, 0.057351558, 0.986682604, 0.035022542, 3.23e-09, 0.134260495, 2.42e-05, 7.85e-08, 0.10010147, 0.000469328, 0.131948909, 0.423869214, 0.099990689, 0.125202078, 0.025319366, 0.870933767, 0.281587122, 0.242311931, 0.043334594, 0.142890518, 0.585764865, 0.081953139, 0.656114872, 0.920863021, 0.814207777, 3.83e-07, 0.00371681, 0.490274083, 1.37e-09, 4.80e-20, 0.001678205, 0.003121806, 2.48e-26, 1.12e-12, 0.734695633, 0.000721844, 0.000951893, 1.16e-05, 1.28e-09, 1.12e-08, 0.324926341, 0.000109885, 2.10e-24, 1.30e-09, 2.18e-09, 0.000358824, 0.191677499, 0.021657286, 0.182665553)
  Quad_complex_bully <- c(0.959560151, 0.026494022, 0.010017234, 0.018973174, 1.95e-05, 0.101492471, 0.785362659, 0.152013468, 0.967615577, 9.73e-08, 0.010649991, 0.029233599, 5.75e-05, 0.504689162, 0.017990783, 0.005779313, 0.190600519, 0.000833704, 0.025438035, 0.180307908, 0.100749946, 0.979511563, 0.001995369, 0.263055383, 0.118653915, 0.001051699, 0.003106154, 0.020796692, 0.000738825, 6.23e-07, 2.87e-07, 0.441909229, 0.200472262, 8.13e-18, 0.034441015, 0.031211289, 0.407183363, 9.41e-07, 2.08e-12, 0.624074873, 0.639362304, 0.517321052, 0.346223873, 0.000246323, 0.002190363, 2.26e-05, 1.79e-10, 0.47677441, 3.27e-05, 0.017678136, 0.54661916, 0.006207351, 0.102310016, 0.038855287, 0.017255026, 0.377809636, 0.120499776, 0.030343461, 0.506935849, 0.024958176, 0.000151797, 0.351720904, 0.509259273, 0.561800187, 5.50e-05, 0.001631626, 0.480598508, 1.42e-13, 1.33e-18, 0.080896176, 0.006194732, 6.49e-22, 7.83e-18, 0.590223909, 0.00067338, 0.003270591, 1.66e-07, 4.47e-11, 2.32e-09, 0.162015489, 6.31e-09, 5.46e-30, 2.06e-08, 5.92e-07, 0.000913152, 1.34e-05, 2.74e-05, 0.000254576)
  Quad_simple_bully <- c(0.867073405, 0.21697344, 0.004664747, 0.223031524, 2.35e-03, 0.129540735, 6.09e-01, 0.319151761, 8.68e-01, 3.41e-05, 0.015832085, 5.31e-02, 1.51e-03, 5.89e-01, 7.37e-02, 3.23e-03, 1.18e-01, 0.010312099, 0.027184673, 0.773878473, 0.07795941, 0.728119955, 0.001135231, 0.231842375, 0.038609673, 4.23e-04, 0.001720944, 3.73e-02, 0.001521935, 1.87e-05, 8.77e-05, 0.397435286, 0.231282327, 1.69e-11, 0.126565964, 0.171809967, 0.378386648, 0.000854956, 4.06e-07, 0.829376447, 0.379859831, 0.514174312, 0.587786574, 0.040883818, 0.004588848, 0.001584751, 3.07e-07, 0.64327322, 0.000598356, 0.035609942, 0.435039966, 0.014930018, 0.171723705, 0.156678592, 0.01388322, 0.260335748, 0.056258761, 0.069995762, 0.317095263, 0.029367006, 4.87e-04, 0.281060647, 0.491783686, 0.587015143, 0.00503075, 0.014254203, 0.551636286, 3.47e-09, 6.34e-10, 0.470566356, 0.026465373, 3.31e-13, 9.83e-13, 0.494677248, 0.008021067, 0.036042515, 1.92e-05, 2.03e-06, 5.68e-06, 0.069088018, 2.70e-07, 5.43e-19, 0.00024012, 0.000293536, 0.016379827, 3.22e-05, 0.00027983, 0.000391517)
  
  
  
  Linear_complex_bully <- c(0.049360101, 1.87e-07, 0.020665151, 5.03e-11, 1.02e-16, 9.78e-08, 0.709296517, 0.98884133, 0.035088618, 6.74e-27, 1.24e-06, 0.185611516, 4.26e-21, 0.499351137, 2.63e-13, 1.62e-08, 0.550308441, 1.19e-18, 2.80e-10, 0.000215386, 0.000230872, 0.734122159, 5.03e-08, 0.000228054, 0.554726643, 2.41e-06, 2.76e-07, 3.55e-07, 8.96e-14, 6.54e-19, 3.31e-34, 0.046882697, 0.024113462, 2.12e-65, 1.84e-05, 5.25e-11, 0.219135369, 1.10e-26, 9.84e-38, 0.00028152, 0.376209909, 0.980186117, 0.000104277, 1.61e-23, 1.67e-08, 7.51e-12, 8.27e-33, 0.581473011, 2.18e-16, 6.86e-05, 0.406846789, 3.22e-13, 0.000217102, 4.55e-05, 1.11e-08, 0.019991468, 0.000219087, 0.000535843, 0.003016076, 0.004438129, 4.60e-06, 0.028505214, 0.012180172, 0.728980997, 4.32e-12, 1.44e-10, 0.011863099, 4.91e-57, 2.89e-59, 3.19e-08, 3.41e-19, 1.15e-75, 9.11e-71, 0.064209034, 1.69e-15, 3.52e-05, 1.03e-25, 1.53e-53, 8.29e-37, 0.004048904, 3.66e-40, 7.25e-115, 5.38e-68, 7.96e-23, 2.27e-06, 5.35e-30, 1.41e-23, 1.59e-11)
  Linear_complex_bully_age <- c(2.77e-07, 9.60e-33, 0.002444818, 1.38e-37, 2.73e-35, 1.85e-21, 0.087100824, 0.948518376, 0.001053434, 2.15e-54, 4.29e-09, 0.713573018, 4.66e-52, 0.355543305, 1.29e-37, 1.49e-05, 0.000332527, 3.22e-48, 7.51e-16, 8.87e-20, 0.001787736, 0.101646326, 0.000511364, 3.24e-06, 1.11e-07, 0.379307782, 0.001707982, 1.18e-11, 4.86e-20, 1.34e-28, 2.59e-74, 0.050539229, 0.02345656, 3.30e-117, 2.06e-23, 6.47e-43, 0.717644428, 7.60e-60, 3.10e-73, 6.23e-19, 0.03880478, 0.11704502, 7.08e-18, 7.58e-79, 7.53e-12, 7.50e-23, 1.31e-60, 0.072763053, 1.73e-31, 1.56e-06, 0.396356701, 5.43e-27, 1.40e-09, 4.58e-10, 2.78e-09, 0.2583291, 0.037690655, 3.27e-07, 4.37e-20, 0.121504925, 0.001369375, 0.115894519, 0.000352616, 0.088204751, 1.82e-30, 5.75e-21, 7.59e-06, 1.25e-110, 3.27e-119, 2.20e-25, 4.85e-56, 1.52e-170, 1.07e-135, 9.03e-07, 4.50e-36, 8.15e-10, 1.34e-48, 2.72e-103, 2.68e-72, 0.436896743, 1.40e-78, 2.84e-216, 3.30e-151, 2.27e-58, 1.26e-11, 7.06e-44, 1.57e-34, 1.83e-17)
  Linear_simple_bully <- c(0.001841273, 1.51e-12, 0.103376595, 8.18e-14, 3.15e-16, 9.63e-14, 0.987529115, 0.979258137, 0.004840689, 3.32e-30, 3.87e-09, 0.201237142, 5.75e-22, 0.39184371, 2.18e-17, 6.35e-11, 0.807538237, 6.65e-22, 1.77e-14, 5.02e-07, 9.87e-06, 0.986193407, 2.05e-09, 2.75e-06, 0.768699183, 6.33e-07, 8.49e-09, 2.79e-10, 2.50e-18, 8.88e-25, 4.36e-29, 0.029958095, 0.006281061, 8.74e-51, 2.15e-10, 2.86e-18, 0.180980093, 5.29e-25, 3.33e-30, 4.13e-07, 0.669040515, 0.709467917, 3.15e-08, 6.42e-22, 6.12e-11, 3.20e-16, 4.11e-32, 0.349365443, 4.00e-20, 1.53e-06, 0.504844612, 2.06e-18, 3.68e-07, 3.47e-06, 1.22e-11, 0.008709649, 4.77e-05, 3.16e-06, 4.77e-05, 0.0011133, 1.57e-07, 0.013720909, 0.000805557, 0.487848523, 3.87e-13, 3.58e-11, 0.000597321, 1.62e-45, 1.51e-50, 1.87e-07, 5.28e-29, 5.95e-67, 1.66e-57, 0.015606933, 1.04e-18, 6.82e-06, 3.97e-26, 3.60e-40, 8.02e-39, 0.003067314, 2.33e-46, 1.77e-88, 3.99e-42, 4.24e-26, 5.52e-07, 1.52e-28, 5.83e-23, 2.82e-17)
  
  Quad_sex_bully_age_simple <- c(0.887747894, 0.925620026, 0.00052688, 0.452481702, 0.36014146, 0.837441107, 3.30E-07, 0.12137398, 6.90E-06, 0.02963273, 0.001364031, 0.00238828, 0.170322143, 0.137207516, 0.006908485, 0.026769835, 0.165897457, 0.319414536, 0.081070065, 0.002575782, 0.00390065, 0.450248793, 0.028493164, 0.000712734, 0.182106415, 0.686795052, 0.223902528, 0.000187869, 0.848724456, 0.291580468, 0.867475796, 0.85124788, 0.009915836, 0.792346297, 6.96E-05, 0.983394315, 0.001930183, 0.455173905, 0.309129159, 0.65866872, 0.046516604, 0.149704009, 0.006278389, 0.318488579, 0.894342884, 0.648130463, 0.743758842, 0.872941643, 2.31E-07, 0.000303726, 0.422645413, 0.886807444, 0.242197405, 0.010000973, 0.000868774, 0.470107819, 8.35E-05, 0.002416943, 0.066596711, 0.677948381, 0.069271614, 0.000585776, 0.232057728, 0.078735784, 0.065453644, 0.234377592, 0.001208223, 0.022529527, 0.098151476, 0.922545377, 0.084195068, 0.585501813, 0.332793036, 0.000448911, 0.864609316, 0.015554019, 0.000141508, 4.23E-07, 0.178543521, 0.40286421, 0.268058175, 0.006692774, 0.131972466, 0.247020465, 0.006079444, 0.115246438, 7.46E-08, 0.181430045)
  
  Quad_sex_bully_age_complex <- c(0.894782793, 0.945236759, 0.000499554, 0.455049639, 0.411113041, 0.818745925, 2.89E-07, 0.109236944, 6.80E-06, 0.032452336, 0.001502326, 0.002450623, 0.137705682, 0.133755183, 0.00635687, 0.02592859, 0.162331972, 0.293021114, 0.081653209, 0.002507509, 0.003855694, 0.441665747, 0.02822474, 0.000698318, 0.191771023, 0.669328294, 0.222051496, 0.000204815, 0.864653728, 0.273791683, 0.78750864, 0.847416845, 0.010035637, 0.884560343, 4.80E-05, 0.959546771, 0.001905383, 0.486746764, 0.376596535, 0.683546041, 0.044231753, 0.149492664, 0.006528989, 0.354213524, 0.928721397, 0.625385942, 0.667069114, 0.840460249, 1.66E-07, 0.000327996, 0.415933184, 0.906478217, 0.251554388, 0.010318252, 0.000863976, 0.463669867, 7.76E-05, 0.002673183, 0.068962614, 0.686025804, 0.075102018, 0.000568692, 0.231150203, 0.077957407, 0.086740746, 0.201808429, 0.001242554, 0.01678691, 0.100498936, 0.900891465, 0.081188043, 0.465962989, 0.237803674, 0.000449867, 0.907735853, 0.018024499, 0.000192989, 2.14E-07, 0.183859119, 0.407667341, 0.251504165, 0.004722837, 0.158682291, 0.263326124, 0.007377224, 0.120564553, 5.75E-08, 0.197077917)
  
  Linear_sex_bully_age_complex <- c(0.955771706, 0.983863597, 0.00046293, 0.470226126, 0.472783385, 0.648873298, 2.97E-07, 0.129569602, 8.42E-06, 0.041844841, 0.00242477, 0.00226086, 0.089937118, 0.134322068, 0.00458465, 0.035328738, 0.155212534, 0.232302403, 0.099543008, 0.002572197, 0.004549664, 0.447384688, 0.033220544, 0.001066577, 0.205087008, 0.705247555, 0.244426016, 0.000310484, 0.956684065, 0.243804897, 0.611346294, 0.892948413, 0.010553781, 0.954195559, 3.40E-05, 0.871840647, 0.00199834, 0.534878757, 0.461419789, 0.811059937, 0.045415033, 0.161220833, 0.008106005, 0.397730116, 0.966675367, 0.601303627, 0.577491886, 0.850893244, 1.04E-07, 0.00040435, 0.419164761, 0.998506085, 0.278856105, 0.010934704, 0.001299438, 0.489885266, 0.00010046, 0.003071845, 0.091079644, 0.697395963, 0.077315151, 0.000670019, 0.260625476, 0.08442438, 0.10875662, 0.160201275, 0.001566581, 0.014542468, 0.111098129, 0.820930414, 0.060221339, 0.304022128, 0.095383473, 0.000469989, 0.975394604, 0.019628424, 0.000531253, 4.03E-07, 0.211449556, 0.395892475, 0.169205238, 0.003547114, 0.298722783, 0.314018443, 0.008373548, 0.218004788, 4.73E-08, 0.286127938)
  Linear_sex_bully_age_simple <- list(0.965904933, 0.975700113, 0.00044561, 0.438635216, 0.327238911, 0.650230588, 3.01E-07, 0.129407263, 8.36E-06, 0.024326848, 0.00183257, 0.002248677, 0.164295535, 0.135439979, 0.003039059, 0.034128334, 0.153433838, 0.291992808, 0.102131949, 0.002667102, 0.004426717, 0.444036827, 0.031200596, 0.000950542, 0.203083253, 0.696332099, 0.236085534, 0.000251442, 0.884750789, 0.284359506, 0.699978181, 0.875087641, 0.010610153, 0.512088273, 4.12E-05, 0.788793794, 0.001983885, 0.418124077, 0.24439556, 0.793287422, 0.04508123, 0.153125246, 0.008816723, 0.293363386, 0.927216729, 0.590873464, 0.786212669, 0.865349339, 1.54E-07, 0.000408769, 0.419992272, 0.915130147, 0.263861774, 0.010994653, 0.001167369, 0.491351725, 9.52E-05, 0.002889564, 0.096962773, 0.688495613, 0.072383467, 0.000646593, 0.249508576, 0.081162371, 0.077826983, 0.229066652, 0.001536325, 0.047841228, 0.125313541, 0.807658782, 0.082195819, 0.407819399, 0.247616821, 0.000427879, 0.931338284, 0.018511352, 0.00049907, 1.42E-06, 0.267235281, 0.395938494, 0.248359841, 0.009018565, 0.266711084, 0.280458372, 0.00769199, 0.173287729, 5.07E-08, 0.211148145)
  
  
  Quad_complex_bully_LEQ <- c(0.95847686, 0.03271316, 0.022891991, 0.037464224, 2.83e-05, 0.077958182, 0.975052641, 0.209, 0.558, 4.29e-07, 0.0271, 0.070867793, 0.00065683, 0.658848297, 0.00319, 0.022187065, 0.265495201, 0.00221, 0.061999122, 0.522564143, 0.132261827, 0.590734355, 4.35e-03, 3.99e-01, 3.18e-01, 4.80e-03, 8.19e-03, 3.67e-02, 0.0020904, 4.34e-06, 5.84e-06, 0.610540803, 3.79e-01, 2.45e-16, 0.029900174, 0.012528577, 0.589396822, 6.52e-06, 3.74e-12, 5.23e-01, 0.734695707, 0.709875961, 6.16e-01, 0.00100788, 7.74e-03, 3.05e-04, 3.63e-08, 0.383498803, 4.68e-06, 5.99e-02, 8.76e-01, 6.97e-03, 0.259865434, 1.32e-01, 0.040279749, 0.559264225, 0.154414918, 0.063242606, 0.643813908, 0.048142266, 0.000832993, 0.424972381, 0.742083484, 0.396499836, 0.000183571, 0.002212774, 0.572749869, 7.08e-12, 6.07e-17, 0.044093648, 0.010965147, 1.24e-19, 5.16e-15, 0.650523256, 0.000653437, 0.000991564, 2.58e-08, 6.41e-10, 3.53e-08, 0.203061721, 1.48e-08, 1.34e-29, 3.99e-07, 9.93e-08, 0.000499763, 3.83e-06, 0.00011749, 0.001106536)
  Quad_complex_bully_INT_LEQ <- c(0.457844812, 1.82e-06, 0.116342564, 0.000100224, 1.30e-05, 1.82e-01, 9.17e-02, 4.76e-02, 2.65e-01, 2.15e-05, 0.583127731, 0.424263594, 0.000102164, 0.617367023, 0.011079255, 0.36203837, 0.095482216, 1.14e-03, 0.975732302, 1.79e-03, 0.360453563, 4.77e-02, 5.88e-01, 5.82e-01, 7.86e-03, 1.99e-01, 6.18e-01, 3.86e-01, 5.26e-01, 2.60e-03, 2.48e-05, 0.668139762, 8.46e-01, 1.23e-11, 4.94e-06, 0.000174804, 6.77e-01, 9.61e-07, 3.12e-11, 4.22e-02, 0.025066888, 0.945157772, 0.054537312, 7.16e-08, 2.25e-01, 0.000168379, 2.42e-06, 0.059601008, 5.55e-04, 1.96e-01, 3.44e-01, 1.97e-01, 2.03e-01, 0.044600403, 0.591816453, 0.170492652, 0.12305281, 0.080093341, 0.128354011, 0.914111831, 0.157336191, 0.515710127, 0.58894848, 0.656986846, 1.69e-06, 0.009302644, 0.938523772, 1.25e-08, 2.56e-19, 0.000856246, 0.001614797, 7.64e-25, 9.79e-12, 0.789188815, 0.001809917, 0.000403335, 2.88e-05, 6.08e-09, 1.16e-08, 0.199916497, 1.27e-05, 6.59e-24, 3.18e-08, 1.10e-08, 0.000323555, 0.132346633, 0.032430291, 0.348539152)
  Quad_simple_bully_LEQ <- c(0.862949833, 0.220768959, 0.010651499, 0.29681636, 0.003315968, 0.104851637, 0.796768066, 0.371919385, 0.698687769, 5.18e-05, 0.031410277, 0.094388329, 0.009564045, 0.718715164, 0.013715234, 0.013122481, 0.156695222, 0.019070996, 0.057334285, 0.791949242, 0.090381001, 0.86886211, 0.002082252, 0.336760578, 0.11483787, 0.001906973, 0.004086081, 0.051618978, 0.002604271, 7.00e-05, 0.000475456, 0.522388362, 0.391111371, 1.29e-10, 0.119654944, 0.065126216, 0.531300306, 0.002044107, 3.56e-07, 0.7302149, 0.431269272, 0.714551366, 0.87217275, 0.06082252, 0.013145689, 0.006941571, 9.63e-06, 0.550388692, 9.48e-05, 0.098235374, 0.72950735, 0.013134958, 0.359475564, 0.355199844, 0.028935136, 0.390381593, 0.063655646, 0.117233108, 0.430591037, 0.046706683, 0.001916706, 0.323839748, 0.672682527, 0.438783987, 0.008876492, 0.014232471, 0.576421014, 4.30e-08, 3.79e-09, 0.335710292, 0.040304074, 6.93e-12, 9.52e-11, 0.575569808, 0.005631387, 0.015777362, 2.53e-06, 8.52e-06, 3.42e-05, 0.077327078, 7.27e-07, 3.53e-19, 0.000846082, 3.80e-05, 0.009842282, 1.19e-05, 0.000838146, 0.001437121)
  
  Linear_simple_bully_LEQ <- c(0.021337028, 3.39e-08, 0.223363007, 1.32e-07, 9.40e-11, 2.42e-09, 0.514992847, 0.84070655, 0.405632816, 1.26e-19, 1.78e-05, 0.623936612, 5.88e-13, 0.860332856, 1.26e-13, 4.11e-06, 0.831416274, 1.55e-13, 7.70e-08, 0.021471935, 0.001981895, 0.292693125, 5.52e-06, 0.002428352, 0.592371348, 0.00022381, 1.74e-05, 9.50e-06, 8.44e-11, 2.14e-14, 4.10e-18, 0.126437532, 0.153236321, 3.97e-35, 1.79e-07, 8.09e-14, 0.593241027, 6.24e-16, 2.54e-22, 8.44e-05, 0.977102418, 0.567039629, 0.00070545, 1.07e-13, 1.22e-06, 6.52e-08, 3.24e-20, 0.200055202, 2.09e-16, 0.001542507, 0.950421751, 1.14e-11, 0.003306759, 0.014420674, 1.62e-06, 0.139011106, 0.002327167, 0.002275796, 0.001780892, 0.01862203, 9.16e-05, 0.105970928, 0.060743173, 0.683442524, 8.78e-09, 4.57e-08, 0.033313383, 1.67e-32, 6.34e-34, 1.60e-05, 3.31e-19, 2.64e-46, 1.83e-38, 0.085290112, 2.15e-13, 1.08e-05, 2.25e-21, 4.39e-26, 4.93e-24, 0.01686153, 8.49e-33, 5.27e-66, 2.02e-26, 4.87e-21, 6.73e-06, 9.74e-22, 6.33e-15, 2.60e-10)
  Linear_complex_bully_LEQ <- c(0.099195168, 5.58e-06, 0.077017816, 3.54e-07, 4.70e-13, 2.11e-06, 0.801137009, 0.88850492, 0.505282942, 2.05e-19, 0.000122285, 0.510199591, 9.83e-14, 0.861368671, 3.76e-12, 2.02e-05, 0.69823406, 4.59e-13, 1.88e-06, 0.062986602, 0.003990614, 0.501707048, 9.93e-06, 0.007661242, 0.989746674, 0.000218438, 3.29e-05, 6.44e-05, 1.94e-09, 5.81e-13, 4.84e-24, 0.132012392, 0.188215677, 4.83e-50, 6.28e-05, 4.84e-10, 0.549547773, 2.02e-18, 8.09e-31, 0.001683747, 0.702353789, 0.723171659, 0.0114698, 3.38e-15, 5.85e-06, 7.69e-07, 3.15e-22, 0.338303513, 2.66e-15, 0.004049959, 0.836401275, 6.72e-10, 0.018925655, 0.015996392, 1.23e-05, 0.139584883, 0.002873762, 0.01034989, 0.009300334, 0.022813971, 0.000196783, 0.114873496, 0.111138461, 0.652848741, 2.77e-09, 8.57e-09, 0.074753882, 9.93e-42, 1.14e-44, 4.23e-07, 1.10e-13, 6.97e-58, 4.37e-50, 0.115127726, 1.73e-12, 7.28e-06, 9.41e-24, 3.89e-39, 8.75e-26, 0.01689583, 1.25e-30, 1.74e-91, 1.78e-47, 1.90e-20, 3.12e-06, 6.43e-25, 9.87e-17, 5.48e-08)
  Linear_complex_bully_INT_LEQ <- c(1.85e-06, 4.53e-28, 0.000212804, 1.85e-26, 2.70e-28, 3.86e-18, 0.005270096, 0.516078847, 0.20623306, 2.32e-36, 3.28e-05, 0.106952893, 1.06e-38, 0.997915608, 5.52e-29, 0.005734287, 5.02e-05, 2.54e-35, 7.57e-09, 4.63e-11, 0.139706281, 0.003754819, 0.053618973, 0.001862784, 3.22e-09, 0.790673493, 0.059214633, 1.47e-06, 8.19e-11, 1.41e-17, 5.05e-56, 0.150603379, 0.328688852, 7.61e-92, 4.91e-23, 9.37e-35, 0.586819233, 1.86e-42, 9.10e-60, 3.62e-16, 0.002057818, 0.099644056, 1.10e-11, 3.59e-54, 7.21e-08, 1.89e-12, 4.14e-43, 0.01054666, 2.12e-26, 0.000423269, 0.222952451, 6.08e-18, 2.82e-05, 3.91e-05, 9.32e-05, 0.937967546, 0.421764443, 0.000302282, 4.12e-17, 0.640660352, 0.034923889, 0.577315854, 0.043427493, 0.674804909, 3.88e-24, 6.26e-17, 0.008148056, 1.46e-84, 2.56e-95, 2.78e-22, 1.92e-44, 1.48e-142, 8.16e-109, 1.85e-05, 3.15e-28, 7.45e-11, 2.53e-43, 1.97e-80, 3.77e-56, 0.987410248, 4.57e-65, 1.31e-179, 5.36e-118, 2.23e-47, 7.57e-11, 2.38e-36, 2.66e-25, 1.41e-10)
  
  
  
  Linear_complex_bully_IQ <- c(0.048446345, 4.11e-06, 0.105643534, 5.54e-11, 8.85e-17, 3.19e-07, 0.915842414, 0.584226379, 0.035774721, 1.66e-26, 9.21e-06, 0.102590825, 3.01e-18, 0.481378156, 2.29e-11, 3.24e-09, 0.919028565, 3.75e-18, 1.14e-11, 0.000131137, 2.27e-05, 0.74620228, 2.36e-07, 0.000161599, 0.505853511, 2.17e-06, 5.46e-06, 2.13e-07, 3.59e-14, 4.79e-19, 1.64e-35, 0.164447203, 0.004090238, 2.28e-62, 9.48e-07, 2.24e-10, 0.267328199, 1.08e-27, 5.95e-40, 7.37e-05, 0.458586918, 0.826589945, 0.00039262, 2.56e-23, 3.56e-08, 2.11e-11, 3.65e-30, 0.828007014, 5.87e-14, 0.000375044, 0.629617454, 6.92e-13, 0.000745747, 3.63e-06, 5.90e-08, 0.013562997, 0.000319161, 0.003404487, 0.001311809, 0.003170769, 2.33e-05, 0.020069009, 0.020046649, 0.365779232, 1.76e-12, 6.75e-12, 0.022637745, 7.59e-53, 4.86e-60, 5.43e-07, 1.18e-18, 2.82e-77, 4.55e-67, 0.087442284, 9.16e-16, 3.29e-05, 4.16e-25, 2.82e-51, 1.84e-38, 0.016996635, 1.51e-37, 2.15e-117, 1.34e-67, 7.17e-23, 2.05e-06, 2.53e-29, 1.07e-23, 1.60e-11)
  Linear_complex_bully_INT_IQ <- c(9.76e-08, 2.37e-31, 0.009206728, 1.64e-34, 3.77e-36, 2.91e-19, 0.051277191, 0.735691631, 0.001309778, 1.50e-52, 5.85e-10, 0.729587954, 1.07e-51, 0.539112745, 3.27e-36, 7.32e-06, 0.002118735, 2.68e-46, 1.77e-15, 9.85e-18, 0.004770896, 0.058827242, 0.000880123, 1.20e-06, 1.50e-07, 0.322470539, 0.000621372, 2.11e-11, 1.68e-19, 1.01e-26, 1.85e-69, 0.035969115, 0.116534246, 2.44e-113, 8.92e-19, 1.21e-38, 0.380497402, 1.80e-52, 7.18e-74, 1.06e-16, 0.055050958, 0.19543903, 8.74e-17, 6.43e-77, 6.24e-11, 1.29e-19, 3.33e-58, 0.099503633, 1.26e-27, 7.83e-07, 0.625969035, 5.12e-27, 4.35e-10, 6.23e-07, 8.64e-09, 0.480685177, 0.01693886, 2.78e-07, 3.69e-18, 0.040979523, 0.000820557, 0.120083901, 0.000274682, 0.087720981, 7.44e-30, 9.80e-21, 1.57e-05, 5.52e-112, 8.63e-113, 5.99e-26, 4.44e-53, 8.18e-163, 5.48e-134, 4.62e-08, 4.73e-35, 4.29e-11, 1.45e-49, 2.16e-95, 2.72e-69, 0.474260872, 5.31e-79, 1.16e-217, 1.92e-153, 1.36e-62, 5.57e-12, 2.35e-46, 2.26e-30, 2.30e-17)
  Linear_complex_bully_bmi <-  c(0.048446345, 4.11e-06, 0.105643534, 5.54e-11, 8.85e-17, 3.19e-07, 0.915842414, 0.584226379, 0.035774721, 1.66e-26, 9.21e-06, 0.102590825, 3.01e-18, 0.481378156, 2.29e-11, 3.24e-09, 0.919028565, 3.75e-18, 1.14e-11, 0.000131137, 2.27e-05, 0.74620228, 2.36e-07, 0.000161599, 0.505853511, 2.17e-06, 5.46e-06, 2.13e-07, 3.59e-14, 4.79e-19, 1.64e-35, 0.164447203, 0.004090238, 2.28e-62, 9.48e-07, 2.24e-10, 0.267328199, 1.08e-27, 5.95e-40, 7.37e-05, 0.458586918, 0.826589945, 0.00039262, 2.56e-23, 3.56e-08, 2.11e-11, 3.65e-30, 0.828007014, 5.87e-14, 0.000375044, 0.629617454, 6.92e-13, 0.000745747, 3.63e-06, 5.90e-08, 0.013562997, 0.000319161, 0.003404487, 0.001311809, 0.003170769, 2.33e-05, 0.020069009, 0.020046649, 0.365779232, 1.76e-12, 6.75e-12, 0.022637745, 7.59e-53, 4.86e-60, 5.43e-07, 1.18e-18, 2.82e-77, 4.55e-67, 0.087442284, 9.16e-16, 3.29e-05, 4.16e-25, 2.82e-51, 1.84e-38, 0.016996635, 1.51e-37, 2.15e-117, 1.34e-67, 7.17e-23, 2.05e-06, 2.53e-29, 1.07e-23, 1.60e-11)
  Linear_complex_bully_bmi_INT <-  c(9.76e-08, 2.37e-31, 0.009206728, 1.64e-34, 3.77e-36, 2.91e-19, 0.051277191, 0.735691631, 0.001309778, 1.50e-52, 5.85e-10, 0.729587954, 1.07e-51, 0.539112745, 3.27e-36, 7.32e-06, 0.002118735, 2.68e-46, 1.77e-15, 9.85e-18, 0.004770896, 0.058827242, 0.000880123, 1.20e-06, 1.50e-07, 0.322470539, 0.000621372, 2.11e-11, 1.68e-19, 1.01e-26, 1.85e-69, 0.035969115, 0.116534246, 2.44e-113, 8.92e-19, 1.21e-38, 0.380497402, 1.80e-52, 7.18e-74, 1.06e-16, 0.055050958, 0.19543903, 8.74e-17, 6.43e-77, 6.24e-11, 1.29e-19, 3.33e-58, 0.099503633, 1.26e-27, 7.83e-07, 0.625969035, 5.12e-27, 4.35e-10, 6.23e-07, 8.64e-09, 0.480685177, 0.01693886, 2.78e-07, 3.69e-18, 0.040979523, 0.000820557, 0.120083901, 0.000274682, 0.087720981, 7.44e-30, 9.80e-21, 1.57e-05, 5.52e-112, 8.63e-113, 5.99e-26, 4.44e-53, 8.18e-163, 5.48e-134, 4.62e-08, 4.73e-35, 4.29e-11, 1.45e-49, 2.16e-95, 2.72e-69, 0.474260872, 5.31e-79, 1.16e-217, 1.92e-153, 1.36e-62, 5.57e-12, 2.35e-46, 2.26e-30, 2.30e-17)


  # Your existing code
  # Your existing code
  # Assuming 'Quad_complex_bully' is already defined
  
  # Adjust p-values using False Discovery Rate (FDR) method
  p_adjusted_Linear_complex_bully_bmi_INT <- p.adjust(Linear_complex_bully_bmi_INT, method = "fdr")
  
  
  # Print adjusted p-values
  # print(p_adjusted_Quad_complex_bully_age)
  
  # Significance threshold after FDR correction
  alpha <- 0.05
  
  # Identify significant p-values
  significant_indices <- p_adjusted_Linear_complex_bully_bmi_INT <= alpha

  
  # Create a DataFrame with DVlist, adjusted p-values, and significance information
  data <- data.frame(
    DV = DVlist,
    Adjusted_p_value = p_adjusted_Linear_complex_bully_bmi_INT,
    Significant = significant_indices
  )
  

  # Write data to CSV file
  write.csv(data, file = "/Users/michaelconnaughton/Desktop/IMAGEN/sensitivity/p_adjusted_Linear_complex_bully_bmi_INT", row.names = FALSE)
}

# PLOTS_REMEBER_FIX_AGE
#define_predictions
{
mean_bully_victim <- mean(IMAGEN_bully$bully_victim, na.rm = TRUE)
p25_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.25, na.rm = TRUE)
p75_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.75, na.rm = TRUE)


newdata <- expand.grid(
  EstimatedTotalIntraCranialVol = mean(IMAGEN_bully$EstimatedTotalIntraCranialVol, na.rm = TRUE),
  sex = factor(levels(IMAGEN_bully$sex)[1]),
  age = seq(min(IMAGEN_bully$age, na.rm = TRUE), max(IMAGEN_bully$age, na.rm = TRUE), length.out = 100),
  SES = mean(IMAGEN_bully$SES, na.rm = TRUE),
  mode_c_pds = mean(IMAGEN_bully$mode_c_pds, na.rm = TRUE),
  bully_victim = c(mean_bully_victim, p25_bully_victim, p75_bully_victim),
  subID = unique(IMAGEN_bully$subID)[1]
)
}

#Optimal Models Effect Sizes
#Quad_Complex

{


lh_caudalanteriorcingulate_volume <- lmer(lh_caudalanteriorcingulate_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_caudalanteriorcingulate_volume)

lh_cuneus_volume <- lmer(lh_cuneus_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_cuneus_volume)

lh_entorhinal_volume <- lmer(lh_entorhinal_volume ~ bully_victim * age + I(age^2) + bully_victim*sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
summary(lh_entorhinal_volume)
effectsize(lh_entorhinal_volume)
lh_frontalpole_volume <- lmer(lh_frontalpole_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))


lh_lateraloccipital_volume <- lmer(lh_lateraloccipital_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_lateraloccipital_volume)

lh_medialorbitofrontal_volume <- lmer(lh_medialorbitofrontal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_medialorbitofrontal_volume)

lh_parahippocampal_volume <- lmer(lh_parahippocampal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_parahippocampal_volume)

lh_parsorbitalis_volume <- lmer(lh_parsorbitalis_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_parsorbitalis_volume)

lh_pericalcarine_volume <- lmer(lh_pericalcarine_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_pericalcarine_volume)

lh_precentral_volume <- lmer(lh_precentral_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_precentral_volume)


lh_rostralanteriorcingulate_volume <- lmer(lh_rostralanteriorcingulate_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_rostralanteriorcingulate_volume)

lh_rostralmiddlefrontal_volume <- lmer(lh_rostralmiddlefrontal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_rostralmiddlefrontal_volume)

lh_superiorfrontal_volume <- lmer(lh_superiorfrontal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_superiorfrontal_volume)

lh_superiortemporal_volume <- lmer(lh_superiortemporal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_superiortemporal_volume)

lh_supramarginal_volume <- lmer(lh_supramarginal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_supramarginal_volume)


lh_transversetemporal_volume <- lmer(lh_transversetemporal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_transversetemporal_volume)

lh_insula_volume <- lmer(lh_insula_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_insula_volume)

rh_bankssts_volume <- lmer(rh_bankssts_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_bankssts_volume)

rh_caudalanteriorcingulate_volume <- lmer(rh_caudalanteriorcingulate_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_caudalanteriorcingulate_volume)


rh_cuneus_volume <- lmer(rh_cuneus_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_cuneus_volume)

rh_entorhinal_volume <- lmer(rh_entorhinal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_entorhinal_volume)

rh_lateraloccipital_volume <- lmer(rh_lateraloccipital_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_lateraloccipital_volume)

rh_lateralorbitofrontal_volume <- lmer(rh_lateralorbitofrontal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_lateralorbitofrontal_volume)


rh_lingual_volume <- lmer(rh_lingual_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_lingual_volume)

rh_medialorbitofrontal_volume <- lmer(rh_medialorbitofrontal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_medialorbitofrontal_volume)

rh_parahippocampal_volume <- lmer(rh_parahippocampal_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_parahippocampal_volume)

rh_pericalcarine_volume <- lmer(rh_pericalcarine_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_pericalcarine_volume)

rh_precuneus_volume <- lmer(rh_precuneus_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_precuneus_volume)

rh_temporalpole_volume <- lmer(rh_temporalpole_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_temporalpole_volume)

rh_insula_volume <- lmer(rh_insula_volume ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_insula_volume)

Left_Cerebellum_Cortex <- lmer(Left_Cerebellum_Cortex ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Cerebellum_Cortex)

Left_Caudate <- lmer(Left_Caudate ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Caudate)

Left_Putamen <- lmer(Left_Putamen ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Putamen)


Left_Pallidum <- lmer(Left_Pallidum ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Pallidum)

Left_Hippocampus <- lmer(Left_Hippocampus ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Hippocampus)

Left_Accumbens_area <- lmer(Left_Accumbens_area ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Accumbens_area)

Left_VentralDC <- lmer(Left_VentralDC ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_VentralDC)

Right_Cerebellum_Cortex <- lmer(Right_Cerebellum_Cortex ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Cerebellum_Cortex)

Right_Caudate <- lmer(Right_Caudate ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Caudate)

Right_Putamen <- lmer(Right_Putamen ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Putamen)

Right_Pallidum <- lmer(Right_Pallidum ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Pallidum)

Right_Hippocampus <- lmer(Right_Hippocampus ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Hippocampus)

Right_Accumbens_area <- lmer(Right_Accumbens_area ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Accumbens_area)

Right_VentralDC <- lmer(Right_VentralDC ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_VentralDC)


}

#Quad_Simple

{
lh_bankssts_volume <- lmer(lh_bankssts_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_bankssts_volume)

lh_fusiform_volume <- lmer(lh_fusiform_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_fusiform_volume)

lh_isthmuscingulate_volume <- lmer(lh_isthmuscingulate_volume ~ bully_victim * age + I(age^2) + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_isthmuscingulate_volume)

lh_lateralorbitofrontal_volume <- lmer(lh_lateralorbitofrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_lateralorbitofrontal_volume)

lh_lingual_volume <- lmer(lh_lingual_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_lingual_volume)

lh_paracentral_volume <- lmer(lh_paracentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_paracentral_volume)

lh_parstriangularis_volume <- lmer(lh_parstriangularis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_parstriangularis_volume)

lh_postcentral_volume <- lmer(lh_postcentral_volume ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_postcentral_volume)

lh_precuneus_volume <- lmer(lh_precuneus_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_precuneus_volume)

lh_superiorparietal_volume <- lmer(lh_superiorparietal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_superiorparietal_volume)

lh_temporalpole_volume <- lmer(lh_temporalpole_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(lh_temporalpole_volume)

rh_fusiform_volume <- lmer(rh_fusiform_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_fusiform_volume)

rh_isthmuscingulate_volume <- lmer(rh_isthmuscingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_isthmuscingulate_volume)

rh_paracentral_volume <- lmer(rh_paracentral_volume ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_paracentral_volume)

rh_parsorbitalis_volume <- lmer(rh_parsorbitalis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_parsorbitalis_volume)

rh_parstriangularis_volume <- lmer(rh_parstriangularis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_parstriangularis_volume)

rh_postcentral_volume <- lmer(rh_postcentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_postcentral_volume)

rh_precentral_volume <- lmer(rh_precentral_volume ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_precentral_volume)

rh_rostralanteriorcingulate_volume <- lmer(rh_rostralanteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_rostralanteriorcingulate_volume)

rh_superiortemporal_volume <- lmer(rh_superiortemporal_volume ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_superiortemporal_volume)

rh_frontalpole_volume <- lmer(rh_frontalpole_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_frontalpole_volume)

rh_transversetemporal_volume <- lmer(rh_transversetemporal_volume ~ bully_victim * age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(rh_transversetemporal_volume)

Left_Thalamus_Proper <- lmer(Left_Thalamus_Proper ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Left_Thalamus_Proper)

Brain_Stem <- lmer(Brain_Stem ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Brain_Stem)

Right_Thalamus_Proper <- lmer(Right_Thalamus_Proper ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(Right_Thalamus_Proper)

TotalGrayVol <- lmer(TotalGrayVol ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
effectsize(TotalGrayVol)

}




#Linear_Complex
{
  
  
lh_caudalmiddlefrontal_volume <- lmer(lh_caudalmiddlefrontal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_caudalmiddlefrontal_volume)
  
lh_parsopercularis_volume <- lmer(lh_parsopercularis_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_parsopercularis_volume)

lh_isthmuscingulate_volume <- lmer(lh_isthmuscingulate_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_isthmuscingulate_volume)

lh_pericalcarine_volume <- lmer(lh_pericalcarine_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_pericalcarine_volume)

lh_isthmuscingulate_volume <- lmer(lh_isthmuscingulate_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_isthmuscingulate_volume)

lh_postcentral_volume <- lmer(lh_postcentral_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_postcentral_volume)


lh_precentral_volume <- lmer(lh_precentral_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_precentral_volume)


lh_rostralanteriorcingulate_volume <- lmer(lh_rostralanteriorcingulate_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_rostralanteriorcingulate_volume)


rh_rostralmiddlefrontal_volume <- lmer(rh_rostralmiddlefrontal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_rostralmiddlefrontal_volume)

rh_superiorfrontal_volume <- lmer(rh_superiorfrontal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_superiorfrontal_volume)



rh_lingual_volume <- lmer(rh_lingual_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_lingual_volume)


rh_paracentral_volume <- lmer(rh_paracentral_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_paracentral_volume)


rh_pericalcarine_volume <- lmer(rh_pericalcarine_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_pericalcarine_volume)


rh_precentral_volume <- lmer(rh_precentral_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_precentral_volume)

rh_precuneus_volume <- lmer(rh_precuneus_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_precuneus_volume)


rh_superiorfrontal_volume <- lmer(rh_superiorfrontal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_superiorfrontal_volume)


lh_superiorfrontal_volume <- lmer(lh_superiorfrontal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_superiorfrontal_volume)


rh_superiorfrontal_volume <- lmer(rh_superiorfrontal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_superiorfrontal_volume)



rh_superiortemporal_volume <- lmer(rh_superiortemporal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_superiortemporal_volume)

rh_transversetemporal_volume <- lmer(rh_transversetemporal_volume ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_transversetemporal_volume)


Brain_Stem <- lmer(Brain_Stem ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(Brain_Stem)







Left_Amygdala <- lmer(Left_Amygdala ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(Left_Amygdala)

Right_Amygdala <- lmer(Right_Amygdala ~ bully_victim*age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(Right_Amygdala)
}

#Linear_Simple
{
  lh_temporalpole_volume <- lmer(lh_temporalpole_volume ~ bully_victim + age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(lh_temporalpole_volume)

rh_posteriorcingulate_volume <- lmer(rh_posteriorcingulate_volume ~ bully_victim + age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_posteriorcingulate_volume)

rh_rostralmiddlefrontal_volume <- lmer(rh_rostralmiddlefrontal_volume ~ bully_victim + age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_rostralmiddlefrontal_volume)

rh_superiorparietal_volume <- lmer(rh_superiorparietal_volume ~ bully_victim + age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_superiorparietal_volume)

Right_Thalamus_Proper <- lmer(Right_Thalamus_Proper ~ bully_victim + age + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(Right_Thalamus_Proper)

}


#Cohen's D
# Extract the coefficient and standard error for 'bully_victim'
coef_bully_victim <- fixef(TotalGrayVol)["bully_victim"]
se_bully_victim <- sqrt(diag(vcov(TotalGrayVol))["bully_victim"])

# Approximate the standard deviation of the dependent variable
# We use the residual standard Left_VentralDC as a proxy for the predictor's standard deviation
sigma_residual <- sigma(TotalGrayVol)

# Calculate Cohen's d
cohens_d <- coef_bully_victim / sigma_residual

print(cohens_d)


# Extract the coefficient and standard error for 'bully_victim:age' interaction
coef_interaction <- fixef(Right_Pallidum)["bully_victim:age"]
se_interaction <- sqrt(diag(vcov(Right_Pallidum))["bully_victim:age"])

# Approximate the standard deviation of the dependent variable
sigma_residual <- sigma(Right_Pallidum)

# Calculate Cohen's d for the interaction term
cohens_d_interaction <- coef_interaction / sigma_residual

print(cohens_d_interaction)

effectsize(Right_VentralDC)



#MODELS_TO_VISUALISE_
#Quad_Complex
{
  lh_bankssts_volume <- lmer(lh_bankssts_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  summary(lh_bankssts_volume)
  
  lh_bankssts_volume <- lmer(lh_bankssts_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_caudalanteriorcingulate_volume <- lmer(lh_caudalanteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_caudalmiddlefrontal_volume <- lmer(lh_caudalmiddlefrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_cuneus_volume <- lmer(lh_cuneus_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_entorhinal_volume <- lmer(lh_entorhinal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_fusiform_volume <- lmer(lh_fusiform_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_inferiorparietal_volume <- lmer(lh_inferiorparietal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_inferiortemporal_volume <- lmer(lh_inferiortemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_isthmuscingulate_volume <- lmer(lh_isthmuscingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_lateraloccipital_volume <- lmer(lh_lateraloccipital_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_lateralorbitofrontal_volume <- lmer(lh_lateralorbitofrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_lingual_volume <- lmer(lh_lingual_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_medialorbitofrontal_volume <- lmer(lh_medialorbitofrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_middletemporal_volume <- lmer(lh_middletemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_parahippocampal_volume <- lmer(lh_parahippocampal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_paracentral_volume <- lmer(lh_paracentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_parsopercularis_volume <- lmer(lh_parsopercularis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_parsorbitalis_volume <- lmer(lh_parsorbitalis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_parstriangularis_volume <- lmer(lh_parstriangularis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_pericalcarine_volume <- lmer(lh_pericalcarine_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_postcentral_volume <- lmer(lh_postcentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_posteriorcingulate_volume <- lmer(lh_posteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_precentral_volume <- lmer(lh_precentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_precuneus_volume <- lmer(lh_precuneus_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_rostralanteriorcingulate_volume <- lmer(lh_rostralanteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_rostralmiddlefrontal_volume <- lmer(lh_rostralmiddlefrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_superiorfrontal_volume <- lmer(lh_superiorfrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_superiorparietal_volume <- lmer(lh_superiorparietal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_superiortemporal_volume <- lmer(lh_superiortemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_supramarginal_volume <- lmer(lh_supramarginal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_frontalpole_volume <- lmer(lh_frontalpole_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_temporalpole_volume <- lmer(lh_temporalpole_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_transversetemporal_volume <- lmer(lh_transversetemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  lh_insula_volume <- lmer(lh_insula_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_bankssts_volume <- lmer(rh_bankssts_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_caudalanteriorcingulate_volume <- lmer(rh_caudalanteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_caudalmiddlefrontal_volume <- lmer(rh_caudalmiddlefrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_cuneus_volume <- lmer(rh_cuneus_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_entorhinal_volume <- lmer(rh_entorhinal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_fusiform_volume <- lmer(rh_fusiform_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_inferiorparietal_volume <- lmer(rh_inferiorparietal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_inferiortemporal_volume <- lmer(rh_inferiortemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_isthmuscingulate_volume <- lmer(rh_isthmuscingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_lateraloccipital_volume <- lmer(rh_lateraloccipital_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_lateralorbitofrontal_volume <- lmer(rh_lateralorbitofrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_lingual_volume <- lmer(rh_lingual_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_medialorbitofrontal_volume <- lmer(rh_medialorbitofrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_middletemporal_volume <- lmer(rh_middletemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_parahippocampal_volume <- lmer(rh_parahippocampal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_paracentral_volume <- lmer(rh_paracentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_parsopercularis_volume <- lmer(rh_parsopercularis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_parsorbitalis_volume <- lmer(rh_parsorbitalis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_parstriangularis_volume <- lmer(rh_parstriangularis_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_pericalcarine_volume <- lmer(rh_pericalcarine_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_postcentral_volume <- lmer(rh_postcentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_posteriorcingulate_volume <- lmer(rh_posteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_precentral_volume <- lmer(rh_precentral_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_precuneus_volume <- lmer(rh_precuneus_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_rostralanteriorcingulate_volume <- lmer(rh_rostralanteriorcingulate_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_rostralmiddlefrontal_volume <- lmer(rh_rostralmiddlefrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_superiorfrontal_volume <- lmer(rh_superiorfrontal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_superiorparietal_volume <- lmer(rh_superiorparietal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_superiortemporal_volume <- lmer(rh_superiortemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_supramarginal_volume <- lmer(rh_supramarginal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_frontalpole_volume <- lmer(rh_frontalpole_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_temporalpole_volume <- lmer(rh_temporalpole_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_transversetemporal_volume <- lmer(rh_transversetemporal_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  rh_insula_volume <- lmer(rh_insula_volume ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Cerebellum_Cortex <- lmer(Left_Cerebellum_Cortex ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Thalamus_Proper <- lmer(Left_Thalamus_Proper ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Caudate <- lmer(Left_Caudate ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Putamen <- lmer(Left_Putamen ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Pallidum <- lmer(Left_Pallidum ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Brain_Stem <- lmer(Brain_Stem ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Hippocampus <- lmer(Left_Hippocampus ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Amygdala <- lmer(Left_Amygdala ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_Accumbens_area <- lmer(Left_Accumbens_area ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Left_VentralDC <- lmer(Left_VentralDC ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Cerebellum_Cortex <- lmer(Right_Cerebellum_Cortex ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Thalamus_Proper <- lmer(Right_Thalamus_Proper ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Caudate <- lmer(Right_Caudate ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Putamen <- lmer(Right_Putamen ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Pallidum <- lmer(Right_Pallidum ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Hippocampus <- lmer(Right_Hippocampus ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Amygdala <- lmer(Right_Amygdala ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_Accumbens_area <- lmer(Right_Accumbens_area ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  Right_VentralDC <- lmer(Right_VentralDC ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  TotalGrayVol <- lmer(TotalGrayVol ~ bully_victim + age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "bobyqa"))
  
  
  
  #Quad_simple
  
#Linear_complex
  
#Linear_simple

# Predicting all dependent variables (DVs) and storing predictions in newdata
  newdata$predicted_lh_bankssts_volume <- predict(lh_bankssts_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_caudalanteriorcingulate_volume <- predict(lh_caudalanteriorcingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_caudalmiddlefrontal_volume <- predict(lh_caudalmiddlefrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_cuneus_volume <- predict(lh_cuneus_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_entorhinal_volume <- predict(lh_entorhinal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_fusiform_volume <- predict(lh_fusiform_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_inferiorparietal_volume <- predict(lh_inferiorparietal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_inferiortemporal_volume <- predict(lh_inferiortemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_isthmuscingulate_volume <- predict(lh_isthmuscingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_lateraloccipital_volume <- predict(lh_lateraloccipital_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_lateralorbitofrontal_volume <- predict(lh_lateralorbitofrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_lingual_volume <- predict(lh_lingual_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_medialorbitofrontal_volume <- predict(lh_medialorbitofrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_middletemporal_volume <- predict(lh_middletemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_parahippocampal_volume <- predict(lh_parahippocampal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_paracentral_volume <- predict(lh_paracentral_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_parsopercularis_volume <- predict(lh_parsopercularis_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_parsorbitalis_volume <- predict(lh_parsorbitalis_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_parstriangularis_volume <- predict(lh_parstriangularis_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_pericalcarine_volume <- predict(lh_pericalcarine_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_postcentral_volume <- predict(lh_postcentral_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_posteriorcingulate_volume <- predict(lh_posteriorcingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_precentral_volume <- predict(lh_precentral_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_precuneus_volume <- predict(lh_precuneus_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_rostralanteriorcingulate_volume <- predict(lh_rostralanteriorcingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_rostralmiddlefrontal_volume <- predict(lh_rostralmiddlefrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_superiorfrontal_volume <- predict(lh_superiorfrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_superiorparietal_volume <- predict(lh_superiorparietal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_superiortemporal_volume <- predict(lh_superiortemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_supramarginal_volume <- predict(lh_supramarginal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_frontalpole_volume <- predict(lh_frontalpole_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_temporalpole_volume <- predict(lh_temporalpole_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_transversetemporal_volume <- predict(lh_transversetemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_lh_insula_volume <- predict(lh_insula_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_bankssts_volume <- predict(rh_bankssts_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_caudalanteriorcingulate_volume <- predict(rh_caudalanteriorcingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_caudalmiddlefrontal_volume <- predict(rh_caudalmiddlefrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_cuneus_volume <- predict(rh_cuneus_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_entorhinal_volume <- predict(rh_entorhinal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_fusiform_volume <- predict(rh_fusiform_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_inferiorparietal_volume <- predict(rh_inferiorparietal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_inferiortemporal_volume <- predict(rh_inferiortemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_isthmuscingulate_volume <- predict(rh_isthmuscingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_lateraloccipital_volume <- predict(rh_lateraloccipital_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_lateralorbitofrontal_volume <- predict(rh_lateralorbitofrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_lingual_volume <- predict(rh_lingual_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_medialorbitofrontal_volume <- predict(rh_medialorbitofrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_middletemporal_volume <- predict(rh_middletemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_parahippocampal_volume <- predict(rh_parahippocampal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_paracentral_volume <- predict(rh_paracentral_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_parsopercularis_volume <- predict(rh_parsopercularis_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_parsorbitalis_volume <- predict(rh_parsorbitalis_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_parstriangularis_volume <- predict(rh_parstriangularis_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_pericalcarine_volume <- predict(rh_pericalcarine_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_postcentral_volume <- predict(rh_postcentral_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_posteriorcingulate_volume <- predict(rh_posteriorcingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_precentral_volume <- predict(rh_precentral_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_precuneus_volume <- predict(rh_precuneus_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_rostralanteriorcingulate_volume <- predict(rh_rostralanteriorcingulate_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_rostralmiddlefrontal_volume <- predict(rh_rostralmiddlefrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_superiorfrontal_volume <- predict(rh_superiorfrontal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_superiorparietal_volume <- predict(rh_superiorparietal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_superiortemporal_volume <- predict(rh_superiortemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_supramarginal_volume <- predict(rh_supramarginal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_frontalpole_volume <- predict(rh_frontalpole_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_temporalpole_volume <- predict(rh_temporalpole_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_transversetemporal_volume <- predict(rh_transversetemporal_volume, newdata = newdata, re.form = NA)
  newdata$predicted_rh_insula_volume <- predict(rh_insula_volume, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Cerebellum_Cortex <- predict(Left_Cerebellum_Cortex, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Thalamus_Proper <- predict(Left_Thalamus_Proper, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Caudate <- predict(Left_Caudate, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Putamen <- predict(Left_Putamen, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Pallidum <- predict(Left_Pallidum, newdata = newdata, re.form = NA)
  newdata$predicted_Brain_Stem <- predict(Brain_Stem, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Hippocampus <- predict(Left_Hippocampus, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Amygdala <- predict(Left_Amygdala, newdata = newdata, re.form = NA)
  newdata$predicted_Left_Accumbens_area <- predict(Left_Accumbens_area, newdata = newdata, re.form = NA)
  newdata$predicted_Left_VentralDC <- predict(Left_VentralDC, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Cerebellum_Cortex <- predict(Right_Cerebellum_Cortex, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Thalamus_Proper <- predict(Right_Thalamus_Proper, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Caudate <- predict(Right_Caudate, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Putamen <- predict(Right_Putamen, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Pallidum <- predict(Right_Pallidum, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Hippocampus <- predict(Right_Hippocampus, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Amygdala <- predict(Right_Amygdala, newdata = newdata, re.form = NA)
  newdata$predicted_Right_Accumbens_area <- predict(Right_Accumbens_area, newdata = newdata, re.form = NA)
  newdata$predicted_Right_VentralDC <- predict(Right_VentralDC, newdata = newdata, re.form = NA)
  newdata$predicted_TotalGrayVol <- predict(TotalGrayVol, newdata = newdata, re.form = NA)
  

# Create a vector containing the names of all predicted volumes
  predicted_volumes <- c(
    "predicted_lh_bankssts_volume",
    "predicted_lh_caudalanteriorcingulate_volume",
    "predicted_lh_caudalmiddlefrontal_volume",
    "predicted_lh_cuneus_volume",
    "predicted_lh_entorhinal_volume",
    "predicted_lh_fusiform_volume",
    "predicted_lh_inferiorparietal_volume",
    "predicted_lh_inferiortemporal_volume",
    "predicted_lh_isthmuscingulate_volume",
    "predicted_lh_lateraloccipital_volume",
    "predicted_lh_lateralorbitofrontal_volume",
    "predicted_lh_lingual_volume",
    "predicted_lh_medialorbitofrontal_volume",
    "predicted_lh_middletemporal_volume",
    "predicted_lh_parahippocampal_volume",
    "predicted_lh_paracentral_volume",
    "predicted_lh_parsopercularis_volume",
    "predicted_lh_parsorbitalis_volume",
    "predicted_lh_parstriangularis_volume",
    "predicted_lh_pericalcarine_volume",
    "predicted_lh_postcentral_volume",
    "predicted_lh_posteriorcingulate_volume",
    "predicted_lh_precentral_volume",
    "predicted_lh_precuneus_volume",
    "predicted_lh_rostralanteriorcingulate_volume",
    "predicted_lh_rostralmiddlefrontal_volume",
    "predicted_lh_superiorfrontal_volume",
    "predicted_lh_superiorparietal_volume",
    "predicted_lh_superiortemporal_volume",
    "predicted_lh_supramarginal_volume",
    "predicted_lh_frontalpole_volume",
    "predicted_lh_temporalpole_volume",
    "predicted_lh_transversetemporal_volume",
    "predicted_lh_insula_volume",
    "predicted_rh_bankssts_volume",
    "predicted_rh_caudalanteriorcingulate_volume",
    "predicted_rh_caudalmiddlefrontal_volume",
    "predicted_rh_cuneus_volume",
    "predicted_rh_entorhinal_volume",
    "predicted_rh_fusiform_volume",
    "predicted_rh_inferiorparietal_volume",
    "predicted_rh_inferiortemporal_volume",
    "predicted_rh_isthmuscingulate_volume",
    "predicted_rh_lateraloccipital_volume",
    "predicted_rh_lateralorbitofrontal_volume",
    "predicted_rh_lingual_volume",
    "predicted_rh_medialorbitofrontal_volume",
    "predicted_rh_middletemporal_volume",
    "predicted_rh_parahippocampal_volume",
    "predicted_rh_paracentral_volume",
    "predicted_rh_parsopercularis_volume",
    "predicted_rh_parsorbitalis_volume",
    "predicted_rh_parstriangularis_volume",
    "predicted_rh_pericalcarine_volume",
    "predicted_rh_postcentral_volume",
    "predicted_rh_posteriorcingulate_volume",
    "predicted_rh_precentral_volume",
    "predicted_rh_precuneus_volume",
    "predicted_rh_rostralanteriorcingulate_volume",
    "predicted_rh_rostralmiddlefrontal_volume",
    "predicted_rh_superiorfrontal_volume",
    "predicted_rh_superiorparietal_volume",
    "predicted_rh_superiortemporal_volume",
    "predicted_rh_supramarginal_volume",
    "predicted_rh_frontalpole_volume",
    "predicted_rh_temporalpole_volume",
    "predicted_rh_transversetemporal_volume",
    "predicted_rh_insula_volume",
    "predicted_Left_Cerebellum_Cortex",
    "predicted_Left_Thalamus_Proper",
    "predicted_Left_Caudate",
    "predicted_Left_Putamen",
    "predicted_Left_Pallidum",
    "predicted_Brain_Stem",
    "predicted_Left_Hippocampus",
    "predicted_Left_Amygdala",
    "predicted_Left_Accumbens_area",
    "predicted_Left_VentralDC",
    "predicted_Right_Cerebellum_Cortex",
    "predicted_Right_Thalamus_Proper",
    "predicted_Right_Caudate",
    "predicted_Right_Putamen",
    "predicted_Right_Pallidum",
    "predicted_Right_Hippocampus",
    "predicted_Right_Amygdala",
    "predicted_Right_Accumbens_area",
    "predicted_Right_VentralDC",
    "predicted_TotalGrayVol"
  )
  
}


#Figure Loop
{
# Create plots for each predicted volume and save them to the desktop
for (volume in predicted_volumes) {
  # Create plot
  p <- ggplot(newdata, aes(x = age, y = !!sym(volume), color = as.factor(bully_victim))) +
    geom_line(size = 1.5, alpha = 0.7) +  
    scale_color_manual(values = c("red", "green", "blue"), 
                       labels = c("25th percentile", "Mean", "75th percentile"),
                       name = "Bully Victim Percentiles") +
    labs(title = paste("Trajectory of", volume, "by Bully Victim Percentiles"),
         x = "Age",
         y = "Volume mm^3") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(reverse = TRUE))
  
  # Save plot to desktop with appropriate filename
  ggsave(filename = paste("~/Desktop/IMAGEN/results_graphs/Quad_simple/Graph_", volume, ".png", sep = ""), plot = p, dpi = 400, width = 8, height = 6, units = "in")
}

}

#Figure
# Find the minimum age
min_age <- min(IMAGEN_bully$age_years, na.rm = TRUE)

# Remove the youngest participant
IMAGEN_bully <- IMAGEN_bully[IMAGEN_bully$age_years != min_age, ]

# Check the dataset after removal
head(IMAGEN_bully)



#Quad

# Loop through brain volumes
for (volume in DVlist) {
  # Plot the data with quadratic lines for each group
  p <- ggplot(data = IMAGEN_bully, aes_string(x = "age_years", y = volume, color = "bully_group")) +
    geom_point(alpha = 0.1) + # Scatter plot
    geom_smooth(aes(group = bully_group), method = "lm", formula = y ~ poly(x, 2), se = FALSE) + # Add quadratic regression lines
    scale_color_manual(values = colors) + # Assign colors to groups
    labs(title = paste("Trajectory of", volume, "by Bully Victim Percentiles"),
         x = "Age",
         y = "Volume mm^3") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(reverse = TRUE))
  # Save the plot
  ggsave(filename = paste("~/Desktop/IMAGEN/results_graphs/Quad/Graph_lm_", volume, ".png", sep = ""), 
         plot = p, dpi = 400, width = 6, height = 6, units = "in")
}

#Linear

# Loop through brain volumes
for (volume in DVlist) {
  # Plot the data with linear regression lines for each group
  p <- ggplot(data = IMAGEN_bully, aes_string(x = "age_years", y = volume, color = "bully_group")) +
    geom_point(alpha = 0.1) + # Scatter plot
    geom_smooth(aes(group = bully_group), method = "lm", se = FALSE) + # Add linear regression lines
    scale_color_manual(values = colors) + # Assign colors to groups
    labs(title = paste("Trajectory of", volume, "by Bully Victim Percentiles"),
         x = "Age",
         y = "Volume mm^3") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),  
      legend.position = "bottom"
    ) +
    guides(color = guide_legend(reverse = TRUE))
  # Save the plot
  ggsave(filename = paste("~/Desktop/IMAGEN/results_graphs/Linear/Graph_lm_", volume, ".png", sep = ""), 
         plot = p, dpi = 400, width = 6, height = 6, units = "in")
}






# Plot the data with quadratic lines for each group
ggplot(data = IMAGEN_bully, aes(x = age_years, y = Left_Amygdala, color = bully_group)) +
  geom_point(alpha = 0.1) + # Scatter plot
  geom_smooth(aes(group = bully_group), method = "lm", se = FALSE) + # Add quadratic regression lines
  scale_color_manual(values = colors) + # Assign colors to groups
  labs(x = "Age", y = "Brain Volume (lh_cuneus)", color = "Bullying Score") + # Set axis labels
  theme_minimal() # Use a minimal theme









#Sex_Figures
{
# Calculate percentiles
p25_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.25, na.rm = TRUE)
p75_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.75, na.rm = TRUE)

# Categorize bully_victim variable into two groups based on percentiles
IMAGEN_bully$bully_victim_group <- ifelse(IMAGEN_bully$bully_victim < p25_bully_victim, "Low", "High")
}
#Quad_simple
lh_lateralorbitofrontal_volume <- lmer(lh_lateralorbitofrontal_volume ~ bully_victim + age + I(age^2) + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
summary(lh_lateralorbitofrontal_volume)
effectsize(lh_lateralorbitofrontal_volume)

lh_precuneus_volume <- lmer(lh_precuneus_volume ~ bully_victim + age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
lh_superiorparietal_volume <- lmer(lh_superiorparietal_volume ~ bully_victim + age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_isthmuscingulate_volume <- lmer(rh_isthmuscingulate_volume ~ bully_victim + age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_postcentral_volume <- lmer(rh_postcentral_volume ~ bully_victim + age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

#Quad_complex
lh_parahippocampal_volume <- lmer(lh_parahippocampal_volume ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_parahippocampal_volume <- lmer(rh_parahippocampal_volume ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
effectsize(rh_parahippocampal_volume)

lh_transversetemporal_volume <- lmer(lh_transversetemporal_volume ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_bankssts_volume <- lmer(rh_bankssts_volume ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
Left_Accumbens_area <- lmer(Left_Accumbens_area ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
Left_VentralDC <- lmer(Left_VentralDC ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
Right_VentralDC <- lmer(Right_VentralDC ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
Right_Putamen <- lmer(Right_Putamen ~ bully_victim * age + I(age^2) + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

#Linear_complex
lh_caudalmiddlefrontal_volume <- lmer(lh_caudalmiddlefrontal_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
lh_isthmuscingulate_volume <- lmer(lh_isthmuscingulate_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
lh_pericalcarine_volume <- lmer(lh_pericalcarine_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
lh_postcentral_volume <- lmer(lh_postcentral_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_paracentral_volume <- lmer(rh_paracentral_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_precentral_volume <- lmer(rh_precentral_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_precuneus_volume <- lmer(rh_precuneus_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_transversetemporal_volume <- lmer(rh_transversetemporal_volume ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
Brain_Stem <- lmer(Brain_Stem ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
Right_Amygdala <- lmer(Right_Amygdala ~ bully_victim * age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

#Linear_linear
lh_inferiorparietal_volume <- lmer(lh_inferiorparietal_volume ~ bully_victim + age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
lh_lingual_volume <- lmer(lh_lingual_volume ~ bully_victim + age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_caudalmiddlefrontal_volume <- lmer(rh_caudalmiddlefrontal_volume ~ bully_victim + age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))
rh_superiorparietal_volume <- lmer(rh_superiorparietal_volume ~ bully_victim + age + sex*bully_victim + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

{
ggpredict(lh_precuneus_volume, terms = c("bully_victim[all]", "sex")) %>% 
  plot() +
  scale_x_continuous(name="Months Post Baseline", breaks = seq(0, 20, 2)) +
  geom_point(data=IMAGEN_bully, aes(x = bully_victim, y =  lh_precuneus_volume, group = sex, color = bully_victim_group), alpha = 0.1, inherit.aes = F) + 
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Lateral Orbitofrontal Volume") + 
  theme_minimal() 
}

{

ggpredict(lh_lateralorbitofrontal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name="Months Post Baseline", breaks = seq(0, 20, 2)) +
  geom_point(data=IMAGEN_bully, aes(x = bully_victim, y =  lh_lateralorbitofrontal_volume, group = subID, color = sex), inherit.aes = F, alpha = 0.1) + 
  labs(x = "Months post baseline", y = "Volume mm³", title = "Left Lateral Orbitofrontal Cortex") + 
  theme_minimal() 

ggpredict(lh_precuneus_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name="Months Post Baseline", breaks = seq(0, 20, 2)) +
  geom_point(data=IMAGEN_bully, aes(x = bully_victim, y =  lh_precuneus_volume, group = subID, color = sex), inherit.aes = F, alpha = 0.1) + 
  labs(x = "Months post baseline", y = "Volume mm³", title = "Left Precuneus") + 
  theme_minimal() 


ggpredict(lh_superiorparietal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name="Months Post Baseline", breaks = seq(0, 20, 2)) +
  geom_point(data=IMAGEN_bully, aes(x = bully_victim, y =  lh_superiorparietal_volume, group = subID, color = sex), inherit.aes = F, alpha = 0.1) + 
  labs(x = "Months post baseline", y = "Volume mm³", title = "Left Superior Parietal") + 
  theme_minimal() 

ggpredict(rh_isthmuscingulate_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name="Months Post Baseline", breaks = seq(0, 20, 2)) +
  geom_point(data=IMAGEN_bully, aes(x = bully_victim, y =  rh_isthmuscingulate_volume, group = subID, color = sex), inherit.aes = F, alpha = 0.1) + 
  labs(x = "Months post baseline", y = "Volume mm³", title = "Right Isthmus Cingulate") + 
  theme_minimal() 

ggpredict(rh_postcentral_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name="Months Post Baseline", breaks = seq(0, 20, 2)) +
  geom_point(data=IMAGEN_bully, aes(x = bully_victim, y =  rh_postcentral_volume, group = subID, color = sex), inherit.aes = F, alpha = 0.1) + 
  labs(x = "Months post baseline", y = "Volume mm³", title = "Right Post Cental") + 
  theme_minimal() 
}

# Load necessary libraries
library(ggplot2)
library(patchwork)
#Quad_simple
{
# Generate predictions and plots for each brain region
plot1 <- ggpredict(lh_lateralorbitofrontal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_lateralorbitofrontal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
  labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Lateral Orbitofrontal Cortex") + 
  theme_minimal()

plot2 <- ggpredict(lh_precuneus_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_precuneus_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
  labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Precuneus") + 
  theme_minimal()

plot3 <- ggpredict(lh_superiorparietal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_superiorparietal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
  labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Superior Parietal") + 
  theme_minimal()

plot4 <- ggpredict(rh_isthmuscingulate_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_isthmuscingulate_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
  labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Isthmus Cingulate") + 
  theme_minimal()

plot5 <- ggpredict(rh_postcentral_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_postcentral_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
  labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Post Central") + 
  theme_minimal()

# Combine all plots into one figure
combined_plots_Quad_simple <- plot1 + plot2 + plot3 + plot4 + plot5

# Print the combined figure
print(plot1)

# Save the combined figure with 400 DPI
ggsave("combined_plots_Quad_simple.png", combined_plots_Quad_simple, dpi = 400)
}

#Quad_complex
{
  # Generate predictions and plots for each brain region
  plot6 <- ggpredict(lh_parahippocampal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_parahippocampal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Parahippocampal") + 
    theme_minimal()
  
  plot7 <- ggpredict(rh_parahippocampal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_parahippocampal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Parahippocampal") + 
    theme_minimal()
  
  plot8 <- ggpredict(lh_transversetemporal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_transversetemporal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Transverse Temporal") + 
    theme_minimal()
  
  plot9 <- ggpredict(rh_bankssts_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_bankssts_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Bankssts") + 
    theme_minimal()
  
  plot10 <- ggpredict(Left_Accumbens_area, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = Left_Accumbens_area, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Accumbens") + 
    theme_minimal()
  
  
  plot11 <- ggpredict(Left_VentralDC, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = Left_VentralDC, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Ventral DC") + 
    theme_minimal()
  
  
  plot12 <- ggpredict(Right_VentralDC, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = Right_VentralDC, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Ventral DC") + 
    theme_minimal()
  
  plot13 <- ggpredict(Right_Putamen, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = Right_Putamen, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Putamen") + 
    theme_minimal()
  
  # Combine all plots into one figure
  combined_plots_Quad_complex <- plot6 + plot7 + plot8 + plot9 + plot10 + plot11 + plot12 + plot13
  
  # Print the combined figure
  print(plot1)
  
  # Save the combined figure with 400 DPI
  ggsave("combined_plots_Quad_complex", combined_plots_Quad_simple, dpi = 400)
}


ggpredict(rh_parahippocampal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
  plot() +
  scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_parahippocampal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
  labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Parahippocampal") + 
  theme_minimal() +
  scale_color_manual(values = c("female" = "blue", "male" = "red"))  # Specify the correct colors for females and males


#Linear_simple
{
  # Generate predictions and plots for each brain region
  plot14 <- ggpredict(lh_inferiorparietal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_inferiorparietal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Inferior Parietal") + 
    theme_minimal()
  
  plot15 <- ggpredict(lh_lingual_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_lingual_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Lingual") + 
    theme_minimal()
  
  plot16 <- ggpredict(rh_caudalmiddlefrontal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_caudalmiddlefrontal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Caudal Middle Frontal") + 
    theme_minimal()
  
  plot17 <- ggpredict(rh_superiorparietal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_superiorparietal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Superior Parietal") + 
    theme_minimal()
  
  # Combine all plots into one figure
  combined_plots_L_simple <- plot14 + plot15 + plot16 + plot17
  
  # Print the combined figure
  print(plot1)
  
  # Save the combined figure with 400 DPI
  ggsave("combined_plots_L_simple.png", combined_plots_L_simple, dpi = 400)
}

#Linear_complex
{
  # Generate predictions and plots for each brain region
  plot18 <- ggpredict(lh_caudalmiddlefrontal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_caudalmiddlefrontal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Caudal Middle Frontal") + 
    theme_minimal()
  
  plot19 <- ggpredict(lh_isthmuscingulate_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_isthmuscingulate_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Isthmus Cingulate") + 
    theme_minimal()
  
  plot20 <- ggpredict(lh_pericalcarine_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_pericalcarine_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Pericalarine") + 
    theme_minimal()
  
  plot21 <- ggpredict(lh_postcentral_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = lh_postcentral_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Left Postcentral") + 
    theme_minimal()
  
  plot22 <- ggpredict(rh_paracentral_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_paracentral_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Paracentral") + 
    theme_minimal()
  
  plot23 <- ggpredict(rh_precentral_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_precentral_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Precentral") + 
    theme_minimal()
  
  plot24 <- ggpredict(rh_precuneus_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_precuneus_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Precuneus") + 
    theme_minimal()
  
  plot25 <- ggpredict(rh_transversetemporal_volume, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = rh_transversetemporal_volume, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Transverse Temporal") + 
    theme_minimal()
  
  plot26 <- ggpredict(Brain_Stem, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = Brain_Stem, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Brain Stem") + 
    theme_minimal()
  
  plot27 <- ggpredict(Right_Amygdala, terms = c("bully_victim[all]", "sex[all]")) %>% 
    plot() +
    scale_x_continuous(name = "OB/VQ Score", breaks = seq(0, 20, 4)) +
    geom_point(data = IMAGEN_bully, aes(x = bully_victim, y = Right_Amygdala, group = subID, color = sex), inherit.aes = FALSE, alpha = 0.1) + 
    labs(x = "OB/VQ Score", y = "Volume mm³", title = "Right Amygdala") + 
    theme_minimal()
  
  
  # Combine all plots into one figure
  combined_plots_L_complex <- plot18 + plot19 + plot20 + plot21 + plot22 + plot23 + plot24 + plot25 + plot26 + plot27
  
  # Print the combined figure
  print(plot7)
  
  # Save the combined figure with 400 DPI
  ggsave("combined_plots_L_complex.png", combined_plots_L_complex, dpi = 400)
}




{

  
  # Calculate quartiles separately for males and females
  male_bully <- IMAGEN_bully$bully_victim[IMAGEN_bully$sex == "male"]
  female_bully <- IMAGEN_bully$bully_victim[IMAGEN_bully$sex == "female"]
  
  p25_male <- quantile(male_bully, 0.25, na.rm = TRUE) # 25th percentile for males
  p75_male <- quantile(male_bully, 0.75, na.rm = TRUE) # 75th percentile for males
  p25_female <- quantile(female_bully, 0.25, na.rm = TRUE) # 25th percentile for females
  p75_female <- quantile(female_bully, 0.75, na.rm = TRUE) # 75th percentile for females
  
  # Create a factor variable for grouping the bullying scores into four groups based on gender and quartiles
  IMAGEN_bully$bully_victim_group <- ifelse(IMAGEN_bully$sex == "male",
                                            ifelse(IMAGEN_bully$bully_victim <= p25_male, "male_low",
                                                   ifelse(IMAGEN_bully$bully_victim <= p75_male, "male_high", NA)),
                                            ifelse(IMAGEN_bully$bully_victim <= p25_female, "female_low",
                                                   ifelse(IMAGEN_bully$bully_victim <= p75_female, "female_high", NA)))
  
  IMAGEN_bully$bully_victim_group <- factor(IMAGEN_bully$bully_victim_group,
                                            levels = c("male_low", "male_high", "female_low", "female_high"),
                                            labels = c("Male Low", "Male High", "Female Low", "Female High"))
  
  IMAGEN_bully <- IMAGEN_bully[!is.na(IMAGEN_bully$bully_victim_group), ]
  # Create a color palette for differentiating the groups
  bully_colors <- c("male_low" = "red", "male_high" = "blue", "female_low" = "darkgreen", "female_high" = "purple") # Only two colors for two groups
  
  
   
Left_Accumbens_area <- lmer(Left_Accumbens_area ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Left_Accumbens_area <- ggpredict(Left_Accumbens_area, 
                              terms = c("age_years[all]", "bully_victim_group[all]"))


rh_parahippocampal_volume <- lmer(rh_parahippocampal_volume ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_parahippocampal_volume <- ggpredict(rh_parahippocampal_volume, 
                                                  terms = c("age_years[all]", "bully_victim_group[all]"))

lh_parahippocampal_volume <- lmer(lh_parahippocampal_volume ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_parahippocampal_volume <- ggpredict(lh_parahippocampal_volume, 
                                                        terms = c("age_years[all]", "bully_victim_group[all]"))

Right_Putamen <- lmer(Right_Putamen ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Right_Putamen <- ggpredict(Right_Putamen, 
                                                        terms = c("age_years[all]", "bully_victim_group[all]"))


Right_VentralDC <- lmer(Right_VentralDC ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Right_VentralDC <- ggpredict(Right_VentralDC, 
                                            terms = c("age_years[all]", "bully_victim_group[all]"))

Left_VentralDC <- lmer(Left_VentralDC ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Left_VentralDC <- ggpredict(Left_VentralDC, 
                                              terms = c("age_years[all]", "bully_victim_group[all]"))

Right_Amygdala <- lmer(Right_Amygdala ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Right_Amygdala <- ggpredict(Right_Amygdala, 
                                             terms = c("age_years[all]", "bully_victim_group[all]"))

Brain_Stem <- lmer(Brain_Stem ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Brain_Stem <- ggpredict(Brain_Stem, 
                                             terms = c("age_years[all]", "bully_victim_group[all]"))

rh_bankssts_volume <- lmer(rh_bankssts_volume ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_bankssts_volume <- ggpredict(rh_bankssts_volume, 
                                         terms = c("age_years[all]", "bully_victim_group[all]"))

lh_isthmuscingulate_volume <- lmer(lh_isthmuscingulate_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_isthmuscingulate_volume <- ggpredict(lh_isthmuscingulate_volume, 
                                                 terms = c("age_years[all]", "bully_victim_group[all]"))


lh_caudalmiddlefrontal_volume <- lmer(lh_caudalmiddlefrontal_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_caudalmiddlefrontal_volume <- ggpredict(lh_caudalmiddlefrontal_volume, 
                                                         terms = c("age_years[all]", "bully_victim_group[all]"))

rh_precuneus_volume <- lmer(rh_precuneus_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_precuneus_volume<- ggpredict(rh_precuneus_volume, 
                                                            terms = c("age_years[all]", "bully_victim_group[all]"))

rh_transversetemporal_volume <- lmer(rh_transversetemporal_volume ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_transversetemporal_volume<- ggpredict(rh_transversetemporal_volume, 
                                                 terms = c("age_years[all]", "bully_victim_group[all]"))


lh_pericalcarine_volume <- lmer(lh_pericalcarine_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_pericalcarine_volume<- ggpredict(lh_pericalcarine_volume, 
                                                 terms = c("age_years[all]", "bully_victim_group[all]"))



lh_postcentral_volume <- lmer(lh_postcentral_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_postcentral_volume<- ggpredict(lh_postcentral_volume, 
                                                     terms = c("age_years[all]", "bully_victim_group[all]"))


Left_Pallidum <- lmer(Left_Pallidum ~ bully_victim_group * age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_Left_Pallidum<- ggpredict(Left_Pallidum, 
                                                   terms = c("age_years[all]", "bully_victim_group[all]"))


rh_paracentral_volume <- lmer(rh_paracentral_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_paracentral_volume<- ggpredict(rh_paracentral_volume, 
                                                   terms = c("age_years[all]", "bully_victim_group[all]"))


lh_superiorparietal_volume <- lmer(lh_superiorparietal_volume ~ bully_victim_group + age_years + sex*bully_victim_group + I(age_years^2) + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_superiorparietal_volume<- ggpredict(lh_superiorparietal_volume, 
                                                   terms = c("age_years[all]", "bully_victim_group[all]"))


rh_superiorparietal_volume <- lmer(rh_superiorparietal_volume ~ bully_victim_group + age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_superiorparietal_volume<- ggpredict(rh_superiorparietal_volume, 
                                                   terms = c("age_years[all]", "bully_victim_group[all]"))

lh_inferiorparietal_volume <- lmer(lh_inferiorparietal_volume ~ bully_victim_group + age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_lh_inferiorparietal_volume<- ggpredict(lh_inferiorparietal_volume, 
                                                        terms = c("age_years[all]", "bully_victim_group[all]"))

rh_isthmuscingulate_volume <- lmer(rh_isthmuscingulate_volume ~ bully_victim_group + age_years + I(age_years^2) + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))

predicted_values_rh_isthmuscingulate_volume<- ggpredict(rh_isthmuscingulate_volume, 
                                                          terms = c("age_years[all]", "bully_victim_group[all]"))



rh_caudalmiddlefrontal_volume <- lmer(rh_caudalmiddlefrontal_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))


predicted_values_rh_caudalmiddlefrontal_volume<- ggpredict(rh_caudalmiddlefrontal_volume, 
                                                        terms = c("age_years[all]", "bully_victim_group[all]"))


rh_precentral_volume <- lmer(rh_precentral_volume ~ bully_victim_group * age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))


predicted_values_rh_precentral_volume<- ggpredict(rh_precentral_volume, 
                                                           terms = c("age_years[all]", "bully_victim_group[all]"))

lh_lingual_volume <- lmer(lh_lingual_volume ~ bully_victim_group + age_years + sex*bully_victim_group + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1|subID) + (1|scan_site), data = IMAGEN_bully, REML = TRUE, control = lmerControl(optimizer = "nloptwrap"))


predicted_values_lh_lingual_volume<- ggpredict(lh_lingual_volume, 
                                                  terms = c("age_years[all]", "bully_victim_group[all]"))






#Bully sex graphs
plot18 <- plot(predicted_values_Left_Accumbens_area) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = Left_Accumbens_area, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Accumbens") + 
  coord_cartesian(ylim = c(400, 1100)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente

# Plot predicted values with smoother prediction lines
plot19 <- plot(predicted_values_rh_parahippocampal_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_parahippocampal_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Parahippocampal Gyrus") + 
  coord_cartesian(ylim = c(1800, 2800)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente


# Plot predicted values with smoother prediction lines
plot20 <- plot(predicted_values_lh_parahippocampal_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = lh_parahippocampal_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Parahippocampal Gyrus") + 
  coord_cartesian(ylim = c(1800, 3000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente




# Plot predicted values with smoother prediction lines
plot21 <- plot(predicted_values_Right_Putamen) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = Right_Putamen, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Putamen") + 
  coord_cartesian(ylim = c(4000, 8000)) +  # Adjust the y-axis limits to zoom in on the range of interest

  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente



# Plot predicted values with smoother prediction lines
plot22 <- plot(predicted_values_Right_VentralDC) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = Right_VentralDC, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Ventral Dienchephalon") + 
  theme_minimal() +  # Use minimal theme
  coord_cartesian(ylim = c(3000, 5000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente

plot23 <- plot(predicted_values_Left_VentralDC) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = Left_VentralDC, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Ventral Dienchephalon") + 
  coord_cartesian(ylim = c(2750, 5250)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente



plot24 <- plot(predicted_values_Right_Amygdala) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = Right_Amygdala, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Amygdala") + 
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente


plot27 <- plot(predicted_values_rh_bankssts_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_bankssts_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Bankssts") + 
  coord_cartesian(ylim = c(1500, 4000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente

plot28 <- plot(predicted_values_rh_isthmuscingulate_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_isthmuscingulate_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Isthmus Cingulate") + 
  coord_cartesian(ylim = c(1750, 4000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente

plot29 <- plot(predicted_values_rh_caudalmiddlefrontal_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_caudalmiddlefrontal_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Caudal Middle Frontal") + 
  coord_cartesian(ylim = c(4000, 9000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente


plot30 <- plot(predicted_values_rh_precuneus_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_precuneus_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Precuneus") + 
  coord_cartesian(ylim = c(7000, 15000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente




plot33 <- plot(predicted_values_lh_postcentral_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = lh_postcentral_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Post Central Gyrus") + 
  coord_cartesian(ylim = c(7000, 14000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente


plot34 <- plot(predicted_values_rh_paracentral_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_paracentral_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Paracental Gyrus") + 
  coord_cartesian(ylim = c(3000, 6000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente



plot35 <- plot(predicted_values_lh_inferiorparietal_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = lh_inferiorparietal_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Inferior Parietal Gyrus") + 
  coord_cartesian(ylim = c(9000, 18000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente


plot36 <- plot(predicted_values_rh_precentral_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_precentral_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Right Precentral Gyrus") + 
  coord_cartesian(ylim = c(10000, 17500)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente



plot37 <- plot(predicted_values_lh_superiorparietal_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_precentral_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Superior Parietal Gyrus") + 
  coord_cartesian(ylim = c(10000, 17500)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente


plot38 <- plot(predicted_values_rh_precuneus_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = rh_precuneus_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Precuneus") + 
  coord_cartesian(ylim = c(7000, 15000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente

plot39 <- plot(predicted_values_lh_lingual_volume) +
  scale_x_continuous(name = "Age (Years)", breaks = seq(0, 24, 4)) +
  geom_point(data = IMAGEN_bully, aes(x = age_years, y = lh_lingual_volume, 
                                      group = subID, color = bully_victim_group), alpha = 0.1, inherit.aes = FALSE) + 
  geom_smooth(method = "gam", se = TRUE, size = 1.5, alpha = 0.3) +  # Increase line size for better visibility
  labs(x = "Bully Victim Score", y = "Volume mm³", title = "Left Lingual Gyrus ") + 
  coord_cartesian(ylim = c(5000, 10000)) +  # Adjust the y-axis limits to zoom in on the range of interest
  theme_minimal() +  # Use minimal theme
  theme(text = element_text(family = "Times New Roman"),  # Set font family
        axis.title = element_text(size = 10),  # Customize axis title
        axis.text = element_text(size = 8),  # Customize axis text
        legend.position = "none",  # Hide the legend
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))# Cente




plot23 <- plot23 + theme(legend.position = "none")
plot22 <- plot22 + theme(legend.position = "none")
plot18 <- plot18 + theme(legend.position = "none")
plot24 <- plot24 + theme(legend.position = "none")
plot20 <- plot20 + theme(legend.position = "none")
plot19 <- plot19 + theme(legend.position = "none")
plot34 <- plot34 + theme(legend.position = "none")
plot27 <- plot27 + theme(legend.position = "none")
plot26 <- plot26 + theme(legend.position = "none")
plot28 <- plot28 + theme(legend.position = "none")
plot29 <- plot29 + theme(legend.position = "none")
plot30 <- plot30 + theme(legend.position = "none")
plot31 <- plot31 + theme(legend.position = "none")
plot32 <- plot32 + theme(legend.position = "none")
plot33 <- plot33 + theme(legend.position = "none")
plot34 <- plot34 + theme(legend.position = "none")
plot35 <- plot35 + theme(legend.position = "none")
plot36 <- plot36 + theme(legend.position = "none")
plot37 <- plot37 + theme(legend.position = "none")
plot38 <- plot38 + theme(legend.position = "none")
plot39 <- plot39 + theme(legend.position = "none")


library(ggplot2)
library(patchwork)

# Combine the plots into a 2-column and 9-row layout and position the legend to the top right
combined_plots_1_4_legend <- plot18 + plot24 + 
  plot27 + plot29 +   plot_layout(guides = 'collect', ncol = 2) & theme(legend.position = "bottom")


combined_plots_5_8 <- plot35 + plot28 +
  plot39 + plot20 + plot_layout(guides = 'collect', ncol = 2) 

combined_plots_9_12 <- plot19 + plot34 + 
  plot33 + plot38 + plot_layout(guides = 'collect', ncol = 2) 
  

ombined_plots_13_19 <- plot30 + plot36 +  
  plot21 + plot37 + 
  plot23 + plot22 + 
  plot_layout(guides = 'collect', ncol = 2) & 

# Save the combined plot with the legend at the top right
ggsave("/Users/michaelconnaughton/Desktop/IMAGEN/IMAGEN_Results/IMAGES/combined_plots_1_4_legend.png", combined_plots_1_4_legend, width = 15, height = 10, dpi = 400)
ggsave("/Users/michaelconnaughton/Desktop/IMAGEN/IMAGEN_Results/IMAGES/combined_plots_5_8.png", combined_plots_5_8, width = 15, height = 10, dpi = 400)
ggsave("/Users/michaelconnaughton/Desktop/IMAGEN/IMAGEN_Results/IMAGES/combined_plots_9_12.png", combined_plots_9_12, width = 15, height = 10, dpi = 400)
ggsave("/Users/michaelconnaughton/Desktop/IMAGEN/IMAGEN_Results/IMAGES/combined_plots_13_19.png", ombined_plots_13_19, width = 15, height = 10, dpi = 400)




}

#Single_Graphs_for_inspection
{

  
  mean_bully_victim <- mean(IMAGEN_bully$bully_victim, na.rm = TRUE)
  p25_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.25, na.rm = TRUE)
  p75_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.75, na.rm = TRUE)
  
  
  newdata <- expand.grid(
    EstimatedTotalIntraCranialVol = mean(IMAGEN_bully$EstimatedTotalIntraCranialVol, na.rm = TRUE),
    sex = factor(levels(IMAGEN_bully$sex)[1]),
    age = seq(min(IMAGEN_bully$age, na.rm = TRUE), max(IMAGEN_bully$age, na.rm = TRUE), length.out = 100),
    SES = mean(IMAGEN_bully$SES, na.rm = TRUE),
    mode_c_pds = mean(IMAGEN_bully$mode_c_pds, na.rm = TRUE),
    bully_victim = c(mean_bully_victim, p25_bully_victim, p75_bully_victim),
    subID = unique(IMAGEN_bully$subID)[1]
  )
  
  mean_bully_victim <- mean(IMAGEN_bully$bully_victim, na.rm = TRUE)
  p25_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.25, na.rm = TRUE) # 25th percentile
  p75_bully_victim <- quantile(IMAGEN_bully$bully_victim, 0.75, na.rm = TRUE) # 75th percentile
  
  # Create a color palette for differentiating the groups
  colors <- c("low" = "blue", "mean" = "green", "high" = "red") # Only two colors for two groups
  
  
  newdata$predicted_Left_Pallidum <- predict(Left_Pallidum, newdata = newdata, re.form = NA)
  predicted_volumes <- c(
    "predicted_Left_Pallidum")

# Your ggplot code
my_plot <- ggplot(newdata, aes(x = age, y = predicted_Left_Pallidum, color = as.factor(bully_victim))) +
  geom_line(size = 1.5, alpha = 0.7) +  # Use solid lines
  scale_color_manual(values = c("red", "green", "blue"), 
                     labels = c("25th percentile", "Mean", "75th percentile"),
                     name = "Bully Victim Percentiles") +
  labs(title = "Trajectory of the Left Pallidum by Bully Victim Percentiles",
       x = "Age",
       y = "Volume (mm^3)") +
{  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman"),  # Center the title and set font family to Times New Roman
    legend.position = "bottom",  # Move legend to bottom
    legend.text = element_text(family = "Times New Roman"),  # Set legend text font family to Times New Roman
    legend.title = element_text(family = "Times New Roman"),  # Set legend title font family to Times New Roman
    axis.title.x = element_text(family = "Times New Roman"),  # Set x-axis label font family to Times New Roman
    axis.title.y = element_text(family = "Times New Roman"),  # Set y-axis label font family to Times New Roman
    axis.text.x = element_text(family = "Times New Roman"),  # Set x-axis tick label font family to Times New Roman
    axis.text.y = element_text(family = "Times New Roman")  # Set y-axis tick label font family to Times New Roman
  ) +
  guides(color = guide_legend(reverse = TRUE))  # Reverse order of legend
}
# Save the plot as an image with 400 dpi
ggsave("Trajectory of Left Pallidum by Bully Victim Percentiles.png", plot = my_plot, dpi = 400, width = 8, height = 6, units = "in")

}


summary(lh_caudalanteriorcingulate_volume)
effectsize(lh_caudalanteriorcingulate_volume)

#LME Model Fitting
{
plot(fitted(lh_inferiorparietal_volume_scale), resid(lh_inferiorparietal_volume_scale))
hist(resid(lh_inferiorparietal_volume))
qqnorm(resid(lh_inferiorparietal_volume))
qqline(resid(lh_inferiorparietal_volume))

plot(fitted(Brain_Stem_volume), abs(resid(Brain_Stem_volume))^0.5)
acf(resid(Brain_Stem_volume))
summary(Brain_Stem_volume)

}


#GAMM

#Data Frames 
{
LL_Null_GAMM <- c();
AIC_Null_GAMM <- c();
BIC_Null_GAMM <- c();

#FFX_Null_Model

# Null Quad MODEL



  
  # Null GAMM MODEL
  Null_GAMM.outcomes <- lapply(DVlist, function(x) {
    formula <- as.formula(paste(x, "~ s(age) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol"))
    mgcv::gamm(formula,
               random = list(subID =~ age, scan_site =~ 1),
               data = IMAGEN_bully, 
               method = "REML")
  })


# Simple GAMM MODEL
Simple_GAMM.outcomes <- lapply(DVlist, function(x) {
  formula <- as.formula(paste(x, "~ s(bully_victim, k = 4) + s(age, k = 4) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol"))
  mgcv::gamm(formula,
             random = list(subID =~ age, scan_site =~ 1),
             data = IMAGEN_bully, 
             method = "REML")
})

# Complex GAMM MODEL
Complex_GAMM.outcomes <- lapply(DVlist, function(x) {
  formula <- as.formula(paste(x, "~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol"))
  mgcv::gamm(formula,
             random = list(subID =~ age, scan_site =~ 1),
             data = IMAGEN_bully, 
             method = "REML")
})     



# LL.FFX.pmat <- c()
for (outcome in 1:3) {
  # extract p-values only
  # do the interaction effect and the PE variable add anything to the null prediction model?
  dif.0.2_FFX_Q <- anova(Null_GAMM.outcomes[[outcome]], Quad_simple.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.0.2_FFX_Q[2,1])
  dif.0.2B_FFX_Q <- anova(Null_GAMM.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.0.2B_FFX_Q[2,1])
  dif.2.2B_FFX_Q <- anova(Quad_simple.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.2.2B_FFX_Q[2,1])
}



for (outcome in 1:88) {
  LL_FFX_simple_Q <- append(LL_FFX_simple_Q, logLik(Quad_simple.outcomes[[outcome]], REML=T))
  AIC_FFX_simple_Q <- append(AIC_FFX_simple_Q, AICc(Quad_simple.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_FFX_simple_Q <- append(BIC_FFX_simple_Q, BICc(Null_Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  
}


# Quad MODEL 1
Quad_complex.outcomes <- lapply(DVlist, function(x) {
  lme4::lmer(substitute(i ~ bully_victim*age + I(age^2) + sex + SES + mode_c_pds + EstimatedTotalIntraCranialVol + (1 + age|subID) + (1|scan_site), list(i = as.name(x))), data=IMAGEN_bully, control = lmerControl(optimizer ="bobyqa"))})


for (outcome in 1:88) {
  LL_FFX_complex_Q <- append(LL_FFX_complex_Q, logLik(Quad_complex.outcomes[[outcome]], REML=T))
  AIC_FFX_complex_Q <- append(AIC_FFX_complex_Q, AICc(Quad_complex.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  BIC_FFX_complex_Q <- append(BIC_FFX_complex_Q, BICc(Null_Quad.outcomes[[outcome]], second.ord = TRUE, refit = TRUE))
  
}



# observe all log likelihoods in a table
LLcompare_Q_FFX <- data.frame(DVlist, LL_FFX_null_Q, LL_FFX_simple_Q, LL_FFX_complex_Q)
AICcompare_Q_FFX <- data.frame(DVlist, AIC_FFX_null_Q, AIC_FFX_simple_Q, AIC_FFX_complex_Q)
BICcompare_Q_FFX <- data.frame(DVlist, BIC_FFX_null_Q, BIC_FFX_simple_Q, BIC_FFX_complex_Q)


compared_FFX_IS_Q <- c("Null_Simple", "Null_Complex", "Simple_Complex")


# LL.FFX.pmat <- c()
for (outcome in 1:88) {
  # extract p-values only
  # do the interaction effect and the PE variable add anything to the null prediction model?
  dif.0.2_FFX_Q <- anova(Null_Quad.outcomes[[outcome]], Quad_simple.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.0.2_FFX_Q[2,1])
  dif.0.2B_FFX_Q <- anova(Null_Quad.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.0.2B_FFX_Q[2,1])
  dif.2.2B_FFX_Q <- anova(Quad_simple.outcomes[[outcome]], Quad_complex.outcomes[[outcome]], refit = TRUE)["Pr(>Chisq)"]
  LL.FFX_Q.pmat <- append(LL.FFX_Q.pmat,dif.2.2B_FFX_Q[2,1])
}
#data frame
{
LL.FFX_Q.pvals <- data.frame(
  compared_FFX_IS_Q,
  "lh_bankssts_volume" = LL.FFX_Q.pmat[1:3],
  "lh_caudalanteriorcingulate_volume" = LL.FFX_Q.pmat[4:6],
  "lh_caudalmiddlefrontal_volume" = LL.FFX_Q.pmat[7:9],
  "lh_cuneus_volume" = LL.FFX_Q.pmat[10:12],
  "lh_entorhinal_volume" = LL.FFX_Q.pmat[13:15],
  "lh_fusiform_volume" = LL.FFX_Q.pmat[16:18],
  "lh_inferiorparietal_volume" = LL.FFX_Q.pmat[19:21],
  "lh_inferiortemporal_volume" = LL.FFX_Q.pmat[22:24],
  "lh_isthmuscingulate_volume" = LL.FFX_Q.pmat[25:27],
  "lh_lateraloccipital_volume" = LL.FFX_Q.pmat[28:30],
  "lh_lateralorbitofrontal_volume" = LL.FFX_Q.pmat[31:33],
  "lh_lingual_volume" = LL.FFX_Q.pmat[34:36],
  "lh_medialorbitofrontal_volume" = LL.FFX_Q.pmat[37:39],
  "lh_middletemporal_volume" = LL.FFX_Q.pmat[40:42],
  "lh_parahippocampal_volume" = LL.FFX_Q.pmat[43:45],
  "lh_paracentral_volume" = LL.FFX_Q.pmat[46:48],
  "lh_parsopercularis_volume" = LL.FFX_Q.pmat[49:51],
  "lh_parsorbitalis_volume" = LL.FFX_Q.pmat[52:54],
  "lh_parstriangularis_volume" = LL.FFX_Q.pmat[55:57],
  "lh_pericalcarine_volume" = LL.FFX_Q.pmat[58:60],
  "lh_postcentral_volume" = LL.FFX_Q.pmat[61:63],
  "lh_posteriorcingulate_volume" = LL.FFX_Q.pmat[64:66],
  "lh_precentral_volume" = LL.FFX_Q.pmat[67:69],
  "lh_precuneus_volume" = LL.FFX_Q.pmat[70:72],
  "lh_rostralanteriorcingulate_volume" = LL.FFX_Q.pmat[73:75],
  "lh_rostralmiddlefrontal_volume" = LL.FFX_Q.pmat[76:78],
  "lh_superiorfrontal_volume" = LL.FFX_Q.pmat[79:81],
  "lh_superiorparietal_volume" = LL.FFX_Q.pmat[82:84],
  "lh_superiortemporal_volume" = LL.FFX_Q.pmat[85:87],
  "lh_supramarginal_volume" = LL.FFX_Q.pmat[88:90],
  "lh_frontalpole_volume" = LL.FFX_Q.pmat[91:93],
  "lh_temporalpole_volume" = LL.FFX_Q.pmat[94:96],
  "lh_transversetemporal_volume" = LL.FFX_Q.pmat[97:99],
  "lh_insula_volume" = LL.FFX_Q.pmat[100:102],
  "rh_bankssts_volume" = LL.FFX_Q.pmat[103:105],
  "rh_caudalanteriorcingulate_volume" = LL.FFX_Q.pmat[106:108],
  "rh_caudalmiddlefrontal_volume" = LL.FFX_Q.pmat[109:111],
  "rh_cuneus_volume" = LL.FFX_Q.pmat[112:114],
  "rh_entorhinal_volume" = LL.FFX_Q.pmat[115:117],
  "rh_fusiform_volume" = LL.FFX_Q.pmat[118:120],
  "rh_inferiorparietal_volume" = LL.FFX_Q.pmat[121:123],
  "rh_inferiortemporal_volume" = LL.FFX_Q.pmat[124:126],
  "rh_isthmuscingulate_volume" = LL.FFX_Q.pmat[127:129],
  "rh_lateraloccipital_volume" = LL.FFX_Q.pmat[130:132],
  "rh_lateralorbitofrontal_volume" = LL.FFX_Q.pmat[133:135],
  "rh_lingual_volume" = LL.FFX_Q.pmat[136:138],
  "rh_medialorbitofrontal_volume" = LL.FFX_Q.pmat[139:141],
  "rh_middletemporal_volume" = LL.FFX_Q.pmat[142:144],
  "rh_parahippocampal_volume" = LL.FFX_Q.pmat[145:147],
  "rh_paracentral_volume" = LL.FFX_Q.pmat[148:150],
  "rh_parsopercularis_volume" = LL.FFX_Q.pmat[151:153],
  "rh_parsorbitalis_volume" = LL.FFX_Q.pmat[154:156],
  "rh_parstriangularis_volume" = LL.FFX_Q.pmat[157:159],
  "rh_pericalcarine_volume" = LL.FFX_Q.pmat[160:162],
  "rh_postcentral_volume" = LL.FFX_Q.pmat[163:165],
  "rh_posteriorcingulate_volume" = LL.FFX_Q.pmat[166:168],
  "rh_precentral_volume" = LL.FFX_Q.pmat[169:171],
  "rh_precuneus_volume" = LL.FFX_Q.pmat[172:174],
  "rh_rostralanteriorcingulate_volume" = LL.FFX_Q.pmat[175:177],
  "rh_rostralmiddlefrontal_volume" = LL.FFX_Q.pmat[178:180],
  "rh_superiorfrontal_volume" = LL.FFX_Q.pmat[181:183],
  "rh_superiorparietal_volume" = LL.FFX_Q.pmat[184:186],
  "rh_superiortemporal_volume" = LL.FFX_Q.pmat[187:189],
  "rh_supramarginal_volume" = LL.FFX_Q.pmat[190:192],
  "rh_frontalpole_volume" = LL.FFX_Q.pmat[193:195],
  "rh_temporalpole_volume" = LL.FFX_Q.pmat[196:198],
  "rh_transversetemporal_volume" = LL.FFX_Q.pmat[199:201],
  "rh_insula_volume" = LL.FFX_Q.pmat[202:204],
  "Left_Cerebellum_Cortex" = LL.FFX_Q.pmat[205:207],
  "Left_Thalamus_Proper" = LL.FFX_Q.pmat[208:210],
  "Left_Caudate" = LL.FFX_Q.pmat[211:213],
  "Left_Putamen" = LL.FFX_Q.pmat[214:216],
  "Left_Pallidum" = LL.FFX_Q.pmat[217:219],
  "Brain_Stem" = LL.FFX_Q.pmat[220:222],
  "Left_Hippocampus" = LL.FFX_Q.pmat[223:225],
  "Left_Amygdala" = LL.FFX_Q.pmat[226:228],
  "Left_Accumbens_area" = LL.FFX_Q.pmat[229:231],
  "Left_VentralDC" = LL.FFX_Q.pmat[232:234],
  "Right_Cerebellum_Cortex" = LL.FFX_Q.pmat[235:237],
  "Right_Thalamus_Proper" = LL.FFX_Q.pmat[238:240],
  "Right_Caudate" = LL.FFX_Q.pmat[241:243],
  "Right_Putamen" = LL.FFX_Q.pmat[244:246],
  "Right_Pallidum" = LL.FFX_Q.pmat[247:249],
  "Right_Hippocampus" = LL.FFX_Q.pmat[250:252],
  "Right_Amygdala" = LL.FFX_Q.pmat[253:255],
  "Right_Accumbens_area" = LL.FFX_Q.pmat[256:258],
  "Right_VentralDC" = LL.FFX_Q.pmat[259:261],
  "TotalGrayVol" = LL.FFX_Q.pmat[262:264]
)
}


LL.FFX_Q.pvals<-t(LL.FFX_Q.pvals)

#save IS output
saveRDS(LL.FFX_Q.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.Q_FFX.pvals.Rds")
saveRDS(LLcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_Q_FFX.Rds")
saveRDS(AICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_Q_FFX.Rds")
saveRDS(BICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_Q_FFX.Rds")

write.table(LL.FFX_Q.pvals, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LL.Q_FFX.pval.txt", quote = F, sep=",", col.names = F)
write.table(LLcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/LLcompare_Q_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(AICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/AICcompare_Q_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)
write.table(BICcompare_Q_FFX, file = "/Users/michaelconnaughton/Desktop/IMAGEN/Model_Performance_results/BICcompare_Q_FFX.txt", quote = F, sep=",", row.names = F, col.names = T)









#FFX_Simple

#FFX_Complex

# Fit the GAMM with basic splines
Left_Pallidum_GAMM <- gamm(
  Left_Pallidum ~ s(bully_victim, k = 4) + s(age, bully_victim, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)
Left_Pallidum_GAMM <- Left_Pallidum_GAMM$


  
rh_bankssts_volume_GAMM <- gamm(
  rh_bankssts_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)


lh_caudalanteriorcingulate_volume_GAMM <- gamm(
  lh_caudalanteriorcingulate_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

rh_caudalanteriorcingulate_volume_GAMM <- gamm(
  rh_caudalanteriorcingulate_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

lh_caudalmiddlefrontal_volume_GAMM <- gamm(
  lh_caudalmiddlefrontal_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

rh_caudalmiddlefrontal_volume_volume_GAMM <- gamm(
  rh_caudalmiddlefrontal_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

lh_cuneus_volume_GAMM <- gamm(
  lh_cuneus_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

rh_cuneus_volume_GAMM <- gamm(
  rh_cuneus_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

lh_entorhinal_volume_GAMM <- gamm(
  lh_entorhinal_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex + EstimatedTotalIntraCranialVol,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)

lh_cuneus_volume_GAMM <- gamm(
  lh_cuneus_volume ~ s(bully_victim, k = 4) + s(age, k = 4) + s(bully_victim, age) + SES + mode_c_pds + sex,
  random = list(subID =~ age, scan_site =~ 1),
  data = IMAGEN_bully,
  method = "REML",
  optimizer = "bobyqa"
)


lh_cuneus_volume_GAMM <- lh_cuneus_volume_GAMM$gam
summary(lh_cuneus_volume_GAMM)


plot(lh_entorhinal_volume_GAMM)

AIC(lh_bankssts_volume_GAMM)
BIC(lh_bankssts_volume_GAMM)

lh_bankssts_volume_GAMM_K4 <- lh_bankssts_volume_GAMM_K4$gam
summary(lh_bankssts_volume_GAMM_K4)


rh_bankssts_volume_GAMM <- rh_bankssts_volume_GAMM$gam
summary(rh_bankssts_volume_GAMM)
effectsize(rh_bankssts_volume_GAMM)


# Create newdata with variables matching the original dataset
newdata_GAMM <- expand.grid(
  EstimatedTotalIntraCranialVol = mean(IMAGEN_bully$EstimatedTotalIntraCranialVol, na.rm = TRUE),
  sex = factor(levels(IMAGEN_bully$sex)[1]),
  age = seq(min(IMAGEN_bully$age, na.rm = TRUE), max(IMAGEN_bully$age, na.rm = TRUE), length.out = 100),
  SES = mean(IMAGEN_bully$SES, na.rm = TRUE),
  mode_c_pds = mean(IMAGEN_bully$mode_c_pds, na.rm = TRUE),
  bully_victim = c(mean_bully_victim, p25_bully_victim, p75_bully_victim),
  subID = unique(IMAGEN_bully$subID)[1]
)

# Ensure the types of variables match the original dataset

# Predict
newdata_GAMM$predicted_lh_cuneus_volume <- predict(lh_cuneus_volume, newdata = newdata_GAMM, re.form = ~(1 + age | subID))
newdata_GAMM$predicted_rh_cuneus_volume <- predict(rh_cuneus_volume, newdata = newdata_GAMM, re.form = ~(1 + age | subID))

# Create a vector containing the names of all predicted volumes
predicted_volumes <- c(
  "predicted_lh_bankssts_volume",

# Plot
ggplot(newdata_GAMM, aes(x = age, y = predicted_lh_entorhinal_volume_GAMM, color = as.factor(bully_victim))) +
  geom_line(size = 1.5, alpha = 0.7) +
  scale_color_manual(values = c("red", "green", "blue"),
                     labels = c("25th percentile", "Mean", "75th percentile"),
                     name = "Bully Victim Percentiles") +
  labs(title = "Trajectory of Left Entorhinal Volume by Bully Victim Percentiles",
       x = "Age",
       y = "Volume mm^3") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  guides(color = guide_legend(reverse = TRUE))








# Plot
ggplot(newdata_GAMM, aes(x = age, y = predicted_lh_cuneus_volume, color = as.factor(bully_victim))) +
  geom_line(size = 1.5, alpha = 0.7) +
  geom_smooth(aes(group = as.factor(bully_victim)), method = "loess", se = TRUE, span = 0.5) +
  scale_color_manual(values = c("red", "green", "blue"),
                     labels = c("25th percentile", "Mean", "75th percentile"),
                     name = "Bully Victim Percentiles") +
  labs(title = "Trajectory of Left Entorhinal Volume by Bully Victim Percentiles",
       x = "Age",
       y = "Volume mm^3") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )  


# Predict from the model
newdata_GAMM$predicted_rh_cuneus_volume <- predict(rh_cuneus_volume_GAMM, newdata= newdata_GAMM, type = "response")

newdata_GAMM$predicted_lh_cuneus_volume <- predict(lh_cuneus_volume, newdata = newdata_GAMM, re.form = ~(1 + age | subID))


# Plot with ggplot2
ggplot(newdata_GAMM, aes(x = age, y = predicted_lh_cuneus_volume, group = bully_victim)) +
  geom_line(aes(color = bully_victim), size = 1.5) +
  geom_ribbon(aes(ymin = predicted_lh_cuneus_volume - 2 * se.fit,
                  ymax = predicted_lh_cuneus_volume + 2 * se.fit, fill = bully_victim), alpha = 0.2) +
  scale_color_manual(values = c("red", "green", "blue")) +
  labs(title = "Trajectory of Right Cuneus Volume by Bully Victim Status",
       x = "Age",
       y = "Volume (mm^3)") +
  theme_minimal() 
)



# Assuming 'gam_model' is the GAM part of your fitted 'gamm' object and 'newdata_GAMM' is the data used for prediction
pred <- predict(lh_cuneus_volume, newdata_GAMM, se.fit = TRUE)

# Add the predictions and standard errors to your newdata_GAMM dataframe
newdata_GAMM$predicted_lh_cuneus_volume <- pred$fit
newdata_GAMM$se.fit <- pred$se.fit


library(ggseg)
library(ggbrain)

library(ggplot2)

# Example data frame with region names from the DKT atlas and your beta values
cortical_data <- data.frame(
  region = c("lh_caudalanteriorcingulate", "lh_rostralanteriorcingulate"), # Replace with correct DKT atlas region names
  beta = c(-0.04, -0.05) # Your beta values
)

# Create brain maps for Cortical Volume and Cortical Surface Area
# This is a simplified example. You will have to adapt this based on your data structure.
cortical_volume_plot <- ggseg(atlas = "dkt", data = cortical_data, mapping = aes(fill = beta)) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_void() +
  labs(title = "A) Cortical Volume", fill = "Std. Beta")

}

#Descriptive Stats
{


bully_1 <- subset(IMAGEN_bully, timepoint == 1)
bully_2 <- subset(IMAGEN_bully, timepoint == 2)
bully_3 <- subset(IMAGEN_bully, timepoint == 3)
# Count the number of unique subIDs
number_of_subjects <- length(unique(IMAGEN_bully$subID))
print(number_of_subjects)

summary(IMAGEN_bully$sex)
summary(IMAGEN_bully$age_years)
sd(IMAGEN_bully$age_years)




summary(bully_1$age_years)
sd(bully_1$age_years)

summary(bully_2$age_years)
sd(bully_2$age_years)

summary(bully_3$age_years)
sd(bully_3$age_years)

#missing

missing_bully_victim <- is.na(bully_1$bully_victim)
missing_scan_site <- is.na(bully_1$scan_site)

# Remove observations with missing bully_victim values
bully_1 <- bully_1[complete.cases(bully_1$bully_victim), ]
bully_1 <- bully_1[complete.cases(bully_1$scan_site), ]
bully_2 <- bully_2[complete.cases(bully_2$bully_victim), ]
bully_2 <- bully_2[complete.cases(bully_2$scan_site), ]
bully_3 <- bully_3[complete.cases(bully_3$bully_victim), ]
bully_3 <- bully_3[complete.cases(bully_3$scan_site), ]




sum(is.na(bully_1$bully_victim))
sum(is.na(bully_1$scan_site))
sum(is.na(bully_2$bully_victim))
sum(is.na(bully_2$scan_site))
sum(is.na(bully_3$bully_victim))
sum(is.na(bully_3$scan_site))

#Vicitm
summary(bully_1$bully_victim)
sd(bully_1$bully_victim)
summary(bully_2$bully_victim)
sd(bully_2$bully_victim)
summary(bully_3$bully_victim)
sd(bully_3$bully_victim, na.rm = TRUE)


quantile_25 <- quantile(IMAGEN_bully$bully_victim, 0.25, na.rm = TRUE)
quantile_75 <- quantile(IMAGEN_bully$bully_victim, 0.75, na.rm = TRUE)
# Count how many people are in the top 75th percentile
t1_top_75_count <- sum(bully_1$bully_victim > quantile_75, na.rm = TRUE)
t2_top_75_count <- sum(bully_2$bully_victim > quantile_75, na.rm = TRUE)
t3_top_75_count <- sum(bully_3$bully_victim > quantile_75, na.rm = TRUE)
bottom_25_count <- sum(bully_3$bully_victim > quantile_75, na.rm = TRUE)

# Print the result
print(t1_top_75_count)
print(t2_top_75_count)
print(t3_top_75_count)
#SES

summary(bully_1$SES)
sd(bully_1$SES, na.rm = TRUE)

summary(bully_2$SES)
sd(bully_2$SES, na.rm = TRUE)

summary(bully_3$SES)
sd(bully_3$SES, na.rm = TRUE)

#Puberty
summary(bully_1$mode_c_pds)
sd(bully_1$mode_c_pds, na.rm = TRUE)


#Sex
summary(bully_1$sex)
summary(bully_2$sex)
summary(bully_3$sex)


age_test <- wilcox.test(bully_1$SES ~ bully_1$bully_victim)
print(age_test)

#scan site
summary(bully_1$scan_site)
summary(bully_2$scan_site)
summary(bully_3$scan_site)

summary()

# Assuming your data frame is named 'data'
# Assuming 'p_bully' is the dependent variable and other variables are predictors


# Subset the data to include only observations from timepoint 1
timepoint1_data <- IMAGEN_bully[IMAGEN_bully$timepoint == 1, ]

# Check summary statistics
summary(timepoint1_data$bully_victim)

# Create a histogram
hist(timepoint1_data$bully_victim, main = "Distribution of bully_victim at Timepoint 1", xlab = "bully_victim")

# Create a density plot


}

#Graphs



data <- tibble(
  region = c(
    "rh_entorhinal_volume", "lh_frontalpole_volume", "lh_insula_volume",
    "rh_medialorbitofrontal_volume", "lh_entorhinal_volume", "rh_cuneus_volume",
    "rh_lateraloccipital_volume", "rh_insula_volume", "lh_medialorbitofrontal_volume",
    "lh_lateraloccipital_volume", "rh_bankssts_volume", "rh_parahippocampal_volume",
    "lh_caudalanteriorcingulate_volume", "lh_cuneus_volume", "rh_lingual_volume",
    "rh_caudalanteriorcingulate_volume", "lh_parsopercularis_volume"
  ),
  value = c(
    -0.11, -0.08, -0.08, -0.08, -0.08, -0.07, -0.07, -0.07,
    -0.06, -0.06, 0.05, 0.05, 0.05, -0.05, -0.05, 0.04, 0.03
  )
)



#>  [1] "bankssts"                   "caudal anterior cingulate" 
#>  [3] "caudal middle frontal"      "corpus callosum"           
#>  [5] "cuneus"                     "entorhinal"                
#>  [7] "frontal pole"               "fusiform"                  
#>  [9] "inferior parietal"          "inferior temporal"         
#> [11] "insula"                     "isthmus cingulate"         
#> [13] "lateral occipital"          "lateral orbitofrontal"     
#> [15] "lingual"                    "medial orbitofrontal"      
#> [17] "middle temporal"            "paracentral"               
#> [19] "parahippocampal"            "pars opercularis"          
#> [21] "pars orbitalis"             "pars triangularis"         
#> [23] "pericalcarine"              "postcentral"               
#> [25] "posterior cingulate"        "precentral"                
#> [27] "precuneus"                  "rostral anterior cingulate"
#> [29] "rostral middle frontal"     "superior frontal"          
#> [31] "superior parietal"          "superior temporal"         
#> [33] "supramarginal"              "temporal pole"             
#> [35] "transverse temporal"




someData_bully_victim = tibble(
  region = c("insula", "insula", "lingual", "entorhinal", "medial orbitofrontal", 
             "lateral occipital", "supramarginal", "cuneus", "frontal pole", "superior frontal", 
             "entorhinal", "lateral occipital", "medial orbitofrontal", "superior temporal", "pars orbitalis", 
             "rostral middle frontal", "precentral", "superior frontal", "rostral middle frontal", "paracentral", 
             "lateral orbitofrontal", "temporal pole", "postcentral", "pars orbitalis", "precuneus", 
             "pericalcarine", "paracentral", "frontal pole", "postcentral", "precentral", "parahippocampal", 
             "parahippocampal"), 
  cohens_d = abs(c(-0.292, -0.253, -0.222, -0.220, -0.201, 
                   -0.192, -0.181, -0.171, -0.155, -0.144, 
                   -0.133, -0.128, -0.124, -0.122, -0.116, 
                   -0.112, -0.108, -0.107, -0.102, -0.097, 
                   -0.092, -0.091, -0.086, -0.081, -0.079, 
                   -0.072, -0.066, -0.064, -0.064, 0.065, 
                   0.081, 0.137)),
  hemi = c("left", "right", "right", "right", "right", 
           "left", "left", "right", "left", "right", 
           "left", "right", "left", "left", "left", 
           "left", "left", "right", "left", "right", 
           "right", "right", "right", "right", "right", 
           "right", "left", "right", "left", "right", 
           "left", "right"),  # Hemisphere data
  
)



library(tibble)



ggplot(someData_bully_victim) + 
  geom_brain(atlas = dk, mapping = aes(fill = cohens_d)) +  # Visualization of brain data
  scale_fill_gradient(low = "yellow", high = "red") +  theme_void() +  # Removes background elements for a cleaner look
  labs(
  )



# Load necessary library
library(ggplot2)

# Create the Bully_effect_cohens_d list
Bully_effect_cohens_d <- list(
  region = c("Left Cerebellum", "Left Pallidum", "Left Ventral Diencephalon", "Right Cerebellum", 
             "Right Pallidum", "Right Ventral Diencephalon", "Total Gray Matter", "Right Thalamus", 
             "Left Caudate", "Left Hippocampus", "Left Amygdala", "Right Accumbens", 
             "Right Amygdala", "Left Accumbens", "Right Hippocampus", "Right Caudate", 
             "Left Putamen", "Right Putamen"),
  cohen_d = abs(c(-0.306, -0.239, -0.221, -0.198, 
              -0.176, -0.138, -0.114, -0.081, 
              0.102,  0.112,  0.113,  0.118, 
              0.126,  0.152,  0.173,  0.208, 
              0.277,  0.382))
)

# Convert the list to a data frame
Bully_effect_cohens_d <- data.frame(Bully_effect_cohens_d)

# Plotting
ggplot(Bully_effect_cohens_d, aes(x = reorder(region, cohen_d), y = cohen_d, fill = cohen_d)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  labs(x = "", y = "", title = "Effect of Bullying on Subcortical Regions") +
  ylim(0, 0.4) +
  scale_fill_gradient(
    low = "yellow", 
    high = "red", 
    breaks = c(0.1, 0.2, 0.3, 0.4), 
    labels = c("0.1", "0.2", "0.3", "0.4"), 
    guide = guide_colorbar(title = NULL)
  ) +
  theme_void() +
  theme(
    axis.text.y = element_text(size = 8, family = "Times New Roman"),
    axis.title.x = element_text(size = 10, family = "Times New Roman"),
    axis.title.y = element_text(size = 10, family = "Times New Roman"),
    text = element_text(family = "Times New Roman"),
    legend.position = "bottom"
  )



brain_data_INT = tibble(
    region = c("insula", "entorhinal", "insula", "lateral occipital", "cuneus", 
               "medial orbitofrontal", "lateral occipital", "lingual", "frontal pole", "cuneus", 
               "entorhinal", "pericalcarine", "medial orbitofrontal", "pars orbitalis", "supramarginal", 
               "temporal pole", "superior frontal", "pericalcarine", "precuneus", "paracentral", 
               "transverse temporal", "transverse temporal", "superior temporal", "precentral", "superior frontal", 
               "postcentral", "parahippocampal", "parahippocampal", "bankssts", "caudal anterior cingulate", 
               "caudal anterior cingulate"), 
    cohens_d = abs(c(-0.217, -0.195, -0.192, -0.187, -0.163, 
                     -0.156, -0.153, -0.141, -0.130, -0.128, 
                     -0.126, -0.120, -0.115, -0.110, -0.108, 
                     -0.079, -0.072, -0.070, -0.069, -0.043, 
                     -0.023, -0.016, 0.005, 0.008, 0.010, 
                     0.015, 0.082, 0.098, 0.131, 0.132, 0.149)),
    hemi = c("left", "right", "right", "right", "right", 
             "right", "left", "right", "left", "left", 
             "left", "left", "left", "left", "left", 
             "right", "right", "right", "right", "right", 
             "right", "left", "right", "left", "left", 
             "left", "left", "right", "right", "right", 
             "left"),  # Hemisphere data
    
  )



ggplot(brain_data_INT) + 
  geom_brain(atlas = dk, mapping = aes(fill = cohens_d)) +  # Visualization of brain data
  scale_fill_gradient(low = "yellow", high = "red") +  theme_void() +  # Removes background elements for a cleaner look
  labs(
  )





Bully_effect_INT_sub_cohens_d <- list(
cohen_d <- abs(c(-0.281, -0.181, -0.175, -0.174, -0.161, 
                 0.096, 0.096, 0.113, 0.121, 0.124, 
                 0.134, 0.182, 0.274, 0.305)),

region <- c("Left Cerebellum Cortex", "Left Ventral Diencephalon", "Left Pallidum", "Right Pallidum", 
               "Right Cerebellum", "Left Caudate", "Left Hippocampus", "Left Accumbens", 
               "Right Caudate", "Left Amygdala", "Right Amygdala", "Right Hippocampus", 
               "Left Putamen", "Right Putamen")
)
  
  
Bully_effect_INT_sub_cohens_d <- data.frame(Bully_effect_INT_sub_cohens_d)
  
  # Plotting
  ggplot(Bully_effect_INT_sub_cohens_d, aes(x = reorder(region, cohen_d), y = cohen_d, fill = cohen_d)) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    labs(x = "", y = "", title = "Effect of Bullying*Age on Subcortical Regions") +
    ylim(0, 0.4) +
    scale_fill_gradient(
      low = "yellow", 
      high = "red", 
      breaks = c(0.1, 0.2, 0.3, 0.4), 
      labels = c("0.1", "0.2", "0.3", "0.4"), 
      guide = guide_colorbar(title = NULL)
    ) +
    theme_void() +
    theme(
      axis.text.y = element_text(size = 8, family = "Times New Roman"),
      axis.title.x = element_text(size = 10, family = "Times New Roman"),
      axis.title.y = element_text(size = 10, family = "Times New Roman"),
      text = element_text(family = "Times New Roman"),
      legend.position = "bottom"
    )
  
  
  
  
  
  library(powerlmm)
  d <- per_treatment(control = dropout_weibull(0.3, 2),
                     treatment = dropout_weibull(0.2, 2))
  p <- study_parameters(n1 = 11,
                        n2 = 10,
                        n3 = 5,
                        icc_pre_subject = 0.5,
                        icc_pre_cluster = 0,
                        icc_slope = 0.05,
                        var_ratio = 0.02,
                        dropout = d,
                        effect_size = cohend(-0.8, 
                                             standardizer = "pretest_SD"))
  
  plot(p)  #> 
  
  library(powerlmm)
  shiny_powerlmm()