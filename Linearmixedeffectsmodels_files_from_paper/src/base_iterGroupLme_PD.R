### base_iterGroupLme_PD.R - iterative lme by group of variables

library(dplyr); library(nlme);
library(ggplot2); library(gridExtra);
library(predictmeans); library(broom);
library(tidyr)


# also uses tidyr, grid and gdata package for interleave function (not loaded)


### Variable Declarations ####





# load data
labVars.df <- read.csv(file="INP_dat_LME_immunevars.csv", stringsAsFactors = F)

summary(labVars.df)
Hmisc::describe(labVars.df)
glimpse(labVars.df)

# title for each pdf page output
pdf.title <- "Lme_test"
# output file names
pdf.outfile <- "Test_lme.pdf"
tsv.outfile <- "Test_lme.tsv"


labVars.lng.df <- labVars.df %>% tbl_df() %>%
    select(case_id, LEU3N, WBC, visit_N, Racebin1, lastage_minus10, SMOKING_halfpkplus_1yr, cancer_cat) %>%
    gather(variable, num_value_adj, c(LEU3N, WBC))
labVars.lng.df$cancer_cat <- factor(labVars.lng.df$cancer_cat)
labVars.lng.df$Racebin1 <- factor(labVars.lng.df$Racebin1, levels=c("White", "Black"))
labVars.lng.df$SMOKING_halfpkplus_1yr <- as.factor(labVars.lng.df$SMOKING_halfpkplus_1yr)
labVars.lng.df$variable <- as.factor(labVars.lng.df$variable)

labVars.lng.df <-
    labVars.lng.df %>% mutate(variable_desc=ifelse(variable=="WBC", "White Blood Cells (/uL)", "CD4 Count (cells/uL)"))

#summary(labVars.lng.df)


### model parameters - fixed and random effects
## fixed effects
fixed.formula <- "num_value_adj ~ visit_N + Racebin1 + lastage_minus10 + SMOKING_halfpkplus_1yr + cancer_cat"

## random effects
random.formula <- "~ visit_N | case_id"



## grouping variable(s) - names of columns in input data by which to stratify
#   data for each model
#   (for MACS data, standard "variable" column will always be included)
groups <- c("variable")  # iterate through LEU3N and WBC


## lme group - column in input data containing grouping variable used in
#   model group-by-time interaction (i.e., covariate in model formula)
lme.group <- "cancer_cat"



### Main ###
## lme functions
## - see notes in functions file for parameter descriptions
lmeObjs.df <- .base_lme(in.df="labVars.lng.df",
    in.groups=groups, in.fixed.f = fixed.formula, in.random.f = random.formula)

# models are returned in a data.frame with list column "model":
#   one model per row with columns for grouping variables
# e.g. view details for model 1
summary(lmeObjs.df$model[[1]])
summary(lmeObjs.df$model[[2]])


# currently no good high-throughput diagnostics
#  can check individual models of interest with:
#predictmeans::residplot(lmeObjs.df$model[[1]])

# summarize coefficients for fixed effects:
#   use broom package to create a tidy summary table of
#   estimates SEs p-values for each term in each model
summ.tab <- lmeObjs.df %>% broom::tidy(model, effects="fixed")
summ.tab %>% View()


## plotting functions
#  - create a ggplot for each model in lmeObjs.df
#    - options to included shaded CIs, points w/ observed values,
#      faceted panels by groups, add'l custom
#      labels (see function header for parameter descriptions)
#  - prints raw plot out to standard out (Rstudio)

lmePlots.df <- .lmePlots(lmeObjs.df,
    in.groups=groups,
    in.x_col="visit_N", in.y_col="num_value_adj",
    in.lme_group="cancer_cat",
    in.ylab_col = "variable_desc",
    in.xlab="Visit Number",
    in.colors=c("blue", "red"),
    in.ci=TRUE, in.points=TRUE, in.jitterv=3,
    in.trellis = TRUE)

#lmePlots.df
# check 1st plot
#lmePlots.df
lmePlots.df$plot[[1]]
lmePlots.df$plot[[2]]


## add tableGrob with summary by visit per group
visGrobs.df <- .lme_nVisGrobs(lmeObjs.df,
    in.groups=c("variable"), in.lme_group="cancer_cat",
    in.font_adj=0.7)

# view first table
#grid.arrange(visGrobs.df$nVisGrob[[1]])


## make df.list of table grobs with fixed effects coefficients table
##   for output
coefGrobs.df <- .lme_coeffGrobs(lmeObjs.df,
    in.groups=c("variable"), in.lme_group="cancer_cat", in.font_adj=0.7)

# view first table
#grid.arrange(coefGrobs.df$tTabGrob[[1]])


## combine grobs for output

outGrobs.df <- full_join(visGrobs.df, coefGrobs.df)
outGrobs.df <- outGrobs.df %>% group_by(variable) %>%
    do(plot = arrangeGrob(.$nVisGrob[[1]], .$tTabGrob[[1]],
    ncol=1, heights = c(1/4, 3/4)))

#grid.arrange(outGrobs.df$plot[[1]])


## combine and print to .pdf/.tsv
#  interleave plots and Grobs - add new "plot_type" column to distinguish
lmePlots.df$plot_type <- "covariateMeans"
outGrobs.df$plot_type <- "combGrobs"
outGrobs.df$omit <- NA

lme_plotsOut.df <- gdata::interleave(lmePlots.df, outGrobs.df)

# combine cohort and variable to create unique names (.gg_mult_page
#   takes one "variable" column")
lme_plotsOut.df <- lme_plotsOut.df %>%
    tidyr::unite("variable", c(variable), sep="__")

.gg_mult_page(lme_plotsOut.df, n.row=2, n.col=2,
    out.page_title = pdf.title, out.filename = pdf.outfile)


## print summary file out to .tsv
write.table(summ.tab, file=tsv.outfile, sep="\t", row.names=F, quote=F)

