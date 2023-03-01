# Determination and plot of IC50s
## General description
The IC50 (half-maximal inhibitory concentration) describes the concentration which is necessary to replace 50% of known drug bound on a target. The IC50 is used to describe binding capacity on a pharmacological target. The resulting IC50 plot is a sigmoidal curve. The vice versa curve is the EC50 (half-maximal effectife concentration). The code works for IC50 as well as for EC50 determinations based on a non-linear regression model resulting in a sigmoidal curve.

In this example, the experiment based on a radioligand binding assay using a radioligand, competitor and a target receptor. The competitor (test compound) is analyzed in increasing concentrations, while the radioligand concentration remains constant throughout the whole experiment. Commonly, the binding affinity of the radioligand is well described in the literature. Of note, the experiment can also be perfomed in another set up with for example using absorbance instead of CPM (counts per minute) as measurement output.

The data in the example Excel file is based on a real experiment, I performed in the lab in January 2022.

Description of the Excel data file:
 - CPM = counts per minute; can also presented by fluorescence or absorbance values measured
 - Concentration of the competitor in uM (micromolar)
 - all concentrations measured in triplicates A, B C

## Loading R packages

`
library("drc")
library("dplyr")
library("readxl")
library("ggplot2")
library("tidyverse")
library("wesanderson")
`

## load data

`data_measured<-read_excel("IC50_example_data.xlsx", sheet=1, col_names=T)`


## check and reconstruct data

`print(data_measured)
glimpse(data_measured) `


## fit the input data to linear regression model in curve_fit: 

- Here the drc package is used with the function drm() to apply the regression model 
- regression model LL.4 for IC50 / EC50 for sigmoidal curves

` curve_fit <- drm(
  formula = CPM ~ Concentration,
  data = data_measured,
  fct = LL.4(names = c('hill', 'min_value', 'max_value', 'IC_50'))
)`


## define the IC50 in x axis and get it value from the curve fit

- extract the IC50 from the regression model

`IC50 <- curve_fit$coefficients['IC_50:(Intercept)']
IC50_CPM <- predict(curve_fit, as.data.frame(IC50)) `


## print the summary of curve_fit data

`summary(curve_fit) `

## create x values in log10 interval, get the min and max number of power to the Concentration values

`n_min = round(log10(min(data_measured$Concentration)), digits=5)
n_max = round(log10(max(data_measured$Concentration)), digits=5)
n <- seq(n_min, n_max, length.out = 1000) # the len of sequence: 100 
x_exp <- 10^n # 10 to the power of n`


## create data_predict dataframe 

`data_predicted <- tibble(
  Concentration = x_exp
  ) `


## save the predicted values from the curve fit in column "predicted"

`data_predicted$predicted <- predict(
  curve_fit,
  newdata = as.data.frame(data_predicted)
)`


## presenting predicted data 

`print(data_predicted)
glimpse(data_predicted)`

## add hightligt lines to show IC50 value in the graph

`lines_to_highlight_IC50 <- tribble(
  ~x,   ~xend, ~y,   ~yend,
  IC50, IC50,  -Inf, IC50_CPM,
  0,    IC50,  IC50_CPM,  IC50_CPM
)`


## plot the predicted curve from the curve_fit and the experimental datapoints

- IC50 is also shown in the graph

`p <- ggplot() +
  geom_segment(
    data = lines_to_highlight_IC50,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = 'grey', linetype = 'dashed', size = 1
  ) +
  geom_line(data = data_predicted, aes(x = Concentration, y = predicted), 
            size = 1.5) +
  geom_point(
    data = data_measured,
    aes(x = Concentration, y = CPM, fill = Replicate),
    shape = 21, size = 3, color = 'white', show.legend = FALSE
  ) +
  annotate(
    'text', x = IC50*2, y = IC50_CPM, 
    label = paste0('IC50: ', round(IC50*1000, digits=3), ' nM'),
    hjust = 0, vjust = -1, color = 'grey', size = 5
  ) +
  scale_x_log10(name = 'log (competitor) [uM]', breaks = unique(data_measured$Concentration)) +
  scale_y_continuous(name = 'CPM') +
  scale_fill_manual(values = wes_palette('BottleRocket2')) +
  theme_bw()`


#resize plot for publication

` p_resized<- p + theme(axis.text = element_text(size = 12))
p_resized<- p_resized + theme(axis.title = element_text(size = 12))
print(p_resized)`
