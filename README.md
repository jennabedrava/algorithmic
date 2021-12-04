# algorithmic

This package includes three functions that simulate algorithmic up down phase I clinical trial designs. The functions included are storersA, storersD, and biasedcoin. The goal of phase I trials is to find the highest dose of a new drug that is safe in humans. Algorithmic designs are easy to implement, and thus, well liked by clinicians. Each function uses a clinician's best estimate, based on pre-clinical studies, of the true dose toxicities as an input parameter. The original 3+3 design does not require a pre-determined sample size, as the trial will end after too many dose toxicities are observed at a certain dose level. Contrastingly, the Storer's D 3+3 design and the biased coin design require a pre-determined sample size as an input parameter. Simulations of these designs are important to estimate the operating characteristics of trial designs. Overall, we want to minimize the number of dose toxicities in patients and maximize the number of patients treated on the Maximum Tolerated Dose (MTD).

# Installation
```{r}
devtools::install_github("jennabedrava/algorithmic")
```
