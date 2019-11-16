### About

A simple shiny app for exploring various biodiversity measures commonly used by ecologists. By simulating species abundance data, the effect of sample size and species abundance heterogeneity on the measures can be displayed. Created for the module BIOL7002: Ecology for Conservation (Semesters 1 and 2 2019-2020) @ Oxford Brookes.

An online version with monthly usage limit can be found [**here**](https://gerthmicha.shinyapps.io/Richness-Demo). 

To run locally, first install all required packages (only needed once):

```{r}
install.packages("vegan",
                 "MASS",
                 "shiny",
                 "viridis",
                 "markdown",
                 dependencies = TRUE)
```

Open R and run 

```{r}
shiny::runGitHub("Richness-Demo", "gerthmicha")
```