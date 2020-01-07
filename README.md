# simpleLS
Simple least squares regression package

An example package used for Statistical Computing 1 in the COMPASS CDT. This package can fit least squares regression to a dataset, obtain coefficients and use those coefficents to predict for a new dataset.

To see the usage of this package, see the following example using the prostate cancer dataset from the `lasso2` package. Firstly, starting by fitting the model:
```{r include=FALSE, echo=FALSE}
install.packages(c(lasso2))
```
```{r, include=FALSE, echo=FALSE}
library(lasso2)
data(Prostate)
fit = LS.model(lpsa ~ lcavol, data = Prostate)
```
```{r, eval=FALSE}
library(lasso2)
data(Prostate)
fit = LS.model(lpsa ~ lcavol, data = Prostate)
```
The output `fit` can be passed into `LS.plot` and `LS.predict`.
```{r}
head(LS.predict(fit))
LS.plot(fit, var="lcavol")
```
