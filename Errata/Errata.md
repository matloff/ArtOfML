
**installation** If you install from GitHub, I recommend the following:

``` r
remotes::install_github('https://github.com/matloff/qeML',dependencies=TRUE,build_vignettes=TRUE)
```

That argument **dependencies=TRUE** will result in all needed libraries being installed.

**data.table's and tibbles** Version 1.1 of **qeML** converts them to data frames in the qeX functions, but not in the predict.qeX ones. This will be fixed in Version 1.2, but for now, call **as.data.frame()** on the data on which you wish to predict.

**day1 dataset** The first two columns are day number and date, and should not generally be used for prediction.

**p.12** The value 12.0 near the top of the page should be 12.8,
consistent with the one later on that page.

**pp.47-48** In Section 2.7.2, the general call form should be

``` r
qeROC(dataIn, qeOut, yLevelName)

```

Accordingly, the argument 'V7' in the calls to **qeROC** should not be
there.

**p.34** The Telco Churn dataset has moved. It is now

[here](https://www.kaggle.com/datasets/blastchar/telco-customer-chur)



