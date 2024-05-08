# Dependencies

Install the package dependencies:

```{r eval=FALSE}
install.packages('devtools', 'roxygen2', 'profvis', 'httr2', 'vcr')
```

Install the development version of the rtaxonworks package:
```{r eval=FALSE}
remotes::install_github("SpeciesFileGroup/rtaxonworks")
```


## Troubleshooting

At least on Arch Linux, you may need to build the latest version of R devtools, because the current devtools package was built with an older version of the icu unicode library which causes install.packages('devtools') to fail with the errors:

`ERROR: dependencies ‘profvis’, ‘roxygen2’ are not available for package ‘devtools’`

 `libicui18n.so.72: cannot open shared object file: No such file or directory`

To build the latest version of devtools run the following command in R and when prompted select replace all packages (1):

```{r eval=FALSE}
remotes::install_github("r-lib/devtools")
```
Note that the command `remotes::install_github` won't be available until you at least attempt to install devtools.

After that you may still need to install the remaining dependency packages:

```{r eval=FALSE}
install.packages('httr2', 'vcr')
```

### vcr warnings
Until the VCR package supports httr2, it may be necessary to install the httr2 branch of vcr with:

```{r eval=FALSE}
remotes::install_github("ropensci/vcr@httr2")
```
