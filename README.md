Code for the presentation "Exploring place names through new computational tools: Examples from India" at SALA 40 in Bielefeld. Contains lines of code used for the presentation as well as additional code testing more names and maps. The organization closely follows the order of the presentation. 

In order to run the code, the package `toponym` needs to be installed and a path needs to be set for external data.

The following code installs the package and suggests the `toponym` package directory as path for external data using the function `toponymOptions()`. You will be prompted to confirm the path in the console of R. After running this code and confirming the path, you can run the script `sala_40_script.R`.

``` r
install.packages("toponym") # installs the package
library(toponym)            # load the package
toponymOptions("pkgdir")    # "pkgdir" is interpreted as the directory of the toponym package
# you will be prompted to confirm your choice
```
More instructions on the R package toponym is found at:

https://cran.r-project.org/package=toponym & https://github.com/Lennart05/toponym/tree/toponym-CRAN.
