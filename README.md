# mlth.data.frame

This is a package to define, manipulate and write the 'multi-header' dataframes, or nested dataframes, like this:

```    
    Parametric----------- Non-parametric-------
    Mean        SD        Median      MAD      
V1   0.06782323 0.8394908  0.02817213 0.8731994
V2  -0.03523018 0.8219772 -0.12407408 0.8314230
V3   0.45208943 1.2604398  0.14825680 1.1433125
V4   0.18914699 0.9769248  0.03353198 0.8590747
```

It also provides functionality to write the dataframe into Excel spreadsheets and to adjust its appearance
(e.g., fonts, borders etc). The 'xlsx' package is used for this.

The development is still ongoing, the feedback is welcome.

Author: Ivan Voronin, Psychological Institute of Russian Academy of Education, Moscow

e-mail: ivan.a.voronin@gmail.com

# To install:

\# install.packages('devtools')

library(devtools)

install_github('ivanvoronin/mlth.data.frame')
