Packages
================
2020-04-30

*Purpose*: Every time you start an analysis, you will need to *load
packages*. This is a quick warmup to get you in this habit.

*Reading*: (None)

**Note**: If you open this `.Rmd` file in RStudio you can *execute* the
chunks of code (stuff between triple-ticks) by clicking on the green
arrow to the top-right of the chunk, or by placing your cursor within a
chunk (between the ticks) and pressing `Ctrl + Shift + Enter`. See
[here](https://rmarkdown.rstudio.com/authoring_quick_tour.html) for a
brief introduction to RMarakdown files. Note that in RStudio you can
Shift + Click (CMD + Click) to follow a link.

**q1** Create a new code chunk and prepare to load the `tidyverse`.

In [RStudio](https://bookdown.org/yihui/rmarkdown/r-code.html) use the
shortcut `Ctrl + Alt + I` (`Cmd + Option + I` on Mac). Type the command
`library(tidyverse`).

``` r
library(tidyverse)
```

    ## ── Attaching packages ────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ───────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

Make sure to load packages at the *beginning* of your notebooks\! This
is a best-practice.

**q2** Run the chunk with `library(tidyverse)`. What packages are
loaded?

In
[RStudio](https://support.rstudio.com/hc/en-us/articles/200711853-Keyboard-Shortcuts)
press `Ctrl + Alt + T` (`Cmd + Option + T` on Mac) to run the code chunk
at your cursor.

**q3** What are the main differences between `install.packages()` and
`library()`? How often do you need to call each one? *The
install.package command installs a package that didn’t exist before
installation, while library command shows the stuff inside the package
we want.*

<!-- include-exit-ticket -->

# Exit Ticket

<!-- -------------------------------------------------- -->

Once you have completed this exercise, make sure to fill out the **exit
ticket survey**, [linked
here](https://docs.google.com/forms/d/e/1FAIpQLSeuq2LFIwWcm05e8-JU84A3irdEL7JkXhMq5Xtoalib36LFHw/viewform?usp=pp_url&entry.693978880=e-setup01-packages-assignment.Rmd).
