## Scoping review and evidence gap mapping of open science to improve reproducibility

- The original publication can be found on [OSF](https://osf.io/preprints/metaarxiv/a8rmu). .
- The [CODECHECK](https://codecheck.org.uk/) report can be found on [Zenodo](https://zenodo.org/records/13364677).
- This repository contains the data and code required to reproduce Figures 2, 3, 4 and 5.
- Steps for reproduction:
  1. Clone or `Download ZIP` this repository onto your local machine.
  2. Open `scoping.Rproj` within RStudio.
  3. The project uses [renv](https://rstudio.github.io/renv/articles/renv.html) to manage R package versions. If needed, install it using `install.packages("renv")` in the R Console.
  4. When prompted, execute `renv::restore()` in the R Console. This will read the `renv.lock` metadata and install the package versions needed to reproduce the figures.
  4. Render `scope.qmd` and `discipline_figures.qmd` within RStudio. Each will build an `.html` file and save the figures to the `visuals` folder.


