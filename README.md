# Continuous integration (CI) for vegetation surveys

[![License: CC0-1.0](https://img.shields.io/badge/License-CC0_1.0-lightgrey.svg)](http://creativecommons.org/publicdomain/zero/1.0/)
[![data-tests](https://github.com/markus1bauer/CI_vegetation_surveys/actions/workflows/data-tests.yaml/badge.svg)](https://github.com/markus1bauer/CI_vegetation_surveys/actions/workflows/data-tests.yaml)

A workflow for the continuous evaluation of vegetation surveys.

1. Add data
2. Commit
3. Check Actions status in README
4. Check README for reports and warnings

## Reports

### Missing sites

![Missing sites](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/reports_missing_sites.png)

### Missing species

![Missing species](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/reports_missing_species.png)

### Missing traits

![Missing traits](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/reports_missing_traits.png)

## Warnings

### Different total cover (click on table)

[![No typos found)](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/warnings_different_total_cover.png)](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/warnings_different_total_cover.csv)

### Cover typos sites

[![No typos found](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/warnings_species_typos.png)](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/warnings_species_typos.csv)

### Cover typos species

[![No typos found](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/warnings_sites_typos.png)](https://github.com/markus1bauer/CI_vegetation_surveys/blob/main/tests/testthat/warnings_sites_typos.csv)

## References

Kim et al. (2022) Methods Ecol Evol [DOI: 10.1111/2041-210X.13982](https://doi.org/10.1111/2041-210X.13982)

Example of Kim et al. (2022) [SCBImortality](https://github.com/SCBI-ForestGEO/SCBImortality)

Yenni et al. (2019) Plos Biology [DOI: 10.1371/journal.pbio.3000125](https://doi.org/10.1371/journal.pbio.3000125)

Instructions for creating an updating data workflow: [UpdatingData.org/githubactions](https://www.updatingdata.org/githubactions/).

