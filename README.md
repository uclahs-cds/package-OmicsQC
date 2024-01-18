# OmicsQC

1. [Description](#description)
2. [Installation](#installation)
3. [Quick start](#quick-start)
4. [Resources](#resources)
5. [Getting help](#getting-help)
6. [Citation information](#citation-information)
7. [License](#license)

## Description

OmicsQC is a statistical framework for integrating quality control metrics from multi-sample experiments such as those investigated in large cohort-level genomic profiling studies. Metrics are combined aggregated into quality scores which can be used to nominate samples for exclusion.

## Installation

Using devtools in R:
```R
library(devtools);
install_github('https://github.com/uclahs-cds/package-omicsQC');
```

From source:
```shell script
git clone https://github.com/uclahs-cds/package-omicsQC.git
R CMD INSTALL package-omicsQC
```

## Quick start

## Resources
* [Vignette]()

## Getting help

## Citation information

<Include BioRxiv preprint>

## License

Authors: Hugo Anders Frelin, Paul C. Boutros (PBoutros@mednet.ucla.edu)

[This project] is licensed under the GNU General Public License version 2. See the file LICENSE.md for the terms of the GNU GPL license.

OmicsQC aggregates quality control metrics from multi-sample investigations and nominates samples for exclusion using an unbiased strategy.

Copyright (C) University of California Los Angeles ("Boutros Lab") All rights reserved.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
