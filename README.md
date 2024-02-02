# OmicsQC

1. [Description](#description)
2. [Installation](#installation)
3. [Resources](#resources)
4. [Getting help](#getting-help)
5. [Citation information](#citation-information)
6. [License](#license)

## Description

OmicsQC is a statistical framework for integrating quality control metrics from multi-sample experiments such as cohort-level genomic profiling studies. Metrics are aggregated into quality scores which can be used to nominate samples for exclusion.

![overview plot](vignettes/omicsQCFlowchart.png)

## Installation

Using devtools in R:
```R
library(devtools);
install_github('https://github.com/uclahs-cds/package-OmicsQC');
```

From source:
```shell script
git clone https://github.com/uclahs-cds/package-OmicsQC.git
R CMD INSTALL package-OmicsQC
```

## Resources
* For a tutorial on how to use OmicsQC, check out our [Vignette](https://uclahs-cds.github.io/package-OmicsQC/docs/articles/Intro_to_omicsQC.html)!
* For detailed information about available functions, check out our [Function Reference](https://uclahs-cds.github.io/package-OmicsQC/docs/reference/index.html)!
* For updates, check out our [Changelog](https://github.com/uclahs-cds/package-OmicsQC/blob/main/CHANGELOG.md)!

## Getting help

Looking for guidance or support with OmicsQC? Look no further.

* Check out our [Discussions](https://github.com/uclahs-cds/package-OmicsQC/discussions) page!
* Submit bugs :bug:, suggest new features :cherry_blossom: or see current work :mechanical_arm: at our [Issues](https://github.com/uclahs-cds/package-OmicsQC/issues) page.

## Citation information

You have stumbled upon an unpublished software :shushing_face: :shushing_face: :shushing_face:. We are currently preparing the manuscript for HistogramZoo. Please befriend us to learn more or check back later for updated citation information.

## License

Authors: Hugo Anders Frelin, Paul C. Boutros (PBoutros@mednet.ucla.edu)

OmicsQC is licensed under the GNU General Public License version 2. See the file LICENSE.md for the terms of the GNU GPL license.

OmicsQC aggregates quality control metrics from multi-sample investigations and nominates samples for exclusion using an unbiased strategy.

Copyright (C) University of California Los Angeles ("Boutros Lab") All rights reserved.

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
