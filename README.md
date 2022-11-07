Harald Sick
11.11.11

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Qualitative Comparative Social Network Analysis with the `qcsna` package

<!-- badges: start -->
<!-- badges: end -->

The `qcsna` package is a tool that helps you to analyze a network
through qualitative comparative analysis (QCA) (Ragin 1987, 2000). The
network structure as a whole is not to be explained, but rather the node
level, more precisely nodal behavior, for example their interaction with
other nodes through edge formation, or node attributes. Network
structures in which the nodes of interest are integrated serve as
explanatory variables, or — to use the QCA terminology — as conditions.
If you would like to know, for example, which network-based conditions
(like e.g., node-level indices or node connections) are necessary and/or
sufficient for a specific outcome (e.g., node attributes), this package
helps you to build a bridge between descriptive social network analysis
and QCA.

## Installation

You can install the development version of `qcsna` from
[GitHub](https://github.com/haraldsick/qcsna) with:

``` r
# install.packages("devtools")
devtools::install_github("haraldsick/qcsna")
library(qcsna)
```

## Network data

The `qcsna` package works with `network` objects (Butts 2008, 2015), the
format also `statnet` (Handcock et al. 2008) uses to store network data,
and supports one-mode and two-mode networks.

For demonstration purposes, I have included a dataset from another
study.

– if i-graph-data (intergraph)?

Internal Dataset

## Creating the QCA dataset

## Specifying conditions

## Finalizing the dataset to process it with QCA packages

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Butts2008a" class="csl-entry">

Butts, Carter T. 2008. “<span class="nocase">network: a Package for
Managing Relational Data in R.</span>” *Journal of Statistical Software*
24 (2). <https://www.jstatsoft.org/v24/i02/paper>.

</div>

<div id="ref-Butts2015" class="csl-entry">

———. 2015. *<span class="nocase">network: Classes for Relational
Data</span>*. The Statnet Project.
<https://cran.r-project.org/package=network>.

</div>

<div id="ref-Handcock2008" class="csl-entry">

Handcock, Mark, David Hunter, Carter Butts, Steven Goodreau, and Martina
Morris. 2008. “<span class="nocase">statnet: Software Tools for the
Representation, Visualization, Analysis and Simulation of Network
Data</span>.” *Journal of Statistical Software, Articles* 24 (1): 1–11.
<https://doi.org/10.18637/jss.v024.i01>.

</div>

<div id="ref-Ragin1987" class="csl-entry">

Ragin, Charles C. 1987. *<span class="nocase">The Comparative Method:
Moving Beyond Qualitative and Quantitative Strategies</span>*. Berkeley,
Los Angeles, London.

</div>

<div id="ref-Ragin2000" class="csl-entry">

———. 2000. *Fuzzy-Set Social Science*. Chicago, London.

</div>

</div>
