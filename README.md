Harald Sick
11.11.11

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Qualitative Comparative Social Network Analysis with the `qcsna` package

<!-- badges: start -->
<!-- badges: end -->

The `qcsna` package is a tool that helps you to analyze network data
through qualitative comparative analysis (QCA). The focus is not the
overall network structure, but the node-level – either node behavior,
for instance their tendency towards tie formation, or their attributes –
if these are not immutable but subject to change.

Possible research questions are tackling the (network) conditions that
lead towards the observed nodal behavior or attribute. In other words –
and more aligned to set-theoretic thinking: what are the sufficient
and/or necessary conditions for that behavior or attribute? Why do nodes
have these attributes, e.g., they smoke, are infected, or wealthy, and
what are the conditions that lead to this? Explanatory variables are
preexisting network microstructures and node positions. For example, do
they form connections/edges with key actors, do they attend important
events or interact with nodes that have a very distinct attribute, are
they very popular, or do they hold a broker position in the network? In
the same way specific network position or interactions with other nodes
can be the outcome under investigation. In this case nodal attributes as
well as preceded connections to other actors or events would be
explanatory variables.

This is an alpha version that does not yet support the extraction of
specific node connections to selected nodes, but my private version does
– at least for bipartite networks. As soon as I will find time to
generalize it for all network types, I will release this function along
with an introduction. If you would like to apply the package in your
research and have questions or need more functions, please don’t
hesitate to contact me.

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

</div>
