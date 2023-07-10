README
================
Stephanie Peacock
2023-07-10

## Data sources

This document outlines data sources used to estimate time series of
spawner abundance for each species and region as part of PSF’s State of
Salmon reporting. We report spawner abundance for each of nine regions
that represent all major Pacific salmon-bearing watersheds in Canada:
Yukon, Transboundary, Nass, Skeena, Haida Gwaii, Central Coast,
Vancouver Island & Mainland Inlets, Fraser, and Columbia. These regions
are also used to organize data in the [Pacific Salmon
Explorer](www.salmonexplorer.ca). There are a relatively small number of
Pacific salmon that spawn in the MacKenzie River basin in Arctic Canada
that are currently not considered here.

We separate five species of Pacific salmon: Chinook, chum, coho, pink,
and sockeye. We note that when assessing biological status, pink are
often separated into even- and odd-year lineages due to their consistent
2-year life cycle. However, for the general overviews provided in State
of Salmon we consider generational averages, which take the running
average of even- and odd-year lineages for pink salmon. This approach of
using generation running averages also smooths over dominant lines for
sockeye salmon, for which many populations display cyclic dominance.
Shifts in dominance between even- and odd-year pink populations or
declines in sub-dominant years of sockeye salmon are considered in a
more nuanced way when discussing how changes in abundance have been
reflected in the diversity and distribution of each species within the
region.

For each of these species, we construct spawner abundance at the
regional scale. We chose to use spawner abundance, rather than catch or
run size (i.e., catch plus spawners), because data on spawner abundance
is more readily summarized at different spatial scales. Further,
spawners represents the abundance of salmon available for to meet
cultural and ecological needs and thus provides a measure of status
relevant to communities and ecosystems, rather than industry. We
recognize that commercial catch has historical been a substantial
portion of salmon that return to the coast, and that ignoring declines
in catch will underestimate the declines in overall salmon abundance.
Trends in catch are discussed as part of the Harvest section under
“Factors”.

For most species and regions, we expanded spawner abundance from
river-level estimates to get a regional scale **index** of spawner
abundance using two types of expansion factors (English et al. 2016).
This expansion process is described in more detail
[below](#expansion-factors). For some regions and species, river-level
spawner abundnce data are not readily available or are not reliable. In
particular, stocks that are governed by international treaties may be
monitored by the Pacific Salmon Commission, and tend to have better data
available at coarser management unit scales. For these regions and
species, we may have adapted our approach to incorporate more reliable
datasets (see [Alternative Approaches](#alternative-approaches)).

## Expansion Factors

When expanding to regional scale abundance, we started with river-level
estimates from DFO’s [New Salmon Escapement Database System
(NuSEDS)](https://open.canada.ca/data/en/dataset/c48669a3-045b-400d-b730-48aafe8c5ee6).
We calculated the “maximum estimate” for each year and river population
as the maximum of all fields containing spawner abundance data (e.g.,
natural adult spawners, natural jack spawners, total broodstock
removals). Each of these river populations has been designated as an
“indicator” stream or “non-indicator” stream by DFO. Indicator streams
are observed more consistently in recent decades, tend to have higher
spawner abundance, and tend to be monitored using more intensive methods
that provide greater accuracy.

Expansion Factor 1, $F_{1,y/d}$, expands the observed spawner abundances
in indicator streams to account for indicator streams that are not
monitored in a given year. It is calculated for each year $y$ within
decade $d$ of the spawner time series, and relies on a decadal
contribution of each indicator stream to the total escapement to all
indicator streams, $P_{d,i}$ (English et al. 2016). The calculation of
this decadal contribution requires at least one estimate from each
indicator stream for the decade. If a decade does not contain sufficient
information (i.e., one or more indicator streams are not monitored at
all in a decade), then a reference decade is used to calculate
$P_{d,i}$. This reference decade is chosen to be: (1) the closest decade
(historical or future) with sufficient information, or failing (1), (2)
the 20-year period from 1980-1999 (Challenger et al. 2018).

For each decade (or reference decade if insufficient information) $d$,
the average number of spawners returning to indicator stream $i$ in
decade $d$ is calculated as:

$$\bar{S}_{d,i} = \sum_{y = 1}^{Y_{d,i}} \frac{\hat{S}_{y/d, i}}{Y_{d,i}} $$
where $Y_{d,i}$ is the number of years for which spawner estimates are
available within decade $d$ for stream $i$. From the average number of
spawners for all indicator streams, the decadal proportional
contribution of each indicator stream is calculated as:

$$ P_{d,i} = \frac{\bar{S}_{d,i}}{\sum_{i=1}^{I} \bar{S}_{d,i}}$$ where
$I$ is the total number of indicator streams.

Expansion Factor 1 is then calculated for each year within the decade
$y/d$ based on the decadal contributions and which streams were
monitored or not in a given year:

$$F_{1,y/d}=\left( \sum_{i=1}^I P_{d,i} w_{y/d,i} \right) $$ where
$w_{y/d,i}$ is 1 is stream $i$ was monitored in year $y$ and 0 if stream
$i$ was not monitored in year $y$. Expansion Factor 1 is then multiplied
by the sum of the observed spawners in all indicator stream to yield the
expanded estimate of spawner abundances in all indicator streams in the
region:

$$S'_{y} = F_{1,y/d} \sum_{i=1}^I \hat{S}_{y,i} $$

Expansion Factor 2 $F_{2,d}$ expands the escapement to all indicator
streams, $S'_{y}$, to account for non-indcator streams. Unlike Expansion
Factor 1, this is calculated for each decade (rather than each year) and
then applied to all years within a decade. Like Expansion Factor 1,
there needs to be sufficient information within the given decade in
order to calculate $F_{2,d}$, or else a reference decade is chosen. See
English et al. (2016) for detailed on how reference decades are chosen
in that case.

Expansion Factor 2 is calculated as:

$$F_{2,d} = \frac{\sum_{i = 1}^I \bar{S}_{d,i} + \sum_{j = 1}^{J} \bar{S}_{d,j}}{\sum_{i = 1}^I \bar{S}_{d,i}} $$
where $\bar{S}_{d,i}$ and $\bar{S}_{d,j}$ are the deacdal average number
of spawners in indicator and non-indicator streams, respectively,
calculated above. $J$ is the total number of non-indicator streams. The
adjusted total number of spawners in both indicator and non-indicator
streams is then calculated as: $$ S''_{y} = F_{2,d} S'_{y} $$.

Note that when expanding spawner abundance in run reconstructions for
spawner-recruit analysis, a third expansion factor is applied to account
for streams that are never monitored and for observer (in)efficiency. We
did not apply this expansion factor because it is highly undertain and
we are interested in relative changes in abundance through time, so we
do not require to expand to absolute abundance.

## Alternative Approaches

### Fraser

<!-- From More Information for Fraser spawner abundance: https://www.salmonexplorer.ca/#!/fraser/pink/fraser-river-odd&pop=ABUNDANCE_ESTIMATE&pop-detail=1 -->

In contrast to other species and areas, river-level spawner abundance
for Fraser Chinook are only a subset of streams available in NuSEDS,
including only the most intensively monitored streams that have been
deemed as reliable estimates of abundance by Brown et al. (2020).

In the Fraser region, there are only pink salmon returning in any
significant abundance in odd-years, constituting the **Fraser River
(odd)** Conservation Unit (CU). There are no spawner estimates for
Fraser pink since 2001 in NuSEDS. Therefore, spawner abundance is
estimated for the Fraser River (odd) pink salmon using three different
methods: (1) Up to 2001, estimated spawner abundance was calculated
based on escapement expansion methods described
[above](#expansion-factors), (2) from 2007-2008 estimated spawner
abundance was based on test fishing expansion, and (3) from 2009 to
present estimated spawner abundance is based on hydroacoustic estimates
at Mission (**REF?**).

<!-- When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- ## Including Plots -->

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Brown2020" class="csl-entry">

Brown, Gayle S Gale S, Mary E Thiess, Catarina Wor, Carrie A Holt, Bruce
Patten, Richard E Bailey, Chuck K Parken, et al. 2020. “<span
class="nocase">2020 Summary of Abundance Data for Chinook Salmon
(Oncorhynchus tshawytscha) in Southern British Canadian</span>.”
*Canadian Technical Report of Fisheries and Aquatic Sciences* 3401: xi +
135 p.

</div>

<div id="ref-Challenger2018" class="csl-entry">

Challenger, Wendell, Tony Mochizuki, Karl English, and Yury Bychkov.
2018. “<span class="nocase">North and Central Coast Salmon Database and
Analysis System User Manual</span>.” September. Sidney, BC: LGL Limited
for Pacific Salmon Foundation.
[https://salmonwatersheds.ca/libraryfiles/lib{\\\_}449.pdf](https://salmonwatersheds.ca/libraryfiles/lib{\_}449.pdf).

</div>

<div id="ref-English2016" class="csl-entry">

English, Karl K, Dave Peacock, Wendell Challenger, and Tony Mochizuki.
2016. “<span class="nocase">North and Central Coast Salmon Escapement,
Catch, Run Size and Exploitation Rate Estimates for each Salmon
Conservation Unit for 1954-2014</span>.” April. Report prepared by LGL
Limited for the Pacific Salmon Foundation; Fisheries; Oceans Canada:
Report prepared by LGL Limited for the Pacific Salmon Foundation;
Fisheries; Oceans Canada.
[https://salmonwatersheds.ca/libraryfiles/lib{\\\_}435.pdf](https://salmonwatersheds.ca/libraryfiles/lib{\_}435.pdf).

</div>

</div>
