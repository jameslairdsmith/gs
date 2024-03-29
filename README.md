
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`gs` is a grammar of recurring calendar events, implemented as an R
package. It enables users to easily create and work with schedules, even
in cases when the pattern of events is irregular. `gs` does this by
providing a set of simple to understand functions for making schedules
along with functions for combining them to form more complex ones.

The `gs` grammar is both flexible and declarative. The flexibility
allows users to compose arbitrarily complex schedules from simple and
intuitive buildings blocks. Because the grammar is declarative, the user
need not concern themselves with the details of how a particular
schedule works, rather they can specify what they want from a schedule
and let `gs` implement it for them.

## Installation

The package is currently experimental. Users are cautioned against
relying on it for anything of importance. It can be downloaded from
Github:

``` r
devtools::install_github("jameslairdsmith/gs")
```

Feedback, contributions, bug report and suggestions are welcome and can
be made in the [issues
tab](https://github.com/jameslairdsmith/gs/issues) of the Github repo.

## Getting started

If you have not used `gs` before, the best place to get started is the
introductory
[vignette](https://jameslairdsmith.github.io/gs/articles/intro.html).

## Acknowledgements

I would like to extend an enormous thank you to the
[authors](https://lubridate.tidyverse.org/authors.html) of and
[contributors](https://github.com/tidyverse/lubridate/graphs/contributors)
to the `lubridate` package. `gs` relies on `lubridate` for much of its
functionality and builds explicitly on its syntax.

I would also like to thank [Martin Fowler](https://martinfowler.com/),
whose writing\[1\] on this topic helped guide my thinking in a number of
important ways.

Finally I’d also like to thank Garrett Grolemund and Hadley Wickham who
in their paper\[2\] on `lubridate` inspired me to create `gs` and
pointed me in Fowler’s direction:

> In particular, we hope to create methods for R that work with
> reoccurring temporal date patterns, which were introduced by Fowler
> (1997).

1.  Fowler, M. (1997). “Recurring events for calendars.” \<URL:
    <https://tinyurl.com/yaqlbauj>\>.

2.  Grolemund G, Wickham H (2011). “Dates and Times Made Easy with
    lubridate.” *Journal of Statistical Software*, *40*(3), 1-25. \<URL:
    <http://www.jstatsoft.org/v40/i03/>\>.
