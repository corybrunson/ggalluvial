# Contributing to **ggalluvial** development

The goal of this guide is to help contributors to **ggalluvial** plan their contributions and navigate the process. The sections below cover different types of contributions, usually with increasing degrees of proposed change.

Note that **ggalluvial** is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing, you agree to its terms.

This guide is loosely adapted from the contributing guides [for **ggplot2**](https://github.com/tidyverse/ggplot2/blob/master/CONTRIBUTING.md) and [from ropensci](https://github.com/ropensci/dotgithubfiles/blob/master/dotgithub/CONTRIBUTING.md).
If you have questions about or suggestions for this guide, please feel free to contact a package maintainer.

## Editor

**Small edits to comments, documentation, and other non-code files** may be made using [the GitHub file editor](https://help.github.com/en/github/managing-files-in-a-repository/editing-files-in-your-repository). To edit **roxygen2** documentation, make changes to a source file `R/<topic>.r`, on a line that begins with `#'`, rather than to a documentation file `man/<topic.Rd`.

For more guidance on **roxygen2** documentation, see [this chapter](http://r-pkgs.had.co.nz/man.html) in the online book _R packages_.

## Issues

**Questions, possible bugs, feature requests, other open-ended issues** may be raised as [issues on GitHub](https://help.github.com/en/github/managing-your-work-on-github/creating-an-issue). In most cases, the issue should consist of two sections: a description of the issue, and a reproducible example of it. A reproducible example should attach packages, include data, and execute code:

1. Attach all relevant packages at the top of the example.
2. Use as small a data set as you can conveniently get that illustrates the issue. Once you have such a data set, the best way to include it in the example is to re-create it using `data.frame()` or some other constructor function. Another way is as follows:

    1. Execute `dput(data)` in R, where `data` is the data set.
    2. Copy the output.
    3. Paste into the example after the package attachments.

3. Use as few lines or steps of code as feasible to illustrate the issue. Add comments at key steps.

For more guidance on reproducible examples, see [this vignette](https://reprex.tidyverse.org/articles/reprex-dos-and-donts.html) for the **reprex** package.

## Pull requests

**Bug fixes, feature upgrades, and other substantive, self-contained changes** may be contributed via [pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests). Unless previously discussed with a maintainer, please follow these steps:

1. Fork the home repository (`corybrunson/ggalluvial`).
2. Create a new (local) branch on which to make changes. If appropriate, update the documentation on this branch and run functionality checks (e.g. using the **devtools** package).
3. Push this branch to the fork and open a pull request to the home repository.
4. With the maintainer(s), discuss and (if necessary) update the pull request until it is accepted or decided against. _In the latter case, a clear and consistent reason should be provided by the maintainer(s)._

For more guidance on the pull request process, see [the contributing guidelines for **ggplot2**](https://github.com/tidyverse/ggplot2/blob/master/CONTRIBUTING.md).
