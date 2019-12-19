
# Eazy-Documentation [![img](https://travis-ci.org/eazy-documentation/eazy-documentation.svg)](https://travis-ci.org/numcl/numcl)

-   **One-shot solution to the CL library documentation generator.**
    -   It **does not require any custom macro/read-macro for annotating your API.**
        Existing documentation generator supports only a limited number of macros (e.g. `defun`),
        or requires a user-defined extensions to the parser that extracts the
        documentation strings (e.g. see [docparser](http://quickdocs.org/docparser/)).  In contrast, this library runs based on the heuristic
        decision and ranking.
    -   It **searches the repository for any potential documentation files and embed**
        **them in the output as html**. Some library (especially the old ones like CFFI)
        contains a separate documentation / manual as texinfo documents, but they
        will be all embedded in the output.
    -   It supports any markup format supported by Pandoc and makeinfo.

-   **Best documentation gives you the most with the least text.**
    -   Bloated, redundant entries are harmful.
        Eazy-Documentation therefore provides a compact documentation by compressing the documentation entries
        in a run-length encoding manner &#x2013; similar, partly identical entries are merged together
        as much as possible.

-   **Respect the code structure and ordering.**
    -   Well-written library has a nice, intuitive code structure.
        This must be exploited by the documentation generator &#x2013; inspired by [Quickdocs](<http://quickdocs.org/>).

[Example page containing the documentation of eazy-documentation itself can be found here.](https://guicho271828.github.io/eazy-documentation/)

[Another example for NUMCL library](https://numcl.github.io/numcl/).

Command line interface can be installed via Roswell: `ros install guicho271828/eazy-documentation`


    Usage: eazy-documentation file-or-system output [OPTIONS]

The output is a single file when the output has a html extension;
otherwise it will be considered as a directory name.
It comes with a CSS and an empty javascript file.

Options are specified as follows:

Options:

| keyword         | description                                                               |
|:----------------|:--------------------------------------------------------------------------|
| :title          | Documentation title                                                       |
| :header         | The header inserted after the title                                       |
| :footer         | The footer inserted at the bottom                                         |
| :markup         | Markup langage used in the docstring, should be supported by pandoc.      |
|                 |                                                                           |
| :whitelist      | Whitelist of the package designators for the symbols being documented     |
| :blacklist      | Blacklist of the package designators for the symbols being documented     |
| :external-only  | Generate entries for external symbols only                                |
|                 |                                                                           |
| :toc            | Generate a table of contents (toc)                                        |
| :max-depth      | The maximum depth of a toc                                                |
|                 |                                                                           |
| :template-class | COMMON-HTML template class, no need to be chanded.                        |
| :css-list       | List of CSS scripts to be added to the template.                          |
| :js-list        | List of Javascripts to be added to the template.                          |
| :font-list      | List of Google fonts to be added to the template.                         |
| :clean          | Overwrite CSS/JS in the target directory                                  |
|                 |                                                                           |
| :remote-root    | Used to generate a weblink. Example: `https://github.com/<name>/<proj>`   |
| :local-root     | Used to generate a weblink. Example: `/home/<user>/lisp/<proj>/blob/master` |
| :relative       | When non-nil, the output is relative to the value of local-root.          |
| :static-files   | List of static README files etc.                                          |

## Requirements

`texinfo` and up-to-date `pandoc` (note: some package
managers, e.g., Ubuntu's APT, provides a severely outdated version and results in a terrible output.
Install the latest version from https://pandoc.org/installing.html )

## Author, License, Copyright

Masataro Asai (guicho2.71828@gmail.com)

Licensed under LGPL v3.

Copyright (c) 2019 IBM Corporation

