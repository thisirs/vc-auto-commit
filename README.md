# vc-auto-commit

This package allows you to automatically commit all the changes of a
repository. It is useful when you have a project that needs to be put
under a version control system but you don't want to write any commit
message.

## Installation

Just put the following in your `.emacs`:

```lisp
(require 'vc-auto-commit)
```

If you want to auto-commit all repositories when quitting emacs, add
this:

```lisp
(vc-auto-commit-activate)
```

You can auto-commit current repository, marked for auto-committing or
not, with <kbd>M-x vc-auto-commit</kbd> or auto-commit all
repositories marked for auto-committing with <kbd>M-x
vc-auto-commit-all</kbd>. Repositories are marked for auto-committing
with the variable `vc-auto-commit-repository` or the local variable
`vc-auto-commit`.
