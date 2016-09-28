# Public emacs configuration

This my personal Emacs configuration, composed mostly of elisp functions,
keybindings, various packages (installed
using [package.el](http://www.wikemacs.org/wiki/Package.el)) and mode tweaks.

This configuration makes heavy use of the following:

  1. [`use-package`](https://github.com/jwiegley/use-package) for managing
     various package installations and configurations
  2. [`general.el`](https://github.com/noctuid/general.el) for key
     configurations

Note that the key configurations are rather rough and often changing, and should
probably not be imported wholesale. There are also a set of customizations
applied only to [Aquamacs](http://aquamacs.org) on OS X.

## Acknowledgements

The idea for a neatly partitioned Emacs tree came from Steve Yegge and his blog
posts. So did some of the code. The relevant blog posts are:

  - [Steve Yegge's .emacs file](https://sites.google.com/site/steveyegge2/my-dot-emacs-file)
  - [Effective Emacs](https://sites.google.com/site/steveyegge2/effective-emacs)
  - [Emergency Elisp](http://steve-yegge.blogspot.com/2008/01/emergency-elisp.html)

The current general structure of the configuration, as well as the heavy use of
`use-package` and `general.el` is based on a guide on
[how to build your own Spacemacs](https://sam217pa.github.io/2016/09/02/how-to-build-your-own-spacemacs/).
