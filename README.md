# Public emacs configuration

This is not really the emacs configuration I use. My real emacs configuration
imports lots of other Emacs code for working with various languages, web
services and code management systems. This code is only my custom-emacs code:
elisp functions, keybindings and mode tweaks.

## A note on keybindings

Most of my keybindings are pretty different from the standard Emacs ones. I
rebound them to be easier for me to remember, especially the movement
keybindings. In particular the `f` key moves forward, the `b` key moves
backward, the `w` key kills backwards and the `d` key kills forwards. The Ctrl
modifiers makes each of the above operate by word and the Alt or Meta modifier
makes them work by letter. Ctrl-m is now a prefix key for various functions to
reposition the current open buffer in the window. The functions are defined in
`functions.el`.

## Acknowledgements

The idea for a neatly partitioned Emacs tree came from Steve Yegge and his blog
posts. So did some of the code. The relevant blog posts are:

  - [Steve Yegge's .emacs file](https://sites.google.com/site/steveyegge2/my-dot-emacs-file)
  - [Effective Emacs](https://sites.google.com/site/steveyegge2/effective-emacs)
  - [Emergency Elisp](http://steve-yegge.blogspot.com/2008/01/emergency-elisp.html)
