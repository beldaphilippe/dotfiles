# My Emacs Config
## Structure
```
init/
	encoding.el
    gui.el
    package.el
    speedup.el
langs/
    c.el
    glsl.el
    rmd.el
utils/
    evil.el
	completion.el
	live-server.el
    misc.el // contains emacs behaviour config
install/ 	// install repo, run it at installation
	tree-sitter-grammar-install.el
```

## Information
### Generated directories/files
When emacs is run with a certain configuration, it generates several directories and files. Here is their purposes.
* `eln-cache/`: stores native-compiles files (.eln), which helps improve performance by allowing faster loading of Emacs Lisp code.
* `elpa/`: contains source code for packages installed through elpa repository.
* `custom.el`: contains the custom changes made mostly through GUI configuration.
*`backups/`: contains file backups.
* `autosaves/`: contains file autosaves

## TODO
[ ] C-l support
[ ] packages to check: consult, org-roam, vertico-posframe
[ ] posframe minibuffer mode, fixed size of minibuffer (proportional to space available)
