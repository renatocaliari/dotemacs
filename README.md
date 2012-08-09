#dotemacs

My .emacs.d/init.el with packages to use clojure, repl, auto-complete, documentation, etc.

## How to install emacs 24:
http://batsov.com/articles/2011/10/09/getting-started-with-emacs-24/
http://unschooled.org/2011/10/how-to-setup-emacs-for-clojure-on-mac-os-x-lion/

### Clojure-mode

#### Some known issues 

##### when run clojure-jack-in

- (void-variable --cl-accu--)  
Possible workaround: http://stackoverflow.com/a/9164713

##### when run slime / slime-connect

- “Searching for program: no such file or directory, lisp”  
- Possible workaround (already included in init.el):  
```
(setq inferior-lisp-program "/path/to/lein repl")
```