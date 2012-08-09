#dotemacs

My .emacs.d/init.el with packages to use clojure, repl, auto-complete, documentation, etc.  

Steps:  
1. Install Emacs 24 (if it not already installed) [[here]](http://batsov.com/articles/2011/10/09/getting-started-with-emacs-24/)  
2. Copy init.el to your emacs path (e.g. ~/.emacs.d)  
3. Install Swank Clojure. [[follow these steps]](https://github.com/technomancy/swank-clojure/#usage)  
4. Open emacs and waiting the installation of all packages. [1]

[1]: If you get the following error:  
```
error: Trying to parse HTTP response code in odd buffer:  *http melpa.milkbox.net:80*
```

So, close emacs, reopen, and stil continue waiting install all packages. Repeat those steps until all packages get installed. 

### Knows Issues

#### when run clojure-jack-in

- (void-variable --cl-accu--)  
Possible workaround: http://stackoverflow.com/a/9164713

#### when run slime / slime-connect

- “Searching for program: no such file or directory, lisp”  
Possible workaround (already included in init.el):  
```
(setq inferior-lisp-program "/path/to/lein repl")
```