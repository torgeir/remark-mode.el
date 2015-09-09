# remark-mode.el

A major mode for [remark](https://github.com/gnab/remark) - the simple, in-browser, markdown-driven slideshow tool

<img src="https://cdn.rawgit.com/torgeir/remark-mode.el/gh-pages/emacs-remark-mode.gif" />

Key bindings

- `M-n` next slide
- `M-p` prev slide
- `C-x C-s` save and reload
- `C-c C-s s` new slide
- `C-c C-s i` new incremental slide
- `C-c C-s k` kill slide
- `C-c C-s n` create note
- `C-c C-s c` connect browser

Dependencies

- [markdown-mode](https://github.com/defunkt/markdown-mode)
- node+npm
- [browser-sync](http://www.browsersync.io/) (`npm i -g browser-sync`)

Todo

- [x] Use `xdg-open` instead of `open` on linux, `open` is os x only? Or the built in `(browse-url url)`.

History

- 2015-09-07: Version: 1.0.0
