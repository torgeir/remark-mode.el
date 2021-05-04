# remark-mode.el

A major mode for [remark](https://github.com/gnab/remark) - the simple, in-browser, markdown-driven slideshow tool

<img src="https://decwap8ztgrry.cloudfront.net/items/1v3M2Z44390j3o0n0S0L/remark-demo.gif" />

Key bindings

- `M-n` or `M-<down>` next slide
- `M-p` or `M-<up>` prev slide
- `M-S-<down>` move slide down
- `M-S-<up>` move slide up
- `C-x C-s` save and reload
- `C-c C-s s` new slide
- `C-c C-s i` new incremental slide
- `C-c C-s k` kill slide
- `C-c C-s n` create note
- `C-c C-s p` toggle presenter mode
- `C-c C-s c` connect browser
- `C-c C-s d` disconnect browser

Dependencies

- [markdown-mode](https://github.com/defunkt/markdown-mode)
- node

Todo

Nothing planned.

Changelog
- 2021-05-04: v2.0.2
Fix byte-compiler warnings and void variable access.

- 2019-11-03: v2.0.0
Reimplement browser synchronization in custom node server to drop dependency for browser-sync and support other platforms than os x. `C-c C-s d` to disconnect from browser. `C-c C-s p` to toggle presenter mode. Close the window when disconnecting from the slideshow on os x.

- 2017-12-17: v1.9.0:
Support moving slides around with `M-S-<down>` and `M-S-<up>`

- 2017-11-28: v1.8.0:
Remember if presentation mode is on or off when changing slides. Only change slides in the browser if the remark tab is frontmost.

- 2017-11-27: v1.7.0:
Run slideshow from the folder of the user's .remark file. Output slides in the template index.html provided in the same folder, or create a skeleton index.html if no such file exist.

- 2017-11-27: v1.6.0:
Save buffer after performing slide actions (new, kill, incremental) for the
ultimate reload experience

- 2017-11-27: v1.5.0:
Handle layout: true to always show the correct slide in the browser

- 2017-11-27: v1.4.0:
Support background-image, count and layout keywords in highlighting

- 2017-11-27: v1.3.0:
Automatically visit slide under cursor in browser, not just on `M-n` and `M-p`

- 2017-11-26: v1.2.4:
Expose remark-preferred-browser as var

- 2017-11-26: v1.1.4:
Visit slide in browser on M-n and M-p

- 2017-11-26: v1.0.4:
Fix invalid syntax

- 2017-11-26: v1.0.3:
Reload on all saves, not just C-x C-s

- 2015-09-07: v1.0.0
