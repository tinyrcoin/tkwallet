# tkwallet
TkWallet2X source code

TkWallet requires Tcl/Tk 8.4+

The mining function does not work on Linux or Mac OS X currently.

# wrapping into a binary

Requires Tclkit 8.4+ and SDX.kit.

```
tclkit sdx.kit qwrap tkwallet.tcl -runtime tclkit-binary
```
