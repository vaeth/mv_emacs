# mv_emacs

Packages for (X)Emacs: block support, macrorecorder, verify changes

(C) Martin Väth (martin at mvath.de).
The license of this package is the GNU Public License GPL-2.
SPDX-License-Identifier: GPL-2.0-only

The packages should run with all non-ancient versions of Emacs or XEmacs.

For Gentoo, there is an ebuild in the mv overlay (available by layman).

### `block.el` (revision 1.6)

The standard Emacs treatment of the region is rather different from that
of many of the classical text editors. In many other editors, a "block"
can be marked explicitly which exists independent of the current cursor
position and independent of the buffer. This package is an attempt
to emulate this behaviour by providing such a block and making it act
as if it were the region.

### `kbdmacro.el` (revision 0.3):

This package provides an intuitive way to to define keyboard macros
(during runtime) and bind them to keys. The old meaning of the keys
is remembered and can be restored. It is also possible to save all
recorded macros into a file.

### `verify.el` (revision 0.2):

This package provides the single command `verify` which allows to
verify conveniently whether and where the buffer was actually
changed since its last "save".
