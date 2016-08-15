# inf-mongo.el

Run a MongoDB shell process in a buffer.

## Dependencies

All of inf-mongo's dependencies ship with GNU Emacs:

- comint
- js

## Install

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/endofunky/inf-mongo.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/inf-mongo")
    (require 'inf-mongo)

## Usage

Run with `M-x inf-mongo`.

## License

Copyright 2013 (c) Tobias Svensson <tob@tobiassvensson.co.uk>

Released under the same license as GNU Emacs:

    GNU Emacs is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    GNU Emacs is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
