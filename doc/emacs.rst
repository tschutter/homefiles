=====================
Emacs Tips and Tricks
=====================

Something pithy here.

Useful Commands
---------------

describe-key (C-h k)
    Display documentation of the function invoked by *KEY*.

kill-rectangle (C-x r k)
    Delete the region-rectangle and save it as the last killed one.

sort-lines
    Sort lines in region alphabetically.

string-rectangle (C-x r t)
    Replace rectangle contents with STRING on each line.  The length
    of STRING need not be the same as the rectangle width.


More Complex Procedures
-----------------------

Format a CSV file to a fixed width table

  #. Load the .csv file

  #. ``M-x org-mode``

  #. Select text of entire file

  #. ``M-x org-table-convert-region``
