====================
Mutt Tips and Tricks
====================

Patterns
--------

=================== =====================================================
Pattern             Meaning
=================== =====================================================
~A                  all messages
~b EXPR             messages which contain EXPR in the message body
~B EXPR             messages which contain EXPR in the whole message
~c USER             messages carbon-copied to USER
~C EXPR             message is either to: or cc: EXPR
~d [MIN]-[MAX]      messages with "date-sent" in a Date range
~D                  deleted messages
~E                  expired messages
~e EXPR             message which contains EXPR in the "Sender" field
~F                  flagged messages
~f USER             messages originating from USER
~g                  PGP signed messages
~G                  PGP encrypted messages
~h EXPR             messages which contain EXPR in the message header
~k                  message contains PGP key material
~i ID               message which match ID in the "Message-ID" field
~L EXPR             message is either originated or received by EXPR
~l                  message is addressed to a known mailing list
~m [MIN]-[MAX]      message in the range MIN to MAX [1]_
~n [MIN]-[MAX]      messages with a score in the range MIN to MAX [1]_
~N                  new messages
~O                  old messages
~p                  message is addressed to you (consults $alternates)
~P                  message is from you (consults $alternates)
~Q                  messages which have been replied to
~R                  read messages
~r [MIN]-[MAX]      messages with "date-received" in a Date range
~S                  superseded messages
~s SUBJECT          messages having SUBJECT in the "Subject" field.
~T                  tagged messages
~t USER             messages addressed to USER
~U                  unread messages
~v                  message is part of a collapsed thread.
~x EXPR             messages which contain EXPR in the "References" field
~y EXPR             messages which contain EXPR in the "X-Label" field
~z [MIN]-[MAX]      messages with a size in the range MIN to MAX [1]_
~=                  duplicated messages (see $duplicate_threads)
=================== =====================================================

Where EXPR, USER, ID, and SUBJECT are regular expressions. Special
attention has to be made when using regular expressions inside of
patterns. Specifically, Mutt's parser for these patterns will strip
one level of backslash (\\), which is normally used for quoting. If it
is your intention to use a backslash in the regular expression, you
will need to use two backslashes instead (\\\\).

.. [1] The forms <[MAX], >[MIN], [MIN]- and -[MAX] are allowed, too.

---------------------------------------------------------------------

Searching by Date
-----------------

Mutt supports two types of dates, absolute and relative.

Absolute. Dates must be in DD/MM/YY format (month and year are
optional, defaulting to the current month and year). An example of a
valid range of dates is:

  Limit to messages matching: ``~d 20/1/95-31/10``

If you omit the minimum (first) date, and just specify "-DD/MM/YY",
all messages before the given date will be selected. If you omit the
maximum (second) date, and specify "DD/MM/YY-", all messages after
the given date will be selected. If you specify a single date with no
dash ("-"), only messages sent on the given date will be selected.

Relative. This type of date is relative to the current date, and may
be specified as:

======= ===================================
>offset (messages older than offset units)
<offset (messages newer than offset units)
=offset (messages exactly offset units old)
======= ===================================

offset is specified as a positive number with one of the following units:

= ======
y years
m months
w weeks
d days
= ======

Example: to select messages less than 1 month old, you would use

  Limit to messages matching: ``~d <1m``
