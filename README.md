Fork of https://github.com/raku-community-modules/App-MoarVM-Debug


Usage
---------------------

Start the debugger:

    $ mv2d main.raku

Set a breakpoint on line 42:

    > bp main.raku 42

Then type resume to resume the thread to hit it:

    > resume

Type `help` to see all of the commands.

Known Issues
------------

The only stepping mode currently available is Step Into.

Backtraces will show incorrect line numbers.

Source can be located at: https://github.com/slid1amo2n3e4/mv2d. Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2017 - 2020 Edument AB

Copyright 2024 The Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

