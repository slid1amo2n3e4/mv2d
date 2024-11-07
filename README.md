[![Actions Status](https://github.com/raku-community-modules/App-MoarVM-Debug/actions/workflows/linux.yml/badge.svg)](https://github.com/raku-community-modules/App-MoarVM-Debug/actions) [![Actions Status](https://github.com/raku-community-modules/App-MoarVM-Debug/actions/workflows/macos.yml/badge.svg)](https://github.com/raku-community-modules/App-MoarVM-Debug/actions) [![Actions Status](https://github.com/raku-community-modules/App-MoarVM-Debug/actions/workflows/windows.yml/badge.svg)](https://github.com/raku-community-modules/App-MoarVM-Debug/actions)

App::MoarVM::Debug
==================

The MoarVM Debugger allows you to connect to a local MoarVM instance - if it was started with the --debug-port argument passed to MoarVM itself - and control execution of threads, introspect the stack and individual objects.

MoarVM also takes the --debug-suspend commandline argument, which causes MoarVM to immediately pause execution at the start.

Start the moar-remote script and pass the port you used for --debug-port and it should connect.

Type "help" in the debugger's CLI to see what commands are available to you.

Beta Use Instructions
---------------------

  * Install the module:

    $ zef install App::MoarVM::Debug

  * Locate `raku-m` bash script (or `raku.bat` on Windows): `locate raku-m`

  * Copy it to some other name:

    $ cp raku-m raku-moar-remote-m

  * Edit it to include `--debug-port=9999` and `--debug-suspend` in `moar` options:

    […] install/bin/moar --debug-port=9999 --debug-suspend --execname=[…]

  * Start the program you want to debug using that new script:

    $ raku-moar-remote-m  my-script.raku

  * Start the debugger CLI app and have it connect to the same port that's in the shell script from step 4:

    $ moar-remote 9999

  * (Optional) Write `assume thread 1` to assume tracking of first (main) thread

  * Set a breakpoint

    > breakpoint "my-script.raku" 1234 1 1

The string is the filename and `1234` is the line number (`1 1` is the secret ingredient). Ensure the line number doesn't point to an empty line.

  * Type `resume` to run your script.

  * The breakpoint will trigger, you can type `all lexicals` to view all lexicals. The numbers shown next to them in bold are "handle" numbers.

  * Find the object you want to dump and type `metadata 1234` (`1234` is the handle number).

If the features includes `attributes`, you can enter `attributes 1234` for this object to get information about the object's attributes.

If the features includes `positional`, you can enter `positionals 1234` to get information about the positional elements of the object.

If the features includes `associative`, you can enter `associatives 1234` to get information about the associative elements (keys and values) of the object.

The `metadata` command is only needed if you don't know which of these commands is useful for any given type.

  * Type `help` to see all of the available commands.

Known Issues
------------

The only stepping mode currently available is Step Into.

Backtraces will show incorrect line numbers.

AUTHOR
======

  * Timo Paulssen

Source can be located at: https://github.com/raku-community-modules/App-MoarVM-Debug . Comments and Pull Requests are welcome.

COPYRIGHT AND LICENSE
=====================

Copyright 2017 - 2020 Edument AB

Copyright 2024 The Raku Community

This library is free software; you can redistribute it and/or modify it under the Artistic License 2.0.

