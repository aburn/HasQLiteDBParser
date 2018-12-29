# sqlitedbparser

SQLite DB File reader written in Haskell.
Does *not* try to be intelligent with different different versions
of SQLite. Just a dumb app, to read a SQLite DB and dump out an AST following the documentation [here](https://sqlite.org/fileformat2.html).

For now: Assumes host machine is Little Endian.