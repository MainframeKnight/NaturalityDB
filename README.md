# NaturalityDB
NaturalityDB is a database management system for *categorical databases*.

The corresponding query language (NQL, Naturality Query Language) is a functional query language which
is used to access and/or modify data in categorical DBs. The data itself is stored in binary files along with its header info.

Some features of *NaturalityDB* are:

- **rich type system** for stored data: many type constructors common in functional programming make a return
in NQL, such as the *Maybe* type, *Lists* and *Tuples* in addition to data-based types like **Gen** (a generalization of `AUTOINCREMENT`
from SQL) and **Restrict** (type of values satisfying a given predicate);
- **pure-function** based approach to data quering and the **snapshot concept**: the majority of operations in NQL are
pure functions that take a current DB state (called a *snapshot*) and return a possibly modified snapshot. The I/O operations
available are `eval`, which prints the given value, `open`, which sets the current snapshot to DB stored in the specified files and
`commit`, which writes the current snapshot to the specified file;
- *first-class functions*: functions can be stored directly as data and lambdas are used to define parameters of data operations;
- *natural transactionality* acheived by `open` and `commit`, dual to `START TRANSACTION` and `COMMIT` from SQL: data is not written
to a file unless committed explicitly.