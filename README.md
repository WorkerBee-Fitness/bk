# Beekeeper
The purpose of Beekeeper is to relieve anxiety by remembering things using a
simple command-line interface. Currently, the things it can remember for you are
bookmarks to locations of files + directories and aliases of commands. Then you
can easily recall your bookmarks or run your aliases by using the label you
create for them. The following video illustrates Beekeeper's features:

![Beekeeper Demi](doc/README/assets/bk-demo.gif)

# Installation

You will need [Stack](https://docs.haskellstack.org/en/stable/) a project
management tool for the [Haskell](https://www.haskell.org/) programming
language. Then clone this repository and run the following to install Beekeeper:

```.bash
stack build
stack install
```

After installation there is no setup necessary. Beekeeper saves all of it's data
to `~/.bk` by default. We strive to keep all of Beekeeper's data human readable
and not dependent on any heavy external libraries or tools. This is the reason
we chose [CSV](https://en.wikipedia.org/wiki/Comma-separated_values) as
Beekeeper's underlying data format.

# Usage

The following sections give examples of every feature Beekeeper supports.

## Adding a bookmark or alias
To add a bookmark use the following command:

```.bash
bk add bookmark LABEL=TARGET
```

the string `LABEL` is the name of the bookmark and its value is `TARGET`. For
example, `bk add bookmark math-proj="path/to/math/project"` will add a bookmark
with label `math-proj` whose target is `path/to/math/project`.

Adding an alias is very similar to adding a bookmark:

```.bash
bk add alias code-proj="code path/to/project"
```

The labels of bookmarks must begin with an alphabet symbol and then only consist
of alphanumeric symbols or underscore or hyphen. Targets are not allowed to be
empty.

The main difference between `bk add bookmark` and `bk add alias` is that when we
ask Beekeeper to run a bookmark its target is output to the terminal, but when
we ask Beekeeper to run an alias it will execute its target as a command.

## Running a bookmark or alias
To run a bookmark or alias we simply give `bk` the label of the bookmark:

```.bash
bk LABEL
```

Then if `LABEL` is a bookmark its target is output to the terminal, but if its
an alias its target is executed as a command.

The above is the same as `bk run LABEL`. 

Having the bookmark target simply output to the terminal means that we can use
it with other commands. For example,

```.bash
cd `bk LABEL`
```

will change directory into the target of `LABEL`. This makes it easy to inject
Beekeeper into your workflow on the command line.

# Searching for a bookmark or alias
Use:

```.bash
bk find LABEL
```

to search for the target whose label is `LABEL`. A more sophisticated search is
underdevelopment.

# Removing a bookmark or alias
Use:

```.bash
bk remove LABEL
```

to remove a bookmark or alias.

# Listing bookmarks
If you run Beekeeper with no arguments at all:

```.bash
bk
```

then it will list recent bookmarks. This is the list of all bookmarks and
aliases that were created within the last 10 days.

You can list just bookmarks using:

```.bash
bk bookmarks
```

and just aliases using:

```.bash
bk aliases
```

Use:

```.bash
bk list
```

to see a list of all bookmarks and aliases.


# Help and version
Use:

```.bash
bk help
```

for the help menu and use:

```.bash
bk version
```

to show the currently installed version.