# Getting Started

1. Clone the repository with all submodules

```
git clone --recursive git@github.com:Furetur/itmo-stella-typechecker.git
```

> This project includes the `stella-tests` project as a submodule

2. Import the exact opam switch (installs the exact versions of all dependencies)

```
opam switch import --switch st_switch frozen_switch
```

> Alternatively, you can just install all dependencies on your current switch , but be aware that Stella_typechecker was only tested for OCaml 5.1.1: `opam install . --deps-only`

3. Activate the switch

```
opam switch st_switch

# Opam will ask you to run some eval command -- run it. For example
eval $(opam env)
```

4. Build the project

```
dune b
```

The binary will be located at `./_build/default/bin/main.exe`

5. Run tests

```
pytest .
```
