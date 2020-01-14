# Liquid Syntax

Each file can be used to pattern match a single application (APK/classpath),
we call this file `AppSpec`.

Each `AppSpec` contains some `PatternDecl`s and some `ClassSpec`s.

## Basics

- `name`: alphabetic name pattern
- `id`: identifier pattern
- `expr`: expression pattern
  + literal
    + String literal
    + Boolean literal
    + Integer literal
  + Variable name

## Pattern Declaration

`PatternDecl` is the declaration of a pattern, which can be reused inside other parts of `AppSpec`.
Currently, there are following types of `PatternDecl`:

- `MethodSig` specifies a pattern named `<p>` (`name`) of method `<m>` (`id`) inside class `<c>` (`id`):
  + `methodSig <p>(<c>, <m>)`

## `ClassSpec`

Similar to the Java class, each `ClassSpec` specifies a list of attribute variables and a list
of `MethodSpec`.

Each attribute is a pair of type identifier and name identifier, e.g. `File _f`.

Each `MethodSpec` has a return type (`id`), a method name (`id`),
a list of parameters (each is a pair of type `id` and var `id`), and a method body (a list of `StatementSpec`)

`StatementSpec` specifies different types of statements:

- Invoke statements: `<f>(..., <args>);`
  - `<f>`: `name` name of method signature pattern
  - `<args>`: a list of `expr`s