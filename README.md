# Liquid Language

Liquid project includes a DSL for describing the low-level
pattern of Android/Java bytecode and a matching engine that matches
the pattern against given APK or classpath.

Liquid DSL features an intuitive syntax resembling the actual code to be
matched:

```
methodSig exec(_c, exec)

class _H {
    _ _f(...) {
        exec(...);
    }
}
```

This example pattern matches against arbitrary invocation of any method called
`exec`. The result of matching is a list of bindings to named wildcards:

```
[
  {
    "H" : "com.mobclick.android.MobclickAgent",
    "f" : "CatchLogError",
    "_r1" : "java.lang.String",
    "c" : "java.lang.Runtime"
  },
  ...
]
```

Note that `_r1` corresponds to the nameless wildcard at the return type position
of `_f`.

Liquid is built using Soot static analysis framework.

This is used in an internal research project, and more details will be published in future.

## Dependencies

Besides `build.sbt`, there are also local dependencies in `lib/`.
You should run `bash install.sh` in `lib/` to install them to your local
maven repo first.

## Toolkit

https://github.com/izgzhen/liquid-toolkit

## Contributions & Research

**Call for collaborators**: If you are interested in using/improving it for your own business, you should join us as a collaborator!
