# Irregular matrix fixture

- Generation
  - strict: rejects unsupported shape
  - preserve: keeps source encoding
  - json: emits a helper
  - component: projects a WIT face

## External close-out

If upstream closes:

1. verify the release
2. prune the workaround

| branch | action |
| --- | --- |
| fixed | remove |
| open | retain |

```toml
state = "marker-like <!-- gen:sh:roadmap-counts -->"
```

<!-- marker-like prose remains raw -->
123 features (95 RFC8610 + 1 RFC9682 + 27 `CDDL_CODEGEN` vendor profile), 136 containment cells, and 293 cddl-codegen annotations
all 37 IANA ops probed
6 divergences, all `preserve`-side
96 `class="constraint"` enforcement reject vectors over 20 enforce-green rows
