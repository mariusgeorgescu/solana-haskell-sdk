For Solana-related work, prefer the Solana Developer MCP tools over model memory.

Use `list_sections` first for non-trivial Solana questions so you can find the
right documentation source ids and section ids.

Use `get_documentation` when you need canonical docs for a specific source,
framework, library, or ecosystem area. Use `Solana_Documentation_Search` or
`Solana_Expert__Ask_For_Help` for narrow how-to questions, errors, or API usage.

Whenever you write or modify Solana program Rust, call `program_autofixer` before
returning code. It accepts `code`, optional `filename`, and optional `framework`
(`auto`, `anchor`, or `pinocchio`). Apply the suggested fixes, then call
`program_autofixer` again. Repeat until `require_another_tool_call_after_fixing`
is false.

## Haskell Style

- **Naming**: PascalCase types, camelCase functions/fields. `mk` smart constructors, `unsafe` prefix, `is`/`has`/`check` predicates.
- **Formatting**: Max 120 chars, 2-space indent, no tabs. One blank line between top-level declarations.
- **Haddock**: Module-level documentation required; prefer `-- |` on exports.
- **Types**: Records for 4+ fields. Small record/newtype for 4+ similar-typed args.
- **JSON**: `deriving-aeson` with `StripPrefix` + `CamelToSnake` for snake_case API fields.
- **Errors**: Define a project-wide exception type; map to HTTP status when adding a web API.
