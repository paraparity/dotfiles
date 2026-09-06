# Conventions for This Configuration

This artifact records several conventions and preferences I have for my Emacs configuration.

## File boundaries

- `early-init.el` - as little as possible. Only things that must run before package/UI
  initialization. Examples: GC deferral, disabling `package.el`'s automatic startup, and
  frame-alist tweaks to avoid a flash of unstyled Emacs.
- `init.el` - bootstrap only then hand off to the literate config.
- `configuration.org` - everything else, as a single literate file.

## Prefer `setopt` over `setq`

Default to `setopt` for setting variables as this ensures associated setters are invoked
where `setq` would skip them.

Note: `setopt` is **not** gated on the variable being a `defcustom`. It works on any symbol,
including plain `defvar`s and even undeclared ones.

What this gets us:

- If the variable has a declared `:type`, `setopt` validates the value against it (warns,
  doesn't block).
- If the variable has a `custom-set` function `setopt` invokes it, which runs the mode's
  real activation logic. Plain `setq` on these just flips the variable and silently skips 
  all of that.
- For automatically-buffer-local variables (e.g.`tab-width`, `fill-column`,
  `indent-tabs-mode`), `setopt` always calls `set-default`. This changes the value for every
  buffer that hasn't locally overridden it. A bare top-level `setq` on one of these only 
  patches whichever buffer happened to be current when that form was evaluated. Every other
  buffer keeps the built-in default.

### The mental model: it's about *scope*

Ask **where does this variable live at the point of the call**:

| Situation | Use | Why |
|---|---|---|
| Global option, top-level in this config (`defcustom` *or* plain `defvar`) | `setopt` | Validates type, and invokes any `custom-set` function. Matters most for minor-mode variables |
| A `let`-bound or function-local variable (lexical, no `defvar` in scope) | `setq` | `setopt` always dispatches to `set-default`/`custom-set`, which only touches the symbol's *global* value cell. It cannot see lexical bindings at all. Using `setopt` on a local variable silently leaves the local binding untouched and pollutes an unrelated global instead. |
| An auto-buffer-local variable, and the goal is a **global default** (`tab-width`, `fill-column`, `indent-tabs-mode`) | `setopt` (equivalent to `setq-default`) | Both route through `set-default`. Top-level `setq` is the trap here. See above. |
| An auto-buffer-local variable, and the goal is **this buffer only** (inside a command or hook, e.g. a mode hook lambda, or an interactive toggle) | `setq-local`, or plain `setq` if the hook guarantees the right buffer is already current | `setopt`/`setq-default` would change the global default instead of the current buffer |
| A variable whose declared `:type` doesn't match the value you're assigning (e.g. `org-agenda-custom-commands` holding `org-ql-block` sexps that don't match Org's declared type) | `setq` | `setopt` would only emit a harmless type-mismatch warning here, so `setq` avoids the noise with no behavioral downside. This is a deliberate, workaround. See the comment above `org-agenda-custom-commands` in `configuration.org`. |

### Quick heuristics

- Inside a `let`, a function body, or a lambda that closes over a local variable → `setq`.
- Setting a package/mode option at the top level of a `use-package` block or a plain
  top-level form in `configuration.org` → `setopt`.
- Setting something *inside* a hook or interactive command specifically so it applies to
  "the current buffer" (per-buffer overrides, per-project tool paths, toggle commands) →
  `setq-local` (or `setq`, if you're certain the right buffer is already current, e.g. most
  mode-hook lambdas).
- If in doubt, prefer `setopt` for anything you'd describe as "a setting", and `setq` for
  anything you'd describe as "local program state".

## Avoiding duplication

Outside of `org-capture-templates` and `org-agenda-custom-commands` avoid setting up the
same thing in two places. In particular:

- Don't `require`/`use-package` a library both explicitly and via a mechanism that already
  loads it (e.g. `org-modules`, or `org-babel-do-load-languages`, which already requires the
  matching `ob-*` library for every language marked `t`).
- If two packages solve the same problem (e.g. an HTTP client, a Cargo integration), pick one
  rather than configuring both "just in case."

## Package hygiene

- Prefer packages that are still actively maintained. Before adding or keeping a
  dependency, it's worth checking the installed snapshot date under `~/.emacs.d/elpa`.
- Prefer Emacs's own built-ins over third-party packages that duplicate core functionality
  under the same symbol names.
