# Stabilization findings (novice-user / bad-error sweep, 2026-08-27)

## C. Silent divergence from R (no error, quietly wrong)

| # | Trigger | Result | R gives |
|---|---------|--------|---------|
| C2 | `2000000000L + 2000000000L` | `-294967296` (signed overflow, UB) | `NA` + warning |
