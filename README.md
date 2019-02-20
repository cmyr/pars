# pars – derive simple `FromStr` impls

**status: experimental**

This is an attempt to write a procedural macro that will derive simple template-based implementations of the FromStr trait for custom types.

## structure

- `pars-derive` contains the macro implementation
- `pars-test` exists because proc macros cannot be used inside the crates that declare them.
