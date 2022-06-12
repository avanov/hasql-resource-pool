# 0.5.3.1

* `acquireWithStripes` renamed to `aquireWith` and allows specifying a custom `ConnectionGetter`.


# 0.5.3

Initial release as a new package. The implementation is based on `hasql-pool` v0.5.2.2
and continues using `resource-pool` v0.2.x
(actually, a [fork with important performance and stats changes applied](https://github.com/bos/pool/pull/43)).
The long-term plan is to make a switch to a [newer v0.3.x maintained by scrive](https://github.com/bos/pool/pull/43).
