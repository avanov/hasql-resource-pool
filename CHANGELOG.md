# 0.6.0.0

* Upgrade to support [`reasource-pool`](https://github.com/scrive/pool) from the new maintainer.
* `acquireWith` no longer accepts the stripes number, due to semantic changes in `resource-pool`.


# 0.5.4.1

* Upgrade to support breaking changes of `hasql-1.6.3`. No breaking changes introduced to the library.

# 0.5.3.1

* `acquireWithStripes` renamed to `aquireWith` and allows specifying a custom `ConnectionGetter`.


# 0.5.3

* Initial release as a new package. The implementation is based on `hasql-pool` v0.5.2.2
and continues using `resource-pool` v0.2.x
(actually, a [fork with important performance and stats changes applied](https://github.com/bos/pool/pull/43)).
* The long-term plan is to make a switch to a [newer v0.3.x maintained by scrive](https://github.com/bos/pool/pull/43).
