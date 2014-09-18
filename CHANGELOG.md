# Dataquery v 0.1.0

Forked project and renamed to "dataquery". Changed to allow for asynchronous queries and external indexes. Removed all local storage capacity.

# 0.4.0

Cosmetic changes to better mimic Datomic API. Useful for sharing code between Datomic and DataScript:

- Added `tempid`, `resolve-tempid`, `db`, `transact`, `transact-async`, `index-range`, `squuid`, `squuid-time-millis`
- [ BREAKING ] renamed `transact` to `with`, `with` to `db-with`

# 0.3.1

- Optimized speed of DB’s `equiv` and `hash`, Datom’s `hash`
- Entity’s `touch` call accessible through `datascript` namespace
- Accept sets in entity maps as values for `:db.cardinality/many` attributes

# 0.3.0

Proper entities implementation:

- Entities are now lazy and implement usual Map protocols
- [ BREAKING ] When accessing attribute of `:db/valueType :db.type/ref`, its value will be automatically expanded to entites, allowing for recursive exploration of entities graphs (e.g. `(-> (d/entity db 42) :parent :parent :children)`)
- Entities support backwards navigation (e.g. `(:person/_friends (d/entity db 42))`)

# 0.2.1

- Externs file now can be referred as `:externs [datascript/externs.js"]`

# 0.2.0

Big performance improvements:

- New B-tree based indexes
- New set-at-a-time, hash-join powered query and rules engine
- Queries now up to 10× times faster
- You can specify DB for rule call (like `($db follows ?e1 ?e2)`)
- Datoms are required to have integer id and keyword attributes, but no restriction on types of values

# 0.1.6

- Clojure reader support (pr/read) for DB and Datom

# 0.1.5

- `datoms` and `seek-datoms` API calls
- referencing other entities’ tempids in transaction data (issue #10)

# 0.1.4

- Transactor functions via `:db.fn/call` (thx [@thegeez](https://github.com/thegeez))
- Vanilla JS API bindings
- [ BREAKING ] Schema keywords namespaced on a par with Datomic schema

# 0.1.3

- `entity` added
