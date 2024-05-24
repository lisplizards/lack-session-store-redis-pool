# lack-session-store-redis-pool

This project is a fork of [lack-session-store-redis](https://github.com/fukamachi/lack/blob/master/src/middleware/session/store/redis.lisp) that adds connection pooling using [anypool](https://github.com/fukamachi/anypool/).

## Usage

Wrap app:

```lisp
(funcall lack/middleware/session:*lack-middleware-session*
         *app*
         :store (lack/middleware/session/store/redis-pool:make-redis-store
                 :host "redis01.acme.example.com"
                 :max-open-count 10
                 :max-idle-count 3))
```

Lack Builder:

```lisp
(lack/builder
  (:session :store (lack/middleware/session/store/redis-pool:make-redis-store
                     :host "redis01.acme.example.com"
                     :max-open-count 10
                     :max-idle-count 3))
  *app*)
```

### Store options

* `HOST`: optional; the hostname of the Redis server to connect to (default: "127.0.0.1")
* `PORT`: optional; the port number of the Redis server to connect to (default: 6379)
* `AUTH`: optional; the Redis authentication password (default: NIL)
* `NAMESPACE`: optional; the key prefix under which to write all session data (default: "session")
* `EXPIRES`: optional; TTL for session data, in seconds (default: NIL)
* `SERIALIZER`: optional; function for serializing Lisp data to Redis (default uses cl-marshal)
* `DESERIALIZER`: optional; function for deserializing data from Redis back to Lisp (default uses cl-marshal)
* `POOL`: optional; provide an `ANYPOOL:POOL` instance directly and skip other pool-related options (default: NIL)
* `POOL-NAME`: optional; the name of the `ANYPOOL:POOL` instance (default: "lack-session-store-redis")
* `MAX-OPEN-COUNT`: optional; passed through to `ANYPOOL:MAKE-POOL` keyword arguments (default: 8)
* `MAX-IDLE-COUNT`: optional; passed through to `ANYPOOL:MAKE-POOL` keyword arguments (default: 4)
* `TIMEOUT`: optional; passed through to `ANYPOOL:MAKE-POOL` keyword arguments (default: 2000)
* `IDLE-TIMEOUT`: optional; passed through to `ANYPOOL:MAKE-POOL` keyword arguments (default: 60000)
* `ON-TOO-MANY-OPEN-CONNECTIONS`: optional; function called from `HANDLER-BIND` handler for `ANYPOOL:TOO-MANY-OPEN-CONNECTION` error before re-signalling as `REDIS-POOL-TIMEOUT-ERROR`; may be useful for publishing a metric or some other purpose

## Installation

Not in Quicklisp, so clone the repository to "local-projects/".

## Development

Run tests:

```lisp
(asdf:test-system :foo.lisp.lack-session-store-redis-pool)
```

## Dependencies

* [anypool](https://github.com/fukamachi/anypool)
* [cl-base64](https://github.com/darabi/cl-base64)
* [cl-redis](https://github.com/vseloved/cl-redis)
* [lack-middleware-session](https://github.com/fukamachi/lack/blob/master/lack-middleware-session.asd)
* [marshal](https://github.com/wlbr/cl-marshal)
* [trivial-utf-8](https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8)

### Tests

* [bordeaux-threads](https://github.com/sionescu/bordeaux-threads)
* [rove](https://github.com/fukamachi/rove)

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

* Eitaro Fukamachi (author of Lack, lack-session-store-redis)
* John Newton (adapter & maintainer)

Original source, by Eitaro Fukamachi: [lack/request](https://github.com/fukamachi/lack/blob/8243010b48a10edd527da4e94686b803c70731ef/src/middleware/session/store/redis.lisp)

Adaptations by John Newton:
* extract to separate git repository
* add connection pooling

## License

MIT
