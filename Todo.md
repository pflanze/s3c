- number wrappers, to enforce type separation between uint and int
  etc.

- memory model: pointer wrappers, to enforce pointer type checking;
  etc.

- 'object' type: allow non-pointer objects as long as they fit the
  word size: needs knowledge about the host! How to handle?

- safe implementation: move type predicates to separate namespace
  (s3c-t#)

