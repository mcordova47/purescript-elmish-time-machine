export const formatMessage_ = (full, msg) => full ? toString(msg, true) : msg.constructor.name

export const formatState_ = (state) => toString(state, true)

const toString = (obj, topLevel) =>
  ["number", "string", "boolean"].includes(typeof obj)
  ? JSON.stringify(obj)
  : obj.constructor.name === "Array"
  ? toStringArray(obj)
  : obj.constructor.name === "Object"
  ? toStringObject(obj)
  : toStringCustom(obj, topLevel)

const toStringArray = (arr) => `[${arr.map(toString).join(", ")}]`

const toStringObject = (obj) =>
  chain(Object.keys(obj)).chain((keys) =>
    keys.length === 0
    ? ""
    : keys.every((k) => /value\d+$/.test(k))
    ? keys.map((k) => toString(obj[k])).join(" ")
    : `{ ${keys.map(toStringKeyValue(obj)).join(", ")} }`
  ).result

const toStringKeyValue = (obj) => (k) => `${toString(k)}: ${toString(obj[k])}`

const toStringCustom = (obj, topLevel) =>
  topLevel
  ? `${obj.constructor.name} ${toStringObject(obj)}`
  : hasValues(obj)
  ? `(${obj.constructor.name} ${toStringObject(obj)})`
  : obj.constructor.name

const hasValues = (obj) =>
  chain(Object.keys(obj)).chain((keys) =>
    keys.length > 0 && keys.every((k) => /value\d+$/.test(k))
  ).result

const chain = (x) => ({
  chain: (fn) => chain(fn(x)),
  result: x
})
