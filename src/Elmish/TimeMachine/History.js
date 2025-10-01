export const formatMessage_ = (full, msg) => full ? toString(msg) : msg.constructor.name

export const formatState_ = (state) => toString(state)

const toString = (obj) =>
  ["number", "string", "boolean"].includes(typeof obj)
  ? JSON.stringify(obj)
  : obj.constructor.name === "Array"
  ? toStringArray(obj)
  : obj.constructor.name === "Object"
  ? toStringObject(obj)
  : toStringCustom(obj)

const toStringArray = (arr) => `[${arr.map(toString)}]`

const toStringObject = (obj) =>
  chain(Object.keys(obj)).chain((keys) =>
    keys.length === 0
    ? ""
    : keys.every((k) => /value\d+$/.test(k))
    ? keys.map((k) => toString(obj[k])).join(" ")
    : `{ ${keys.map(toStringKeyValue(obj)).join(", ")} }`
  ).result

const toStringKeyValue = (obj) => (k) => `${toString(k)}: ${toString(obj[k])}`

const toStringCustom = (obj) => `${obj.constructor.name} ${toStringObject(obj)}`

const chain = (x) => ({
  chain: (fn) => chain(fn(x)),
  result: x
})
