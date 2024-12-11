// § Utility Functions
/** Returns a pretty-print tree of the given Object `Obj`. */
const clog = console.log;
export function treestring<T extends Object>(
  Obj: T,
  cbfn?: (node: any) => void
) {
  const prefix = (key: keyof T, last: boolean) => {
    let str = last ? "└" : "├";
    if (key) str += "─ ";
    else str += "──┐";
    return str;
  };
  const getKeys = (obj: T) => {
    const keys: (keyof T)[] = [];
    for (const branch in obj) {
      if (!obj.hasOwnProperty(branch) || typeof obj[branch] === "function") {
        continue;
      }
      keys.push(branch);
    }
    return keys;
  };
  const grow = (
    key: keyof T,
    root: any,
    last: boolean,
    prevstack: [T, boolean][],
    cb: (str: string) => any
  ) => {
    cbfn && cbfn(root);
    let line = "";
    let index = 0;
    let lastKey = false;
    let circ = false;
    let stack = prevstack.slice(0);
    if (stack.push([root, last]) && stack.length > 0) {
      prevstack.forEach(function (lastState, idx) {
        if (idx > 0) line += (lastState[1] ? " " : "│") + "  ";
        if (!circ && lastState[0] === root) circ = true;
      });
      line += prefix(key, last) + key.toString();
      if (typeof root !== "object") line += ": " + root;
      circ && (line += " (circular ref.)");
      cb(line);
    }
    if (!circ && typeof root === "object") {
      const keys = getKeys(root);
      keys.forEach((branch) => {
        lastKey = ++index === keys.length;
        grow(branch, root[branch], lastKey, stack, cb);
      });
    }
  };
  let output = "";
  const obj = Object.assign({}, Obj);
  grow(
    "." as keyof T,
    obj,
    false,
    [],
    (line: string) => (output += line + "\n")
  );
  return output;
}

/** Typeguard : Returns true if `x` is Undefined or null. */
function isNothing(x: any): x is undefined | null {
  return x === undefined || x === null;
}

class None {
  _tag: "None" = "None";
  constructor() {}
  map(f: (a: never) => unknown): None {
    return new None();
  }
}

class Some<T> {
  readonly value: T;
  _tag: "Some" = "Some";
  constructor(value: T) {
    this.value = value;
  }
  map<S>(f: (a: T) => S): Some<S> {
    return new Some(f(this.value));
  }
}

function some<T>(value: T) {
  return new Some<T>(value);
}

function none() {
  return new None();
}

type Option<T> = None | Some<T>;

/** A data structure with two pointers, left and right. */
class Binode<T> {
  _data: T | null;
  private _R: Option<Binode<T>>;
  private _L: Option<Binode<T>>;
  constructor(data: T | null) {
    this._data = data;
    this._R = none();
    this._L = none();
  }
  /**
   * Returns a copy of this bnode.
   */
  copy() {
    const out = new Binode(this._data);
    const left = this._L;
    const right = this._R;
    out._L = left;
    out._R = right;
    return out;
  }
  /**
   * Flattens this bnode.
   */
  flatten(): Binode<T> | T {
    return this._data === null ? Binode.none<T>() : this._data;
  }
  map<K>(callback: (data: T) => K) {
    if (this._data) {
      return Binode.some<K>(callback(this._data));
    } else return Binode.none<K>();
  }
  /**
   * Sets the value of this bnode.
   */
  set value(data: T) {
    this._data = data;
  }

  do<K>(f: (d: T) => K) {
    if (this._data !== null) {
      f(this._data);
    }
    return this;
  }
  isSomething() {
    return this._data !== null;
  }
  isNothing() {
    return this._data === null;
  }
  static none<T>() {
    return new Binode<T>(null);
  }
  static some<T>(data: T) {
    return new Binode(data);
  }
  get _prev() {
    if (this._L._tag === "None") {
      return new Binode<T>(null);
    } else {
      return this._L.value;
    }
  }
  set _prev(node: Binode<T>) {
    this._L = some(node);
  }
  get _next() {
    if (this._R._tag === "None") {
      return new Binode<T>(null);
    } else {
      return this._R.value;
    }
  }
  set _next(node: Binode<T>) {
    this._R = some(node);
  }
  get _left() {
    return this._prev;
  }
  set _left(node: Binode<T>) {
    this._prev = node;
  }
  get _right() {
    return this._next;
  }
  set _right(node: Binode<T>) {
    this._next = node;
  }
}

function binode<T>(data: T | null = null) {
  return new Binode(data);
}

class LinkedList<T> {
  private head: Binode<T>;
  private tail: Binode<T>;
  private count: number;
  cdr() {
    const list = this.clone();
    if (list.isEmpty) return list;
    let previousHead = list.head;
    if (list.count === 1) {
      list.head = binode();
      list.tail = binode();
    } else {
      list.head = previousHead._right!;
      list.head._left = binode();
      previousHead._right = binode();
    }
    list.count--;
    return list;
  }
  car() {
    if (this.isEmpty) return this;
    const head = this.head._data!;
    return new LinkedList<T>().push(head);
  }
  clear() {
    this.head = binode();
  }
  get length() {
    return this.count;
  }
  get isEmpty() {
    return this.count === 0 || this.head.isNothing();
  }
  constructor() {
    this.count = 0;
    this.head = binode();
    this.tail = binode();
  }
  *[Symbol.iterator](): IterableIterator<T> {
    let node = this.head;
    while (node._data !== null) {
      yield node._data;
      node = node._right;
    }
  }
  toArray(): T[] {
    return [...this];
  }
  safeIdx(i: number) {
    return 0 <= i && i < this.count;
  }
  set(element: T, at: number) {
    const node = this.at(at);
    node._data = element;
    return this;
  }
  private at(index: number) {
    if (!this.safeIdx(index)) {
      return binode<T>();
    } else {
      let count = 0;
      let current = this.head;
      while (count !== index) {
        let k = current._right;
        if (k.isNothing()) break;
        current = k;
        count++;
      }
      return current;
    }
  }

  map<K>(f: (data: T, index: number, list: LinkedList<T>) => K) {
    const list = new LinkedList<K>();
    this.forEach((d, i, l) => list.push(f(d, i, l)));
    return list;
  }

  forEach(f: (data: T, index: number, list: LinkedList<T>) => void) {
    if (this.isEmpty) return this;
    let node = this.head;
    let i = 0;
    while (i < this.count) {
      node.do((d) => f(d, i, this));
      node = node._right;
      i++;
    }
  }

  clone() {
    const list = new LinkedList<T>();
    this.forEach((d) => list.push(d));
    return list;
  }
  #reduce<X>(
    from: 0 | 1,
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>
    ) => X,
    initialValue: X
  ) {
    let i = 0;
    const fn = (list: LinkedList<T>, init: X): X => {
      if (list.isEmpty) return init;
      else {
        const popped = list[from === 0 ? "shift" : "pop"]();
        if (popped._tag === "None") return init;
        const updatedValue = reducer(init, popped.value, i++, list);
        return fn(list, updatedValue);
      }
    };
    return fn(this.clone(), initialValue);
  }
  reduceRight<X>(
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>
    ) => X,
    initialValue: X
  ): X {
    return this.#reduce(1, reducer, initialValue);
  }
  reduce<X>(
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>
    ) => X,
    initialValue: X
  ): X {
    return this.#reduce(0, reducer, initialValue);
  }

  /** Returns the string representation of this list, with each element jointed by the given separator (defaults to the empty string). */
  join(separator: string = "") {
    return [...this].join(separator);
  }

  /** Returns th string representation of this list. */
  toString(f?: (x: T, index: number) => string) {
    const out = this.clone();
    const g = f ? f : (x: T, index: number) => x;
    return "(" + out.map((d, i) => g(d, i)).join(",") + ")";
  }

  /** Returns a new list whose elements satisfy the given predicate. */
  filter(predicate: (value: T, index: number, list: LinkedList<T>) => boolean) {
    const out = new LinkedList<T>();
    this.forEach((n, i, list) => predicate(n, i, list) && out.push(n));
    return out;
  }

  /** Reverses this list. */
  reverse() {
    let current = this.head;
    let i = 0;
    while (current.isSomething() && i < this.count) {
      const right = current._right;
      current._right = current._left;
      current._left = right;
      let k = current._left;
      if (k.isNothing() || i > this.count) break;
      current = k;
      i++;
    }
    const tail = this.tail;
    this.tail = this.head;
    this.head = tail;
    return this;
  }

  /** Returns the element at the given index. */
  item(index: number) {
    return this.at(index)._data;
  }

  zip<K>(list: LinkedList<K>) {
    const out = new LinkedList<[T, K]>();
    this.forEach((d, i) => {
      const x = list.item(i);
      if (x !== null) {
        const element: [T, K] = [d, x] as [T, K];
        out.push(element);
      }
    });
    return out;
  }

  /** Removes the last element of this list. */
  pop(): Option<T> {
    if (this.isEmpty) return none();
    let popped = this.tail;
    if (this.length === 1) {
      this.head = binode();
      this.tail = binode();
    } else {
      this.tail = popped._left;
      this.tail._right = binode();
      popped._left = binode();
    }
    this.count--;
    return popped._data === null ? none() : some(popped._data);
  }

  /** Inserts the given element at the head of this list.*/
  unshift(element: T) {
    const node = binode(element);
    if (this.isEmpty) {
      this.head = node;
      this.tail = node;
    } else {
      this.head._prev = node;
      node._next = this.head;
      this.head = node;
    }
    this.count++;
    return this;
  }

  /** Removes the first element of this list. */
  shift() {
    if (this.isEmpty) return none();
    const previousHead = this.head;
    if (this.length === 1) {
      this.head = binode();
      this.tail = binode();
    } else {
      this.head = previousHead._next;
      this.head._prev = binode();
      previousHead._prev = binode();
    }
    this.count--;
    return previousHead._data === null ? none() : some(previousHead._data);
  }

  /** Inserts the given element to this list. */
  push(element: T) {
    const node = binode(element);
    if (this.head.isNothing()) {
      this.head = node;
      this.tail = node;
    } else {
      this.tail._next = node;
      node._prev = this.tail;
      this.tail = node;
    }
    this.count++;
    return this;
  }

  /** Inserts the given elements to this list. */
  append(...elements: T[]) {
    elements.forEach((e) => this.push(e));
    return this;
  }
}

function linkedList<T>(...elements: T[]) {
  return new LinkedList<T>().append(...elements);
}

type Either<A, B> = Left<A> | Right<B>;

/** An object corresponding to failure. */
class Left<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<A>(): Either<T, never> {
    return this as any;
  }
  isLeft(): this is Left<T> {
    return true;
  }
  isRight(): this is never {
    return false;
  }
  unwrap() {
    return this.value;
  }
  chain<X, S>(): Left<T> {
    return this;
  }
}

/** Returns a new Left with value `x` of type `T`. */
const left = <T>(x: T) => new Left(x);

/** An object corresponding to success. */
class Right<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<X>(f: (x: T) => X): Either<never, X> {
    return new Right(f(this.value));
  }
  isLeft(): this is never {
    return false;
  }
  isRight(): this is Right<T> {
    return true;
  }
  unwrap() {
    return this.value;
  }
  chain<N, X>(f: (x: T) => Either<N, X>): Either<never, X> {
    return f(this.value) as Either<never, X>;
  }
}

/** Returns a new Right with value `x` of type `T`. */
const right = <T>(x: T) => new Right(x);

type NumberTokenType =
  | token_type.integer
  | token_type.float
  | token_type.big_integer
  | token_type.scientific
  | token_type.fraction;

type ErrorType =
  | "lexical-error"
  | "syntax-error"
  | "type-error"
  | "runtime-error"
  | "environment-error"
  | "algebra-error"
  | "resolver-error";

// § Error Handling
/** An object corresponding to an error in Winnow. */
class Err extends Error {
  /** This error's message. */
  message: string;
  /** This error's type. */
  $type: ErrorType;
  /** The line where this error occurred. */
  $line: number;
  constructor(message: string, type: ErrorType, line: number) {
    super(message);
    this.message = message;
    this.$type = type;
    this.$line = line;
  }
  toString() {
    const a = this.$type === "environment-error" ? "an" : "a";
    return `On line ${this.$line}, ${a} ${this.$type} occured: ${this.message}`;
  }
}

/** Generates error-making functions. */
const errorFactory = (type: ErrorType) => (message: string, line: number) =>
  new Err(message, type, line);

/** Returns a new lexical error. */
const lexicalError = errorFactory("lexical-error");

/** Returns a new syntax error. */
const syntaxError = errorFactory("syntax-error");

/** Returns a new runtime error. */
const runtimeError = errorFactory("runtime-error");

/** Returns a new resolver error. */
const resolverError = errorFactory("resolver-error");

/** Returns a new environment error. */
const envError = errorFactory("environment-error");

const algebraError = (message: string) => new Err(message, "algebra-error", 0);

/** Returns true if the given expression u is an Err object. */
export function isErr(u: any): u is Err {
  return u instanceof Err;
}

export const {
  floor,
  abs,
  min,
  max,
  PI,
  E,
  tan,
  sin,
  cos,
  cosh,
  sinh,
  tanh,
  log2: lg,
  log: ln,
  log10: log,
  acosh: arccosh,
  asinh: arcsinh,
  atan: arctan,
  sign,
  ceil,
  sqrt,
} = Math;

export const HALF_PI = PI / 2;
export const TWO_PI = 2 * PI;

/** Converts the provided number (assumed to be radians) to degrees. */
export function toDegrees(radians: number) {
  return radians * (180 / Math.PI);
}

/** Converts the provided number (assumed to be degrees) to radians. */
export function toRadians(degrees: number) {
  return degrees * (Math.PI / 180);
}

/** Returns the arccosine of x. */
function arccos(x: number) {
  return x > 1 ? 0 : x < -1 ? PI : Math.acos(x);
}

/** Returns the arcsine of x. */
function arcsin(x: number) {
  return x >= 1 ? HALF_PI : x <= -1 ? -HALF_PI : Math.asin(x);
}

/** Returns the integer quotient of a/b. If b = 0, returns NaN. */
function iquot(a: number, b: number) {
  return b === 0 ? NaN : Math.floor(a / b);
}

/** Returns a % b. */
function mod(dividend: number, divisor: number) {
  return ((dividend % divisor) + divisor) % divisor;
}

/** Returns the a% of b. */
function percent(a: number, b: number) {
  return (a / 100) * b;
}

/** Returns the greatest common denominator of a and b. */
function gcd(a: number, b: number) {
  let t: number;
  while (b !== 0) {
    t = b;
    b = a % b;
    a = t;
  }
  return a;
}

/** Computes the arithmetic mean of the given list of numbers. */
function avg(...nums: number[]) {
  let sum = 0;
  for (let i = 0; i < nums.length; i++) {
    sum += nums[i];
  }
  return sum / nums.length;
}

/** Returns the factorial of the given number. */
function factorialize(num: number) {
  if (num === 0 || num === 1) {
    return 1;
  }
  for (var i = num - 1; i >= 1; i--) {
    num *= i;
  }
  return num;
}

/** Rounds the given number value to the number of given decimal places. */
function round(num: number, places: number = 2) {
  const epsilon = Number.EPSILON;
  return Math.round(num * 10 ** places * (1 + epsilon)) / 10 ** places;
}

/** Transforms 1-based indices to 0-based indices. */
function i0(value: number) {
  return value === 0 ? 0 : value - 1;
}

/** Returns a random integer between the provided minimum and maximum (not including the maximum). */
export function randInt(min: number, max: number) {
  return floor(Math.random() * (max - min + 1)) + min;
}

/** Returns a random floating point number between the provided minimum and maximum (not including the maximum). */
export function randFloat(min: number, max: number) {
  return Math.random() * (max - min) + min;
}

class Vector<T extends number[] = number[]> {
  $elements: T;

  /** Returns the length of this vector. */
  get length() {
    return this.$elements.length;
  }

  constructor(elements: T) {
    this.$elements = elements;
  }

  /** Returns a string representation of this vector. */
  toString() {
    return "[" + this.$elements.toString() + "]";
  }

  /** Utility method for performing binary operations. */
  binop(
    other: Vector | number[] | number,
    op: (a: number, b: number) => number
  ) {
    const arg = isNumber(other)
      ? homogenousVector(other, this.length)
      : vector(other);
    const [A, B] = equalen(this, arg);
    return vector(A.$elements.map((c, i) => op(c, B.$elements[i])));
  }

  /** Returns the smallest component of this vector. */
  min() {
    let min = Infinity;
    for (let i = 0; i < this.$elements.length; i++) {
      const elem = this.$elements[i];
      if (elem < min) {
        min = elem;
      }
    }
    return min;
  }

  /** Returns the largest component of this vector. */
  max() {
    let max = -Infinity;
    for (let i = 0; i < this.$elements.length; i++) {
      const elem = this.$elements[i];
      if (elem > max) {
        max = elem;
      }
    }
    return max;
  }

  /** Returns the magnitude of this vector.  An optional precision value may be passed roundingthe magnitude to a specified number of decimal places. */
  mag(precision?: number) {
    const out = sqrt(this.$elements.reduce((p, c) => p + c ** 2, 0));
    return !isNothing(precision) ? round(out, floor(precision)) : out;
  }

  /** Returns the difference between this vector and the provided argument. If a number is passed, returns the scalar difference. */
  sub(other: Vector | number[] | number) {
    return this.binop(other, (a, b) => a - b);
  }

  /** Returns the product between this vector and the provided argument. If a number is passed, returns the scalar difference. */
  mul(other: Vector | number[] | number) {
    return this.binop(other, (a, b) => a * b);
  }

  /** Returns this pair-wise power of this vector to the provided argument. If a number is passed, returns the scalar difference. */
  pow(other: Vector | number[] | number) {
    return this.binop(other, (a, b) => a ** b);
  }

  /** Returns the sum of this vector and the provided argument. If a number is passed, returns the scalar difference. */
  add(other: Vector | number[] | number) {
    return this.binop(other, (a, b) => a + b);
  }

  /** Returns the component-wise division of this vector. */
  div(other: Vector | number[] | number, alt: number = 0.0001) {
    return this.binop(other, (a, b) => (b === 0 ? a / alt : a / b));
  }

  /** Magnifies this vector by the given magnitude. */
  magnify(magnitude: number) {
    const mag = this.mag();
    const ratio = magnitude / mag;
    return this.mul(ratio);
  }

  /** Returns this vector with each component squared. */
  square() {
    return this.mul(this);
  }

  /** Returns the negation of this vector. */
  neg() {
    return vector(this.$elements.map((c) => -c));
  }

  /** Returns this vector with each component set to its absolute value. */
  abs() {
    return vector(this.$elements.map((c) => Math.abs(c)));
  }

  /** Returns true if this vector equals the provided vector. */
  equals(that: Vector) {
    if (this.length !== that.length) return false;
    for (let i = 0; i < this.length; i++) {
      const e1 = this.$elements[i];
      const e2 = that.$elements[i];
      if (e1 !== e2) return false;
    }
    return true;
  }

  /** Returns true if every component of this vector is zero. */
  isZero() {
    for (let i = 0; i < this.length; i++) {
      if (this.$elements[i] !== 0) return false;
    }
    return true;
  }

  /** Returns true if this vector comprises exactly two elements. */
  is2D(): this is Vector<[number, number]> {
    return this.$elements.length === 2;
  }

  /** Returns true if this vector comprises exactly three elements. */
  is3D(): this is Vector<[number, number, number]> {
    return this.$elements.length === 3;
  }

  /** Returns a copy of this vector. */
  copy() {
    const elements = [];
    for (let i = 0; i < this.$elements.length; i++) {
      elements.push(this.$elements[i]);
    }
    return new Vector(elements);
  }

  /** Appends the given value by the provided number of slots. */
  pad(slots: number, value: number) {
    if (slots < this.length) {
      const diff = this.length - slots;
      const elements = [...this.$elements];
      for (let i = 0; i < diff; i++) {
        elements.push(value);
      }
      return new Vector(elements);
    }
    return this.copy();
  }

  /** Sets the element at the given position to the provided value. Indices start at 1. If the index is greater than the current size of this vector, the vector will insert additional zeros up to the given index to ensure its elements array is contiguous. */
  set(index: number, value: number) {
    index = i0(index);
    if (index > this.length) {
      const diff = index - this.length;
      const vector = this.pad(diff, 0);
      vector.$elements[index] = value;
      return vector;
    }
    const copy = this.copy();
    copy.$elements[index] = value;
    return copy;
  }

  /** Returns the dot product of this vector and the provided vector. */
  dot(vector: Vector | number[]) {
    const other = Vector.from(vector);
    const order = this.length;
    if (other.length !== order) return 0;
    let sum = 0;
    for (let i = 0; i < order; i++) {
      const a = this.$elements[i];
      const b = other.$elements[i];
      const p = a * b;
      sum += p;
    }
    return sum;
  }

  /** Returns the angle between the two provided vectors. */
  theta(other: Vector) {
    const ab = this.dot(other);
    const mag = this.mag();
    const factor = ab / mag;
    return Math.acos(factor);
  }

  /** Sets the first element of this vector to the provided value. */
  px(value: number) {
    return this.set(1, value);
  }

  /** Returns the first element of this vector. */
  get $x() {
    return isNothing(this.$elements[0]) ? 0 : this.$elements[0];
  }
  set $x(n: number) {
    this.$elements[0] = n;
  }

  /** Sets the second element of this vector to the provided value. */
  py(value: number) {
    return this.set(2, value);
  }

  /** Returns the second element of this vector. */
  get $y() {
    return isNothing(this.$elements[1]) ? 0 : this.$elements[1];
  }
  set $y(n: number) {
    this.$elements[1] = n;
  }

  /** Sets the third element of this vector to the provided value. */
  pz(value: number) {
    return this.set(3, value);
  }

  /** Returns the third element of this vector. */
  get $z() {
    return isNothing(this.$elements[2]) ? 0 : this.$elements[2];
  }
  set $z(z: number) {
    this.$elements[2] = z;
  }

  /** Sets the fourt element of this vector to the provided value. */
  pw(value: number) {
    return this.set(4, value);
  }

  /** Returns the fourth element of this vector. */
  get $w() {
    return isNothing(this.$elements[3]) ? 0 : this.$elements[3];
  }
  set $w(w: number) {
    this.$elements[3] = w;
  }

  /** Returns the angle between (a) the difference vector of this vector and the provided vector, and (b) the x-axis. */
  gamma(other: Vector) {
    const dx = this.$x - other.$x;
    const dy = this.$y - other.$y;
    const gamma = Math.atan2(dy, dx);
    return gamma;
  }

  /** Returns the element at the given index (indices start at 1). */
  element(index: number) {
    const out = this.$elements[index - 1];
    return out !== undefined ? out : null;
  }

  /** Returns this vector as a number array. */
  toArray() {
    return this.$elements.map((e) => e);
  }

  /** Returns the unit vector point from this vector 𝑢 to the provided 𝑣. */
  normalTo(v: Vector) {
    const d = this.sub(v);
    return d.normalize();
  }

  /** Returns this vector’s normal. */
  normalize() {
    if (this.isZero()) return this;
    return this.div(this.mag());
  }

  /** Returns the 2D vector normal of this vector. */
  normal2D() {
    return vector([-this.$y, this.$x]);
  }

  /** Returns the cross product of this vector in-place. The cross product is used primarily to compute the vector perpendicular to two vectors. */
  cross(other: Vector) {
    const ax = this.$x;
    const ay = this.$y;
    const az = this.$z;
    const bx = other.$x;
    const by = other.$y;
    const bz = other.$z;
    const cx = ay * bz - az * by;
    const cy = az * bx - ax * bz;
    const cz = ax * by - ay * bx;
    return vector([cx, cy, cz]);
  }

  /** Returns the 2D distance between this vector and the provided vector. */
  distance2D(other: Vector) {
    const dx = other.$x - this.$x;
    const dy = other.$y - this.$y;
    const dsum = dx ** 2 + dy ** 2;
    return Math.sqrt(dsum);
  }

  /** Returns the 3D distance between this vector and the provided vector. */
  distance3D(other: Vector) {
    const x = other.$x - this.$x;
    const y = other.$y - this.$y;
    const z = other.$z - this.$z;
    const xyz = x * x + y * y + z * z;
    return Math.sqrt(xyz);
  }

  /** Returns the projection of this vector (𝑏) onto the provided vector (𝑎) (projₐ𝑏). That is, the projection of 𝑏 onto 𝑎. */
  project(a: Vector): Vector {
    const b = this.copy();
    const prod = a.dot(b);
    const mag = a.mag();
    const mag2 = mag * mag;
    const factor = prod / mag2;
    const res = a.mul(factor);
    return res;
  }

  /** Returns a random 2D vector. The `min` argument is the lower bound of the sampling interval. The `max` argument is The upper bound of the sampling interval. The `restrict` argument takes string values `Z` or `R`. If `Z` is passed, random values are restricted to integers. If `R` is passed, random values are either integers or floats. */
  static random2D(min: number, max: number, restrict: "Z" | "R" = "R") {
    const rfn = restrict === "Z" ? randInt : randFloat;
    const x = rfn(min, max);
    const y = rfn(min, max);
    return new Vector([x, y]);
  }

  /** Returns a random 3D vector. The `min` argument sets the lower bound of the sampling interval. The `max` argument sets the upper bound of the sampling interval. The `restrict` argument takes `Z` or `R`. If `Z` is passed, random values are restricted to integers. If `R` is passed, random values are either integers or floats. */
  static random3D(min: number, max: number, restrict: "Z" | "R" = "R") {
    const v = Vector.random2D(min, max, restrict);
    const x = v.$x;
    const y = v.$y;
    const z = restrict === "Z" ? randInt(min, max) : randFloat(min, max);
    return new Vector([x, y, z]);
  }

  /** Returns a new vector from the given array of numbers or `Vector`. If a `Vector` is passed, returns a copy of that vector. */
  static from(value: number[] | Vector): Vector {
    if (Array.isArray(value)) {
      return new Vector(value);
    } else {
      return value.copy();
    }
  }
}

/** Returns a new vector. If a vector is passed, returns the vector (an identity function). */
function vector(elements: number[] | Vector) {
  if (Array.isArray(elements)) {
    return new Vector(elements);
  } else {
    return elements;
  }
}

/** Returns a new vector of size `length`, where each element is the given `value`.*/
function homogenousVector(value: number, length: number) {
  const elements = [];
  for (let i = 0; i < length; i++) {
    elements.push(value);
  }
  return new Vector(elements);
}

/** Given `vectorA` and `vectorB`, ensures that `vectorA` and `vectorB` have the same sizes (number of elements). If one is smaller than the other, the shorter is padded with additional zeros to ensure the lengths are the same. */
function equalen(vectorA: Vector, vectorB: Vector): [Vector, Vector] {
  const A = [];
  const B = [];
  if (vectorA.length > vectorB.length) {
    let i = 0;
    for (i = 0; i < vectorA.length; i++) {
      A.push(vectorA.$elements[i]);
      B.push(isNothing(vectorB.$elements[i]) ? 0 : vectorB.$elements[i]);
    }
    const n = vectorB.length - i;
    for (let j = 0; j < n; j++) {
      B.push(0);
    }
    return [vector(A), vector(B)];
  } else if (vectorA.length < vectorB.length) {
    let i = 0;
    for (i = 0; i < vectorB.length; i++) {
      A.push(isNothing(vectorA.$elements[i]) ? 0 : vectorA.$elements[i]);
      B.push(vectorB.$elements[i]);
    }
    const n = vectorB.length - i;
    for (let j = 0; j < n; j++) {
      A.push(0);
    }
    return [vector(A), vector(B)];
  } else {
    return [vectorA, vectorB];
  }
}

/** Returns true if the given value is a vector, false otherwise. */
const isVector = (value: any): value is Vector => value instanceof Vector;

function isNumber(u: any): u is number {
  return typeof u === "number";
}

function isString(u: any): u is string {
  return typeof u === "string";
}

class Matrix {
  $vectors: Vector[];
  readonly $R: number;
  readonly $C: number;
  constructor(vectors: Vector[], rowcount: number, colcount: number) {
    this.$vectors = vectors;
    this.$R = rowcount;
    this.$C = colcount;
  }

  /** Returns true if this matrix is a square matrix. */
  isSquare() {
    return this.$C === this.$R;
  }

  /** Returns a copy of this matrix. */
  copy() {
    const vs = this.$vectors.map((v) => v.copy());
    return new Matrix(vs, this.$R, this.$C);
  }

  /** Returns the vector element at the given index (indices start at 1). */
  element(index: number) {
    const out = this.$vectors[index - 1];
    return out !== undefined ? out : null;
  }

  /** Returns a column vector comprising all the vector elements at the given column. */
  column(index: number) {
    if (index > this.$C) {
      const out: number[] = [];
      for (let i = 0; i < this.$C; i++) {
        out.push(0);
      }
      return vector(out);
    }
    const out: number[] = [];
    this.$vectors.forEach((vector) => {
      vector.$elements.forEach((n, i) => {
        if (i === index) out.push(n);
      });
    });
    return vector(out);
  }

  /** Returns the nth element at the given row index and column index. An optional fallback value (defaulting to 0) may be provided in the event the indices are out of bounds. */
  n(rowIndex: number, columnIndex: number, fallback: number = 0) {
    const out = this.element(rowIndex);
    if (out === null) return fallback;
    const n = out.element(columnIndex);
    return isNumber(n) ? n : fallback;
  }

  /** Returns the string form of matrix. */
  toString() {
    const out = this.$vectors.map((v) => v.toString()).join(",");
    return `[${out}]`;
  }

  /** Sets the element at the given row index and column index. The row and column indices are expected to begin at 1. If no element exists at the provided indices, no change is done. */
  set(row: number, column: number, value: number) {
    if (this.$vectors[row - 1] === undefined) return this;
    if (this.$vectors[row - 1].$elements[column - 1] === undefined) return this;
    const copy = this.copy();
    copy.$vectors[row - 1].$elements[column - 1] = value;
    return copy;
  }

  /** Executes the given callback over each element of this matrix. The row and column index provided in the callback begin at 1. */
  forEach(
    callback: (element: number, rowIndex: number, columnIndex: number) => void
  ) {
    for (let i = 1; i <= this.$R; i++) {
      for (let j = 1; j <= this.$C; j++) {
        callback(this.n(i, j), i, j);
      }
    }
    return this;
  }

  /** Returns true if this matrix and the the provided matrix have the same number of rows and the same number of columns. False otherwise. */
  congruent(matrix: Matrix) {
    return this.$R === matrix.$R && this.$C === matrix.$C;
  }

  /** @internal - Utility method for binary operations on matrices. */
  private binop(
    arg: number | number[][] | Matrix,
    op: (a: number, b: number) => number
  ) {
    const other = isNumber(arg)
      ? Matrix.fill(this.$R, this.$C, arg)
      : Array.isArray(arg)
      ? Matrix.from(arg)
      : arg;
    if (this.$R !== other.$R || this.$C !== other.$C) return this;
    const vectors: Vector[] = [];
    for (let i = 0; i < this.$R; i++) {
      const nums: number[] = [];
      const row = this.$vectors[i].$elements;
      for (let j = 0; j < row.length; j++) {
        const a = row[j];
        const b = other.$vectors[i].$elements[j];
        nums.push(op(a, b));
      }
      vectors.push(vector(nums));
    }
    return matrix(vectors);
  }

  /** Returns this matrix minus the provided matrix. */
  sub(matrix: Matrix | number | number[][]) {
    return this.binop(matrix, (a, b) => a - b);
  }

  /** Returns this matrix component-wise-multiplied with provided matrix. */
  times(matrix: Matrix | number | number[][]) {
    return this.binop(matrix, (a, b) => a * b);
  }

  /** Returns this matrix plus the provided matrix. */
  add(matrix: Matrix | number | number[][]) {
    return this.binop(matrix, (a, b) => a + b);
  }

  /** Returns the negation of this matrix.  */
  neg() {
    return this.times(-1);
  }

  /** Returns the transpose of this matrix. */
  transpose() {
    const copy: number[][] = [];
    for (let i = 0; i < this.$R; ++i) {
      const vector = this.$vectors[i];
      for (let j = 0; j < this.$C; ++j) {
        const element = vector.$elements[j];
        if (isNothing(element)) continue;
        if (isNothing(copy[j])) {
          copy[j] = [];
        }
        copy[j][i] = element;
      }
    }
    return matrix(copy.map((c) => vector(c)));
  }

  /** Returns the matrix product of this matrix and the provided matrix. */
  mul(arg: number | Matrix | number[][]) {
    const Ar = this.$R;
    const Ac = this.$C;
    if (arg instanceof Matrix && Ac !== arg.$R) {
      return this;
    }
    const B = Matrix.of(Ar, Ac, arg);
    const Bc = B.$C;
    const result: number[][] = [];
    for (let i = 0; i < Ar; i++) {
      result[i] = [];
      for (let j = 0; j < Bc; j++) {
        let sum = 0;
        for (let k = 0; k < Ac; k++) {
          const a = this.$vectors[i].$elements[k];
          const b = B.$vectors[k].$elements[j];
          sum += a * b;
        }
        result[i][j] = sum;
      }
    }
    return matrix(result.map((r) => vector(r)));
  }

  /** Returns true if this matrix and the provided matrix are equal. */
  equals(matrix: Matrix) {
    if (!this.congruent(matrix)) return false;
    let out = true;
    this.forEach((n, r, c) => {
      const m = matrix.n(r, c);
      if (m !== n) out = false;
    });
    return out;
  }

  static fill(rows: number, columns: number, arg: number) {
    const vectors: Vector[] = [];
    for (let i = 0; i < rows; i++) {
      const nums: number[] = [];
      for (let j = 0; j < columns; j++) {
        nums.push(arg);
      }
      vectors.push(vector(nums));
    }
    return matrix(vectors);
  }

  static from(nums: number[][]) {
    const out = nums.map((ns) => vector(ns));
    return matrix(out);
  }

  static of(rows: number, columns: number, arg: number | number[][] | Matrix) {
    return isNumber(arg)
      ? Matrix.fill(rows, columns, arg)
      : Array.isArray(arg)
      ? Matrix.from(arg)
      : arg;
  }
}

/** Returns a new matrix. */
function matrix(rows: Vector[] | number[][], cols?: number) {
  const vectors = rows.map((v) => (isVector(v) ? v : Vector.from(v)));
  return new Matrix(
    vectors,
    vectors.length,
    cols !== undefined ? cols : vectors[0].length
  );
}

/** Returns true if the given value is a matrix. */
const isMatrix = (value: any): value is Matrix => value instanceof Matrix;

/** An object corresponding to a number of the form `m x 10^n`. */
class Exponential {
  /** The mantissa in `m x 10^n`. */
  $m: number;
  /** The exponent (an integer) in `m x 10^n` */
  $n: number;
  constructor(m: number, n: number) {
    this.$m = m;
    this.$n = Math.floor(n);
  }
  toString() {
    return `${this.$m}E{${this.$n}}`;
  }
  copy() {
    return new Exponential(this.$m, this.$n);
  }
  negate() {
    return new Exponential(-this.$m, this.$n);
  }
}

/**
 * Returns a new instance of a Scientific_Number.
 * @param m The mantissa in `m x 10^n`.
 * @param n The exponent (an integer) in `m x 10^n`.
 * @returns A new scientific number (m x 10^n).
 */
function expo(m: number, n: number) {
  return new Exponential(m, Math.floor(n));
}

/** Returns true, and asserts, if the given expression u is an exponential. */
function isExponential(u: any): u is Exponential {
  return u instanceof Exponential;
}

// § Graphics

/** A value native to Winnow. */
type Primitive =
  | number
  | string
  | null
  | boolean
  | bigint
  | Exponential
  | FRACTION
  | Obj
  | Vector
  | Matrix
  | LinkedList<Primitive>
  | Fn
  | Class
  | Err;

/** Returns true iff `x` is null. */
function isNull(x: any): x is null {
  return x === null;
}

// § Token Types

enum token_type {
  // Utility tokens
  end,
  error,
  empty,
  // Paired Delimiters
  left_paren,
  right_paren,
  left_brace,
  right_brace,
  left_bracket,
  right_bracket,
  // Single Delimiters
  semicolon,
  colon,
  dot,
  comma,
  // Operator Delimiters
  plus,
  minus,
  star,
  slash,
  caret,
  percent,
  bang,
  ampersand,
  tilde,
  vbar,
  equal,
  less,
  greater,
  less_equal,
  greater_equal,
  bang_equal,
  equal_equal,
  plus_plus,
  minus_minus,
  star_star,
  // Vector Operators
  dot_add,
  dot_star,
  dot_minus,
  dot_caret,
  at,
  // Matrix Operators
  pound_plus, // '#+'
  pound_minus, // '#-
  pound_star, // '#*'
  // Literals
  integer,
  float,
  fraction,
  scientific,
  big_integer,
  symbol,
  string,
  boolean,
  nan,
  inf,
  nil,
  numeric_constant,
  algebraic,
  // Keyword Tokens
  and,
  or,
  not,
  nand,
  xor,
  xnor,
  nor,
  if,
  else,
  fn,
  let,
  var,
  return,
  while,
  for,
  class,
  print,
  super,
  this,
  rem,
  mod,
  div,
  native,
  // algebra strings
  algebra_string,
}

// § Token Object

/** An object corresponding to a token in Winnow. */
class Token<
  T extends token_type = token_type,
  L extends Primitive = Primitive
> {
  /** This token's type. */
  $type: T;

  /** This tokene's lexeme. */
  $lexeme: string;

  /** This token's literal value, if any (defaults to null). */
  $literal: L = null as any;

  /** The line where this token was first encountered. */
  $line: number;

  constructor(type: T, lexeme: string, line: number) {
    this.$type = type;
    this.$lexeme = lexeme;
    this.$line = line;
  }

  /** Returns a copy of this token. */
  copy() {
    return new Token(this.$type, this.$lexeme, this.$line);
  }

  /**
   * Returns a copy of this token with a new token type.
   * @param tokenType The new token type.
   * @returns A copy of this token with the given token type.
   */
  withType<X extends token_type>(tokenType: X) {
    const out = this.copy();
    out.$type = tokenType as any;
    return out as any as Token<X, L>;
  }

  /**
   * Returns a copy of this token with a new lexeme.
   * @param lexeme The new lexeme, a string value.
   * @returns A copy of this token with the given string as its lexeme.
   */
  withLexeme(lexeme: string) {
    const out = this.copy();
    out.$lexeme = lexeme;
    return out;
  }

  /**
   * Returns a copy of this token with a new literal value.
   * @param primitive The new literal value, a Primitive.
   * @returns A copy of this token with the given literal as its literal.
   */
  withLiteral<L2 extends Primitive>(primitive: L2) {
    const out = this.copy();
    out.$literal = primitive;
    return out as any as Token<T, L2>;
  }

  /**
   * Returns a copy of this token with a new line number.
   * @param line The new line number.
   * @returns A copy of this token with the given line as its line number.
   */
  withLine(line: number) {
    const out = this.copy();
    out.$line = line;
    return out;
  }

  /**
   * Returns true, and asserts, if this token
   * is of the given type `K`.
   */
  isType<K extends T>(type: K): this is Token<K> {
    return this.$type === type;
  }

  /**
   * Returns true if this token is an error token.
   * If this token is an error token, then there must
   * be an accompanying Err object in its $literal
   * field.
   */
  isErrorToken(): this is Token<token_type.error, Err> {
    return this.$type === token_type.error;
  }

  /**
   * Returns true if this token is a number token.
   * If this token is a number token, then there
   * must be an accompanying JavaScript literal number
   * (either a float or an int).
   */
  isNumber(): this is Token<T, number> {
    return typeof this.$literal === "number";
  }

  /** Returns true, and asserts, if this token is a number constant token. */
  isNumConst(): this is Token<T, number> {
    return (
      typeof this.$literal === "number" &&
      this.$type === token_type.numeric_constant
    );
  }

  /**
   * Returns true if and only if this token is a
   * right-delimiter token. That is, either a `)`,
   * `]`, or `}`.
   * @returns a boolean
   */
  isRightDelimiter() {
    return (
      this.$type === token_type.right_paren ||
      this.$type === token_type.right_brace ||
      this.$type === token_type.right_bracket
    );
  }

  /** The empty token, used as a placeholder. */
  static empty: Token<token_type, any> = new Token(token_type.empty, "", -1);

  /** The end token, marking the end of input. */
  static end: Token<token_type, any> = new Token(token_type.end, "END", -1);

  /** Returns a string form of this token. */
  toString() {
    return `{token: ${token_type[this.$type]}, lexeme: ${this.$lexeme}, line: ${
      this.$line
    }, literal: ${this.$literal}}`;
  }
}

/**
 * Returns a new Winnow token.
 * @param type The new token's type.
 * @param lexeme The new token's lexeme.
 * @param line The line where this token was first encountered.
 * @param column The column where this token was first encountered.
 * @returns A new instance of Token.
 */
function token<X extends token_type>(
  type: X,
  lexeme: string,
  line: number
): Token<X> {
  return new Token(type, lexeme, line);
}

// § Native Function Types

type NativeUnary =
  | "ceil"
  | "floor"
  | "sin"
  | "cos"
  | "cosh"
  | "tan"
  | "lg"
  | "ln"
  | "log"
  | "arcsin"
  | "arccos"
  | "arcsinh"
  | "arctan"
  | "exp"
  | "sinh"
  | "sqrt"
  | "tanh"
  | "gcd"
  | "avg"
  | "arccosh";

/** A native function that takes more than 1 argument. */
type NativePolyAry = "max" | "min";

type NativeFn = NativeUnary | NativePolyAry;

type NativeConstants =
  | "e"
  | "pi"
  | "ln2"
  | "ln10"
  | "log10e"
  | "log2e"
  | "sqrt2";

// § Lexical

export function lexical(code: string) {
  /**
   * A variable corresponding to the
   * current line the scanner's on.
   */
  let $line: number = 1;

  /**
   * A pointer to the first character
   * of the lexeme currently being
   * scanned.
   */
  let $start: number = 0;

  /**
   * A pointer to the character currently
   * being scanned.
   */
  let $current: number = 0;

  /**
   * Error indicator defaulting to null.
   * If initialized, then the scanning will cease.
   */
  let $error: null | Err = null;

  /**
   * Returns true if the scanner has reached the end
   * of code.
   */
  const atEnd = (): boolean => $current >= code.length || !isNull($error);

  /**
   * Consumes and returns the next character
   * in the code.
   */
  const tick = (): string => code[$current++];

  /**
   * Returns the code substring from $start to $current.
   */
  const slice = (): string => code.slice($start, $current);

  /**
   * Returns a new token.
   * @param type The token type.
   * @param lexeme The token's lexeme.
   * @returns A new Token.
   */
  const tkn = (type: token_type, lexeme: string = ""): Token => {
    lexeme = lexeme ? lexeme : slice();
    return token(type, lexeme, $line);
  };

  /**
   * Returns an error token. If called, sets the
   * mutable $error variable, causing scanning to
   * cease.
   * @param message The error message to accompany
   * the Err object.
   * @returns A new Token of type token_type.ERROR.
   */
  const errorToken = (message: string): Token<token_type.error, Err> => {
    $error = lexicalError(message, $line);
    return token(token_type.error, "", $line).withLiteral($error);
  };

  /**
   * Returns the current character being scanned WITHOUT
   * moving the scanner forward.
   * @returns A 1-character string.
   */
  const peek = (): string => (atEnd() ? "" : code[$current]);

  /**
   * Returns the character just ahead of the current character
   * WITHOUT moving the scanner forward.
   * @returns A 1-character string.
   */
  const peekNext = (): string => (atEnd() ? "" : code[$current + 1]);

  /**
   * Returns the character `by` places
   * ahead of the current character
   * WITHOUT moving the scanner forward.
   * @param by The number of places to look ahead.
   * @returns A 1-character string.
   */
  const lookup = (by: number): string => (atEnd() ? "" : code[$current + by]);

  /**
   * If the provided expected string
   * matches, increments $current (moving
   * the scanner forward) and returns true.
   * Otherwise returns false without increment (
   * scanner doesn't move forward).
   * @param expectedChar A 1-character string corresponding
   * to the expected character.
   * @returns A boolean.
   */
  const match = (expectedChar: string): boolean => {
    if (atEnd()) return false;
    if (code[$current] !== expectedChar) return false;
    $current++;
    return true;
  };

  /**
   * Returns true if the current peek (the character
   * pointed at by `$current`) matches the provided
   * number. Otherwise, returns false.
   * @param char A 1-char string corresponding
   * to the expected character.
   * @returns A boolean.
   */
  const peekIs = (char: string): boolean => peek() === char;

  /**
   * Consumes all whitespace while moving
   * the scanner's `$current` pointer
   * forward.
   * @returns Nothing.
   */
  const skipWhitespace = (): void => {
    while (!atEnd()) {
      const char: string = peek();
      switch (char) {
        case " ":
        case "\r":
        case "\t":
          tick();
          break;
        case "\n":
          $line++;
          tick();
          break;
        default:
          return;
      }
    }
  };

  /**
   * Returns true if the given character is a Latin or
   * Greek character, false otherwise.
   * @param char A 1-character string.
   * @returns A boolean.
   */
  const isLatinGreekChar = (char: string): boolean =>
    /^[a-zA-Z_$\u00C0-\u02AF\u0370-\u03FF\u2100-\u214F]$/.test(char);

  /**
   * Returns true if the given character is a Unicode
   * math symbol, false otherwise.
   * @param char A 1-character string.
   * @returns A boolean.
   */
  const isMathSymbol = (char: string): boolean => /^[∀-⋿]/u.test(char);

  /**
   * Returns true if the given character is a valid
   * character to the start of a name, false
   * otherwise.
   * @param char A 1-char string.
   * @returns A boolean.
   */
  const isValidNameChar = (char: string) =>
    isLatinGreekChar(char) || isMathSymbol(char);

  /**
   * Returns true if the given `char` is a digit.
   * @param char A 1-char string.
   * @returns A boolean.
   */
  const isDigit = (char: string) => "0" <= char && char <= "9";

  /** Dictionary of keywords to tokens. */
  const dictionary: Record<string, () => Token> = {
    this: () => tkn(token_type.this),
    super: () => tkn(token_type.super),
    class: () => tkn(token_type.class),
    false: () => tkn(token_type.boolean).withLiteral(false),
    true: () => tkn(token_type.boolean).withLiteral(true),
    NaN: () => tkn(token_type.nan).withLiteral(NaN),
    Inf: () => tkn(token_type.inf).withLiteral(Infinity),
    return: () => tkn(token_type.return),
    while: () => tkn(token_type.while),
    for: () => tkn(token_type.for),
    let: () => tkn(token_type.let),
    var: () => tkn(token_type.var),
    fn: () => tkn(token_type.fn),
    if: () => tkn(token_type.if),
    else: () => tkn(token_type.else),
    print: () => tkn(token_type.print),
    rem: () => tkn(token_type.rem),
    mod: () => tkn(token_type.mod),
    div: () => tkn(token_type.div),
    nil: () => tkn(token_type.nil),
    and: () => tkn(token_type.and),
    or: () => tkn(token_type.or),
    nor: () => tkn(token_type.nor),
    xor: () => tkn(token_type.xor),
    xnor: () => tkn(token_type.xnor),
    not: () => tkn(token_type.not),
    nand: () => tkn(token_type.nand),
  };

  const numConsts: Record<NativeConstants, () => Token> = {
    e: () => tkn(token_type.numeric_constant).withLiteral(Math.E),
    pi: () => tkn(token_type.numeric_constant).withLiteral(Math.PI),
    ln10: () => tkn(token_type.numeric_constant).withLiteral(Math.LN10),
    ln2: () => tkn(token_type.numeric_constant).withLiteral(Math.LN2),
    log10e: () => tkn(token_type.numeric_constant).withLiteral(Math.LOG10E),
    log2e: () => tkn(token_type.numeric_constant).withLiteral(Math.LOG2E),
    sqrt2: () => tkn(token_type.numeric_constant).withLiteral(Math.SQRT2),
  };

  /**
   * Record of native functions. Each key corresponds
   * to the native function name. The number mapped to
   * by the key is the function’s arity (the number
   * of arguments the function takes).
   */
  const nativeFunctions: Record<NativeFn, number> = {
    avg: 1,
    gcd: 1,
    sqrt: 1,
    exp: 1,
    ceil: 1,
    tanh: 1,
    floor: 1,
    sinh: 1,
    cosh: 1,
    sin: 1,
    cos: 1,
    tan: 1,
    lg: 1,
    ln: 1,
    log: 1,
    arctan: 1,
    arccos: 1,
    arccosh: 1,
    arcsin: 1,
    arcsinh: 1,
    max: 1,
    min: 1,
  };

  /** Generates a word token. */
  const wordToken = () => {
    while ((isValidNameChar(peek()) || isDigit(peek())) && !atEnd()) {
      tick();
    }
    const word = slice();
    const native = nativeFunctions[word as NativeFn];
    if (native) {
      return tkn(token_type.native);
    } else if (dictionary[word]) {
      return dictionary[word]();
    } else if (numConsts[word as NativeConstants]) {
      return numConsts[word as NativeConstants]();
    } else {
      return tkn(token_type.symbol);
    }
  };

  const isHexDigit = (char: string) =>
    ("0" <= char && char <= "9") ||
    ("a" <= char && char <= "f") ||
    ("A" <= char && char <= "F");

  const isOctalDigit = (char: string) => "0" <= char && char <= "7";

  /**
   * Scans and returns a BIG_NUMBER token.
   * @returns A Token of type BIG_NUMBER.
   */
  const bigNumberToken = () => {
    while (isDigit(peek()) && !atEnd()) tick();
    const n = slice().replace("#", "");
    return tkn(token_type.big_integer).withLiteral(BigInt(n));
  };

  /**
   * Scans a binary number token.
   * @returns A Token of type INTEGER.
   */
  const binaryNumberToken = () => {
    if (!(peekIs("0") || peekIs("1"))) {
      return errorToken(`Expected binary digits after “0b”`);
    }
    while ((peekIs("0") || peekIs("1")) && !atEnd()) {
      tick();
    }
    const numberString = slice().replace("0b", "");
    const integerValue = Number.parseInt(numberString, 2);
    return tkn(token_type.integer).withLiteral(integerValue);
  };

  /**
   * Scans an octal number token.
   * @returns A Token of type INTEGER.
   */
  const octalNumberToken = () => {
    if (!isOctalDigit(peek())) {
      return errorToken("Expected octal digits after");
    }
    while (isOctalDigit(peek()) && !atEnd()) {
      tick();
    }
    const numberString = slice().replace("0o", "");
    const integerValue = Number.parseInt(numberString, 8);
    return tkn(token_type.integer).withLiteral(integerValue);
  };

  /**
   * Scans a hexadecimal number token.
   * @returns A Token of type INTEGER.
   */
  const hexNumberToken = () => {
    if (!isHexDigit(peek())) {
      return errorToken("Expected hexadecimals after 0x");
    }
    while (isHexDigit(peek()) && !atEnd()) {
      tick();
    }
    const numberString = slice().replace("0x", "");
    const integerValue = Number.parseInt(numberString, 16);
    return tkn(token_type.integer).withLiteral(integerValue);
  };

  /** Generates number token. */
  const numTkn = (
    numberString: string,
    type: NumberTokenType,
    hasSeparators: boolean
  ) => {
    const n = hasSeparators ? numberString.replaceAll("_", "") : numberString;
    switch (type) {
      // handle integers
      case token_type.integer: {
        const num = Number.parseInt(n);
        if (num > Number.MAX_SAFE_INTEGER) {
          return errorToken(
            `Encountered an integer overflow. Consider rewriting “${numberString}” as a bignumber: “#${numberString}”. If “${numberString}” is to be used symbolically, consider rewriting “${numberString}” as a scientific number.`
          );
        } else {
          return tkn(type).withLiteral(num);
        }
      }
      // handle floats
      case token_type.float: {
        const num = Number.parseFloat(n);
        if (num > Number.MAX_VALUE) {
          return errorToken(
            `Encountered a floating point overflow. Consider rewriting "${n}" as a fraction or bigfraction. If "${n}" is to be used symbolically, consider rewriting "${n}" as a scientific number.`
          );
        } else {
          return tkn(type).withLiteral(num);
        }
      }
      // handle fractions
      case token_type.fraction: {
        const [a, b] = n.split("|");
        const N = Number.parseInt(a);
        const D = Number.parseInt(b);
        return tkn(type).withLiteral(fraction(N, D));
      }
      // handle scientific numbers
      case token_type.scientific: {
        const [a, b] = n.split("E");
        const base = Number.parseFloat(a);
        const exponent = Number.parseInt(b);
        return tkn(type).withLiteral(expo(base, exponent));
      }
    }
    return errorToken(`Unrecognized number: "${n}".`);
  };

  const numberToken = (initialType: NumberTokenType): Token => {
    let type = initialType;
    let hasSeparators = false;
    while (isDigit(peek()) && !atEnd()) {
      tick();
    }

    // handle number with separators
    if (peekIs("_") && isDigit(peekNext())) {
      tick(); // eat the '_'
      hasSeparators = true;
      let digits = 0;
      while (isDigit(peek()) && !atEnd()) {
        tick();
        digits++;
        if (peekIs("_") && isDigit(peekNext())) {
          if (digits === 3) {
            tick();
            digits = 0;
          } else {
            return errorToken(
              'Expected 3 ASCII digits after the separator "_"'
            );
          }
        }
      }
      // there must be 3 ASCII digits after the "_"
      if (digits !== 3) {
        return errorToken('Expected 3 ASCII digits after the separator "_"');
      }
    }

    // handle floating point numbers
    if (peekIs(".") && isDigit(peekNext())) {
      tick();
      type = token_type.float;
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
    }

    // handle fractions
    if (peekIs("|")) {
      if (type !== token_type.integer) {
        return errorToken('Expected an integer before "|"');
      }
      type = token_type.fraction;
      tick();
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
      return numTkn(slice(), type, hasSeparators);
    }

    if (peekIs("E")) {
      if (isDigit(peekNext())) {
        type = token_type.scientific;
        tick();
        while (isDigit(peek())) tick();
      } else if (
        (peekNext() === "+" || peekNext() === "-") &&
        isDigit(lookup(2))
      ) {
        type = token_type.scientific;
        tick();
        tick();
        while (isDigit(peek())) tick();
      }
    }
    return numTkn(slice(), type, hasSeparators);
  };

  /**
   * Scans for a string token.
   * @returns A Token.
   */
  const stringToken = () => {
    while (peek() !== '"' && !atEnd()) {
      if (peek() === "\n") {
        $line++;
      }
      tick();
    }
    if (atEnd()) return errorToken("Unterminated string");
    tick();
    const lex = slice().slice(1, -1);
    return tkn(token_type.string, lex);
  };

  const algebraStringToken = () => {
    while (peek() !== `'` && !atEnd()) {
      if (peek() === `\n`) {
        $line++;
      }
      tick();
    }
    if (atEnd()) {
      return errorToken(`Unterminated algebraic string`);
    }
    tick(); // eat the ':'
    const s = slice().replaceAll(`'`, "");
    return tkn(token_type.algebra_string).withLiteral(s);
  };

  const scan = (): Token => {
    // Start by skipping whitespace.
    skipWhitespace();

    // Set the $start and $current pointers
    // to the same characters.
    $start = $current;

    // If we've reached the end of the source code,
    // immediately return an END token.
    if (atEnd()) return tkn(token_type.end, "END");

    // Now get the current character and move the
    // scanner forward.
    const char = tick();

    // If the character is a valid name starter (a Latin
    // or Greek character, a unicode math symbol,
    // an underscore, or a `$`), returns a word token.
    if (isValidNameChar(char)) return wordToken();

    // If the character is '#' then we have either
    // a BIG_NUMBER token, or a matrix operator.
    if (char === "#") {
      if (isDigit(peek())) {
        return bigNumberToken();
      } else if (match("+")) {
        return tkn(token_type.pound_plus);
      } else if (match("-")) {
        return tkn(token_type.pound_minus);
      } else if (match("*")) {
        return tkn(token_type.pound_star);
      } else {
        return errorToken('Expected digits after "#".');
      }
    }
    // If the character is a digit, then we have
    // a number token.
    if (isDigit(char)) {
      if (char === "0" && match("b")) {
        return binaryNumberToken();
      } else if (char === "0" && match("o")) {
        return octalNumberToken();
      } else if (char === "0" && match("x")) {
        return hexNumberToken();
      } else {
        return numberToken(token_type.integer);
      }
    }
    switch (char) {
      case "@":
        return tkn(token_type.at);
      case ":":
        return tkn(token_type.colon);
      case "&":
        return tkn(token_type.ampersand);
      case "~":
        return tkn(token_type.tilde);
      case "|":
        return tkn(token_type.vbar);
      case "(":
        return tkn(token_type.left_paren);
      case ")":
        return tkn(token_type.right_paren);
      case "[":
        return tkn(token_type.left_bracket);
      case "]":
        return tkn(token_type.right_bracket);
      case "{":
        return tkn(token_type.left_brace);
      case "}":
        return tkn(token_type.right_brace);
      case ",":
        return tkn(token_type.comma);
      case "*":
        return tkn(token_type.star);
      case ";":
        return tkn(token_type.semicolon);
      case "%":
        return tkn(token_type.percent);
      case "/":
        return tkn(token_type.slash);
      case "^":
        return tkn(token_type.caret);
      case "!":
        return tkn(match("=") ? token_type.bang_equal : token_type.bang);
      case "<":
        return tkn(match("=") ? token_type.less_equal : token_type.less);
      case ">":
        return tkn(match("=") ? token_type.greater_equal : token_type.greater);
      case '"':
        return stringToken();
      case "+":
        return tkn(match("+") ? token_type.plus_plus : token_type.plus);
      case "'":
        return algebraStringToken();
      // Special handling of dot for vector operators.
      case ".": {
        if (match("+")) {
          return tkn(token_type.dot_add);
        } else if (match("-")) {
          return tkn(token_type.dot_minus);
        } else if (match("*")) {
          return tkn(token_type.dot_star);
        } else if (match("^")) {
          return tkn(token_type.dot_caret);
        } else {
          return tkn(token_type.dot);
        }
      }

      // Special handling of dash for inline comments.
      case "-": {
        if (peek() === "-" && peekNext() === "-") {
          while (peek() !== "\n" && !atEnd()) {
            tick();
          }
          return Token.empty;
        } else {
          return tkn(match("-") ? token_type.minus_minus : token_type.minus);
        }
      }

      // special handling of '=' for block comments.
      case "=": {
        if (peek() === "=" && peekNext() === "=") {
          while (peek() === "=") tick();
          while (!atEnd()) {
            tick();
            if (peek() === "=" && peekNext() === "=" && lookup(2) === "=") {
              break;
            }
          }
          if (atEnd()) {
            return errorToken("Unterminated block comment");
          }
          while (peek() === "=") tick();
          return Token.empty;
        } else {
          return tkn(match("=") ? token_type.equal_equal : token_type.equal);
        }
      }
    }
    return errorToken(`Unknown token: ${char}`);
  };

  const stream = () => {
    const out: Token[] = [];
    let prev = Token.empty;
    let now = scan();
    if (!now.isType(token_type.empty)) {
      out.push(now);
    } else if ($error !== null) {
      return left($error);
    }
    let peek = scan();
    if ($error !== null) {
      return left($error);
    }
    while (!atEnd()) {
      prev = now;
      now = peek;
      const k = scan();
      if ($error !== null) {
        return left($error);
      }
      if (k.isType(token_type.empty)) {
        continue;
      } else {
        peek = k;
      }
      // remove trailing commas
      if (
        prev.isRightDelimiter() &&
        now.isType(token_type.comma) &&
        peek.isRightDelimiter()
      ) {
        continue;
      }
      out.push(now);
    }
    out.push(peek);
    return right(out);
  };

  return { stream, scan, atEnd };
}

// § Primitives
/** An object corresponding to a number of the form `n/d`, where `n` and `d` are integers. */
class FRACTION {
  arglen(): number {
    return 0;
  }
  /** The numerator of this fraction. */
  $n: number;
  /** The denominator of this fraction. */
  $d: number;
  constructor(n: number, d: number) {
    this.$n = n;
    this.$d = d;
  }

  /** Returns this fraction as a string. */
  toString() {
    const out = `${this.$n}|${this.$d}`;
    return out;
  }

  /** Negates this fraction. */
  negate() {
    return new FRACTION(-this.$n, this.$d);
  }

  /** Returns this fraction times the other fraction. */
  times(other: FRACTION) {
    return FRACTION.simplify(
      new FRACTION(other.$n * this.$n, other.$d * this.$d)
    );
  }

  /** Returns this fraction times the other fraction. */
  divide(other: FRACTION) {
    return FRACTION.simplify(
      new FRACTION(this.$n * other.$d, this.$d * other.$n)
    );
  }

  /** Returns this fraction plus the other fraction. */
  plus(other: FRACTION) {
    return FRACTION.simplify(
      new FRACTION(this.$n * other.$d + other.$n * this.$d, this.$d * other.$d)
    );
  }

  /** Returns this fraction minus the other fraction. */
  minus(other: FRACTION) {
    return FRACTION.simplify(
      new FRACTION(this.$n * other.$d - other.$n * this.$d, this.$d * other.$d)
    );
  }

  /** Returns true if this fraction is less than or equal to the other fraction. */
  lte(other: FRACTION) {
    const thisN = this.$n;
    const thisD = this.$d;
    const otherN = other.$n;
    const otherD = other.$d;
    return thisN * otherD <= otherN * thisD;
  }

  /** Returns true if this fraction is less than the other. */
  lt(other: FRACTION) {
    return this.lte(other) && !this.equals(other);
  }

  /** Returns true if this fraction is greater than the other. */
  gt(other: FRACTION) {
    return !this.lte(other);
  }

  /** Returns true if this fraction is greater than or equal to the other. */
  gte(other: FRACTION) {
    return this.gt(other) || this.equals(other);
  }

  /** Returns true if this fraction equals the other fraction. */
  equals(other: FRACTION) {
    const a = FRACTION.simplify(this);
    const b = FRACTION.simplify(other);
    return a.$n === b.$n && a.$d === b.$d;
  }
  float() {
    return this.$n / this.$d;
  }
  /** Simplifies the given fraction. */
  static simplify(frac: FRACTION) {
    const numerator = frac.$n;
    const denominator = frac.$d;
    const sgn = Math.sign(numerator) * Math.sign(denominator);
    const n = Math.abs(numerator);
    const d = Math.abs(denominator);
    const f = gcd(n, d);
    return new FRACTION((sgn * n) / f, d / f);
  }

  /** Returns the given number as a fraction. */
  static from(value: number | FRACTION) {
    // We use the method of continued fractions here.
    if (value instanceof FRACTION) {
      return value;
    } else if (Number.isInteger(value)) {
      return new FRACTION(value, 1);
    } else {
      let eps = 1.0e-15;
      let x = value;
      let a = Math.floor(x);
      let h1 = 1;
      let h2: number;
      let k1 = 0;
      let k2: number;
      let h = a;
      let k = 1;

      while (x - a > eps * k * k) {
        x = 1 / (x - a);
        a = Math.floor(x);
        h2 = h1;
        h1 = h;
        k2 = k1;
        k1 = k;
        h = h2 + a * h1;
        k = k2 + a * k1;
      }

      return new FRACTION(h, k);
    }
  }
}

/**
 * Returns a new FRACTION. Both arguments must be integers.
 * If the arguments are not integers, they will be floored.
 * @param n The numerator of this fraction (must be an integer).
 * @param d The denominator of this fraction (must be an integer).
 * @returns A new instance of a fraction.
 */
function fraction(n: number, d: number) {
  const N = Math.floor(n);
  const D = Math.abs(Math.floor(d));
  if (n < 0 && d < 0) {
    return new FRACTION(Math.abs(N), Math.abs(D));
  } else if (n < 0 && d > 0) {
    return new FRACTION(N, D);
  } else if (n > 0 && d < 0) {
    return new FRACTION(-N, D);
  } else if (d === 0) {
    return new FRACTION(NaN, NaN);
  } else {
    return new FRACTION(Math.floor(n), Math.floor(d));
  }
}

/** Returns true if the given object is a fraction. */
function isFraction(u: any): u is FRACTION {
  return u instanceof FRACTION;
}

function cons<T>(list: T[], x: T) {
  return [x, ...list];
}

function cdr<T>(list: T[]) {
  if (list.length === 0) {
    return [];
  }
  if (list.length === 1) {
    return list;
  }
  const out = [];
  for (let i = 1; i < list.length; i++) {
    out.push(list[i]);
  }
  return out;
}

enum expression_type {
  relation,
  int,
  float64,
  fraction,
  boolean,
  symbol,
  sum,
  difference,
  product,
  quotient,
  power,
  function,
  call,
  undefined,
}

abstract class MathObj {
  parenLevel: number = 0;
  parend() {
    this.parenLevel++;
    return this;
  }
  abstract kind(): expression_type;
  abstract equals(other: MathObj): boolean;
  abstract toString(): string;
  abstract map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this;
}

type RelationOperator = "=" | "<" | ">" | "<=" | ">=" | "!=";

function argsEqual(a: MathObj[], b: MathObj[]) {
  if (a.length !== b.length) return false;
  if (a.length === 1 && b.length === 1) return a[0].equals(b[0]);
  if (a.length === 0 && b.length === 0) return true;
  if (a.length === 0) return false;
  if (b.length === 0) return false;
  if (a[0].equals(b[0])) return argsEqual(cdr(a), cdr(b));
  return false;
}

class Relation extends MathObj {
  kind(): expression_type {
    return expression_type.relation;
  }
  toString(): string {
    const out = this.args.map((arg) => arg.toString()).join(` ${this.op} `);
    return this.parenLevel ? `(${out})` : out;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: RelationOperator;
  args: MathObj[];
  equals(other: MathObj): boolean {
    if (!isRelation(other)) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  constructor(op: RelationOperator, args: MathObj[]) {
    super();
    this.op = op;
    this.args = args;
  }
}

function relate(op: RelationOperator, args: MathObj[]) {
  return new Relation(op, args);
}

function isRelation(u: MathObj): u is Relation {
  return u.kind() === expression_type.relation;
}

class Boolean extends MathObj {
  kind(): expression_type {
    return expression_type.boolean;
  }
  equals(other: MathObj): boolean {
    if (!isBool(other)) {
      return false;
    } else {
      return other.bool === this.bool;
    }
  }
  toString(): string {
    return `${this.bool}`;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    return this;
  }
  bool: boolean;
  constructor(value: boolean) {
    super();
    this.bool = value;
  }
}
function bool(value: boolean) {
  return new Boolean(value);
}
function isBool(u: MathObj): u is Boolean {
  return u.kind() === expression_type.boolean;
}

abstract class Numeric extends MathObj {}

class Int extends Numeric {
  kind(): expression_type {
    return expression_type.int;
  }
  rational() {
    return frac(this, int(1));
  }
  equals(other: MathObj): boolean {
    if (!isInt(other)) {
      return false;
    } else {
      return this.int === other.int;
    }
  }
  toString(): string {
    return `${this.int}`;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    return this;
  }
  int: number;
  constructor(int: number) {
    super();
    this.int = int;
  }
  get denominator() {
    return int(1);
  }
  get numerator() {
    return int(this.int);
  }
}

function int(value: number) {
  return new Int(value);
}

function isInt(u: MathObj): u is Int {
  return u.kind() === expression_type.int;
}

class Float64 extends Numeric {
  kind(): expression_type {
    return expression_type.float64;
  }
  equals(other: MathObj): boolean {
    if (!isFloat64(other)) {
      return false;
    } else {
      return this.float === other.float;
    }
  }
  toString(): string {
    return `${this.float}`;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    return this;
  }
  float: number;
  constructor(value: number) {
    super();
    this.float = value;
  }
}

function float64(value: number) {
  return new Float64(value);
}

function isFloat64(u: MathObj): u is Float64 {
  return u.kind() === expression_type.float64;
}

class Sym extends MathObj {
  kind(): expression_type {
    return expression_type.symbol;
  }
  equals(other: MathObj): boolean {
    if (!isSym(other)) {
      return false;
    } else {
      return this.sym === other.sym;
    }
  }
  toString(): string {
    return this.sym;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    return this;
  }
  sym: string;
  constructor(sym: string) {
    super();
    this.sym = sym;
  }
}

class Undefined extends MathObj {
  kind(): expression_type {
    return expression_type.undefined;
  }
  equals(other: MathObj): boolean {
    return isUndefined(other);
  }
  toString(): string {
    return this.sym;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    return this;
  }
  sym: "UNDEFINED";
  constructor() {
    super();
    this.sym = "UNDEFINED";
  }
}
function UNDEFINED() {
  return new Undefined();
}
function isUndefined(u: MathObj): u is Undefined {
  return u.kind() === expression_type.undefined;
}

function sym(str: string) {
  return new Sym(str);
}

function isSym(u: MathObj): u is Sym {
  return u.kind() === expression_type.symbol;
}

class Fraction extends Numeric {
  /** Simplifies the given fraction. */
  static simplify(frac: Fraction) {
    const numerator = frac.numerator.int;
    const denominator = frac.denominator.int;
    const sgn = Math.sign(numerator) * Math.sign(denominator);
    const n = Math.abs(numerator);
    const d = Math.abs(denominator);
    const f = gcd(n, d);
    return new Fraction(int((sgn * n) / f), int(d / f));
  }

  float64() {
    return float64(this.numerator.int / this.denominator.int);
  }

  kind(): expression_type {
    return expression_type.fraction;
  }

  negate() {
    return frac(int(-this.numerator.int), this.denominator);
  }

  times(other: Fraction) {
    return Fraction.simplify(
      frac(
        int(other.numerator.int * this.numerator.int),
        int(other.denominator.int * this.denominator.int)
      )
    );
  }

  div(other: Fraction) {
    return Fraction.simplify(
      frac(
        int(other.numerator.int * this.denominator.int),
        int(this.denominator.int * other.denominator.int)
      )
    );
  }

  plus(other: Fraction) {
    return Fraction.simplify(
      frac(
        int(
          this.numerator.int * other.denominator.int +
            other.numerator.int * this.denominator.int
        ),
        int(this.denominator.int * other.denominator.int)
      )
    );
  }

  minus(other: Fraction) {
    return Fraction.simplify(
      frac(
        int(
          this.numerator.int * other.denominator.int -
            other.numerator.int * this.denominator.int
        ),
        int(this.denominator.int * other.denominator.int)
      )
    );
  }

  lte(other: Fraction) {
    const thisN = this.numerator.int;
    const thisD = this.denominator.int;
    const otherN = other.numerator.int;
    const otherD = other.denominator.int;
    return thisN * otherD <= otherN * thisD;
  }

  lt(other: Fraction) {
    return this.lte(other) && !this.equals(other);
  }

  gt(other: Fraction) {
    return !this.lte(other);
  }

  gte(other: Fraction) {
    return this.gt(other) || this.equals(other);
  }

  equals(other: MathObj): boolean {
    if (!isFrac(other)) {
      return false;
    } else {
      const a = Fraction.simplify(this);
      const b = Fraction.simplify(other);
      return (
        a.numerator.int === b.numerator.int &&
        a.denominator.int === b.denominator.int
      );
    }
  }
  toString(): string {
    return `${this.numerator.int}|${this.denominator.int}`;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    return this;
  }
  public readonly numerator: Int;
  public readonly denominator: Int;
  constructor(numerator: Int, denominator: Int) {
    super();
    this.numerator = numerator;
    this.denominator = denominator;
  }
  rational() {
    return this;
  }
}

function frac(a: Int, b: Int) {
  return new Fraction(a, b);
}

function isFrac(u: MathObj): u is Fraction {
  return u.kind() === expression_type.fraction;
}

class Sum extends MathObj {
  kind(): expression_type {
    return expression_type.sum;
  }
  equals(other: MathObj): boolean {
    if (!isSum(other)) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  toString(): string {
    const out = this.args.map((arg) => arg.toString()).join(" + ");
    return this.parenLevel ? `(${out})` : out;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: "+" = "+";
  args: MathObj[];
  constructor(args: MathObj[]) {
    super();
    this.args = args;
  }
}

function sum(...args: MathObj[]) {
  return new Sum(args);
}

function isSum(u: MathObj): u is Sum {
  return u.kind() === expression_type.sum;
}

class Difference extends MathObj {
  kind(): expression_type {
    return expression_type.difference;
  }
  equals(other: MathObj): boolean {
    if (!isDiff(other)) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  toString(): string {
    const out = this.args.map((arg) => arg.toString()).join(" - ");
    return this.parenLevel ? `(${out})` : out;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: "-" = "-";
  args: MathObj[];
  constructor(args: MathObj[]) {
    super();
    this.args = args;
  }
}

function diff(...args: MathObj[]) {
  return new Difference(args);
}

function isDiff(u: MathObj): u is Difference {
  return u.kind() === expression_type.difference;
}

class Product extends MathObj {
  kind(): expression_type {
    return expression_type.product;
  }
  equals(other: MathObj): boolean {
    if (!isProduct(other)) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  toString(): string {
    const out = this.args.map((arg) => arg.toString()).join(" * ");
    return this.parenLevel ? `(${out})` : out;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: "*" = "*";
  args: MathObj[];
  constructor(args: MathObj[]) {
    super();
    this.args = args;
  }
}

function prod(...args: MathObj[]) {
  return new Product(args);
}

function isProduct(u: MathObj): u is Product {
  return u.kind() === expression_type.product;
}

class Quotient extends MathObj {
  kind(): expression_type {
    return expression_type.quotient;
  }
  equals(other: MathObj): boolean {
    if (!isQuotient(other)) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  toString(): string {
    const out = this.args.map((arg) => arg.toString()).join(" / ");
    return this.parenLevel ? `(${out})` : out;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: "/" = "/";
  args: MathObj[];
  constructor(args: MathObj[]) {
    super();
    this.args = args;
  }
}

function quot(...args: MathObj[]) {
  return new Quotient(args);
}

function isQuotient(u: MathObj): u is Quotient {
  return u.kind() === expression_type.quotient;
}

class Power extends MathObj {
  kind(): expression_type {
    return expression_type.power;
  }
  equals(other: MathObj): boolean {
    if (!isPower(other)) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  toString(): string {
    const out = this.args.map((arg) => arg.toString()).join("^");
    return this.parenLevel ? `(${out})` : out;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: "^" = "^";
  args: MathObj[];
  constructor(base: MathObj, exponent: MathObj) {
    super();
    this.args = [base, exponent];
  }
  get base() {
    return this.args[0];
  }
  get exponent() {
    return this.args[1];
  }
  simplify() {
    let v = this.base;
    let w = this.exponent;
    if (isInt(v)) {
      if (v.int === 0) return int(0);
      if (v.int === 1) return int(1);
    }
    if (isInt(w)) {
      if (w.int === 0) return int(1);
      if (w.int === 1) return v;
    }
    let n = w;
  }
}

function pow(base: MathObj, exponent: MathObj) {
  return new Power(base, exponent);
}

function isPower(u: MathObj): u is Power {
  return u.kind() === expression_type.power;
}

class Func extends MathObj {
  kind(): expression_type {
    return expression_type.call;
  }
  equals(other: MathObj): boolean {
    if (!isFunc(other)) {
      return false;
    } else if (other.op !== this.op) {
      return false;
    } else {
      return argsEqual(this.args, other.args);
    }
  }
  toString(): string {
    const f = this.op;
    const args = this.args.map((arg) => arg.toString()).join(",");
    return `${f}(${args})`;
  }
  map<T extends MathObj>(
    callbackfn: (value: MathObj, index: number, array: MathObj[]) => T
  ): this {
    this.args = this.args.map(callbackfn);
    return this;
  }
  op: string;
  args: MathObj[];
  constructor(op: string, args: MathObj[]) {
    super();
    this.op = op;
    this.args = args;
  }
}

function fn(fname: string, args: MathObj[]) {
  return new Func(fname, args);
}

function isFunc(u: MathObj): u is Func {
  return u.kind() === expression_type.call;
}

function expr(source: string) {
  const parse = () => {
    let $current = 0;
    let _tkns = lexical(source).stream();
    if (_tkns.isLeft()) return _tkns;
    const $tokens = _tkns.unwrap();

    const consume = (type: token_type, message: string) => {
      if (check(type)) return advance();
      throw syntaxError(message, peek().$line);
    };

    const peek = () => {
      return $tokens[$current];
    };

    const previous = () => {
      return $tokens[$current - 1];
    };

    const atEnd = () => {
      return $current >= $tokens.length;
    };

    const advance = () => {
      if (!atEnd()) $current++;
      return previous();
    };

    const check = (type: token_type) => {
      if (atEnd()) return false;
      return peek().$type === type;
    };

    const match = (...types: token_type[]) => {
      for (let i = 0; i < types.length; i++) {
        const type = types[i];
        if (check(type)) {
          advance();
          return true;
        }
      }
      return false;
    };

    const expression = () => {
      return equality();
    };

    const equality = () => {
      let left = compare();
      while (match(token_type.bang_equal, token_type.equal_equal)) {
        let op = previous();
        let right = compare();
        left = relate(op.$lexeme as RelationOperator, [left, right]);
      }
      return left;
    };

    const compare = (): MathObj => {
      let left = addition();
      while (
        match(
          token_type.greater,
          token_type.greater_equal,
          token_type.less,
          token_type.less_equal
        )
      ) {
        let op = previous();
        let right = addition();
        left = relate(op.$lexeme as RelationOperator, [left, right]);
      }
      return left;
    };

    const addition = (): MathObj => {
      let left = subtraction();
      while (match(token_type.plus)) {
        let right = subtraction();
        left = sum(left, right);
      }
      return left;
    };

    const subtraction = (): MathObj => {
      let left = product();
      while (match(token_type.minus)) {
        let right = product();
        left = diff(left, right);
      }
      return left;
    };

    const product = (): MathObj => {
      let left = imul();
      while (match(token_type.star)) {
        let right = imul();
        left = prod(left, right);
      }
      return left;
    };

    const imul = (): MathObj => {
      let left = quotient();
      if (
        (isInt(left) || isFloat64(left)) &&
        (check(token_type.symbol) || check(token_type.native))
      ) {
        let right = quotient();
        left = prod(left, right);
      }
      return left;
    };

    const quotient = (): MathObj => {
      let left = power();
      while (match(token_type.slash)) {
        let right = power();
        left = quot(left, right);
      }
      return left;
    };

    const power = (): MathObj => {
      let left = negate();
      while (match(token_type.caret)) {
        let right = negate();
        left = pow(left, right);
      }
      return left;
    };

    const negate = (): MathObj => {
      if (match(token_type.minus)) {
        let right = factorial();
        if (isInt(right)) {
          right = int(-right.int);
          return right;
        } else if (isFloat64(right)) {
          right = float64(-right.float);
          return right;
        } else {
          return diff(right);
        }
      }
      return factorial();
    };

    const factorial = (): MathObj => {
      let left = call();
      if (match(token_type.bang)) {
        left = fn("!", [left]);
      }
      return left;
    };

    const call = (): MathObj => {
      let left = primary();
      while (true) {
        if (match(token_type.left_paren)) {
          left = finishCall(left);
        } else {
          break;
        }
      }
      return left;
    };

    const finishCall = (callee: MathObj) => {
      const args: MathObj[] = [];
      if (!check(token_type.right_paren)) {
        do {
          args.push(expression());
        } while (match(token_type.comma));
      }
      consume(token_type.right_paren, 'Expected ")" after arguments');
      if (!isSym(callee)) {
        throw syntaxError(
          "Expected a symbol for function call",
          previous().$line
        );
      }
      return fn(callee.sym, args);
    };

    const primary = (): MathObj => {
      if (match(token_type.boolean))
        return bool(previous().$literal as boolean);
      if (match(token_type.integer)) return int(previous().$literal as number);
      if (match(token_type.float))
        return float64(previous().$literal as number);
      if (match(token_type.symbol)) return sym(previous().$lexeme);
      if (match(token_type.native)) return sym(previous().$lexeme);
      if (match(token_type.fraction)) {
        const f = previous().$literal as FRACTION;
        return frac(int(f.$n), int(f.$d));
      }
      if (match(token_type.left_paren)) {
        let expr = expression();
        consume(token_type.right_paren, `Expected a closing ")"`);
        expr = expr.parend();
        if (check(token_type.left_paren)) {
          let right = expression();
          return prod(expr, right);
        }
        return expr;
      }
      throw syntaxError(
        `Unrecognized lexeme: ${peek().$lexeme}.`,
        peek().$line
      );
    };

    const run = () => {
      try {
        const result = expression();
        return right(result);
      } catch (error) {
        return left(error as Err);
      }
    };

    return run();
  };

  return {
    ast() {
      const res = parse();
      if (res.isLeft()) {
        return res.unwrap().toString();
      } else {
        return treestring(res.unwrap());
      }
    },
    obj() {
      const res = parse();
      if (res.isLeft()) {
        return UNDEFINED();
      } else {
        return res.unwrap();
      }
    },
  };
}

function simplifyRationalNumber(u: MathObj) {
  if (isInt(u)) return u;
  if (isFrac(u)) {
    const n = u.numerator;
    const d = u.denominator;
    if (mod(n.int, d.int) === 0) return int(iquot(n.int, d.int));
    let g = gcd(n.int, d.int);
    if (d.int > 0) return frac(int(iquot(n.int, g)), int(iquot(d.int, g)));
    if (d.int < 0) return frac(int(iquot(-n.int, g)), int(iquot(-d.int, g)));
  }
  throw algebraError(
    `simplifyRationalNumber called with nonrational argument.`
  );
}

type Rational = Int | Fraction;

function isRational(v: MathObj): v is Rational {
  return isInt(v) || isFrac(v);
}

function isNum(v: MathObj): v is Numeric {
  return isRational(v) || isFloat64(v);
}

function evalQuotient(v: MathObj, w: MathObj) {
  if (isRational(v) && isRational(w)) {
    if (w.numerator.int === 0) return UNDEFINED();
    return frac(
      int(v.numerator.int * w.denominator.int),
      int(w.numerator.int * v.denominator.int)
    );
  }
  throw algebraError(`expected rational in call to evalQuotient`);
}

function evalSum(v: MathObj, w: MathObj) {
  if (isRational(v) && isRational(w)) {
    return frac(
      int(
        v.numerator.int * w.denominator.int +
          w.numerator.int * v.denominator.int
      ),
      int(v.denominator.int * w.denominator.int)
    );
  }
  throw algebraError(`expected rational in call to evalSum`);
}

function evalDiff(v: MathObj, w: MathObj) {
  if (isRational(v) && isRational(w)) {
    return frac(
      int(
        v.numerator.int * w.denominator.int -
          w.numerator.int * v.denominator.int
      ),
      int(v.denominator.int * w.denominator.int)
    );
  }
  throw algebraError(`expected rational in call to evalDiff`);
}

function evalProduct(v: MathObj, w: MathObj) {
  if (isRational(v) && isRational(w)) {
    return frac(
      int(v.numerator.int * w.numerator.int),
      int(v.denominator.int * w.denominator.int)
    );
  }
  throw algebraError(`expected rational in call to evalProduct`);
}

function evalPower(v: MathObj, n: MathObj): Rational | Undefined {
  if (isRational(v) && isInt(n)) {
    if (v.numerator.int !== 0) {
      if (n.int > 0) return evalProduct(evalPower(v, int(n.int - 1)), v);
      if (n.int === 0) return int(1);
      if (n.int === -1) return frac(v.denominator, v.numerator);
      if (n.int < -1) {
        let s = frac(v.denominator, v.numerator);
        return evalPower(s, int(-n.int));
      }
    }
    if (n.int >= 1) return int(0);
    if (n.int <= 0) return UNDEFINED();
  }
  throw algebraError(
    `expected rational base and integer exponent in call to evalPower`
  );
}

function simplifyRNERec(u: MathObj): MathObj {
  if (isInt(u)) return u;
  if (isFrac(u)) {
    if (u.denominator.int === 0) return UNDEFINED();
    else return u;
  }
  if (isSum(u) && u.args.length === 1) {
    return simplifyRNERec(u.args[0]);
  }
  if (isDiff(u) && u.args.length === 1) {
    let v = simplifyRNERec(u.args[0]);
    if (isUndefined(v)) return v;
    return evalProduct(int(-1), v);
  }
  if (isSum(u) && u.args.length === 2) {
    let v = simplifyRNERec(u.args[0]);
    let w = simplifyRNERec(u.args[1]);
    if (isUndefined(v) || isUndefined(w)) {
      return UNDEFINED();
    }
    return evalSum(v, w);
  }
  if (isProduct(u) && u.args.length === 2) {
    let v = simplifyRNERec(u.args[0]);
    let w = simplifyRNERec(u.args[1]);
    if (isUndefined(v) || isUndefined(w)) {
      return UNDEFINED();
    }
    return evalProduct(v, w);
  }
  if (isDiff(u) && u.args.length === 2) {
    let v = simplifyRNERec(u.args[0]);
    let w = simplifyRNERec(u.args[1]);
    if (isUndefined(v) || isUndefined(w)) {
      return UNDEFINED();
    }
    return evalDiff(v, w);
  }
  if (isFrac(u)) {
    let v = simplifyRNERec(u.numerator);
    let w = simplifyRNERec(u.denominator);
    if (isUndefined(v) || isUndefined(w)) {
      return UNDEFINED();
    }
    return evalQuotient(v, w);
  }
  if (isPower(u)) {
    let v = simplifyRNERec(u.base);
    if (isUndefined(v)) return v;
    return evalPower(v, u.exponent);
  }
  return UNDEFINED();
}

function simplifyRNE(u: MathObj) {
  const v = simplifyRNERec(u);
  if (isUndefined(v)) return UNDEFINED();
  return simplifyRationalNumber(v);
}

// O-3
const O3 = (uElts: MathObj[], vElts: MathObj[]): boolean => {
  if (uElts.length === 0) return true;
  if (vElts.length === 0) return false;
  let u = uElts[0];
  let v = vElts[0];
  return !u.equals(v) ? order(u, v) : O3(cdr(uElts), cdr(vElts));
};

function order(u: MathObj, v: MathObj): boolean {
  // O-1
  if (isRational(u) && isRational(v)) {
    return u.rational().lt(v.rational());
  }
  if (isFloat64(u) && isInt(v)) {
    return u.float < v.int;
  }
  if (isFloat64(u) && isFrac(v)) {
    return u.float < v.numerator.int / v.denominator.int;
  }
  if (isInt(u) && isFloat64(v)) {
    return u.int < v.float;
  }
  if (isFrac(u) && isFloat64(v)) {
    return u.numerator.int / u.denominator.int < v.float;
  }
  // O-2
  if (isSym(u) && isSym(v)) {
    return u.sym < v.sym;
  }
  if (isProduct(u) && isProduct(v)) {
    return O3(u.args.reverse(), v.args.reverse());
  }
  if (isSum(u) && isSum(v)) {
    return O3(u.args.reverse(), v.args.reverse());
  }
  if (isPower(u) && isPower(v)) {
    return u.base.equals(v.base)
      ? order(u.exponent, v.exponent)
      : order(u.base, v.base);
  }
  if (isFunc(u) && isFunc(v)) {
    return u.op === v.op ? O3(u.args, v.args) : u.op < v.op;
  }
  if (isNum(u) && !isNum(v)) {
    return true;
  }
  if (isProduct(u) && (isPower(v) || isSum(v) || isFunc(v) || isSym(v))) {
    return order(u, prod(v));
  }
  if (isPower(u) && (isSum(v) || isFunc(v) || isSym(v))) {
    return order(u, pow(v, int(1)));
  }
  if (isSum(u) && (isFunc(v) || isSym(v))) {
    return order(u, sum(v));
  }
  if (isFunc(u) && isSym(v)) {
    return u.op === v.sym ? false : order(sym(u.op), v);
  }
  return !order(v, u);
}

// this is ok
function simplifyIntPower(v: MathObj, n: MathObj) {
  // sintpow-1
  if (isInt(v) || isFrac(v)) {
    return simplifyRNE(pow(v, n));
  }
  // sintpow-2
  if (isInt(n) && n.int === 0) return int(1);
  // sintpow-3
  if (isInt(n) && n.int === 1) return v;
  // sintpow-4
  if (isPower(v)) {
    let r = v.args[0];
    let s = v.args[1];
    let p = simplifyProduct(prod(s, n));
    if (isInt(p)) {
      return simplifyIntPower(r, p);
    } else {
      return pow(r, p);
    }
  }
  // sint pow5
  if (isProduct(v)) {
    let r = v.map((arg) => pow(arg, n));
    return r;
  }

  // sintpow6
  return pow(v, n);
}

// this is ok
function simplifyPower(u: Power) {
  // v^w
  let v = u.base;
  let w = u.exponent;

  // spow-1
  if (isUndefined(v) || isUndefined(w)) return UNDEFINED();

  // spow-2
  if (isInt(v) && v.int === 0) return int(0);

  // spow-3
  if (isInt(v) && v.int === 1) return int(1);
  if (isInt(w) && w.int === 0) return int(1);
  if (isInt(w) && w.int === 1) return v;
  if ((isInt(v) || isFrac(v)) && isInt(w)) return simplifyRNE(pow(v, w));
  if (isFloat64(v) && isInt(w)) return float64(v.float ** w.int);
  if (isFloat64(v) && isFrac(w)) return float64(v.float ** w.float64().float);
  if (isInt(v) && isFloat64(w)) return float64(v.int ** w.float);
  if (isFrac(v) && isFloat64(w)) return float64(v.float64().float ** w.float);

  // spow-4
  if (isInt(w)) return simplifyIntPower(v, w);

  // spow-5
  return u;
}

function reduce(u: MathObj) {
  if (isInt(u) || isSym(u)) return u;
  else if (isFrac(u)) return simplifyRationalNumber(u);
  else {
    if (isPower(u)) {
      return simplifyPower(u);
    }
  }
}

const a = `a + c + d`;
const b = `b + c + d`;
const ax = expr(a).obj();
const bx = expr(b).obj();
const ab = order(ax, bx);
clog(ab);
// const h = reduce(expr(k).obj());
// clog(j);
// clog(h.toString());

// § Nodekind Enum
enum nodekind {
  class_statement,
  block_statement,
  expression_statement,
  negation_expression,
  positivization_expression,
  function_declaration,
  variable_declaration,
  if_statement,
  print_statement,
  return_statement,
  algebraic_binex,
  vector_binex,
  while_statement,
  algebra_string,
  tuple_expression,
  vector_expression,
  matrix_expression,
  factorial_expression,
  assignment_expression,
  parend_expression,
  string_concatenation,
  not_expression,
  native_call,
  call_expression,
  numeric_constant,
  symbol,
  logical_binex,
  let_expression,
  get_expression,
  set_expression,
  super_expression,
  this_expression,
  relation_expression,
  index_expression,
  string,
  bool,
  integer,
  big_integer,
  scientific_number,
  float,
  frac,
  nil,
}

interface Visitor<T> {
  blockStmt(node: BlockStmt): T;
  exprStmt(node: ExprStmt): T;
  fnStmt(node: FnStmt): T;
  ifStmt(node: IfStmt): T;
  printStmt(node: PrintStmt): T;
  returnStmt(node: ReturnStmt): T;
  variableStmt(node: VariableStmt): T;
  whileStmt(node: WhileStmt): T;
  classStmt(node: ClassStmt): T;
  // expressions
  indexExpr(node: IndexExpr): T;
  algebraString(node: AlgebraString): T;
  tupleExpr(node: TupleExpr): T;
  vectorExpr(node: VectorExpr): T;
  matrixExpr(node: MatrixExpr): T;
  relationExpr(node: RelationExpr): T;
  assignmentExpr(node: AssignmentExpr): T;
  nativeCallExpr(node: NativeCallExpr): T;
  negExpr(node: NegExpr): T;
  posExpr(node: PosExpr): T;
  factorialExpr(node: FactorialExpr): T;
  notExpr(node: NotExpr): T;
  vectorBinex(node: VectorBinex): T;
  algebraicBinex(node: AlgebraicBinex): T;
  logicalBinex(node: LogicalBinex): T;
  callExpr(node: CallExpr): T;
  parendExpr(node: ParendExpr): T;
  getExpr(node: GetExpr): T;
  setExpr(node: SetExpr): T;
  superExpr(node: SuperExpr): T;
  thisExpr(node: ThisExpr): T;
  stringConcat(node: StringConcatExpr): T;
  sym(node: Identifier): T;
  string(node: StringLit): T;
  bool(node: Bool): T;
  nil(node: Nil): T;
  integer(node: Integer): T;
  float(node: Float): T;
  bigInteger(node: BigInteger): T;
  sciNum(node: SciNum): T;
  frac(node: Frac): T;
  numConst(node: NumConst): T;
}

/** A node corresponding to some syntax tree node. */
abstract class TreeNode {
  abstract kind(): nodekind;
}

/** A node corresponding to an abstract syntax tree node. */
abstract class ASTNode extends TreeNode {
  abstract accept<T>(visitor: Visitor<T>): T;
  abstract toString(): string;
  abstract isStatement(): this is Statement;
  abstract isExpr(): this is Expr;
}

/** A node corresponding to a statement node. */
abstract class Statement extends ASTNode {
  isStatement(): this is Statement {
    return true;
  }
  isExpr(): this is Expr {
    return false;
  }
}

/** A node corresponding to a class statement node. */
class ClassStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.classStmt(this);
  }
  kind(): nodekind {
    return nodekind.class_statement;
  }
  toString(): string {
    return `class-statement`;
  }
  $name: Token;
  $methods: FnStmt[];
  constructor(name: Token, methods: FnStmt[]) {
    super();
    this.$name = name;
    this.$methods = methods;
  }
}

/** Returns a new class statement node. */
function $classStmt(name: Token, methods: FnStmt[]) {
  return new ClassStmt(name, methods);
}

/** A node corresponding to a block statement node. */
class BlockStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.blockStmt(this);
  }
  kind(): nodekind {
    return nodekind.block_statement;
  }
  toString(): string {
    return "block-statement";
  }
  /** The statements comprising this block. */
  $statements: Statement[];
  constructor(statements: Statement[]) {
    super();
    this.$statements = statements;
  }
}

/** Returns true, and asserts, iff the given node is a block statement node. */
function isBlockStmt(node: ASTNode): node is BlockStmt {
  return node.kind() === nodekind.block_statement;
}

/** Returns a new block statement node. */
function $blockStmt(statements: Statement[]) {
  return new BlockStmt(statements);
}

/** A node corresponding to an expression statement. */
class ExprStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.exprStmt(this);
  }
  kind(): nodekind {
    return nodekind.expression_statement;
  }
  toString(): string {
    return "expression-statement";
  }
  $expression: Expr;
  constructor(expression: Expr) {
    super();
    this.$expression = expression;
  }
}

/** Returns a new Expression Statement. */
function $exprStmt(expression: Expr) {
  return new ExprStmt(expression);
}

/** A node corresponding to a function declaration statement. */
class FnStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.fnStmt(this);
  }
  kind(): nodekind {
    return nodekind.function_declaration;
  }
  toString(): string {
    return "fn-declaration";
  }
  $name: Token<token_type.symbol>;
  $params: Identifier[];
  $body: Statement[];
  constructor(
    name: Token<token_type.symbol>,
    params: Identifier[],
    body: Statement[]
  ) {
    super();
    this.$name = name;
    this.$params = params;
    this.$body = body;
  }
}

/** Returns a new function declaration statement. */
function $fnStmt(
  name: Token<token_type.symbol>,
  params: Identifier[],
  body: Statement[]
) {
  return new FnStmt(name, params, body);
}

/** An AST node corresponding to an if-then statement. */
class IfStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.ifStmt(this);
  }
  kind(): nodekind {
    return nodekind.if_statement;
  }
  toString(): string {
    return "if-statement";
  }
  $keyword: Token;
  $condition: Expr;
  $then: Statement;
  $alt: Statement;
  constructor(
    keyword: Token,
    condition: Expr,
    then: Statement,
    alt: Statement
  ) {
    super();
    this.$keyword = keyword;
    this.$condition = condition;
    this.$then = then;
    this.$alt = alt;
  }
}

/** Returns a new if-then statement. */
function $ifStmt(
  keyword: Token,
  condition: Expr,
  then: Statement,
  alt: Statement
) {
  return new IfStmt(keyword, condition, then, alt);
}

/** An AST node corresponding to a print statement. */
class PrintStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.printStmt(this);
  }
  kind(): nodekind {
    return nodekind.print_statement;
  }
  toString(): string {
    return "print-statement";
  }
  $keyword: Token;
  $expression: Expr;
  constructor(keyword: Token, expression: Expr) {
    super();
    this.$keyword = keyword;
    this.$expression = expression;
  }
}

/** Returns a new print statement node. */
function $printStmt(keyword: Token, expression: Expr) {
  return new PrintStmt(keyword, expression);
}

/** A node corresponding to a return statement. */
class ReturnStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.returnStmt(this);
  }
  kind(): nodekind {
    return nodekind.return_statement;
  }
  toString(): string {
    return "return-statement";
  }
  $keyword: Token;
  $value: Expr;
  constructor(keyword: Token, value: Expr) {
    super();
    this.$keyword = keyword;
    this.$value = value;
  }
}

/** Returns a new return statement node. */
function $returnStmt(keyword: Token, expression: Expr) {
  return new ReturnStmt(keyword, expression);
}

class VariableStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.variableStmt(this);
  }
  kind(): nodekind {
    return nodekind.variable_declaration;
  }
  toString(): string {
    return "variable-declaration";
  }
  $variable: Identifier;
  $value: Expr;
  $mutable: boolean;
  constructor(variable: Identifier, value: Expr, mutable: boolean) {
    super();
    this.$variable = variable;
    this.$value = value;
    this.$mutable = mutable;
  }
}

/** Returns a new 'var' statement node. */
function $var(symbol: Identifier, value: Expr) {
  return new VariableStmt(symbol, value, true);
}

/** Returns a new 'let' statement node. */
function $let(symbol: Identifier, value: Expr) {
  return new VariableStmt(symbol, value, false);
}

/** An AST node corresponding to a while statement. */
class WhileStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.whileStmt(this);
  }
  kind(): nodekind {
    return nodekind.while_statement;
  }
  toString(): string {
    return "while-statement";
  }
  $keyword: Token;
  $condition: Expr;
  $body: Statement;
  constructor(keyword: Token, condition: Expr, body: Statement) {
    super();
    this.$keyword = keyword;
    this.$condition = condition;
    this.$body = body;
  }
}

/** Returns a new while statement node. */
function $while(keyword: Token, condition: Expr, body: Statement) {
  return new WhileStmt(keyword, condition, body);
}

/** A node corresponding to an expression node. */
abstract class Expr extends ASTNode {
  isStatement(): this is Statement {
    return false;
  }
  isExpr(): this is Expr {
    return true;
  }
}

/** An AST node corresponding to an indexing expression. */
class IndexExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.indexExpr(this);
  }
  kind(): nodekind {
    return nodekind.index_expression;
  }
  toString(): string {
    return `${this.$list.toString()}[${this.$index.toString()}]`;
  }
  $list: Expr;
  $index: Expr;
  $op: Token;
  constructor(list: Expr, index: Expr, op: Token) {
    super();
    this.$list = list;
    this.$index = index;
    this.$op = op;
  }
}

/** Returns a new indexing expression node. */
function $index(list: Expr, index: Expr, op: Token) {
  return new IndexExpr(list, index, op);
}

/** An AST node corresponding to an algebra string. */
class AlgebraString extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraString(this);
  }
  kind(): nodekind {
    return nodekind.algebra_string;
  }
  toString(): string {
    return this.$expression.toString();
  }
  $expression: Expr;
  $op: Token;
  constructor(expression: Expr, op: Token) {
    super();
    this.$expression = expression;
    this.$op = op;
  }
}

/** Returns a new algebra string node. */
function $algebraString(expression: Expr, op: Token) {
  return new AlgebraString(expression, op);
}

/** A node corresponding to a tuple expression. */
class TupleExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.tupleExpr(this);
  }
  kind(): nodekind {
    return nodekind.tuple_expression;
  }
  toString(): string {
    const elems = this.$elements.map((e) => e.toString()).join(",");
    return `(${elems})`;
  }
  $elements: LinkedList<Expr>;
  constructor(elements: Expr[]) {
    super();
    this.$elements = linkedList(...elements);
  }
}

/** Returns a new tuple expression. */
function $tuple(elements: Expr[]) {
  return new TupleExpr(elements);
}

class VectorExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.vectorExpr(this);
  }
  kind(): nodekind {
    return nodekind.vector_expression;
  }
  toString(): string {
    const elems = this.$elements.map((e) => e.toString()).join(",");
    return `[${elems}]`;
  }
  $op: Token;
  $elements: Expr[];
  constructor(op: Token, elements: Expr[]) {
    super();
    this.$op = op;
    this.$elements = elements;
  }
}

/** Returns a new vector expression node. */
function $vector(op: Token, elements: Expr[]) {
  return new VectorExpr(op, elements);
}

/** Returns true, and asserts, if the given node is a vector expression node. */
function isVectorExpr(node: ASTNode): node is VectorExpr {
  return node.kind() === nodekind.vector_expression;
}

/** A node corresponding to a matrix expression. */
class MatrixExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.matrixExpr(this);
  }
  kind(): nodekind {
    return nodekind.matrix_expression;
  }
  toString(): string {
    const vectors = this.$vectors.map((v) => v.toString()).join(",");
    return `[${vectors}]`;
  }
  $op: Token;
  $vectors: VectorExpr[];
  $rowCount: number;
  $colCount: number;
  constructor(vectors: VectorExpr[], rows: number, columns: number, op: Token) {
    super();
    this.$vectors = vectors;
    this.$rowCount = rows;
    this.$colCount = columns;
    this.$op = op;
  }
}

/** Returns a new matrix expression. */
function $matrix(
  vectors: VectorExpr[],
  rowCount: number,
  colCount: number,
  op: Token
) {
  return new MatrixExpr(vectors, rowCount, colCount, op);
}

/** A node corresponding to a symbol. */
class Identifier extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.sym(this);
  }
  kind(): nodekind {
    return nodekind.symbol;
  }
  toString(): string {
    return this.$symbol.$lexeme;
  }
  $symbol: Token<token_type.symbol>;
  constructor(symbol: Token<token_type.symbol>) {
    super();
    this.$symbol = symbol;
  }
}

/** Returns a new symbol node. */
function $sym(symbol: Token<token_type.symbol>) {
  return new Identifier(symbol);
}

/** Returns true, and asserts, if the given node is a symbol node. */
function isSymbol(node: ASTNode): node is Identifier {
  return node.kind() === nodekind.symbol;
}

/** An AST node corresponding to an assignment expression. */
class AssignmentExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.assignmentExpr(this);
  }
  kind(): nodekind {
    return nodekind.assignment_expression;
  }
  toString(): string {
    return `${this.$symbol.toString()} = ${this.$value.toString()}`;
  }
  $symbol: Identifier;
  $value: Expr;
  constructor(symbol: Identifier, value: Expr) {
    super();
    this.$symbol = symbol;
    this.$value = value;
  }
}

/** Returns a new assignment expression node. */
function $assign(symbol: Identifier, value: Expr) {
  return new AssignmentExpr(symbol, value);
}

/** An AST node corresponding to a native call expression.  */
class NativeCallExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.nativeCallExpr(this);
  }
  toString(): string {
    const args = this.$args.map((x) => x.toString()).join(",");
    return `${this.$name.$lexeme}(${args})`;
  }
  kind(): nodekind {
    return nodekind.native_call;
  }
  $name: Token<token_type.native>;
  $args: Expr[];
  constructor(name: Token<token_type.native>, args: Expr[]) {
    super();
    this.$name = name;
    this.$args = args;
  }
}

/** Returns a new native call expression node. */
function $nativeCall(name: Token<token_type.native>, args: Expr[]) {
  return new NativeCallExpr(name, args);
}

/** An AST node corresponding to algebraic negation */
class NegExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.negExpr(this);
  }
  kind(): nodekind {
    return nodekind.negation_expression;
  }
  toString(): string {
    return `-${this.$arg.toString()}`;
  }
  $op: Token<token_type.minus>;
  $arg: Expr;
  constructor(op: Token<token_type.minus>, arg: Expr) {
    super();
    this.$op = op;
    this.$arg = arg;
  }
}

/** Returns a new positivation expression node. */
function $neg(op: Token<token_type.minus>, arg: Expr) {
  return new NegExpr(op, arg);
}

/** An AST node corresponding to algebraic positivization. */
class PosExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.posExpr(this);
  }
  kind(): nodekind {
    return nodekind.positivization_expression;
  }
  toString(): string {
    return `+${this.$arg.toString()}`;
  }
  $op: Token<token_type.plus>;
  $arg: Expr;
  constructor(op: Token<token_type.plus>, arg: Expr) {
    super();
    this.$op = op;
    this.$arg = arg;
  }
}

/** Returns a new positivization expression node. */
function $pos(op: Token<token_type.plus>, arg: Expr) {
  return new PosExpr(op, arg);
}

/** A node corresponding to a factorial expression. */
class FactorialExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.factorialExpr(this);
  }
  kind(): nodekind {
    return nodekind.factorial_expression;
  }
  toString(): string {
    return `${this.$arg.toString()}!`;
  }
  $op: Token<token_type.bang>;
  $arg: Expr;
  constructor(op: Token<token_type.bang>, arg: Expr) {
    super();
    this.$op = op;
    this.$arg = arg;
  }
}

/** Returns a new factorial expression node. */
function $factorial(op: Token<token_type.bang>, arg: Expr) {
  return new FactorialExpr(op, arg);
}

/** A node corresponding to a not expression. */
class NotExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.notExpr(this);
  }
  kind(): nodekind {
    return nodekind.not_expression;
  }
  toString(): string {
    return `not ${this.$arg.toString()}`;
  }
  $op: Token<token_type.not>;
  $arg: Expr;
  constructor(op: Token<token_type.not>, arg: Expr) {
    super();
    this.$op = op;
    this.$arg = arg;
  }
}

/** Returns a new Not Expression node. */
function $not(op: Token<token_type.not>, arg: Expr) {
  return new NotExpr(op, arg);
}

/** A token corresponding to a vectory binary operator */
type VectorBinop =
  | token_type.dot_add // scalar/pairwise addition
  | token_type.dot_minus // scalar/pairwise subtraction
  | token_type.dot_star // scalar/pairwise multiplication
  | token_type.dot_caret // scalar/pairwise exponentiation
  | token_type.at; // dot product

/** An AST node corresponding to a binary expression node. */
class VectorBinex extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.vectorBinex(this);
  }
  kind(): nodekind {
    return nodekind.vector_binex;
  }
  toString(): string {
    const left = this.$left.toString();
    const op = this.$op.$lexeme;
    const right = this.$right.toString();
    return `${left} ${op} ${right}`;
  }
  $left: Expr;
  $op: Token<VectorBinop>;
  $right: Expr;
  constructor(left: Expr, op: Token<VectorBinop>, right: Expr) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}

/** Returns a new vector binary expression. */
function $vectorBinex(left: Expr, op: Token<VectorBinop>, right: Expr) {
  return new VectorBinex(left, op, right);
}

/** A union of all algebraic operator token types. */
type AlgebraicOp =
  | token_type.plus // addition; 1 + 2 -> 3
  | token_type.star // multiplication; 3 * 4 -> 12
  | token_type.caret // exponentiation; 3^2 -> 9
  | token_type.slash // division; 3/6 -> 1/2 -> 0.5
  | token_type.minus // subtraction; 5 - 2 -> 3
  | token_type.rem // remainder; -10 rem 3 -> -1
  | token_type.mod // modulo; -10 mod 3 -> 2
  | token_type.percent // percent operator; 3 % 325 -> 9.75
  | token_type.div; // int division (divide number, round down to nearest int); 10 div 3 -> 3

/** An AST node corresponding to an algebraic binary expression. */
class AlgebraicBinex extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraicBinex(this);
  }
  kind(): nodekind {
    return nodekind.algebraic_binex;
  }
  toString(): string {
    const left = this.$left.toString();
    const right = this.$right.toString();
    const op = this.$op.$lexeme;
    return `${left} ${op} ${right}`;
  }
  $left: Expr;
  $op: Token<AlgebraicOp>;
  $right: Expr;
  constructor(left: Expr, op: Token<AlgebraicOp>, right: Expr) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}

/** Returns a new algebraic binary expression. */
function $algebraicBinex(left: Expr, op: Token<AlgebraicOp>, right: Expr) {
  return new AlgebraicBinex(left, op, right);
}

/** An AST node corresponding to a function call expression. */
class CallExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.callExpr(this);
  }
  kind(): nodekind {
    return nodekind.call_expression;
  }
  toString(): string {
    const callee = this.$callee.toString();
    const args = this.$args.map((arg) => arg.toString()).join(",");
    return `${callee}(${args})`;
  }
  $callee: Expr;
  $paren: Token;
  $args: Expr[];
  constructor(callee: Expr, paren: Token, args: Expr[]) {
    super();
    this.$callee = callee;
    this.$paren = paren;
    this.$args = args;
  }
}

/** Returns a new call expression. */
function $call(callee: Expr, paren: Token, args: Expr[]) {
  return new CallExpr(callee, paren, args);
}

/** An AST node corresponding to a parenthesized expression. */
class ParendExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.parendExpr(this);
  }
  kind(): nodekind {
    return nodekind.parend_expression;
  }
  toString(): string {
    return `(${this.$inner.toString()})`;
  }
  $inner: Expr;
  constructor(innerExpression: Expr) {
    super();
    this.$inner = innerExpression;
  }
}

/** Returns a new parenthesized expression node. */
function $parend(innerExpression: Expr) {
  return new ParendExpr(innerExpression);
}

/** Returns true, and asserts, if the given node is a parenthesized-expression node. */
function isParendExpr(node: ASTNode): node is ParendExpr {
  return node.kind() === nodekind.parend_expression;
}

/** An AST node corresponding to a string literal. */
class StringLit extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.string(this);
  }
  kind(): nodekind {
    return nodekind.string;
  }
  toString(): string {
    return this.$value;
  }
  $value: string;
  constructor(value: string) {
    super();
    this.$value = value;
  }
}

/** Returns a new string literal node. */
function $string(value: string) {
  return new StringLit(value);
}

/** An AST node corresponding to a Boolean literal. */
class Bool extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.bool(this);
  }
  kind(): nodekind {
    return nodekind.bool;
  }
  toString(): string {
    return `${this.$value}`;
  }
  $value: boolean;
  constructor(value: boolean) {
    super();
    this.$value = value;
  }
}

/** Returns a new Boolean literal node. */
function $bool(value: boolean) {
  return new Bool(value);
}

/** An AST node corresponding to a nil value. */
class Nil extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.nil(this);
  }
  kind(): nodekind {
    return nodekind.nil;
  }
  toString(): string {
    return `nil`;
  }
  $value: null;
  constructor() {
    super();
    this.$value = null;
  }
}

/** Returns a new nil node. */
function $nil() {
  return new Nil();
}

/** An AST node corresponding to an integer. */
class Integer extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.integer(this);
  }
  kind(): nodekind {
    return nodekind.integer;
  }
  toString(): string {
    return `${this.$value}`;
  }
  $value: number;
  constructor(value: number) {
    super();
    this.$value = value;
  }
}

/** Returns a new integer node. */
function $int(value: number) {
  return new Integer(value);
}

class Float extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.float(this);
  }
  kind(): nodekind {
    return nodekind.float;
  }
  toString(): string {
    return `${this.$value}`;
  }
  $value: number;
  constructor(value: number) {
    super();
    this.$value = value;
  }
}

/** Returns a new float node. */
function $float(value: number) {
  return new Float(value);
}

class BigInteger extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.bigInteger(this);
  }
  kind(): nodekind {
    return nodekind.big_integer;
  }
  toString(): string {
    return `${this.$value}`;
  }
  $value: bigint;
  constructor(value: bigint) {
    super();
    this.$value = value;
  }
}

/** Returns a new Big Integer node. */
function $bigInteger(value: bigint) {
  return new BigInteger(value);
}

class SciNum extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.sciNum(this);
  }
  kind(): nodekind {
    return nodekind.scientific_number;
  }
  toString(): string {
    return this.$value.toString();
  }
  $value: Exponential;
  constructor(value: Exponential) {
    super();
    this.$value = value;
  }
}

/** Returns a new scientific number node. */
function $scinum(value: Exponential) {
  return new SciNum(value);
}

/** An AST node corresponding to a fraction. */
class Frac extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.frac(this);
  }
  kind(): nodekind {
    return nodekind.frac;
  }
  toString(): string {
    return this.$value.toString();
  }
  $value: FRACTION;
  constructor(value: FRACTION) {
    super();
    this.$value = value;
  }
}

/* Returns a new fraction node. */
function $frac(value: FRACTION) {
  return new Frac(value);
}

/** Returns an empty statement. */
function emptyStmt() {
  return $exprStmt($nil());
}

/** An AST node corresponding to a numeric constant expression. */
class NumConst extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.numConst(this);
  }
  kind(): nodekind {
    return nodekind.numeric_constant;
  }
  toString(): string {
    return this.$sym;
  }
  $sym: string;
  $value: number;
  constructor(sym: string, value: number) {
    super();
    this.$sym = sym;
    this.$value = value;
  }
}

/** Returns a new numeric constant node. */
function $numConst(symbol: string, value: number) {
  return new NumConst(symbol, value);
}

/** A token type corresponding to a binary logic operator. */
type BinaryLogicOp =
  | token_type.and
  | token_type.nand
  | token_type.nor
  | token_type.xnor
  | token_type.xor
  | token_type.or;

/** An AST node corresponding to a logical binary expression. */
class LogicalBinex extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.logicalBinex(this);
  }
  kind(): nodekind {
    return nodekind.logical_binex;
  }
  toString(): string {
    const left = this.$left.toString();
    const op = this.$op.$lexeme;
    const right = this.$right.toString();
    return `${left} ${op} ${right}`;
  }
  $left: Expr;
  $op: Token<BinaryLogicOp>;
  $right: Expr;
  constructor(left: Expr, op: Token<BinaryLogicOp>, right: Expr) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}

/** Returns a new logical binary expression node. */
function $logicalBinex(left: Expr, op: Token<BinaryLogicOp>, right: Expr) {
  return new LogicalBinex(left, op, right);
}

/** A token type corresponding to a relational operator. */
type RelationOp =
  | token_type.less
  | token_type.greater
  | token_type.equal_equal
  | token_type.bang_equal
  | token_type.greater_equal
  | token_type.less_equal;

/** An AST node corresponding to a relation expression node. */
class RelationExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.relationExpr(this);
  }
  kind(): nodekind {
    return nodekind.relation_expression;
  }
  toString(): string {
    const left = this.$left.toString();
    const op = this.$op.$lexeme;
    const right = this.$right.toString();
    return `${left} ${op} ${right}`;
  }
  $left: Expr;
  $op: Token<RelationOp>;
  $right: Expr;
  constructor(left: Expr, op: Token<RelationOp>, right: Expr) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}

/** Returns a new relation expression node. */
function $relation(left: Expr, op: Token<RelationOp>, right: Expr) {
  return new RelationExpr(left, op, right);
}

/** An AST node corresponding to a get expression. */
class GetExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.getExpr(this);
  }
  kind(): nodekind {
    return nodekind.get_expression;
  }
  toString(): string {
    return "";
  }
  $object: Expr;
  $name: Token;
  constructor(object: Expr, name: Token) {
    super();
    this.$object = object;
    this.$name = name;
  }
}

/** Returns a new Get Expression node. */
function $get(object: Expr, name: Token) {
  return new GetExpr(object, name);
}

/** Returns true, and asserts, if the given node is a get-expression node. */
function isGetExpr(node: ASTNode): node is GetExpr {
  return node.kind() === nodekind.get_expression;
}

/* An AST node corresponding to a string concatenation expression. */
class StringConcatExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.stringConcat(this);
  }
  kind(): nodekind {
    return nodekind.string_concatenation;
  }
  toString(): string {
    const left = this.$left.toString();
    const right = this.$right.toString();
    return `${left} & ${right}`;
  }
  $left: Expr;
  $op: Token<token_type.ampersand>;
  $right: Expr;
  constructor(left: Expr, op: Token<token_type.ampersand>, right: Expr) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}

/* Returns a new string concatenation node. */
function $stringConcat(
  left: Expr,
  op: Token<token_type.ampersand>,
  right: Expr
) {
  return new StringConcatExpr(left, op, right);
}

/** An AST node corresponding to a set expression. */
class SetExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.setExpr(this);
  }
  kind(): nodekind {
    return nodekind.set_expression;
  }
  toString(): string {
    return "";
  }
  $object: Expr;
  $name: Token;
  $value: Expr;
  constructor(object: Expr, name: Token, value: Expr) {
    super();
    this.$object = object;
    this.$name = name;
    this.$value = value;
  }
}

/** Returns a new set expression node. */
function $set(object: Expr, name: Token, value: Expr) {
  return new SetExpr(object, name, value);
}

/** An AST node corresponding to a super expression. */
class SuperExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.superExpr(this);
  }
  kind(): nodekind {
    return nodekind.super_expression;
  }
  toString(): string {
    return `super-expression`;
  }
  $method: Token;
  constructor(method: Token) {
    super();
    this.$method = method;
  }
}

/** Returns a new super expression node. */
function $super(method: Token) {
  return new SuperExpr(method);
}

/** An AST node corresponding to a this expression. */
class ThisExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.thisExpr(this);
  }
  kind(): nodekind {
    return nodekind.this_expression;
  }
  toString(): string {
    return `this`;
  }
  $keyword: Token;
  constructor(keyword: Token) {
    super();
    this.$keyword = keyword;
  }
}

/** Returns a new this-expression node. */
function $this(keyword: Token) {
  return new ThisExpr(keyword);
}

class ParserState<STMT extends TreeNode, EXPR extends TreeNode> {
  $error: null | Err = null;
  private lexer!: ReturnType<typeof lexical>;
  init(source: string) {
    this.lexer = lexical(source);
    this.next();
    return this;
  }
  $prev: Token = Token.empty;
  $cursor: number = -1;
  $peek: Token = Token.empty;
  $current: Token = Token.empty;
  $lastExpression: EXPR;
  $currentExpression: EXPR;
  $lastStmt: nodekind;
  $currentStmt: nodekind;
  constructor(nilExpression: EXPR, emptyStatement: STMT) {
    this.$lastExpression = nilExpression;
    this.$currentExpression = nilExpression;
    this.$lastStmt = emptyStatement.kind();
    this.$currentStmt = emptyStatement.kind();
  }
  /** Returns true if an implicit semicolon is permissible. */
  implicitSemicolonOK() {
    return this.$peek.isType(token_type.end) || this.atEnd();
  }
  /** Returns a new expression (in a RIGHT monad). */
  newExpr<E extends EXPR>(expression: E) {
    const prev = this.$currentExpression;
    this.$currentExpression = expression;
    this.$lastExpression = prev;
    return right(expression);
  }
  /** Returns a new statement (in a LEFT monad). */
  newStmt<S extends STMT>(statement: S) {
    const prev = this.$currentStmt;
    this.$currentStmt = statement.kind();
    this.$lastStmt = prev;
    return right(statement);
  }

  /** Moves the parser state forward. */
  next() {
    this.$cursor++;
    this.$current = this.$peek;
    const nxtToken = this.lexer.scan();
    if (nxtToken.isErrorToken()) {
      this.$error = nxtToken.$literal;
      return Token.end;
    }
    this.$peek = nxtToken;
    return this.$current;
  }
  /** Returns true if there is nothing left to parse in the parser state. */
  atEnd() {
    return this.$peek.isType(token_type.end) || this.$error !== null;
  }

  /** Returns a new error in a LEFT monad. */
  error(message: string, line: number) {
    const e = syntaxError(message, line);
    this.$error = e;
    return left(e);
  }

  /** Returns true if the current token is of the given type. */
  check(type: token_type) {
    if (this.atEnd()) return false;
    return this.$peek.isType(type);
  }

  /** Returns true and moves the parser forward if the next token is of the given type.  */
  nextIs(type: token_type) {
    if (this.$peek.isType(type)) {
      this.next();
      return true;
    }
    return false;
  }
}

/**
 * A function that returns a new parser state.
 * @param nilExpr A nil expression to serve as a placeholder expression.
 * @param emptyStmt An empty statement to serve as a placeholder statement.
 * @returns A new Parser State.
 */
function enstate<EXPR extends TreeNode, STMT extends TreeNode>(
  nilExpr: EXPR,
  emptyStmt: STMT
) {
  return new ParserState(nilExpr, emptyStmt);
}

/** The binding power of a given operator. Values of type `bp` are used the parsers to determinate operator precedence (both the Twine and CAM parsers use Pratt parsing for expressions). */
enum bp {
  nil,
  lowest,
  stringop,
  assign,
  atom,
  or,
  nor,
  and,
  nand,
  xor,
  xnor,
  not,
  eq,
  rel,
  sum,
  difference,
  product,
  quotient,
  imul,
  power,
  dot_product,
  postfix,
  call,
}

/** @internal A Pratt parsing function. */
type Parslet<T> = (current: Token, lastNode: T) => Either<Err, T>;

/** @internal An entry within parser’s BP table. The first element is a prefix parslet, the second element is an infix parslet, and the last element is the binding power of the operator. */
type ParsletEntry<T> = [Parslet<T>, Parslet<T>, bp];

/** @internal A record of parslet entries, where each key is a token type (`tt`). */
type BPTable<T> = Record<token_type, ParsletEntry<T>>;

/**
 * @param source - the source code to parse.
 * @returns An object with two methods:
 * 1. parsex - parses a single expression.
 * 2. parset - parses a list of statements.
 */
export function syntax(source: string) {
  /** Begin by initializing the state. */
  const state = enstate<Expr, Statement>($nil(), emptyStmt()).init(source);

  /**
   * The “blank” binding power. This particular binding power
   * is bound either (1) the {@link ___|blank parslet}
   * or (2) parlsets that should not trigger recursive calls.
   */
  const ___o = bp.nil;

  /**
   * The “blank” parslet. This parslet is used as a placeholder.
   * If the {@link expr|expression parser} calls this parslet,
   * then the {@link error} variable is set and parsing shall cease.
   */
  const ___: Parslet<Expr> = (token) => {
    if (state.$error !== null) {
      return left(state.$error);
    } else {
      return state.error(`Unexpected lexeme: ${token.$lexeme}`, token.$line);
    }
  };

  /** Parses a parenthesized expression */
  const primary = () => {
    const innerExpression = expr();
    if (innerExpression.isLeft()) return innerExpression;
    if (state.nextIs(token_type.comma)) {
      const elements: Expr[] = [innerExpression.unwrap()];
      do {
        const e = expr();
        if (e.isLeft()) return e;
        elements.push(e.unwrap());
      } while (state.nextIs(token_type.comma));
      if (!state.nextIs(token_type.right_paren))
        return state.error(
          `Expected a ")" to close the tuple.`,
          state.$current.$line
        );
      return state.newExpr($tuple(elements));
    }
    if (!state.nextIs(token_type.right_paren)) {
      return state.error(`Expected a closing ")".`, state.$current.$line);
    }
    return innerExpression.map((e) => $parend(e));
  };

  /**
   * Returns true if the given node kind is of the type
   * that allows implicit multiplication.
   */
  const allowImplicit = (kind: nodekind) =>
    kind === nodekind.algebraic_binex ||
    kind === nodekind.negation_expression ||
    kind === nodekind.float ||
    kind === nodekind.numeric_constant ||
    kind === nodekind.native_call ||
    kind === nodekind.integer ||
    kind === nodekind.parend_expression;

  /** Parses a comma separated list. */
  const commaSepList = <E extends Expr>(
    filter: (e: Expr) => e is E,
    errorMessage: string
  ) => {
    const elements: E[] = [];
    do {
      const e = expr();
      if (e.isLeft()) return e;
      const element = e.unwrap();
      if (!filter(element)) {
        return state.error(errorMessage, state.$current.$line);
      }
      elements.push(element);
    } while (state.nextIs(token_type.comma));
    return right(elements);
  };

  /** Parses a function call expression */
  const funCall = (op: Token, node: Expr) => {
    const callee = node;
    // If the callee is a parenthesized expression, we want to check
    // if this is implicit multiplication.
    // E.g., (x + 2)(y + 9)
    if (isParendExpr(callee) && allowImplicit(callee.$inner.kind())) {
      const left = callee.$inner;
      const r = expr();
      if (r.isLeft()) return r;
      if (!state.nextIs(token_type.right_paren)) {
        return state.error(`Expected a closing ")"`, state.$current.$line);
      }
      const right = r.unwrap();
      const star = token(token_type.star, "*", state.$current.$line);
      return state.newExpr($algebraicBinex(left, star, right));
    }
    let args: Expr[] = [];
    if (!state.check(token_type.right_paren)) {
      const arglist = commaSepList(
        (node): node is Expr => node instanceof Expr,
        "Expected an expression"
      );
      if (arglist.isLeft()) return arglist;
      args = arglist.unwrap();
    }
    const paren = state.next();
    if (!paren.isType(token_type.right_paren)) {
      return state.error('Expected a closing ")', paren.$line);
    }
    return state.newExpr($call(callee, op, args));
  };

  /** Parses a vector expression */
  const vectorExpression = (prev: Token) => {
    const elements: Expr[] = [];
    const vectors: VectorExpr[] = [];
    let rows = 0;
    let columns = 0;
    if (!state.check(token_type.right_bracket)) {
      do {
        const elem = expr();
        if (elem.isLeft()) return elem;
        const element = elem.unwrap();
        // if this element is a vector expression, then we have a matrix
        if (isVectorExpr(element)) {
          rows++;
          columns = element.$elements.length;
          vectors.push(element);
        } else {
          elements.push(element);
        }
      } while (state.nextIs(token_type.comma) && !state.atEnd());
    }
    if (!state.nextIs(token_type.right_bracket)) {
      return state.error(`Expected a right bracket "]"`, state.$current.$line);
    }
    if (vectors.length !== 0) {
      if (vectors.length !== columns) {
        return state.error(
          `Encountered a jagged matrix.`,
          state.$current.$line
        );
      }
      return state.newExpr($matrix(vectors, rows, columns, prev));
    }
    return state.newExpr($vector(prev, elements));
  };

  /** Parses an indexing expression. */
  const indexingExpression: Parslet<Expr> = (op, lhs) => {
    const index = expr();
    if (index.isLeft()) return index;
    const rbracket = state.next();
    if (!rbracket.isType(token_type.right_bracket)) {
      return state.error(`Expected a right bracket "]"`, rbracket.$line);
    }
    return state.newExpr($index(lhs, index.unwrap(), op));
  };

  /** Parses a get expression */
  const getExpression = (op: Token, lhs: Expr) => {
    const nxt = state.next();
    if (!nxt.isType(token_type.symbol)) {
      return state.error("Expected a property name", nxt.$line);
    }
    let exp = $get(lhs, nxt);
    if (state.nextIs(token_type.left_paren)) {
      const args: Expr[] = [];
      if (!state.check(token_type.right_paren)) {
        do {
          const x = expr();
          if (x.isLeft()) return x;
          const arg = x.unwrap();
          args.push(arg);
        } while (state.nextIs(token_type.comma));
      }
      const rparen = state.next();
      if (!rparen.isType(token_type.right_paren)) {
        return state.error(`Expected ")" after method arguments`, rparen.$line);
      }
      return state.newExpr($call(exp, op, args));
    }
    return state.newExpr(exp);
  };

  /** Parses an assignment expression */
  const assignment = (op: Token, node: Expr) => {
    if (isSymbol(node)) {
      return expr().chain((n) => state.newExpr($assign(node, n)));
    } else if (isGetExpr(node)) {
      const rhs = expr();
      if (rhs.isLeft()) return rhs;
      return state.newExpr($set(node.$object, node.$name, rhs.unwrap()));
    } else {
      return state.error(
        `Expected a valid assignment target, but got ${node.toString()}`,
        op.$line
      );
    }
  };

  /** Returns a factorial expression parser. */
  const factorialExpression = (op: Token, node: Expr) => {
    if (op.isType(token_type.bang)) {
      return state.newExpr($factorial(op, node));
    }
    return state.error(`Expected "!" but got ${op.$lexeme}`, op.$line);
  };

  /** Returns an increment or decrement parser. */
  const incdec = (operator: "+" | "-" | "*") => (op: Token, node: Expr) => {
    const tt =
      operator === "+"
        ? token_type.plus
        : operator === "*"
        ? token_type.star
        : token_type.minus;
    if (isSymbol(node)) {
      const right = $algebraicBinex(
        node,
        token(tt, operator, op.$line),
        operator === "*" ? node : $int(1)
      );
      return state.newExpr($assign(node, right));
    } else {
      return state.error(
        `Expected the lefthand side of "${operator}${operator}" to be either a variable or property accessor, but got ${node.toString()}`,
        state.$current.$line
      );
    }
  };

  /** Parses a decrement expression. */
  const decrementExpression = incdec("-");

  /** Parses an increment expression. */
  const incrementExpression = incdec("+");

  /** Parses an increment expression. */
  const squareExpression = incdec("+");

  /** Parses a vector infix expression. */
  const vectorInfix: Parslet<Expr> = (op, left) => {
    const p = precof(op.$type);
    return expr(p).chain((right) =>
      state.newExpr($vectorBinex(left, op as Token<VectorBinop>, right))
    );
  };

  /* Parses a comparison expression. */
  const compare = (op: Token, lhs: Expr) => {
    const p = precof(op.$type);
    return expr(p).chain((rhs) => {
      return state.newExpr($relation(lhs, op as Token<RelationOp>, rhs));
    });
  };

  /* Parses a prefix expression. */
  const prefix: Parslet<Expr> = (op) => {
    const p = precof(op.$type);
    const a = expr(p);
    if (a.isLeft()) return a;
    const arg = a.unwrap();
    if (op.isType(token_type.minus)) {
      return state.newExpr($neg(op, arg));
    } else if (op.isType(token_type.plus)) {
      return state.newExpr($pos(op, arg));
    } else {
      return state.error(`Unknown prefix operator "${op.$lexeme}"`, op.$line);
    }
  };

  /* Parses an infix expression. */
  const infix = (op: Token, lhs: Expr) => {
    // Detour to handling complex assignments.
    // E.g., x += 1
    if (state.nextIs(token_type.equal)) {
      if (isSymbol(lhs)) {
        const name = lhs;
        const r = expr();
        if (r.isLeft()) return r;
        const rhs = r.unwrap();
        const value = $algebraicBinex(lhs, op as Token<AlgebraicOp>, rhs);
        return state.newExpr($assign(name, value));
      } else {
        return state.error(
          `Invalid lefthand side of assignment. Expected a variable to the left of "${
            op.$lexeme
          }", but got "${lhs.toString()}"`,
          op.$line
        );
      }
    }
    // actual handling of infix expressions
    const p = precof(op.$type);
    const RHS = expr(p);
    if (RHS.isLeft()) return RHS;
    const rhs = RHS.unwrap();
    return state.newExpr($algebraicBinex(lhs, op as Token<AlgebraicOp>, rhs));
  };

  /* Parses a logical infix expression. */
  const logicInfix = (op: Token, lhs: Expr) => {
    const p = precof(op.$type);
    return expr(p).chain((rhs) => {
      return state.newExpr($logicalBinex(lhs, op as Token<BinaryLogicOp>, rhs));
    });
  };

  /* Parses a logical not expression. */
  const logicNot = (op: Token) => {
    const p = precof(op.$type);
    return expr(p).chain((arg) =>
      state.newExpr($not(op as Token<token_type.not>, arg))
    );
  };

  /* Parses a symbol. */
  const varname: Parslet<Expr> = (op) => {
    if (op.isType(token_type.symbol)) {
      const out = $sym(op);
      return state.newExpr(out);
    } else {
      return state.error(`Unexpected variable "${op.$lexeme}"`, op.$line);
    }
  };

  /* Parses an implicit multiplication. */
  const impMul: Parslet<Expr> = (op, left) => {
    if (op.isType(token_type.symbol)) {
      const right = $sym(op);
      const star = token(token_type.star, "*", op.$line);
      return state.newExpr($algebraicBinex(left, star, right));
    } else {
      return state.error(
        `Expected a symbol for implicit multiplication, but got "${op.$lexeme}"`,
        op.$line
      );
    }
  };

  /* Parses a string literal. */
  const stringLiteral: Parslet<Expr> = (t) => state.newExpr($string(t.$lexeme));

  /* Parses a boolean literal. */
  const boolLiteral: Parslet<Expr> = (op) => {
    if (op.isType(token_type.boolean) && typeof op.$literal === "boolean") {
      return state.newExpr($bool(op.$literal));
    } else {
      return state.error(`Unexpected boolean literal`, op.$line);
    }
  };

  /* Parses a native constant. */
  const constant = (op: Token) => {
    const type = op.$type;
    const erm = `Unexpected constant "${op.$lexeme}"`;
    switch (type) {
      case token_type.nan:
        return state.newExpr($numConst("NaN", NaN));
      case token_type.inf:
        return state.newExpr($numConst("Inf", Infinity));
      case token_type.numeric_constant: {
        switch (op.$lexeme as NativeConstants) {
          case "e":
            return state.newExpr($numConst("e", Math.E));
          case "pi":
            return state.newExpr($numConst("pi", Math.PI));
          case "ln2":
            return state.newExpr($numConst("ln2", Math.LN2));
          case "ln10":
            return state.newExpr($numConst("ln10", Math.LN10));
          case "log10e":
            return state.newExpr($numConst("log10e", Math.LOG10E));
          case "log2e":
            return state.newExpr($numConst("log2e", Math.LOG2E));
          case "sqrt2":
            return state.newExpr($numConst("sqrt2", Math.SQRT2));
        }
      }
      default:
        return state.error(erm, op.$line);
    }
  };

  /* Parses a number */
  const number = (t: Token) => {
    if (t.isNumber()) {
      const out = t.isType(token_type.integer)
        ? state.newExpr($int(t.$literal))
        : state.newExpr($float(t.$literal));
      const peek = state.$peek;
      if (
        peek.isType(token_type.left_paren) ||
        peek.isType(token_type.native) ||
        peek.isType(token_type.symbol)
      ) {
        const r = expr(bp.imul);
        if (r.isLeft()) return r;
        const right = r.unwrap();
        const star = token(token_type.star, "*", peek.$line);
        const left = out.unwrap();
        return state.newExpr($parend($algebraicBinex(left, star, right)));
      }
      return out;
    } else {
      return state.error(`Expected a number, but got "${t.$lexeme}"`, t.$line);
    }
  };

  /* Parses a native call expression. */
  const ncall = (op: Token) => {
    if (!state.nextIs(token_type.left_paren)) {
      return state.error(`Expected a "(" to open the argument list`, op.$line);
    }
    let args: Expr[] = [];
    if (!state.check(token_type.right_paren)) {
      const arglist = commaSepList(
        (e): e is Expr => e instanceof Expr,
        `Expected an expression`
      );
      if (arglist.isLeft()) return arglist;
      args = arglist.unwrap();
    }
    if (!state.nextIs(token_type.right_paren)) {
      return state.error(
        `Expected ")" to close the argument list`,
        state.$current.$line
      );
    }
    return state.newExpr($nativeCall(op as Token<token_type.native>, args));
  };

  /* Parses a string concatenation expression. */
  const concatenation: Parslet<Expr> = (op: Token, left: Expr) => {
    const p = precof(op.$type);
    return expr(p).chain((right) => {
      return state.newExpr(
        $stringConcat(left, op as Token<token_type.ampersand>, right)
      );
    });
  };

  /** Parses a fraction literal. */
  const fract = (op: Token) => {
    if (op.isType(token_type.fraction) && op.$literal instanceof FRACTION) {
      return state.newExpr($frac(op.$literal));
    } else {
      return state.error(`Unexpected fraction`, op.$line);
    }
  };

  /* Parses an algebraic string literal */
  const algString = (op: Token) => {
    if (
      op.isType(token_type.algebra_string) &&
      typeof op.$literal === "string"
    ) {
      const tkns = op.$literal;
      const t = token(token_type.algebra_string, "", op.$line);
      const result = syntax(tkns).parsex();
      if (result.isLeft()) return result;
      const expression = result.unwrap();
      return state.newExpr($algebraString(expression, op));
    } else {
      return state.error(`Unexpected algebraic string`, op.$line);
    }
  };

  /* Parses a this expression. */
  const thisExpression = (t: Token) => state.newExpr($this(t));

  /**
   * The rules table comprises mappings from every
   * token type to a triple `(Prefix, Infix, B)`,
   * where `Prefix` and `Infix` are parslets (small
   * parsers that handle a single grammar rule), and `B` is a
   * binding power.
   */
  const rules: BPTable<Expr> = {
    [token_type.end]: [___, ___, ___o],
    [token_type.error]: [___, ___, ___o],
    [token_type.empty]: [___, ___, ___o],
    [token_type.left_paren]: [primary, funCall, bp.call],
    [token_type.right_paren]: [___, ___, ___o],
    [token_type.left_brace]: [___, ___, ___o],
    [token_type.right_brace]: [___, ___, ___o],
    [token_type.left_bracket]: [vectorExpression, indexingExpression, bp.call],
    [token_type.right_bracket]: [___, ___, ___o],
    [token_type.semicolon]: [___, ___, ___o],
    [token_type.colon]: [___, ___, ___o],
    [token_type.dot]: [___, getExpression, bp.call],
    [token_type.comma]: [___, ___, ___o],
    // algebraic expressions
    [token_type.plus]: [prefix, infix, bp.sum],
    [token_type.minus]: [prefix, infix, bp.difference],
    [token_type.star]: [___, infix, bp.product],
    [token_type.slash]: [___, infix, bp.quotient],
    [token_type.caret]: [___, infix, bp.power],
    [token_type.percent]: [___, infix, bp.quotient],
    [token_type.rem]: [___, infix, bp.quotient],
    [token_type.mod]: [___, infix, bp.quotient],
    [token_type.div]: [___, infix, bp.quotient],

    [token_type.bang]: [___, factorialExpression, bp.postfix],
    [token_type.ampersand]: [___, concatenation, bp.stringop],
    [token_type.tilde]: [___, ___, ___o],
    [token_type.vbar]: [___, ___, ___o],
    [token_type.equal]: [___, assignment, bp.assign],

    // comparison expressions
    [token_type.less]: [___, compare, bp.rel],
    [token_type.greater]: [___, compare, bp.rel],
    [token_type.less_equal]: [___, compare, bp.rel],
    [token_type.greater_equal]: [___, compare, bp.rel],
    [token_type.bang_equal]: [___, compare, bp.rel],
    [token_type.equal_equal]: [___, compare, bp.rel],

    // tickers
    [token_type.plus_plus]: [___, incrementExpression, bp.postfix],
    [token_type.minus_minus]: [___, decrementExpression, bp.postfix],
    [token_type.star_star]: [___, squareExpression, bp.postfix],

    // Vector operation expressions
    [token_type.dot_add]: [___, vectorInfix, bp.sum],
    [token_type.dot_star]: [___, vectorInfix, bp.product],
    [token_type.dot_minus]: [___, vectorInfix, bp.sum],
    [token_type.dot_caret]: [___, vectorInfix, bp.power],
    [token_type.at]: [___, vectorInfix, bp.dot_product],

    // Matrix operation expressions
    [token_type.pound_plus]: [___, ___, ___o],
    [token_type.pound_minus]: [___, ___, ___o],
    [token_type.pound_star]: [___, ___, ___o],

    // Literals
    [token_type.integer]: [number, ___, bp.atom],
    [token_type.float]: [number, ___, bp.atom],
    [token_type.fraction]: [fract, ___, bp.atom],
    [token_type.scientific]: [___, ___, ___o],
    [token_type.big_integer]: [___, ___, ___o],
    [token_type.symbol]: [varname, impMul, bp.atom],
    [token_type.string]: [stringLiteral, ___, bp.atom],
    [token_type.boolean]: [boolLiteral, ___, bp.atom],
    [token_type.nan]: [constant, ___, bp.atom],
    [token_type.inf]: [constant, ___, bp.atom],
    [token_type.nil]: [constant, ___, bp.atom],
    [token_type.numeric_constant]: [constant, ___, bp.atom],
    [token_type.algebraic]: [algString, ___, bp.atom],

    // logical operations
    [token_type.and]: [___, logicInfix, bp.and],
    [token_type.or]: [___, logicInfix, bp.or],
    [token_type.nand]: [___, logicInfix, bp.nand],
    [token_type.xor]: [___, logicInfix, bp.xor],
    [token_type.xnor]: [___, logicInfix, bp.xnor],
    [token_type.nor]: [___, logicInfix, bp.nor],
    [token_type.not]: [logicNot, ___, bp.not],

    [token_type.if]: [___, ___, ___o],
    [token_type.else]: [___, ___, ___o],
    [token_type.fn]: [___, ___, ___o],
    [token_type.let]: [___, ___, ___o],
    [token_type.var]: [___, ___, ___o],
    [token_type.return]: [___, ___, ___o],
    [token_type.while]: [___, ___, ___o],
    [token_type.for]: [___, ___, ___o],
    [token_type.class]: [___, ___, ___o],
    [token_type.print]: [___, ___, ___o],
    [token_type.super]: [___, ___, ___o],
    [token_type.this]: [thisExpression, ___, bp.atom],

    // native calls
    [token_type.native]: [ncall, ___, bp.call],

    [token_type.algebra_string]: [___, ___, ___o],
  };
  /**
   * Returns the prefix parsing rule mapped to by the given
   * token type.
   */
  const prefixRule = (t: token_type): Parslet<Expr> => rules[t][0];

  /**
   * Returns the infix parsing rule mapped to by the given
   * token type.
   */
  const infixRule = (t: token_type): Parslet<Expr> => rules[t][1];

  /**
   * Returns the {@link bp|precedence} of the given token type.
   */
  const precof = (t: token_type): bp => rules[t][2];

  const expr = (minbp: number = bp.lowest): Either<Err, Expr> => {
    let token = state.next();
    const pre = prefixRule(token.$type);
    let lhs = pre(token, $nil());
    if (lhs.isLeft()) {
      return lhs;
    }
    while (minbp < precof(state.$peek.$type)) {
      token = state.next();
      const r = infixRule(token.$type);
      const rhs = r(token, lhs.unwrap());
      if (rhs.isLeft()) return rhs;
      lhs = rhs;
    }
    return lhs;
  };

  // Statement parsers hereinafter.
  const EXPRESSION = () => {
    const out = expr();
    if (out.isLeft()) return out;
    const expression = out.unwrap();
    if (state.nextIs(token_type.semicolon) || state.implicitSemicolonOK()) {
      return state.newStmt($exprStmt(expression));
    } else {
      return state.error(
        `Expected ";" to end the statement`,
        state.$current.$line
      );
    }
  };

  const VAR = (prev: token_type.let | token_type.var) => {
    const name = state.next();
    if (!name.isType(token_type.symbol)) {
      return state.error(`Expected a valid identifier`, name.$line);
    }
    if (!state.nextIs(token_type.equal)) {
      return state.error(
        `Expected an assignment operator`,
        state.$current.$line
      );
    }
    const init = EXPRESSION();
    if (init.isLeft()) return init;
    const value = init.unwrap();
    return state.newStmt(
      (prev === token_type.let ? $let : $var)($sym(name), value.$expression)
    );
  };

  const BLOCK = () => {
    const statements: Statement[] = [];
    while (!state.atEnd() && !state.check(token_type.right_brace)) {
      const stmt = STATEMENT();
      if (stmt.isLeft()) return stmt;
      statements.push(stmt.unwrap());
    }
    if (!state.nextIs(token_type.right_brace)) {
      return state.error(
        `Expected a "}" to close the block`,
        state.$current.$line
      );
    }
    return state.newStmt($blockStmt(statements));
  };

  const FN = (): Either<Err, FnStmt> => {
    const name = state.next();
    if (!name.isType(token_type.symbol)) {
      return state.error(
        `Expected a valid identifier for the function's name, but got ${name.$lexeme}`,
        name.$line
      );
    }
    if (!state.nextIs(token_type.left_paren)) {
      return state.error(
        `Expected a "(" to begin the parameter list`,
        state.$current.$line
      );
    }
    const params: Token<token_type.symbol>[] = [];
    if (!state.$peek.isType(token_type.right_paren)) {
      do {
        const expression = state.next();
        if (!expression.isType(token_type.symbol)) {
          return state.error(
            `Expected a valid identifier as a parameter, but got "${expression.$lexeme}"`,
            expression.$line
          );
        }
        params.push(expression);
      } while (state.nextIs(token_type.comma));
    }
    if (!state.nextIs(token_type.right_paren)) {
      return state.error(
        `Expected a ")" to close the parameter list`,
        state.$current.$line
      );
    }
    if (state.nextIs(token_type.equal)) {
      const body = EXPRESSION();
      return body.chain((b) =>
        state.newStmt(
          $fnStmt(
            name,
            params.map((s) => $sym(s)),
            [b]
          )
        )
      );
    }
    if (!state.nextIs(token_type.left_brace)) {
      return state.error(
        `Expected a "{" to open the function's body`,
        state.$current.$line
      );
    }
    const body = BLOCK();
    return body.chain((b) =>
      state.newStmt(
        $fnStmt(
          name,
          params.map((p) => $sym(p)),
          b.$statements
        )
      )
    );
  };

  const IF = () => {
    const keyword = state.$current;
    const c = expr();
    if (c.isLeft()) return c;
    const condition = c.unwrap();
    if (!state.nextIs(token_type.left_brace)) {
      return state.error(
        `Expected a left brace "{" to begin the consequent block`,
        state.$current.$line
      );
    }
    const consequent = BLOCK();
    if (consequent.isLeft()) return consequent;
    const thenBranch = consequent.unwrap();
    let elseBranch: Statement = $returnStmt(state.$current, $nil());
    if (state.nextIs(token_type.else)) {
      const _else = STATEMENT();
      if (_else.isLeft()) return _else;
      elseBranch = _else.unwrap();
    }
    return state.newStmt($ifStmt(keyword, condition, thenBranch, elseBranch));
  };

  const RETURN = () => {
    const c = state.$current;
    const out = EXPRESSION();
    return out.chain((e) => state.newStmt($returnStmt(c, e.$expression)));
  };

  const WHILE = () => {
    const current = state.$current;
    const loopCondition = expr();
    if (loopCondition.isLeft()) return loopCondition;
    if (!state.nextIs(token_type.left_brace)) {
      return state.error(
        `Expected a block after the condition`,
        state.$current.$line
      );
    }
    const body = BLOCK();
    if (body.isLeft()) return body;
    return body.chain((loopBody) =>
      state.newStmt($while(current, loopCondition.unwrap(), loopBody))
    );
  };

  const FOR = () => {
    const current = state.$current;
    const preclauseToken = state.next();
    if (!preclauseToken.isType(token_type.left_paren)) {
      return state.error(
        `Expected a "(" after "for" to begin the loop's clauses, but got "${preclauseToken.$lexeme}"`,
        preclauseToken.$line
      );
    }
    let init: Statement | null = null;
    if (state.nextIs(token_type.semicolon)) {
      init = init;
    } else if (state.nextIs(token_type.var)) {
      const initializer = VAR(token_type.var);
      if (initializer.isLeft()) return initializer;
      init = initializer.unwrap();
    } else {
      const exp = EXPRESSION();
      if (exp.isLeft()) return exp;
      init = exp.unwrap();
    }
    let condition: Expr | null = null;
    if (!state.check(token_type.semicolon)) {
      const c = expr();
      if (c.isLeft()) return c;
      condition = c.unwrap();
    }
    const postConditionToken = state.next();
    if (!postConditionToken.isType(token_type.semicolon)) {
      return state.error(
        `Expected a ";" after the for-loop condition, but got "${postConditionToken.$lexeme}"`,
        postConditionToken.$line
      );
    }
    let ticker: Expr | null = null;
    if (!state.check(token_type.right_paren)) {
      const _ticker = expr();
      if (_ticker.isLeft()) return _ticker;
      ticker = _ticker.unwrap();
    }
    const postIncrementToken = state.next();
    if (!postIncrementToken.isType(token_type.right_paren)) {
      return state.error(
        `Expected a ")" to close the for-loop's clauses, but got "${postIncrementToken.$lexeme}"`,
        postConditionToken.$line
      );
    }
    const b = STATEMENT();
    if (b.isLeft()) return b;
    let body: Statement = b.unwrap();
    if (ticker !== null) {
      if (isBlockStmt(body)) {
        body.$statements.push($exprStmt(ticker));
      } else {
        body = $blockStmt([body, $exprStmt(ticker)]);
      }
    }
    let loopCondition: Expr = $bool(true);
    if (condition !== null) {
      loopCondition = condition;
    }
    body = $while(current, loopCondition, body);
    if (init !== null) {
      body = $blockStmt([init, body]);
    }
    return state.newStmt(body);
  };

  const CLASS = () => {
    const name = state.next();
    if (!name.isType(token_type.symbol)) {
      return state.error(
        `Expected a valid identifier after "class" but got "${name.$lexeme}"`,
        name.$line
      );
    }
    const lbrace = state.next();
    if (!lbrace.isType(token_type.left_brace)) {
      return state.error(
        `Expected a "{" to begin the body of class "${name.$lexeme}", but got "${lbrace.$lexeme}".`,
        lbrace.$line
      );
    }
    const methods: FnStmt[] = [];
    while (!state.check(token_type.left_brace) && !state.atEnd()) {
      const f = FN();
      if (f.isLeft()) return f;
      methods.push(f.unwrap());
    }
    const postMethodsToken = state.next();
    if (!postMethodsToken.isType(token_type.right_brace)) {
      return state.error(
        `Expected a "}" after the body of class "${name.$lexeme}", but got "${postMethodsToken.$lexeme}".`,
        postMethodsToken.$line
      );
    }
    return state.newStmt($classStmt(name, methods));
  };

  const PRINT = () => {
    const current = state.$current;
    const arg = EXPRESSION();
    return arg.map((x) => $printStmt(current, x.$expression));
  };

  const STATEMENT = (): Either<Err, Statement> => {
    if (state.nextIs(token_type.var)) {
      return VAR(token_type.var);
    } else if (state.nextIs(token_type.let)) {
      return VAR(token_type.let);
    } else if (state.nextIs(token_type.fn)) {
      return FN();
    } else if (state.nextIs(token_type.left_brace)) {
      return BLOCK();
    } else if (state.nextIs(token_type.if)) {
      return IF();
    } else if (state.nextIs(token_type.return)) {
      return RETURN();
    } else if (state.nextIs(token_type.while)) {
      return WHILE();
    } else if (state.nextIs(token_type.for)) {
      return FOR();
    } else if (state.nextIs(token_type.class)) {
      return CLASS();
    } else if (state.nextIs(token_type.print)) {
      return PRINT();
    } else {
      return EXPRESSION();
    }
  };

  return {
    /** Returns a syntax analysis of a single expression. */
    parsex() {
      if (state.$error !== null) {
        return left(state.$error);
      } else {
        const out = expr();
        return out;
      }
    },
    /** Returns a syntax analysis of the source code's statements. */
    parset() {
      if (state.$error !== null) {
        return left(state.$error);
      }
      const statements: Statement[] = [];
      while (!state.atEnd()) {
        const statement = STATEMENT();
        if (statement.isLeft()) {
          return statement;
        }
        statements.push(statement.unwrap());
      }
      return right(statements);
    },
  };
}

type TypeName =
  | "nil"
  | "integer"
  | "float"
  | "string"
  | "bool"
  | "big_integer"
  | "fraction"
  | "exponential"
  | "error"
  | "obj"
  | "vector"
  | "matrix"
  | "fn"
  | "class"
  | "list"
  | "unknown";

function typename(x: Primitive): TypeName {
  if (x === null) {
    return "nil";
  } else if (typeof x === "number") {
    if (Number.isInteger(x)) {
      return "integer";
    } else {
      return "float";
    }
  } else if (typeof x === "string") {
    return "string";
  } else if (typeof x === "boolean") {
    return "bool";
  } else if (typeof x === "bigint") {
    return "big_integer";
  } else if (x instanceof FRACTION) {
    return "fraction";
  } else if (x instanceof Exponential) {
    return "exponential";
  } else if (x instanceof Err) {
    return "error";
  } else if (x instanceof Obj) {
    return "obj";
  } else if (x instanceof Vector) {
    return "vector";
  } else if (x instanceof Matrix) {
    return "matrix";
  } else if (x instanceof Fn) {
    return "fn";
  } else if (x instanceof Class) {
    return "class";
  } else if (x instanceof LinkedList) {
    return "list";
  } else {
    return "unknown";
  }
}

/** Returns a string form of the given Winnow primitive. */
export function strof(u: Primitive) {
  if (isExponential(u) || isFraction(u) || isErr(u)) {
    return u.toString();
  } else if (u === null) {
    return "nil";
  } else {
    return `${u}`;
  }
}

interface Resolvable<X = any> {
  resolve(expr: Expr, i: number): X;
}

enum functionType {
  none,
  function,
  method,
  initializer,
}

enum classType {
  none,
  class,
}

class Resolver<T extends Resolvable = Resolvable> implements Visitor<void> {
  private $scopes: Map<string, boolean>[] = [];
  private scopesIsEmpty() {
    return this.$scopes.length === 0;
  }
  private $currentFunction: functionType = functionType.none;
  private $currentClass: classType = classType.none;
  private beginScope() {
    this.$scopes.push(new Map());
  }
  private endScope() {
    this.$scopes.pop();
  }
  private resolveEach(nodes: ASTNode[]) {
    for (let i = 0; i < nodes.length; i++) {
      this.resolve(nodes[i]);
    }
    return;
  }
  private resolve(node: ASTNode) {
    node.accept(this);
  }
  private peek(): Map<string, boolean> {
    return this.$scopes[this.$scopes.length - 1];
  }
  private declare(name: Token) {
    if (this.$scopes.length === 0) {
      return;
    }
    const scope = this.peek();
    if (scope.has(name.$lexeme)) {
      throw resolverError(
        `Encountered a name collision. The variable “${name.$lexeme}” has already been declared in the current scope.`,
        name.$line
      );
    }
    scope.set(name.$lexeme, false);
  }
  private define(name: string) {
    if (this.$scopes.length === 0) return;
    const peek = this.peek();
    peek.set(name, true);
  }

  private resolveFn(node: FnStmt, type: functionType) {
    const enclosingFunction = this.$currentFunction;
    this.$currentFunction = type;
    this.beginScope();
    for (let i = 0; i < node.$params.length; i++) {
      this.declare(node.$params[i].$symbol);
      this.define(node.$params[i].$symbol.$lexeme);
    }
    this.resolveEach(node.$body);
    this.endScope();
    this.$currentFunction = enclosingFunction;
  }

  resolveLocal(node: Expr, name: string) {
    for (let i = this.$scopes.length - 1; i >= 0; i--) {
      const scope = this.$scopes[i];
      if (scope !== undefined && scope.has(name)) {
        this.client.resolve(node, this.$scopes.length - 1 - i);
        return;
      }
    }
  }

  resolved(statements: Statement[]) {
    try {
      for (let i = 0; i < statements.length; i++) {
        this.resolve(statements[i]);
      }
      return right(1);
    } catch (error) {
      return left(error as Err);
    }
  }
  client: T;
  constructor(client: T) {
    this.client = client;
  }
  blockStmt(node: BlockStmt): void {
    this.beginScope();
    this.resolveEach(node.$statements);
    this.endScope();
    return;
  }
  exprStmt(node: ExprStmt): void {
    this.resolve(node.$expression);
    return;
  }
  fnStmt(node: FnStmt): void {
    this.declare(node.$name);
    this.define(node.$name.$lexeme);
    this.resolveFn(node, functionType.function);
    return;
  }
  ifStmt(node: IfStmt): void {
    this.resolve(node.$condition);
    this.resolve(node.$then);
    this.resolve(node.$alt);
    return;
  }
  printStmt(node: PrintStmt): void {
    this.resolve(node.$expression);
    return;
  }
  returnStmt(node: ReturnStmt): void {
    if (this.$currentFunction === functionType.none) {
      throw resolverError(
        `Encountered the “return” keyword at the top-level. This syntax has no semantic.`,
        node.$keyword.$line
      );
    }
    if (this.$currentFunction === functionType.initializer) {
      throw resolverError(
        `Encounterd the “return” keyword within an initializer.`,
        node.$keyword.$line
      );
    }
    this.resolve(node.$value);
    return;
  }
  variableStmt(node: VariableStmt): void {
    this.declare(node.$variable.$symbol);
    this.resolve(node.$value);
    this.define(node.$variable.$symbol.$lexeme);
    return;
  }
  whileStmt(node: WhileStmt): void {
    this.resolve(node.$condition);
    this.resolve(node.$body);
  }
  classStmt(node: ClassStmt): void {
    const enclosingClass = this.$currentClass;
    this.$currentClass = classType.class;
    this.declare(node.$name);
    this.define(node.$name.$lexeme);
    this.beginScope();
    const peek = this.peek();
    peek.set("this", true);
    const methods = node.$methods;
    for (let i = 0; i < methods.length; i++) {
      const method = methods[i];
      let declaration = functionType.method;
      if (method.$name.$lexeme === "init") {
        declaration = functionType.initializer;
      }
      this.resolveFn(method, declaration);
    }
    this.endScope();
    this.$currentClass = enclosingClass;
    return;
  }
  indexExpr(node: IndexExpr): void {
    this.resolve(node.$list);
    this.resolve(node.$index);
    return;
  }
  algebraString(node: AlgebraString): void {
    return;
  }
  tupleExpr(node: TupleExpr): void {
    this.resolveEach(node.$elements.toArray());
    return;
  }
  vectorExpr(node: VectorExpr): void {
    this.resolveEach(node.$elements);
    return;
  }
  matrixExpr(node: MatrixExpr): void {
    this.resolveEach(node.$vectors);
    return;
  }
  relationExpr(node: RelationExpr): void {
    this.resolve(node.$left);
    this.resolve(node.$right);
    return;
  }
  assignmentExpr(node: AssignmentExpr): void {
    this.resolve(node.$value);
    this.resolveLocal(node, node.$symbol.$symbol.$lexeme);
    return;
  }
  nativeCallExpr(node: NativeCallExpr): void {
    this.resolveEach(node.$args);
    return;
  }
  negExpr(node: NegExpr): void {
    this.resolve(node.$arg);
    return;
  }
  posExpr(node: PosExpr): void {
    this.resolve(node.$arg);
    return;
  }
  factorialExpr(node: FactorialExpr): void {
    this.resolve(node.$arg);
    return;
  }
  notExpr(node: NotExpr): void {
    this.resolve(node.$arg);
    return;
  }
  vectorBinex(node: VectorBinex): void {
    this.resolve(node.$left);
    this.resolve(node.$right);
    return;
  }
  algebraicBinex(node: AlgebraicBinex): void {
    this.resolve(node.$left);
    this.resolve(node.$right);
    return;
  }
  logicalBinex(node: LogicalBinex): void {
    this.resolve(node.$left);
    this.resolve(node.$right);
    return;
  }
  callExpr(node: CallExpr): void {
    this.resolve(node.$callee);
    this.resolveEach(node.$args);
    return;
  }
  parendExpr(node: ParendExpr): void {
    this.resolve(node.$inner);
    return;
  }
  getExpr(node: GetExpr): void {
    this.resolve(node.$object);
    return;
  }
  setExpr(node: SetExpr): void {
    this.resolve(node.$value);
    this.resolve(node.$object);
    return;
  }
  superExpr(node: SuperExpr): void {
    throw new Error("super not implemented.");
  }
  thisExpr(node: ThisExpr): void {
    if (this.$currentClass === classType.none) {
      throw resolverError(
        `Encountered the keyword “this” outside of a class definition. This syntax has no semantic, since “this” points to nothing.`,
        node.$keyword.$line
      );
    }
    this.resolveLocal(node, "this");
    return;
  }
  stringConcat(node: StringConcatExpr): void {
    throw new Error("Method not implemented.");
  }
  sym(node: Identifier): void {
    const name = node.$symbol;
    if (!this.scopesIsEmpty() && this.peek().get(name.$lexeme) === false) {
      throw resolverError(
        `The user is attempting to read the variable “${name.$lexeme}” from its own initializer. This syntax has no semantic.`,
        name.$line
      );
    }
    this.resolveLocal(node, node.$symbol.$lexeme);
    return;
  }
  string(node: StringLit): void {
    return;
  }
  bool(node: Bool): void {
    return;
  }
  nil(node: Nil): void {
    return;
  }
  integer(node: Integer): void {
    return;
  }
  float(node: Float): void {
    return;
  }
  bigInteger(node: BigInteger): void {
    return;
  }
  sciNum(node: SciNum): void {
    return;
  }
  frac(node: Frac): void {
    return;
  }
  numConst(node: NumConst): void {
    return;
  }
}

function resolvable(client: Resolvable) {
  return new Resolver(client);
}

/** An object that maps variable names to values. */
class Environment<T> {
  /** This environment's map of variable names to values. */
  private $values: Map<string, T>;

  /** This environment's set of mutable variable names. */
  private $mutables: Set<string>;

  private $enclosing: Environment<T> | null;
  constructor(enclosing: Environment<T> | null) {
    this.$values = new Map();
    this.$mutables = new Set();
    this.$enclosing = enclosing;
  }

  /** Returns the parent environment `d` links away. */
  ancestor(d: number) {
    let env: Environment<T> | null = this;
    for (let i = 0; i < d; i++) {
      env = this.$enclosing;
    }
    return env;
  }

  /**
   * Stores the name-value binding at the parent environment
   * `d` links away.
   */
  assignAt(d: number, name: string, value: T): T {
    this.ancestor(d)?.$values.set(name, value);
    return value;
  }

  /**
   * Returns the value bound to the given name
   * in the parent environment `d` links away.
   */
  getAt(d: number, name: string): T {
    return this.ancestor(d)?.$values.get(name)!;
  }

  /**
   * Assigns a new value to the given name.
   * If no such name exists, throws a new resolver error.
   * The name provided must be a {@link Token|token} to
   * ensure line and column numbers are reported.
   */
  assign(name: Token, value: T): T {
    if (this.$values.has(name.$lexeme)) {
      if (this.$mutables.has(name.$lexeme)) {
        this.$values.set(name.$lexeme, value);
        return value;
      }
      throw envError(
        `The variable “${name.$lexeme}” is not a mutable variable.`,
        name.$line
      );
    }
    if (this.$enclosing !== null) {
      return this.$enclosing.assign(name, value);
    }
    throw envError(
      `The variable “${name.$lexeme}” is not defined and only defined variables may be assigned.`,
      name.$line
    );
  }

  /**
   * Stores and binds the given name to the value.
   * The argument `mutable` indicates whether
   * the name in this binding can be mutated (assigned
   * a different value later on).
   */
  define(name: string, value: T, mutable: boolean) {
    this.$values.set(name, value);
    if (mutable) this.$mutables.add(name);
    return value;
  }

  /**
   * Retrieves the value bound to the given name.
   * If no such name exists, throws a new resolver error.
   * The name provided must be a T to ensure
   * line numbers are reported.
   */
  get(name: Token): T {
    if (this.$values.has(name.$lexeme)) {
      return this.$values.get(name.$lexeme)!;
    }
    if (this.$enclosing !== null) {
      return this.$enclosing.get(name);
    }
    throw envError(
      `The variable “${name.$lexeme}” is not defined.`,
      name.$line
    );
  }
}

/** Returns a new Winnow runtime environment. */
function runtimeEnv(enclosing: Environment<Primitive> | null) {
  return new Environment<Primitive>(enclosing);
}

function truthy(u: Primitive) {
  return typeof u === "boolean" ? u : u !== null;
}

/** An object representing a class instance in Twine. */
class Obj {
  private klass: Class;
  private fields: Map<string, Primitive>;
  constructor(klass: Class) {
    this.klass = klass;
    this.fields = new Map();
  }
  set(name: string, value: Primitive) {
    this.fields.set(name, value);
    return value;
  }
  get(name: Token): Primitive {
    if (this.fields.has(name.$lexeme)) {
      return this.fields.get(name.$lexeme)!;
    }
    const method = this.klass.findMethod(name.$lexeme);
    if (method !== null) {
      return method.bind(this);
    }
    throw runtimeError(
      `User accessed a non-existent property “${name}”.`,
      name.$line
    );
  }
  toString() {
    return `${this.klass.name} instance`;
  }
}

function $isKlassInstance(x: any): x is Obj {
  return x instanceof Obj;
}

class Class {
  name: string;
  methods: Map<string, Fn>;
  constructor(name: string, methods: Map<string, Fn>) {
    this.name = name;
    this.methods = methods;
  }
  arity() {
    const initalizer = this.findMethod("def");
    if (initalizer === null) {
      return 0;
    }
    return initalizer.arity();
  }
  findMethod(name: string) {
    if (this.methods.has(name)) {
      return this.methods.get(name)!;
    }
    return null;
  }
  call(interpreter: Compiler, args: Primitive[]) {
    const instance = new Obj(this);
    const initializer = this.findMethod("def");
    if (initializer !== null) {
      initializer.bind(instance).call(interpreter, args);
    }
    return instance;
  }
  toString() {
    return this.name;
  }
}

function $isKlass(x: any): x is Class {
  return x instanceof Class;
}

function klassObj(name: string, methods: Map<string, Fn>) {
  return new Class(name, methods);
}

class RETURN {
  value: Primitive;
  constructor(value: Primitive) {
    this.value = value;
  }
}

/** Returns a new `RETURN`. */
function returnValue(value: Primitive) {
  return new RETURN(value);
}

/** An object representing a function in Twine.  */
class Fn {
  private declaration: FnStmt;
  private closure: Environment<Primitive>;
  private isInitializer: boolean;
  constructor(
    declaration: FnStmt,
    closure: Environment<Primitive>,
    isInitializer: boolean
  ) {
    this.declaration = declaration;
    this.closure = closure;
    this.isInitializer = isInitializer;
  }
  arity() {
    return this.declaration.$params.length;
  }
  toString() {
    return `𝑓 ${this.declaration.$name.$lexeme}`;
  }
  bind(instance: Obj) {
    const environment = runtimeEnv(this.closure);
    environment.define("this", instance, true);
    return new Fn(this.declaration, environment, this.isInitializer);
  }
  call(interpreter: Compiler, args: Primitive[]) {
    const environment = runtimeEnv(this.closure);
    for (let i = 0; i < this.declaration.$params.length; i++) {
      environment.define(
        this.declaration.$params[i].$symbol.$lexeme,
        args[i],
        false
      );
    }
    try {
      const out = interpreter.executeBlock(this.declaration.$body, environment);
      if (this.isInitializer) {
        return this.closure.getAt(0, "this");
      }
      return out;
    } catch (E) {
      if (this.isInitializer) {
        return this.closure.getAt(0, "this");
      } else if (E instanceof RETURN) {
        return E.value;
      } else {
        throw E;
      }
    }
  }
}

/** Returns a new `Fn` object. */
function callable(
  declaration: FnStmt,
  closure: Environment<Primitive>,
  isInitializer: boolean
) {
  return new Fn(declaration, closure, isInitializer);
}

/** Returns true if `x` is an `Fn` oobject, false otherwise. */
function $isFn(x: any): x is Fn {
  return x instanceof Fn;
}

class Compiler implements Visitor<Primitive> {
  /** This interpreter's environment. */
  $environment: Environment<Primitive>;

  /** This interpreter's global environment. */
  $globals: Environment<Primitive>;

  /** This interpreter's local variables. */
  $locals: Map<Expr, number>;

  /** Looks up the given variable name. */
  lookupVariable(name: Token, expr: Expr) {
    const distance = this.$locals.get(expr);
    if (distance !== undefined) {
      return this.$environment.getAt(distance, name.$lexeme);
    } else {
      return this.$globals.get(name);
    }
  }

  resolve(expression: Expr, depth: number) {
    this.$locals.set(expression, depth);
  }

  constructor() {
    this.$globals = runtimeEnv(null);
    this.$environment = this.$globals;
    this.$locals = new Map();
  }

  executeBlock(statements: Statement[], environment: Environment<Primitive>) {
    const previous = this.$environment;
    try {
      this.$environment = environment;
      let result: Primitive = null;
      const L = statements.length;
      for (let i = 0; i < L; i++) {
        result = this.eval(statements[i]);
      }
      return result;
    } finally {
      this.$environment = previous;
    }
  }

  interpret(statements: Statement[]) {
    try {
      let result: Primitive = null;
      const L = statements.length;
      for (let i = 0; i < L; i++) {
        result = this.eval(statements[i]);
      }
      return right(result);
    } catch (error) {
      return left(error as Err);
    }
  }

  eval(expr: ASTNode): Primitive {
    return expr.accept(this);
  }

  blockStmt(node: BlockStmt): Primitive {
    const env = runtimeEnv(this.$environment);
    return this.executeBlock(node.$statements, env);
  }

  exprStmt(node: ExprStmt): Primitive {
    return this.eval(node.$expression);
  }

  fnStmt(node: FnStmt): Primitive {
    const f = callable(node, this.$environment, false);
    this.$environment.define(node.$name.$lexeme, f, false);
    return f;
  }

  ifStmt(node: IfStmt): Primitive {
    return truthy(this.eval(node))
      ? this.eval(node.$then)
      : this.eval(node.$alt);
  }

  printStmt(node: PrintStmt): Primitive {
    const result = this.eval(node.$expression);
    return strof(result);
  }

  returnStmt(node: ReturnStmt): Primitive {
    const value = this.eval(node.$value);
    throw returnValue(value);
  }

  variableStmt(node: VariableStmt): Primitive {
    const value = this.eval(node.$value);
    this.$environment.define(
      node.$variable.$symbol.$lexeme,
      value,
      node.$mutable
    );
    return value;
  }

  /** The upper limit on how many iterations to perform. By default, Infinity (no limit). */
  private $loopLimit: number = Infinity;

  /** Sets this engine's limit on iterations. */
  loopLimit(n: number) {
    this.$loopLimit = n;
    return this;
  }

  whileStmt(node: WhileStmt): Primitive {
    let out: Primitive = null;
    let i = 0;
    while (truthy(this.eval(node.$condition))) {
      out = this.eval(node.$body);
      i++;
      if (i > this.$loopLimit) {
        throw runtimeError(
          `Iterations exceed this environment’s loop limit`,
          node.$keyword.$line
        );
      }
    }
    return out;
  }

  classStmt(node: ClassStmt): Primitive {
    this.$environment.define(node.$name.$lexeme, null, true);
    const methods = new Map<string, Fn>();
    for (let i = 0; i < node.$methods.length; i++) {
      const method = node.$methods[i];
      const f = callable(
        method,
        this.$environment,
        method.$name.$lexeme === "init"
      );
      methods.set(method.$name.$lexeme, f);
    }
    const klass = klassObj(node.$name.$lexeme, methods);
    this.$environment.assign(node.$name, klass);
    return null;
  }

  indexExpr(node: IndexExpr): Primitive {
    const L = this.eval(node.$list);
    const I = this.eval(node.$index) as number;
    if (!isNumber(I)) {
      throw runtimeError(
        `Expected a number index, but got “${strof(I)}”`,
        node.$op.$line
      );
    }
    if (isVector(L) || isMatrix(L)) {
      const out = L.element(I);
      if (out === null) {
        throw runtimeError(
          `Encountered an out-of-bounds index.\nThe provided index exceeds the length of the targeted sequential.`,
          node.$op.$line
        );
      } else {
        return out;
      }
    } else if (Array.isArray(L)) {
      const out = L[I - 1];
      if (out === undefined) {
        return null;
      } else {
        return out;
      }
    } else {
      throw runtimeError(
        `Expected a sequential for indexing, but got “${strof(L)}”`,
        node.$op.$line
      );
    }
  }

  algebraString(node: AlgebraString): Primitive {
    throw new Error("Method not implemented.");
  }

  tupleExpr(node: TupleExpr): Primitive {
    const elements = node.$elements.map((e) => this.eval(e));
    return elements;
  }

  vectorExpr(node: VectorExpr): Primitive {
    const nums: number[] = [];
    const elements = node.$elements;
    for (let i = 0; i < elements.length; i++) {
      const n = this.eval(elements[i]);
      if (typeof n !== "number") {
        throw runtimeError(
          `Vectors must only contain either numbers or expressions that reduce to numbers. The value ${strof(
            n
          )} is not a number.`,
          node.$op.$line
        );
      }
      nums.push(n);
    }
    return vector(nums);
  }

  matrixExpr(node: MatrixExpr): Primitive {
    const vs = node.$vectors;
    const vectors: Vector[] = [];
    for (let i = 0; i < vs.length; i++) {
      const v = this.eval(vs[i]);
      if (!isVector(v)) {
        throw runtimeError(
          `Expected a vector but got a ${typename(v)}`,
          node.$op.$line
        );
      }
      vectors.push(v);
    }
    return matrix(vectors);
  }

  relationExpr(node: RelationExpr): Primitive {
    let L = this.eval(node.$left) as any;
    let R = this.eval(node.$right) as any;
    const op = node.$op;
    if ((isFraction(L) && isNumber(R)) || (isNumber(L) && isFraction(R))) {
      L = FRACTION.from(L);
      R = FRACTION.from(R);
    }
    if (isFraction(L) && isFraction(R)) {
      switch (op.$type) {
        case token_type.less:
          return L.lt(R);
        case token_type.greater:
          return L.gt(R);
        case token_type.equal_equal:
          return L.equals(R);
        case token_type.bang_equal:
          return !L.equals(R);
        case token_type.greater_equal:
          return L.gte(R);
        case token_type.less_equal:
          return L.lte(R);
      }
    }
    if (isNumber(L) && isNumber(R)) {
      switch (op.$type) {
        case token_type.less:
          return L < R;
        case token_type.greater:
          return L > R;
        case token_type.equal_equal:
          return L === R;
        case token_type.bang_equal:
          return L !== R;
        case token_type.greater_equal:
          return L >= R;
        case token_type.less_equal:
          return L <= R;
      }
    }
    throw runtimeError(
      `Operator "${op.$lexeme}" does not apply to (${typename(L)} × ${typename(
        R
      )})`,
      op.$line
    );
  }

  assignmentExpr(node: AssignmentExpr): Primitive {
    const value = this.eval(node.$value);
    const distance = this.$locals.get(node);
    if (distance !== undefined) {
      this.$environment.assignAt(distance, node.$symbol.$symbol.$lexeme, value);
    } else {
      this.$globals.assign(node.$symbol.$symbol, value);
    }
    return value;
  }

  nativeCallExpr(node: NativeCallExpr): Primitive {
    const val = node.$args.map((v) => this.eval(v)) as any[];
    switch (node.$name.$lexeme) {
      case "gcd": {
        const a = floor(val[0]);
        const b = floor(val[1]);
        return gcd(a, b);
      }
      case "tanh":
        return tanh(val[0]);
      case "sqrt":
        return sqrt(val[0]);
      case "sinh":
        return sinh(val[0]);
      case "exp":
        return Math.exp(val[0]);
      case "cosh":
        return cosh(val[0]);
      case "floor":
        return floor(val[0]);
      case "ceil":
        return ceil(val[0]);
      case "arctan":
        return arctan(val[0]);
      case "arcsinh":
        return arcsinh(val[0]);
      case "arcsin":
        return arcsin(val[0]);
      case "arccosh":
        return arccosh(val[0]);
      case "arccos":
        return arccos(val[0]);
      case "cos":
        return cos(val[0]);
      case "sin":
        return sin(val[0]);
      case "tan":
        return tan(val[0]);
      case "lg":
        return lg(val[0]);
      case "ln":
        return ln(val[0]);
      case "log":
        return log(val[0]);
      case "max":
        return max(...val);
      case "min":
        return min(...val);
      case "avg":
        return avg(...val);
    }
    throw runtimeError(
      `Native function "${node.$name.$lexeme}" not currently supported.`,
      node.$name.$line
    );
  }

  negExpr(node: NegExpr): Primitive {
    const x = this.eval(node.$arg);
    if (typeof x === "number" || typeof x === "bigint") {
      return -x;
    } else if (x instanceof FRACTION) {
      return x.negate();
    } else if (x instanceof Exponential) {
      return x.negate();
    } else {
      throw runtimeError(
        `Negation operator "-" cannot be applied to ${typename(x)}`,
        node.$op.$line
      );
    }
  }

  posExpr(node: PosExpr): Primitive {
    const x = this.eval(node.$arg);
    if (isNumber(x)) {
      return +x;
    } else if (x instanceof FRACTION || x instanceof Exponential) {
      return x;
    } else {
      throw runtimeError(
        `Operator "+" cannot be applied to ${typename(x)}`,
        node.$op.$line
      );
    }
  }

  factorialExpr(node: FactorialExpr): Primitive {
    const result = this.eval(node.$arg);
    if (isNumber(result) && Number.isInteger(result)) {
      return factorialize(result);
    }
    throw runtimeError(
      `Operator "!" does not apply to ${typename(result)}`,
      node.$op.$line
    );
  }

  notExpr(node: NotExpr): Primitive {
    const x = this.eval(node.$arg);
    if (typeof x === "boolean") {
      return !x;
    } else {
      throw runtimeError(
        `Operator "not" cannot be applied to ${typename(x)}`,
        node.$op.$line
      );
    }
  }

  vectorBinex(node: VectorBinex): Primitive {
    const op = node.$op;
    const left = this.eval(node.$left);
    const right = this.eval(node.$right);
    if (!isVector(left) || !isVector(right)) {
      throw runtimeError(
        `The operator "${op.$lexeme}" is restricted to (vector × vector).`,
        op.$line
      );
    }
    switch (op.$type) {
      case token_type.dot_add:
        return left.add(right);
      case token_type.dot_minus:
        return left.sub(right);
      case token_type.dot_star:
        return left.mul(right);
      case token_type.dot_caret:
        return left.pow(right);
      case token_type.at:
        return left.dot(right);
    }
  }

  algebraicBinex(node: AlgebraicBinex): Primitive {
    let L = this.eval(node.$left) as any;
    let R = this.eval(node.$right) as any;
    const op = node.$op.$type;
    if ((isFraction(L) && isNumber(R)) || (isNumber(L) && isFraction(R))) {
      L = FRACTION.from(L);
      R = FRACTION.from(R);
    }
    if (isFraction(L) && isFraction(R)) {
      switch (op) {
        case token_type.star:
          return L.times(R);
        case token_type.slash:
          return L.divide(R);
        case token_type.plus:
          return L.plus(R);
        case token_type.minus:
          return L.minus(R);
        case token_type.percent:
          return FRACTION.from(percent(L.float(), R.float()));
        case token_type.rem:
          throw runtimeError(
            `Operator "rem" cannot be applied to fractions`,
            node.$op.$line
          );
        case token_type.mod:
          throw runtimeError(
            `Operator "mod" cannot be applied to fractions`,
            node.$op.$line
          );
        case token_type.div:
          throw runtimeError(
            `Operator "div" cannot be applied to fractions`,
            node.$op.$line
          );
        case token_type.caret:
          throw runtimeError(
            `Exponentiation on fractions currently unsupported`,
            node.$op.$line
          );
      }
    }
    if (isNumber(L) && isNumber(R)) {
      switch (op) {
        case token_type.plus:
          return L + R;
        case token_type.star:
          return L * R;
        case token_type.caret:
          return L ** R;
        case token_type.slash:
          return L / R;
        case token_type.minus:
          return L - R;
        case token_type.rem:
          return L % R;
        case token_type.mod:
          return mod(L, R);
        case token_type.percent:
          return percent(L, R);
        case token_type.div:
          return iquot(L, R);
      }
    }
    const o = node.$op.$lexeme;
    throw runtimeError(
      `"${o}" cannot be applied to (${typename(L)} × ${typename(R)})`,
      node.$op.$line
    );
  }

  logicalBinex(node: LogicalBinex): Primitive {
    const L = this.eval(node.$left);
    const R = this.eval(node.$right);
    const op = node.$op;
    if (typeof L !== "boolean" || typeof R !== "boolean") {
      throw runtimeError(
        `Operator ${op.$lexeme} does not apply to (${typename(L)} × ${typename(
          R
        )}).`,
        op.$line
      );
    }
    switch (op.$type) {
      case token_type.and:
        return L && R;
      case token_type.or:
        return L || R;
      case token_type.nand:
        return !(L && R);
      case token_type.nor:
        return !(L || R);
      case token_type.xnor:
        return L === R;
      case token_type.xor:
        return L !== R;
    }
  }

  callExpr(node: CallExpr): Primitive {
    const callee = this.eval(node.$callee);
    const args: Primitive[] = [];
    for (let i = 0; i < node.$args.length; i++) {
      args.push(this.eval(node.$args[i]));
    }
    if ($isKlass(callee)) {
      return callee.call(this, args);
    }
    if ($isFn(callee)) {
      return callee.call(this, args);
    }
    // deno-fmt-ignore
    throw runtimeError(
      `“${strof(
        callee
      )}” is neither a function nor a class. Only functions and classes may be called.`,
      node.$paren.$line
    );
  }

  parendExpr(node: ParendExpr): Primitive {
    return this.eval(node.$inner);
  }

  getExpr(node: GetExpr): Primitive {
    throw new Error("Method not implemented.");
  }

  setExpr(node: SetExpr): Primitive {
    throw new Error("Method not implemented.");
  }

  superExpr(node: SuperExpr): Primitive {
    throw new Error("Method not implemented.");
  }

  thisExpr(node: ThisExpr): Primitive {
    throw new Error("Method not implemented.");
  }

  stringConcat(node: StringConcatExpr): Primitive {
    const L = this.eval(node.$left);
    const R = this.eval(node.$right);
    if (isString(L) && isString(R)) {
      return L + R;
    } else {
      throw runtimeError(
        `Operator "&" does not apply to (${typename(L)} × ${typename(R)})`,
        node.$op.$line
      );
    }
  }

  sym(node: Identifier): Primitive {
    return this.lookupVariable(node.$symbol, node);
  }

  string(node: StringLit): Primitive {
    return node.$value;
  }

  bool(node: Bool): Primitive {
    return node.$value;
  }

  nil(node: Nil): Primitive {
    return node.$value;
  }

  integer(node: Integer): Primitive {
    return node.$value;
  }

  float(node: Float): Primitive {
    return node.$value;
  }

  bigInteger(node: BigInteger): Primitive {
    return node.$value;
  }

  sciNum(node: SciNum): Primitive {
    return node.$value;
  }

  frac(node: Frac): Primitive {
    return node.$value;
  }

  numConst(node: NumConst): Primitive {
    return node.$value;
  }
}

export function engine() {
  const compiler = new Compiler();
  const parse = (code: string) => syntax(code).parset();
  const compile = (code: string) => {
    const prog = parse(code);
    if (prog.isLeft()) {
      return prog.unwrap();
    }
    const stmts = prog.unwrap();
    const interpreter = compiler;
    const resolved = resolvable(interpreter).resolved(stmts);
    if (resolved.isLeft()) return resolved.unwrap();
    const out = interpreter.interpret(stmts);
    return out.unwrap();
  };
  const tokens = (code: string) => {
    const prog = lexical(code).stream();
    if (prog.isLeft()) {
      return prog.unwrap().toString();
    } else {
      return treestring(prog.unwrap());
    }
  };
  const ast = (code: string) => {
    const prog = syntax(code).parset();
    if (prog.isLeft()) {
      return prog.unwrap().toString();
    } else {
      return treestring(prog.unwrap());
    }
  };
  return {
    compile,
    ast,
    tokens,
  };
}
