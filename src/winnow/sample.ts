// ============================================================= Utility Methods

/** Utility method - Logs to the console. */
const print = console.log;

/** A utility method that generates a pseudorandom string. @param length - The max length of the resulting string. @param base - The base from which to draw characters. */
function uid(length: number = 4, base = 36) {
  return Math.random()
    .toString(base)
    .replace(/[^a-z]+/g, "")
    .substring(0, length + 1);
}

/** Given an array of type `T[]`, splits the array in two and returns the two halves as a pair. */
function arraySplit<T>(array: T[]) {
  const L = array.length;
  const half = Math.ceil(L / 2);
  const left = array.slice(0, half);
  const right = array.slice(half);
  return [left, right] as [T[], T[]];
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
  return (new Some<T>(value));
}

function none() {
  return (new None());
}

type Option<T> = None | Some<T>;

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

  forEach(
    f: (data: T, index: number, list: LinkedList<T>) => void,
  ) {
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
      list: LinkedList<T>,
    ) => X,
    initialValue: X,
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
      list: LinkedList<T>,
    ) => X,
    initialValue: X,
  ): X {
    return this.#reduce(1, reducer, initialValue);
  }
  reduce<X>(
    reducer: (
      accumulator: X,
      currentValue: T,
      index: number,
      list: LinkedList<T>,
    ) => X,
    initialValue: X,
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
    return out.map((d, i) => g(d, i)).join();
  }

  /** Returns a new list whose elements satisfy the given predicate. */
  filter(
    predicate: (value: T, index: number, list: LinkedList<T>) => boolean,
  ) {
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

/** Latex transformers. */
const latex = {
  esc: (x: string | number) => (`{\\${x}}`),
  block: (x: string | number) => (`\\${x}`),
  tie: (...xs: (string | number)[]) => (xs.map((x) => `${x}`).join("~")),
  join: (...xs: (string | number)[]) => (xs.map((x) => `${x}`).join("")),
  txt: (x: string) => (`\\text{${x}}`),
  linebreak: () => `\\\\`,
  percent: () => `\\%`,
  brace: (x: string | number) => (`{${x}}`),
  frac: (n: string | number, d: string | number) => (`\\dfrac{${n}}{${d}}`),
  surround: (x: string | number, leftDelim: string, rightDelim: string) => (
    `\\left${leftDelim}${x}\\right${rightDelim}`
  ),
  dquoted: (x: string) => (
    `\\text{\\textquotedblleft}\\text{${x}}\\text{\\textquotedblright}`
  ),
  and: () => `\\land`,
  nand: () => `\\uparrow`,
  nor: () => `\\downarrow`,
  xnor: () => `\\odot`,
  xor: () => `\\oplus`,
  or: () => `\\lor`,
  to: () => `~{=}~`,
};

// ============================================================ global constants
/** These are constants and functions heavily used throughout the code base. */

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

/** Global maximum integer. */
const MAX_INT = Number.MAX_SAFE_INTEGER;

// ========================================================== pairwise functions
/** Divides each element of `divisors` by the given dividend. */
export const aDiv = (divisors: number[], dividend: number) => (
  divisors.map((n) => n / dividend)
);

// ========================================================== geometry functions

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
  return (x > 1 ? 0 : x < -1 ? PI : Math.acos(x));
}

/** Returns the arcsine of x. */
function arcsin(x: number) {
  return (x >= 1 ? HALF_PI : x <= -1 ? -HALF_PI : Math.asin(x));
}

/*
================================================================ stats functions
The following functions perform statistical computations.
*/

function inRange(min: number, input: number, max: number) {
  return (min <= input && input <= max);
}

/** Returns the number of decimal places of the given number. */
function decimalPlacesOf(n: number) {
  const s = `${n}`;
  if (!s.includes(".")) return 0;
  const [a, b] = s.split(".");
  return b.length;
}

/**
---
* Given an array of numbers, returns the number with
* the most decimal places. If all the numbers have the
* same number of decimal places (e.g., all the numbers
* are integers), returns the smallest number within the
* array.
---
*/
function mostPrecise(numbers: number[]) {
  let n = numbers[0];
  for (let i = 1; i < numbers.length; i++) {
    const m = numbers[i];
    if (decimalPlacesOf(n) < decimalPlacesOf(m)) {
      n = m;
    }
  }
  if ($isInt(n)) {
    return minimum(numbers);
  }
  return n;
}

/** Returns the maximum value from the given array of numbers. */
function minimum(numbers: number[]) {
  let min = Infinity;
  for (let i = 0; i < numbers.length; i++) {
    (numbers[i] < min) && (min = numbers[i]);
  }
  return min;
}

/** Returns the maximum value from the given array of numbers. */
function maximum(numbers: number[]) {
  let max = -Infinity;
  for (let i = 0; i < numbers.length; i++) {
    (numbers[i] > max) && (max = numbers[i]);
  }
  return max;
}

function frequencyTable<T>(data: T[]) {
  const out = new Map<T, number>();
  for (let i = 0; i < data.length; i++) {
    const key = data[i];
    if (out.has(key)) {
      const value = out.get(key)!;
      out.set(key, value + 1);
    } else {
      out.set(key, 1);
    }
  }
  return out;
}

/** Returns a table of intervals from the given array of numbers.*/
function autoBin(
  numbers: number[],
  precision: number = 0.05,
  intervals?: number,
): Map<[number, number], number> {
  intervals === undefined && (intervals = ceil(sqrt(numbers.length)));
  const mdp = mostPrecise(numbers);
  const max = maximum(numbers);
  const min = minimum(numbers);
  const start = (mdp === min ? mdp : min) - precision;
  const end = max + precision;
  const width = ceil((end - start) / intervals);
  const boundaries = new Map<[number, number], number>();
  boundaries.set([0, start], 0);
  let x = start;
  for (let i = 0; i < intervals; i++) {
    const prev = x;
    x = x + width;
    boundaries.set([prev, x], 0);
  }
  return boundaries;
}

/** Returns a frequency table from the given array of numbers.  */
function frequencyIntervalTable(
  numbers: number[],
  precision: number = 0.05,
  intervals?: number,
): Map<[number, number], number> {
  const boundaries = autoBin(numbers, precision, intervals);
  for (const [key] of boundaries) {
    for (let i = 0; i < numbers.length; i++) {
      const num = numbers[i];
      const [a, b] = key;
      if (a < num && num < b) {
        const v = boundaries.get(key)!;
        boundaries.set(key, v + 1);
      }
    }
  }
  return boundaries;
}

/** Returns a relative frequency table from the given array of numbers.  */
function relativeFrequencyTable(
  numbers: number[],
  precision: number = 0.05,
  intervals?: number,
): Map<[number, number], number> {
  const boundaries = frequencyIntervalTable(numbers, precision, intervals);
  for (const [key, value] of boundaries) {
    boundaries.set(key, value / numbers.length);
  }
  return boundaries;
}

/** Returns a random integer between the provided minimum and maximum (not including the maximum). */
export function randInt(min: number, max: number) {
  return (
    floor(Math.random() * (max - min + 1)) + min
  );
}

/** Returns a random floating point number between the provided minimum and maximum (not including the maximum). */
export function randFloat(min: number, max: number) {
  return (
    Math.random() * (max - min) + min
  );
}

/** Transforms 1-based indices to 0-based indices. */
function i0(value: number) {
  return value === 0 ? 0 : value - 1;
}

/** Rounds the given number value to the number of given decimal places. */
function round(num: number, places: number = 2) {
  const epsilon = Number.EPSILON;
  return (
    Math.round(
      (num * (10 ** places)) * (1 + epsilon),
    ) / (10 ** places)
  );
}

/** Computes the arithmetic mean of the given list of numbers. */
function avg(...nums: number[]) {
  let sum = 0;
  for (let i = 0; i < nums.length; i++) {
    sum += nums[i];
  }
  return sum / nums.length;
}

/** Returns the a% of b. */
function percent(a: number, b: number) {
  return (a / 100) * b;
}

/** Returns `a rem b` (the signed remainder). */
function rem(a: number, b: number) {
  return (a % b);
}

/** Returns `a mod b` (the unsigned remainder). */
function mod(a: number, b: number) {
  return (
    ((a % b) + b) % b
  );
}

/** Returns an array of numbers running from start (inclusive) to stop (inclusive) */
export function range(start: number, stop: number, step = 1): number[] {
  const out = [];
  for (let i = start; i < stop; i += step) {
    out.push(i);
  }
  return out;
}

/** Returns the integer quotient of `a` and `b`. */
function quot(a: number, b: number) {
  return (floor(a / b));
}

/** Returns a tuple. */
function tuple<T extends any[]>(...data: T) {
  return data;
}

/** Returns the greatest common divisor of integers `a` and `b`. */
function gcd(a: number, b: number) {
  let A = floor(a);
  let B = floor(b);
  while (B !== 0) {
    let R = rem(A, B);
    A = B;
    B = R;
  }
  return abs(A);
}

/** Returns the triple of applying the extended Euclidean algorithm. */
function xgcd(a: number, b: number) {
  let A = floor(a);
  let B = floor(b);
  let mpp = 1;
  let mp = 0;
  let npp = 0;
  let np = 1;
  while (B !== 0) {
    let Q = quot(A, B);
    let R = rem(A, B);
    A = B;
    B = R;
    let m = mpp - Q * mp;
    let n = npp - Q * np;
    mpp = mp;
    mp = m;
    npp = np;
    np = n;
  }
  if (A >= 0) {
    return tuple(A, mpp, npp);
  } else {
    return tuple(-A, -mpp, -npp);
  }
}

/** Returns the clamping of the given input. I.e., if `input` is less than `min`, returns `min`. If `input` is greater than `max`, returns `max`. Otherwise, returns `input`. */
function clamp(minimum: number, input: number, maximum: number) {
  return min(max(input, minimum), maximum);
}

/** Given the number pair `(x1,x2)` returns the value between `x1` and `x2` at `p` percent of the dsitance between `x1` and `x2`. Useful for computations like: “What x-coordinate is 35% between 46 and 182?” Note that the percentage `p` is assumed to be between `0` and `1`. */
function lerp([x1, x2]: [number, number], p: number) {
  return (
    x1 * (1 - p) + x2 * p
  );
}

/** Given the number pair `(x,y)`, returns the value at the given decimal point `a`. Used primarily for computations like: How far through this line has this point moved? */
function inverseLerp([x, y]: [number, number], a: number) {
  return clamp(0, (a - x) / (y - x), 1);
}

/** Returns a linear interpolator. The `domain` is the interval of input values – a pair of numbers `(a,b)` where `a` is the smallest possible input and `b` is the largest. The `range` is the interval of scale values - a pair of numbers `(a,b)` where `a` is the smallest possible scaled value and `b` is the largest. */
export function interpolator(
  domain: [number, number],
  range: [number, number],
) {
  return (n: number) => (
    (range[0]) + ((
      ((range[1]) - (range[0])) / ((domain[1]) - (domain[0]))
    ) * (n - (domain[0])))
  );
}

/** Interpolates the number `n` based on the specified domain and range. */
export function interpolate(
  n: number,
  domain: [number, number],
  range: [number, number],
) {
  return (
    interpolator(domain, range)(n)
  );
}

/** Typeguard: Returns true if `x` is any string, false otherwise. */
function $isString(x: any): x is string {
  return (typeof x === "string");
}

/** Typeguard: Returns true if `x` is any number, false otherwise. */
function $isNumber(x: any): x is number {
  return (typeof x === "number");
}

/** Typeguard: Returns true if `x` is any array, false otherwise. */
function $isArray(x: any): x is any[] {
  return (Array.isArray(x));
}

/** Typeguard: Returns true if `x` is a boolean, false otherwise. */
function $isBoolean(x: any): x is boolean {
  return (typeof x === "boolean");
}

/** Typeguard: Returns true if `x` is NaN, false otherwise.. */
function $isNaN(x: any) {
  return Number.isNaN(x);
}

/** Typeguard : Returns true if `x` is Undefined or null. */
function $isNothing(x: any): x is undefined | null {
  return (
    (x === undefined) || (x === null)
  );
}

/** Typeguard: Returns true if `x` is an integer. */
function $isInt(x: number) {
  return Number.isInteger(x);
}

/** Typeguard: Returns true if `x` is Infinity. */
function $isInfinity(x: any) {
  return !(Number.isFinite(x));
}

/** Typeguard: Returns true if `x` is a big integer. */
function $isBigInt(x: any): x is bigint {
  return (typeof x === "bigint");
}

/** Typeguard: Returns true if `x` is any object. */
function $isObject(x: any): x is Object {
  return (typeof x === "object");
}

/** Typeguard: Returns true `x` is a function. */
function $isFunction(x: any): x is Function {
  return (typeof x === "function");
}

/** Returns true if the given string contains digits. */
function isNumericString(s: string) {
  return /\d+/.test(s);
}

// ============================================================ VECTOR DATA TYPE

class Vector<T extends number[] = number[]> {
  /** The elements of this vector. */
  _elements: T;
  toLatex() {
    const out = this._elements.map((x) => `${x}`).join(",~");
    return latex.surround(out, "[", "]");
  }
  constructor(elements: T) {
    this._elements = elements;
  }

  vxm(matrix: Matrix) {
    if (this.length !== matrix._C) return this;
    const vector = new Vector([] as number[]);
    for (let i = 1; i <= matrix._R; i++) {
      const v = matrix.element(i);
      if (v === null) return this;
      const d = this.dot(v);
      vector._elements[i - 1] = d;
    }
    return vector;
  }

  /** Utility method for performing binary operations. */
  binop(
    other: Vector | number[] | number,
    op: (a: number, b: number) => number,
  ) {
    const arg = ($isNumber(other))
      ? homogenousVector(other, this.length)
      : vector(other);
    const [A, B] = equalen(this, arg);
    return vector(A._elements.map((c, i) => op(c, B._elements[i])));
  }

  /** Returns the smallest component of this vector. */
  min() {
    let min = Infinity;
    for (let i = 0; i < this._elements.length; i++) {
      const elem = this._elements[i];
      if (elem < min) {
        min = elem;
      }
    }
    return min;
  }

  /** Returns the largest component of this vector. */
  max() {
    let max = -Infinity;
    for (let i = 0; i < this._elements.length; i++) {
      const elem = this._elements[i];
      if (elem > max) {
        max = elem;
      }
    }
    return max;
  }

  /** Returns this vector as a matrix. */
  matrix() {
    const elements = this._elements.map((n) => new Vector([n]));
    return new Matrix(elements, elements.length, 1);
  }

  /** Returns the magnitude of this vector.  An optional precision value may be passed roundingthe magnitude to a specified number of decimal places. */
  mag(precision?: number) {
    const out = sqrt(this._elements.reduce((p, c) => (p) + (c ** 2), 0));
    return !$isNothing(precision) ? round(out, floor(precision)) : out;
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
    return this.binop(other, (a, b) => b === 0 ? a / alt : a / b);
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
    return vector(this._elements.map((c) => -c));
  }

  /** Returns this vector with each component set to its absolute value. */
  abs() {
    return vector(this._elements.map((c) => Math.abs(c)));
  }

  /** Returns this vector with each component set to zero */
  zero() {
    return vector(this._elements.map((_) => 0));
  }

  /** Returns true if this vector equals the provided vector. */
  equals(that: Vector) {
    if (this.length !== that.length) return false;
    for (let i = 0; i < this.length; i++) {
      const e1 = this._elements[i];
      const e2 = that._elements[i];
      if (e1 !== e2) return false;
    }
    return true;
  }

  /** Returns true if every component of this vector is zero. */
  isZero() {
    for (let i = 0; i < this.length; i++) {
      if (this._elements[i] !== 0) return false;
    }
    return true;
  }

  /** Returns true if this vector comprises exactly two elements. */
  is2D(): this is Vector<[number, number]> {
    return this._elements.length === 2;
  }

  /** Returns true if this vector comprises exactly three elements. */
  is3D(): this is Vector<[number, number, number]> {
    return this._elements.length === 3;
  }

  /** Returns a copy of this vector. */
  copy() {
    const elements = [];
    for (let i = 0; i < this._elements.length; i++) {
      elements.push(this._elements[i]);
    }
    return new Vector(elements);
  }

  /** Appends the given value by the provided number of slots. */
  pad(slots: number, value: number) {
    if (slots < this.length) {
      const diff = this.length - slots;
      const elements = [...this._elements];
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
      vector._elements[index] = value;
      return vector;
    }
    const copy = this.copy();
    copy._elements[index] = value;
    return copy;
  }

  /** Sets the first element of this vector to the provided value. */
  px(value: number) {
    return this.set(1, value);
  }

  /** Returns the first element of this vector. */
  get _x() {
    return $isNothing(this._elements[0]) ? 0 : this._elements[0];
  }
  set _x(n: number) {
    this._elements[0] = n;
  }

  /** Sets the second element of this vector to the provided value. */
  py(value: number) {
    return this.set(2, value);
  }

  /** Returns the second element of this vector. */
  get _y() {
    return $isNothing(this._elements[1]) ? 0 : this._elements[1];
  }
  set _y(n: number) {
    this._elements[1] = n;
  }

  /** Sets the third element of this vector to the provided value. */
  pz(value: number) {
    return this.set(3, value);
  }

  /** Returns the third element of this vector. */
  get _z() {
    return $isNothing(this._elements[2]) ? 0 : this._elements[2];
  }
  set _z(z: number) {
    this._elements[2] = z;
  }

  /** Sets the fourt element of this vector to the provided value. */
  pw(value: number) {
    return this.set(4, value);
  }

  /** Returns the fourth element of this vector. */
  get _w() {
    return $isNothing(this._elements[3]) ? 0 : this._elements[3];
  }
  set _w(w: number) {
    this._elements[3] = w;
  }

  /** Returns the dot product of this vector and the provided vector. */
  dot(vector: Vector | number[]) {
    const other = Vector.from(vector);
    const order = this.length;
    if (other.length !== order) return 0;
    let sum = 0;
    for (let i = 0; i < order; i++) {
      const a = this._elements[i];
      const b = other._elements[i];
      const p = a * b;
      sum += p;
    }
    return sum;
  }

  /** Returns the element at the given index (indices start at 1). */
  element(index: number) {
    const out = this._elements[index - 1];
    return (out !== undefined) ? out : null;
  }

  /** Returns the length of this vector. */
  get length() {
    return this._elements.length;
  }

  /** Returns the string representation of this vector. */
  toString() {
    const elements = this._elements.map((n) => `${n}`).join(",");
    return `[${elements}]`;
  }

  /** Returns this vector as a number array. */
  toArray() {
    return this._elements.map((e) => e);
  }

  /** Returns a new vector from the given array of numbers or `Vector`. If a `Vector` is passed, returns a copy of that vector. */
  static from(value: number[] | Vector): Vector {
    if ($isArray(value)) {
      return new Vector(value);
    } else {
      return value.copy();
    }
  }

  /** Returns the angle between the two provided vectors. */
  theta(other: Vector) {
    const ab = this.dot(other);
    const mag = this.mag();
    const factor = ab / (mag);
    return Math.acos(factor);
  }

  /** Returns the angle between (a) the difference vector of this vector and the provided vector, and (b) the x-axis. */
  gamma(other: Vector) {
    const dx = this._x - other._x;
    const dy = this._y - other._y;
    const gamma = Math.atan2(dy, dx);
    return gamma;
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
    return vector([-this._y, this._x]);
  }

  /** Returns the cross product of this vector in-place. The cross product is used primarily to compute the vector perpendicular to two vectors. */
  cross(other: Vector) {
    const ax = this._x;
    const ay = this._y;
    const az = this._z;
    const bx = other._x;
    const by = other._y;
    const bz = other._z;
    const cx = (ay * bz) - (az * by);
    const cy = (az * bx) - (ax * bz);
    const cz = (ax * by) - (ay * bx);
    return vector([cx, cy, cz]);
  }

  /** Returns the 2D distance between this vector and the provided vector. */
  distance2D(other: Vector) {
    const dx = other._x - this._x;
    const dy = other._y - this._y;
    const dsum = (dx ** 2) + (dy ** 2);
    return Math.sqrt(dsum);
  }

  /** Returns the 3D distance between this vector and the provided vector. */
  distance3D(other: Vector) {
    const x = other._x - this._x;
    const y = other._y - this._y;
    const z = other._z - this._z;
    const xyz = (x * x) + (y * y) + (z * z);
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
    const rfn = (restrict === "Z") ? randInt : randFloat;
    const x = rfn(min, max);
    const y = rfn(min, max);
    return new Vector([x, y]);
  }

  /** Returns a random 3D vector. The `min` argument sets the lower bound of the sampling interval. The `max` argument sets the upper bound of the sampling interval. The `restrict` argument takes `Z` or `R`. If `Z` is passed, random values are restricted to integers. If `R` is passed, random values are either integers or floats. */
  static random3D(min: number, max: number, restrict: "Z" | "R" = "R") {
    const v = Vector.random2D(min, max, restrict);
    const x = v._x;
    const y = v._y;
    const z = restrict === "Z" ? randInt(min, max) : randFloat(min, max);
    return new Vector([x, y, z]);
  }
}

/** Given `vectorA` and `vectorB`, ensures that `vectorA` and `vectorB` have the same sizes (number of elements). If one is smaller than the other, the shorter is padded with additional zeros to ensure the lengths are the same. */
function equalen(vectorA: Vector, vectorB: Vector): [Vector, Vector] {
  const A = [];
  const B = [];
  if (vectorA.length > vectorB.length) {
    let i = 0;
    for (i = 0; i < vectorA.length; i++) {
      A.push(vectorA._elements[i]);
      B.push($isNothing(vectorB._elements[i]) ? 0 : vectorB._elements[i]);
    }
    const n = vectorB.length - i;
    for (let j = 0; j < n; j++) {
      B.push(0);
    }
    return [vector(A), vector(B)];
  } else if (vectorA.length < vectorB.length) {
    let i = 0;
    for (i = 0; i < vectorB.length; i++) {
      A.push($isNothing(vectorA._elements[i]) ? 0 : vectorA._elements[i]);
      B.push(vectorB._elements[i]);
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

/** Returns a new vector of size `length`, where each element is the given `value`.*/
function homogenousVector(value: number, length: number) {
  const elements = [];
  for (let i = 0; i < length; i++) {
    elements.push(value);
  }
  return new Vector(elements);
}

/** Returns a new vector. If a vector is passed, returns the vector (an identity function). */
function vector(elements: number[] | Vector) {
  if ($isArray(elements)) {
    return new Vector(elements);
  } else {
    return elements;
  }
}

/** Returns a new 2D vector. */
function v2D(x: number, y: number) {
  return new Vector([x, y]);
}

/** Returns a new 3D vector. */
function v3D(x: number, y: number, z: number) {
  return new Vector([x, y, z]);
}

/** Returns true if the given value is a vector, false otherwise. */
const $isVector = (value: any): value is Vector => (value instanceof Vector);

// ============================================================ MATRIX DATA TYPE

class Matrix {
  _vectors: Vector[];
  readonly _R: number;
  readonly _C: number;
  constructor(vectors: Vector[], rows: number, cols: number) {
    this._vectors = vectors;
    this._R = rows;
    this._C = cols;
  }
  /** Returns true if this matrix is a square matrix. */
  get _square() {
    return this._C === this._R;
  }

  /** Returns a copy of this matrix. */
  copy() {
    const vs = this._vectors.map((v) => v.copy());
    return new Matrix(vs, this._R, this._C);
  }

  /** Returns the vector element at the given index (indices start at 1). */
  element(index: number) {
    const out = this._vectors[index - 1];
    return out !== undefined ? out : null;
  }

  /** Returns a column vector comprising all the vector elements at the given column. */
  column(index: number) {
    if (index > this._C) {
      const out: number[] = [];
      for (let i = 0; i < this._C; i++) {
        out.push(0);
      }
      return vector(out);
    }
    const out: number[] = [];
    this._vectors.forEach((vector) => {
      vector._elements.forEach((n, i) => {
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
    return $isNumber(n) ? n : fallback;
  }

  /** Returns the string form of matrix. */
  toString() {
    const out = this._vectors.map((v) => v.toString()).join(",");
    return `[${out}]`;
  }

  /** Sets the element at the given row index and column index. The row and column indices are expected to begin at 1. If no element exists at the provided indices, no change is done. */
  set(row: number, column: number, value: number) {
    if (this._vectors[row - 1] === undefined) return this;
    if (this._vectors[row - 1]._elements[column - 1] === undefined) return this;
    const copy = this.copy();
    copy._vectors[row - 1]._elements[column - 1] = value;
    return copy;
  }

  /** Executes the given callback over each element of this matrix. The row and column index provided in the callback begin at 1. */
  forEach(
    callback: (element: number, rowIndex: number, columnIndex: number) => void,
  ) {
    for (let i = 1; i <= this._R; i++) {
      for (let j = 1; j <= this._C; j++) {
        callback(this.n(i, j), i, j);
      }
    }
    return this;
  }

  /** Returns true if this matrix and the the provided matrix have the same number of rows and the same number of columns. False otherwise. */
  congruent(matrix: Matrix) {
    return this._R === matrix._R && this._C === matrix._C;
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

  static from(nums: (number[])[]) {
    const out = nums.map((ns) => vector(ns));
    return matrix(out);
  }

  static of(
    rows: number,
    columns: number,
    arg: number | (number[])[] | Matrix,
  ) {
    return $isNumber(arg)
      ? Matrix.fill(rows, columns, arg)
      : $isArray(arg)
      ? Matrix.from(arg)
      : arg;
  }

  /** @internal - Utility method for binary operations on matrices. */
  private binop(
    arg: number | (number[])[] | Matrix,
    op: (a: number, b: number) => number,
  ) {
    const other = $isNumber(arg)
      ? Matrix.fill(this._R, this._C, arg)
      : $isArray(arg)
      ? Matrix.from(arg)
      : arg;
    if (this._R !== other._R || this._C !== other._C) return this;
    const vectors: Vector[] = [];
    for (let i = 0; i < this._R; i++) {
      const nums: number[] = [];
      const row = this._vectors[i]._elements;
      for (let j = 0; j < row.length; j++) {
        const a = row[j];
        const b = other._vectors[i]._elements[j];
        nums.push(op(a, b));
      }
      vectors.push(vector(nums));
    }
    return matrix(vectors);
  }

  /** Returns this matrix minus the provided matrix. */
  sub(matrix: Matrix | number | (number[])[]) {
    return this.binop(matrix, (a, b) => a - b);
  }

  /** Returns this matrix component-wise-multiplied with provided matrix. */
  times(matrix: Matrix | number | (number[])[]) {
    return this.binop(matrix, (a, b) => a * b);
  }

  /** Returns this matrix plus the provided matrix. */
  add(matrix: Matrix | number | (number[])[]) {
    return this.binop(matrix, (a, b) => a + b);
  }

  /** Returns the negation of this matrix.  */
  neg() {
    return this.times(-1);
  }

  /** Returns the transpose of this matrix. */
  transpose() {
    const copy: (number[])[] = [];
    for (let i = 0; i < this._R; ++i) {
      const vector = this._vectors[i];
      for (let j = 0; j < this._C; ++j) {
        const element = vector._elements[j];
        if ($isNothing(element)) continue;
        if ($isNothing(copy[j])) {
          copy[j] = [];
        }
        copy[j][i] = element;
      }
    }
    return matrix(copy.map((c) => vector(c)));
  }

  /** Returns the matrix product of this matrix and the provided matrix. */
  mul(arg: number | Matrix | (number[])[]) {
    const Ar = this._R;
    const Ac = this._C;
    if (arg instanceof Matrix && Ac !== arg._R) {
      return this;
    }
    const B = Matrix.of(Ar, Ac, arg);
    const Bc = B._C;
    const result: (number[])[] = [];
    for (let i = 0; i < Ar; i++) {
      result[i] = [];
      for (let j = 0; j < Bc; j++) {
        let sum = 0;
        for (let k = 0; k < Ac; k++) {
          const a = this._vectors[i]._elements[k];
          const b = B._vectors[k]._elements[j];
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
  toLatex() {
    const open = latex.block("begin{bmatrix}");
    let body = "";
    const vectors = this._vectors;
    const maxRow = vectors.length - 1;
    vectors.forEach((d, i) => {
      const maxCol = d._elements.length - 1;
      d._elements.forEach((e, j) => {
        const element = `${e}`;
        body += element;
        if (j !== maxCol) {
          body += ` & `;
        }
      });
      if (i !== maxRow) {
        body += latex.linebreak();
      }
    });
    const close = latex.block("end{bmatrix}");
    return open + body + close;
  }
}

/** Returns a new matrix. */
function matrix(rows: (Vector[]) | (number[])[], cols?: number) {
  const vectors = rows.map((v) => $isVector(v) ? v : Vector.from(v));
  return new Matrix(
    vectors,
    vectors.length,
    cols !== undefined ? cols : vectors[0].length,
  );
}

/** Returns true if the given value is a matrix. */
const $isMatrix = (value: any): value is Matrix => (value instanceof Matrix);

// ======================================================= begin graphics module

/** An enum of types mapped to SVG Path command prefixes. */
// deno-fmt-ignore
enum pc { M, L, H, V, Q, C, A, Z}

abstract class PathCommand {
  readonly _type: pc;
  _end: Vector;
  constructor(type: pc, end: Vector) {
    this._type = type;
    this._end = end;
  }

  /** Sets the endpoint for this command. */
  abstract endPoint(x: number, y: number, z?: number): PathCommand;

  /** Returns the string value for this command. */
  abstract toString(): string;
}

class MCommand extends PathCommand {
  readonly _type: pc.M;
  constructor(x: number, y: number, z: number) {
    super(pc.M, vector([x, y, z]));
    this._type = pc.M;
  }
  endPoint(x: number, y: number, z: number = 1): MCommand {
    return new MCommand(x, y, z);
  }
  toString() {
    return `M${this._end._x},${this._end._y}`;
  }
}

/** Returns a new {@link MCommand|M-command}. */
const M = (x: number, y: number, z: number = 1) => (new MCommand(x, y, z));

class LCommand extends PathCommand {
  readonly _type: pc.L;
  constructor(x: number, y: number, z: number) {
    super(pc.L, vector([x, y, z]));
    this._type = pc.L;
  }
  endPoint(x: number, y: number, z: number = 1): LCommand {
    return new LCommand(x, y, z);
  }
  toString() {
    return `L${this._end._x},${this._end._y}`;
  }
}

/** Returns a new {@link LCommand|L-command}. */
const L = (x: number, y: number, z: number = 1) => (new LCommand(x, y, z));

class ZCommand extends PathCommand {
  readonly _type: pc.Z;
  constructor() {
    super(pc.Z, vector([0, 0, 0]));
    this._type = pc.Z;
  }
  endPoint(x: number, y: number, z: number = 1): ZCommand {
    return this;
  }
  toString() {
    return `Z`;
  }
}

const Z = (): ZCommand => new ZCommand();

class VCommand extends PathCommand {
  readonly _type: pc.V;
  constructor(x: number, y: number, z: number) {
    super(pc.V, vector([x, y, z]));
    this._type = pc.V;
  }
  endPoint(x: number, y: number, z: number = 1): VCommand {
    return new VCommand(x, y, z);
  }
  toString() {
    return `V${this._end._x},${this._end._y}`;
  }
}

/** Returns a new {@link VCommand|V-command}. */
const V = (x: number, y: number, z: number = 1) => (new VCommand(x, y, z));

class HCommand extends PathCommand {
  readonly _type: pc.H;
  constructor(x: number, y: number, z: number) {
    super(pc.H, vector([x, y, z]));
    this._type = pc.H;
  }
  endPoint(x: number, y: number, z: number = 1): HCommand {
    return new HCommand(x, y, z);
  }
  toString() {
    return `H${this._end._x},${this._end._y}`;
  }
}

/** Returns a new {@link HCommand|H-command}. */
const H = (x: number, y: number, z: number = 1) => (new HCommand(x, y, z));

class QCommand extends PathCommand {
  readonly _type: pc.Q = pc.Q;
  _ctrl1: Vector;
  constructor(x: number, y: number, z: number) {
    super(pc.Q, vector([x, y, z]));
    this._ctrl1 = vector([x, y, z]);
  }
  ctrlPoint(x: number, y: number, z: number = 1): QCommand {
    const out = new QCommand(this._end._x, this._end._y, this._end._z);
    out._ctrl1 = vector([x, y, z]);
    return out;
  }
  endPoint(x: number, y: number, z: number = 1): QCommand {
    return new QCommand(x, y, z);
  }
  toString() {
    return `Q${this._ctrl1._x},${this._ctrl1._y},${this._end._x},${this._end._y}`;
  }
}

/** Returns a new quadratic bezier curve command. */
const Q = (x: number, y: number, z: number = 1) => (new QCommand(x, y, z));

/** A type corresponding to the SVG cubic-bezier-curve command. */
class CCommand extends PathCommand {
  _type: pc.C = pc.C;
  _ctrl1: Vector = vector([0, 0, 1]);
  _ctrl2: Vector = vector([0, 0, 1]);
  constructor(x: number, y: number, z: number = 1) {
    super(pc.C, vector([x, y, z]));
  }
  copy() {
    const out = new CCommand(this._end._x, this._end._y, this._end._z);
    out._ctrl1 = this._ctrl1.copy();
    out._ctrl2 = this._ctrl2.copy();
    return out;
  }
  /** Sets the second control point for this cubic bezier curve. */
  ctrlPoint2(x: number, y: number, z: number = 1) {
    const out = new CCommand(this._end._x, this._end._y, this._end._z);
    out._ctrl2 = vector([x, y, z]);
    return out;
  }
  /** Sets the first control point for this cubic bezier curve. */
  ctrlPoint1(x: number, y: number, z: number = 1) {
    const out = new CCommand(this._end._x, this._end._y, this._end._z);
    out._ctrl1 = vector([x, y, z]);
    return out;
  }
  endPoint(x: number, y: number, z: number = 1): CCommand {
    return new CCommand(x, y, z);
  }
  toString() {
    return `C${this._ctrl1._x},${this._ctrl1._y},${this._ctrl2._x},${this._ctrl2._y},${this._end._x},${this._end._y}`;
  }
}

/** Returns a new cubic bezier curve command. */
const C = (x: number, y: number, z: number = 1) => (new CCommand(x, y, z));

/** An ADT corresponding to the SVG arc-to command. */
class ACommand extends PathCommand {
  _type: pc.A = pc.A;
  /** The x-radius of this arc-to command. */
  _rx: number = 1;
  /** The r-radius of this arc-to command. */
  _ry: number = 1;
  _rotation: number = 0;
  _largeArc: 0 | 1 = 0;
  _sweep: 0 | 1 = 0;
  constructor(x: number, y: number, z: number = 1) {
    super(pc.A, vector([x, y, z]));
  }
  rotate(value: number) {
    this._rotation = value;
    return this;
  }
  ry(value: number) {
    this._ry = value;
    return this;
  }
  rx(value: number) {
    this._rx = value;
    return this;
  }
  sweep(value: 0 | 1) {
    this._sweep = value;
    return this;
  }
  arc(value: 1 | 0) {
    this._largeArc = value;
    return this;
  }
  copy(): ACommand {
    const out = new ACommand(this._end._x, this._end._y, this._end._z);
    out._rx = this._rx;
    out._ry = this._ry;
    out._rotation = this._rotation;
    out._largeArc = this._largeArc;
    out._sweep = this._sweep;
    return out;
  }
  endPoint(x: number, y: number, z: number = 1): ACommand {
    this._end = vector([x, y, z]);
    return this;
  }
  toString() {
    const out = [
      this._rx,
      this._ry,
      this._rotation,
      this._largeArc,
      this._sweep,
      this._end._x,
      this._end._y,
    ].join(",");
    return "A" + out;
  }
}

/** Returns a new arc-to command. */
const A = (x: number, y: number, z: number = 1) => (new ACommand(x, y, z));

interface Renderable {
  _commands: PathCommand[];
  _origin: Vector;
  toString(): string;
  at(x: number, y: number, z?: number): this;
  tfm(op: (v: Vector) => Vector): this;
  rotateZ(angle: number): this;
  rotateY(angle: number): this;
  rotateX(angle: number): this;
  scale(x: number, y: number): this;
  translateZ(z: number): this;
  translateY(y: number): this;
  translateX(x: number): this;
  shearZ(dx: number, dy: number): this;
  shearY(dx: number, dz: number): this;
  shearX(dy: number, dz: number): this;
  translate(x: number, y: number): this;
  interpolate(
    domain: [number, number],
    range: [number, number],
    dimensions: [number, number],
  ): this;
  get length(): number;
  get lastCommand(): PathCommand;
  get firstCommand(): PathCommand;
  _id: string | number;
  id(value: string | number): this;
  end(): this;
  _locked: boolean;
  lock(): this;
}

type Klass<T = {}> = new (...args: any[]) => T;

type And<DataClass, Extender> = DataClass & Klass<Extender>;

function renderable<CLASS extends Klass>(klass: CLASS): And<CLASS, Renderable> {
  return class extends klass {
    _commands: PathCommand[] = [];
    _origin: Vector = v3D(0, 0, 1);
    _id: string | number = uid(5);
    _locked: boolean = false;
    lock() {
      this._locked = true;
      return this;
    }
    id(id: string | number) {
      this._id = id;
      return this;
    }
    end() {
      return this;
    }
    get length() {
      return this._commands.length;
    }
    get firstCommand() {
      const out = this._commands[0];
      if (out === undefined) {
        return M(this._origin._x, this._origin._y, this._origin._z);
      } else return out;
    }
    get lastCommand() {
      const out = this._commands[this.length - 1];
      if (out === undefined) {
        return M(this._origin._x, this._origin._y, this._origin._z);
      } else return out;
    }
    at(x: number, y: number, z: number = 1) {
      this._origin = v3D(x, y, z);
      return this;
    }
    interpolate(
      domain: [number, number],
      range: [number, number],
      dimensions: [number, number],
    ) {
      const X = interpolator(domain, [0, dimensions[0]]);
      const Y = interpolator(range, [dimensions[1], 0]);
      this._commands = this._commands.map((p) => {
        const E = p._end;
        const [x, y, z] = [X(E._x), Y(E._y), E._z];
        switch (p._type) {
          case pc.M:
            return M(x, y, z);
          case pc.H:
          case pc.L:
          case pc.V:
            return L(x, y, z);
          case pc.Q: {
            const c = (p as QCommand)._ctrl1;
            return Q(x, y, z).ctrlPoint(c._x, c._y, c._z);
          }
          case pc.C: {
            const c1 = (p as CCommand)._ctrl1;
            const c2 = (p as CCommand)._ctrl2;
            return C(x, y, z)
              .ctrlPoint1(X(c1._x), Y(c1._y), c1._z)
              .ctrlPoint2(X(c2._x), Y(c2._y), c2._z);
          }
          case pc.A: {
            const j = p as ACommand;
            return A(x, y, z)
              .rx(j._rx)
              .ry(j._ry)
              .rotate(j._rotation)
              .arc(j._largeArc)
              .sweep(j._sweep);
          }
          default:
            return p;
        }
      });
      return this;
    }
    tfm(op: (v: Vector) => Vector) {
      this._commands = this._commands.map((p) => {
        const E = op(p._end);
        switch (p._type) {
          case pc.M:
            return M(E._x, E._y, E._z);
          case pc.H:
          case pc.L:
          case pc.V:
            return L(E._x, E._y, E._z);
          case pc.Q: {
            const c = op((p as QCommand)._ctrl1);
            return Q(E._x, E._y, E._z).ctrlPoint(c._x, c._y, c._z);
          }
          case pc.C: {
            const c1 = op((p as CCommand)._ctrl1);
            const c2 = op((p as CCommand)._ctrl2);
            return C(E._x, E._y, E._z)
              .ctrlPoint1(c1._x, c1._y, c1._z)
              .ctrlPoint2(c2._x, c2._y, c2._z);
          }
          case pc.A: {
            const s = p as ACommand;
            return A(E._x, E._y, E._z)
              .rx(s._rx)
              .ry(s._ry)
              .rotate(s._rotation)
              .arc(s._largeArc)
              .sweep(s._sweep);
          }
          default:
            return p;
        }
      });
      return this;
    }

    rotateZ(angle: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [cos(angle), sin(angle), 0],
          [-sin(angle), cos(angle), 0],
          [0, 0, 1],
        ]))
      );
    }

    rotateY(angle: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [cos(angle), 0, -sin(angle)],
          [0, 1, 0],
          [sin(angle), 0, cos(angle)],
        ]))
      );
    }

    rotateX(angle: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, 0],
          [0, cos(angle), -sin(angle)],
          [0, sin(angle), cos(angle)],
        ]))
      );
    }

    scale(x: number, y: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [x, 0, 0],
          [0, y, 0],
          [0, 0, 1],
        ]))
      );
    }

    translateZ(z: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, 1],
          [0, 1, 1],
          [0, 0, z],
        ]))
      );
    }

    translateY(y: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, 1],
          [0, 1, y],
          [0, 0, 1],
        ]))
      );
    }

    translateX(x: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, x],
          [0, 1, 1],
          [0, 0, 1],
        ]))
      );
    }

    /** Shears along the z-axis. */
    shearZ(dx: number, dy: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, 0],
          [0, 1, 0],
          [dx, dy, 1],
        ]))
      );
    }

    /** Shears along the y-axis. */
    shearY(dx: number, dz: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, 0],
          [dx, 1, dz],
          [0, 0, 1],
        ]))
      );
    }

    /** Shears along the x-axis. */
    shearX(dy: number, dz: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, dy, dz],
          [0, 1, 0],
          [0, 0, 1],
        ]))
      );
    }

    translate(x: number, y: number) {
      return this.tfm((v) =>
        v.vxm(matrix([
          [1, 0, x],
          [0, 1, y],
          [0, 0, 1],
        ]))
      );
    }
    toString() {
      return this._commands.map((x) => x.toString()).join("");
    }
  };
}

interface Colorable {
  _fill: string;
  fill(color: string): this;
  _stroke: string;
  stroke(color: string): this;
  _strokeWidth: number;
  strokeWidth(value: number): this;
  _dash: number;
  dash(value: number): this;
  _opacity: number;
  opacity(value: number): this;
}

function colorable<CLASS extends Klass>(klass: CLASS): And<CLASS, Colorable> {
  return class extends klass {
    _fill: string = "none";
    fill(color: string) {
      this._fill = color;
      return this;
    }
    _stroke: string = "black";
    stroke(color: string) {
      this._stroke = color;
      return this;
    }
    _strokeWidth: number = 1;
    strokeWidth(value: number) {
      this._strokeWidth = value;
      return this;
    }
    _dash: number = 0;
    dash(value: number) {
      this._dash = value;
      return this;
    }
    _opacity: number = 1;
    opacity(value: number) {
      this._opacity = value;
      return this;
    }
  };
}

class BASE {}
const SHAPE = renderable(colorable(BASE));

/** A node corresponding to an SVG path. */
export class Path extends SHAPE {
  /** The SVG commands comprising this path. */
  _commands: PathCommand[] = [];

  end() {
    return this;
  }

  constructor(x: number, y: number, z: number = 1) {
    super();
    this._origin = vector([0, 0, 0]);
    this.cursor = vector([x, y, z]);
    this._commands = [M(x, y, z)];
  }

  /** The current endpoint of this path. */
  cursor: Vector;

  /** The origin of this path. */
  _origin: Vector;

  /** Appends the provided list of commands to this Path’s command list. */
  with(commands: PathCommand[]) {
    commands.forEach((c) => this._commands.push(c));
    return this;
  }

  /** Sets the origin of this path. */
  at(x: number, y: number, z: number = 1) {
    this._origin = vector([x, y, z]);
    return this;
  }

  /** Returns the `d` attribute value resulting from this path. */
  toString(): string {
    const origin = M(this._origin._x, this._origin._y).toString();
    const out = this._commands.map((command) => command.toString());
    return origin + out.join("");
  }

  push(command: PathCommand) {
    this._commands.push(command);
    this.cursor = command._end;
    return this;
  }

  /** @param end - The arc’s end point. @param dimensions - Either a pair `(w,h)` where `w` is the width of the arc, and `h` is the height of the arc, or a number. If a number is passed, draws an arc where `w = h` (a circular arc). Defaults to `[1,1]`. @param rotation - The arc’s rotation along its x-axis. If a string is passed, Weave’s parsers will attempt to parse an angle, defaulting to 0 in failure. If a number is passed, assumes the angle unit is in radians. Defaults to `0`. @param arc - Either `minor` (the smaller half of the arc, corresponding to a large arc flag of `0`) or `major` (the larger half of the arc, corresponding to a large arc flag of `1`). Defaults to `minor`. @param sweep - Either `clockwise` (thus drawing the arc clockwise, a sweep flag of 1) or `counter-clockwise` ( thus drawing the arc counter-clockwise, a sweep flag of 0). Defaults to `clockwise`. */
  A(
    end: [number, number],
    dimensions: [number, number] = [1, 1],
    rotation: number,
    arc: 0 | 1,
    sweep: 1 | 0,
  ) {
    const a = A(end[0], end[1], 1)
      .rx(dimensions[0])
      .ry(dimensions[1])
      .rotate(rotation)
      .arc(arc)
      .sweep(sweep);
    return this.push(a);
  }

  /** Appends a `V` command to this path. */
  V(y: number) {
    return this.push(L(this.cursor._x, y));
  }

  /** Appends an `H` command to this path. */
  H(x: number) {
    return this.push(L(x, this.cursor._y));
  }

  /** Appends an `M` command to this path. */
  M(x: number, y: number, z: number = 1) {
    return this.push(M(x, y, z));
  }

  /** Appends an `L` command to this path. */
  L(x: number, y: number, z: number = 1) {
    return this.push(L(x, y, z));
  }
  Z() {
    return this.push(Z());
  }
}

/** Returns a new path. */
export function path(
  originX: number = 0,
  originY: number = 0,
  originZ: number = 1,
) {
  return (
    new Path(originX, originY, originZ)
  );
}
const FILLABLE = colorable(BASE);
class ArrowHead extends FILLABLE {
  _id: string | number;
  _type: "start" | "end" = "end";
  type(of: "start" | "end") {
    this._type = of;
    return this;
  }
  _refX: number = 0;
  id(value: string | number) {
    this._id = value;
    return this;
  }
  refY(y: number) {
    this._refY = y;
    return this;
  }
  _refY: number = 0;
  refX(x: number) {
    this._refX = x;
    return this;
  }
  get _d() {
    if (this._type === "end") {
      return `M0,-5L10,0L0,5`;
    } else {
      return `M0,0L10,-5L10,5Z`;
    }
  }
  _orient: "auto" | "auto-start-reverse" = "auto";
  _markerWidth: number = 10;
  markerWidth(w: number) {
    this._markerWidth = w;
    return this;
  }
  _markerHeight: number = 7;
  markerHeight(h: number) {
    this._markerHeight = h;
    return this;
  }
  orient(value: "auto" | "auto-start-reverse") {
    this._orient = value;
    return this;
  }
  constructor(id: string | number) {
    super();
    this._id = id;
  }
}
function arrow(id: string | number) {
  return new ArrowHead(id);
}

/** A node corresponding to a line. */
export class Line extends SHAPE {
  _arrowEnd: null | ArrowHead = null;
  _arrowStart: null | ArrowHead = null;
  constructor(start: Vector, end: Vector) {
    super();
    this._commands.push(
      M(start._x, start._y, start._z),
      L(end._x, end._y, end._z),
    );
  }
  arrowed() {
    return this.arrowStart().arrowEnd();
  }
  arrowStart(arrowHead?: ArrowHead) {
    if (arrowHead) {
      this._arrowStart = arrowHead;
    } else {
      this._arrowStart = arrow(this._id)
        .type("start")
        .fill(this._stroke)
        .stroke("none");
    }
    return this;
  }
  arrowEnd(arrowHead?: ArrowHead) {
    if (arrowHead) {
      this._arrowEnd = arrowHead;
    } else {
      this._arrowEnd = arrow(this._id)
        .fill(this._stroke)
        .stroke("none");
    }
    return this;
  }
}

/** Returns a new line. */
export function line(start: Vector | number[], end: Vector | number[]) {
  start = $isArray(start)
    ? v3D(start[0] ?? 0, start[1] ?? 0, start[2] ?? 1)
    : start;
  end = $isArray(end) ? v3D(end[0] ?? 0, end[1] ?? 0, end[2] ?? 1) : end;
  return new Line(start, end);
}

/** A node corresponding to a circle. */
export class Circle extends SHAPE {
  radius: number;
  constructor(radius: number) {
    super();
    this.radius = radius;
    this._commands.push(
      M(this._origin._x, this._origin._y + radius, this._origin._z),
      A(this._origin._x, this._origin._y - radius, this._origin._z),
      A(this._origin._x, this._origin._y + radius, this._origin._z),
    );
  }
  get _cx() {
    const o = this._commands[0];
    return o._end._x;
  }
  get _cz() {
    const o = this._commands[0];
    return o._end._z;
  }
  get _cy() {
    const o = this._commands[0];
    const r = this.radius;
    return o._end._y - r;
  }
  r(R: number) {
    this.radius = R;
    return this;
  }
  at(x: number, y: number, z: number = 1) {
    const radius = this.radius;
    const r = radius / 2;
    this._commands = [
      M(x + r, y + r, z),
      A(x - r, y - r, z),
      A(x + r, y + r, z),
    ];
    return this;
  }
}

/** Returns a new circle. @param r - The circle’s radius. */
export function circle(r: number = 1) {
  return new Circle(r);
}

/** A node corresponding to a quadrilateral. */
export class Quad extends SHAPE {
  _width: number;
  _height: number;
  constructor(width: number, height: number) {
    super();
    this._width = width;
    this._height = height;
  }
  end() {
    const o = this._origin;
    const x = o._x;
    const y = o._y;
    const w = this._width;
    const h = this._height;
    this._commands.push(M(x, y));
    this._commands.push(L(x + w, y));
    this._commands.push(L(x + w, y - h));
    this._commands.push(L(x, y - h));
    this._commands.push(L(x, y));
    this._commands.push(Z());
    return this;
  }
  toString() {
    return this._commands.map((c) => c.toString()).join("");
  }
}

export function quad(width: number, height: number) {
  return new Quad(width, height);
}

const TEXT = renderable(BASE);

/** A node corresponding to an SVG text element. */
export class Text extends TEXT {
  end() {
    return this;
  }
  get _y() {
    return this._commands[0]._end._y;
  }
  get _x() {
    return this._commands[0]._end._x;
  }
  _mode: "LaTeX" | "plain" = "plain";
  mode(as: "LaTeX" | "plain") {
    this._mode = as;
    return this;
  }
  _text: string | number;
  _fontFamily?: string;
  fontFamily(family: string) {
    this._fontFamily = family;
    return this;
  }
  _fontSize?: string | number;
  fontSize(value: number | string) {
    this._fontSize = value;
    return this;
  }
  _fontColor?: string;
  fontColor(color: string) {
    this._fontColor = color;
    return this;
  }
  _textAnchor: "start" | "middle" | "end" = "middle";
  anchor(value: "start" | "middle" | "end") {
    this._textAnchor = value;
    return this;
  }
  content(t: string | number) {
    this._text = t;
    return this;
  }
  constructor(text: string | number) {
    super();
    this._text = text;
    this._commands.push(M(0, 0));
  }
  dy(y: number) {
    return this.at(this._commands[0]._end._x, this._commands[0]._end._y + y);
  }
  dx(x: number) {
    return this.at(this._commands[0]._end._x + x, this._commands[0]._end._y);
  }
  at(x: number, y: number) {
    this._commands[0] = M(x, y);
    return this;
  }
}

export function text(content: string | number) {
  return (new Text(content));
}

export function tex(content: string | number) {
  return (new Text(content).mode("LaTeX"));
}

// ========================================================= tick line generator

/** An object corresponding to a tickline for axes. */
type Tick = {
  /** The line comprising the tick. */
  tick: Line;

  /** The tick’s label. */
  txt: Text;
};

function tickLines(
  length: number,
  min: number,
  max: number,
  step: number,
  orient: "x" | "y",
  label: (n: number, i: number) => Text = (n, i) => text(n),
) {
  const ns = range(min, max, step);
  const out: Tick[] = [];
  let i = 0;
  if (orient === "x") {
    ns.forEach((n) => {
      const tick = line([n, -length], [n, length]);
      const txt = label(n, i).at(
        tick.firstCommand._end._x,
        tick.firstCommand._end._y,
      );
      out.push({ tick, txt });
      i++;
    });
  } else {
    ns.forEach((n) => {
      const tick = line([-length, n], [length, n]);
      const txt = label(n, i).at(
        tick.firstCommand._end._x,
        tick.firstCommand._end._y,
      );
      out.push({ tick, txt });
      i++;
    });
  }
  return out;
}

// ======================================================================== area

/** A node corresponding to a closed, fillable path. */
export class Area2D extends SHAPE {
  constructor() {
    super();
    this._fill = "initial";
    this._opacity = 0.5;
  }
  moveTo(x: number, y: number, z: number = 1) {
    this._origin = v3D(x, y, z);
    return this;
  }
  push(command: PathCommand) {
    this._commands.push(command);
    return this;
  }
  close() {
    this._commands.push(Z());
    return this;
  }
}

/** Returns a new Area2D. */
function area2D() {
  return new Area2D();
}

const CONTEXT = contextual(colorable(BASE));

// ================================================================== linearized
interface Linearized {
  /** The length of each x-axis tick. */
  _xTickLength: number;

  /** Specifies the length for each x-axis tick. */
  xTickLength(length: number): this;

  /** The length of each y-axis tick. */
  _yTickLength: number;

  /** Specifies the length for each y-axis tick. */
  yTickLength(length: number): this;

  /** The axes color. */
  _axisColor: string;

  /** Sets the axis stroke color. */
  axisColor(color: string): this;

  /** The font size for the x-axis ticks. */
  _xAxisFontSize?: number;
  xAxisFontSize(size: number): this;

  /** The font size for the y-axis ticks. */
  _yAxisFontSize?: number;
  yAxisFontSize(size: number): this;

  /** The font size for the axis ticks. Specifying an `_xAxisFontSize` or a `_yAxisFontSize` will override this property. */
  _axisFontSize: number;

  /** Specifies the font size for the axis ticks. This method has no effect on: (1) y-axis ticks if the `_yAxisFontSize` is specified, and (2) x-axis ticks if the `_xAxisFontSize` is specified.  */
  axisFontSize(size: number): this;
}

function linearized<CLASS extends Klass>(klass: CLASS): And<CLASS, Linearized> {
  return class extends klass {
    _axisFontSize: number = 12;
    axisFontSize(size: number) {
      this._axisFontSize = size;
      return this;
    }

    _xAxisFontSize?: number;
    xAxisFontSize(size: number) {
      this._xAxisFontSize = size;
      return this;
    }

    _yAxisFontSize?: number;
    yAxisFontSize(size: number) {
      this._yAxisFontSize = size;
      return this;
    }

    _axisColor: string = "initial";
    axisColor(color: string) {
      this._axisColor = color;
      return this;
    }

    _xTickLength: number = 0.5;
    xTickLength(length: number) {
      this._xTickLength = length;
      return this;
    }

    _yTickLength: number = 0.5;
    yTickLength(length: number) {
      this._yTickLength = length;
      return this;
    }
  };
}

// ================================================================== polar plot
export class PolarPlot2D extends CONTEXT {
  _f: string;
  constructor(f: string) {
    super();
    this._f = f;
    this._domain = [-1, 1];
    this._range = [-1, 1];
  }
  _cycles: number = 2 * PI;
  cycles(n: number) {
    this._cycles = n;
    return this;
  }
  radius(r: number) {
    if (r > 0) {
      this._domain = [-r, r];
      this._range = [-r, r];
    }
    return this;
  }
  _axisColor: string = "initial";
  axisColor(color: string) {
    this._axisColor = color;
    return this;
  }
  end() {
    const out: PathCommand[] = [];
    const e = engine("fn " + this._f + ";");
    const fn = e.execute();
    if (!(fn instanceof Fn)) {
      return this;
    }
    const dataset: [number, number][] = [];
    for (let i = 0; i < this._cycles; i += 0.01) {
      const r = fn.call(e.compiler, [i]);
      if (typeof r !== "number") continue;
      const x = r * cos(i);
      const y = r * sin(i);
      dataset.push([x, y]);
    }
    let moved = false;
    for (let i = 0; i < dataset.length; i++) {
      const [x, y] = dataset[i];
      if (!isNaN(y)) {
        if (!moved) {
          out.push(M(x, y, 1));
          moved = true;
        } else {
          out.push(L(x, y, 1));
        }
      } else {
        const next = dataset[i + 1];
        if (next !== undefined && !isNaN(y)) {
          out.push(M(x, y, 1));
        }
      }
    }
    const p = path(out[0]._end._x, out[0]._end._y, out[0]._end._z)
      .stroke(this._stroke)
      .strokeWidth(this._strokeWidth);
    for (let i = 1; i < out.length; i++) {
      p._commands.push(out[i]);
    }
    const lineLabels: Text[] = [];
    for (let i = 1; i < this._domain[1] * this._tickCount; i++) {
      const c = circle(i).stroke(this._axisColor).opacity(this._axisOpacity);
      const t = text(i)
        .at(0, i)
        .fontColor(this._axisColor)
        .anchor("middle")
        .dy(-0.15);
      lineLabels.push(t);
      this.and(c);
    }
    const lineCoords = range(0, 360, 45);
    const axes: Line[] = [];
    const k = this._domain[1] + 1;
    lineCoords.forEach((n) => {
      const x = cos(toRadians(n)) * k;
      const y = sin(toRadians(n)) * k;

      const L = line([0, 0], [x, y])
        .stroke(this._axisColor)
        .opacity(this._axisOpacity);
      axes.push(L);
    });
    axes.forEach((l) => this.and(l));
    lineLabels.forEach((t) => this.and(t));
    this.and(p.strokeWidth(this._strokeWidth));
    return this.fit();
  }
  _tickCount: number = 2.6;
  ringCount(n: number) {
    if (n > 0) {
      this._tickCount = n;
    }
    return this;
  }
  _axisOpacity: number = 0.4;
  axisOpacity(n: number) {
    this._axisOpacity = n;
    return this;
  }
}

export function polar2D(f: string) {
  return new PolarPlot2D(f);
}

// ===================================================================== plane2d
export class Plane extends CONTEXT {
  constructor(
    domain: [number, number] = [-10, 10],
    range: [number, number] = [-10, 10],
  ) {
    super();
    this._domain = domain;
    this._range = range;
  }
  _xTickLength: number = 0.1;
  xTickLength(n: number) {
    this._xTickLength = n;
    return this;
  }
  _yTickLength: number = 0.1;
  yTickLength(n: number) {
    this._yTickLength = n;
    return this;
  }
  _xTickSep: number = 1;
  xTickSep(n: number) {
    this._xTickSep = n;
    return this;
  }
  _yTickSep: number = 1;
  yTickSep(n: number) {
    this._yTickSep = n;
    return this;
  }
  _axisColor: string = "black";
  _tickFontSize: number = 12;
  tickFontSize(n: number) {
    this._tickFontSize = n;
    return this;
  }
  axisColor(color: string) {
    this._axisColor = color;
    return this;
  }
  axis(on: "x" | "y") {
    if (on === "x") {
      const xmin = this._domain[0];
      const xmax = this._domain[1];
      const xTickFormat = (n: number) =>
        text(n)
          .fontColor(this._axisColor)
          .anchor("middle");
      const xLine = line([xmin, 0], [xmax, 0])
        .stroke(this._axisColor);
      this._children.push(xLine);
      const xticks = tickLines(
        this._xTickLength,
        xmin,
        xmax + this._xTickSep,
        this._xTickSep,
        "x",
        xTickFormat,
      );
      xticks.forEach((tick) => {
        this._children.push(
          tick.tick.stroke(this._axisColor),
          tick.txt.translate(0, -this._xTickLength * 2).fontSize(
            this._tickFontSize,
          ),
        );
      });
    }
    if (on === "y") {
      const ymin = this._range[0];
      const ymax = this._range[1];
      const yLine = line([0, ymin], [0, ymax])
        .stroke(this._axisColor);
      this._children.push(yLine);
      const yTickFormat = (n: number) =>
        text(n)
          .fontColor(this._axisColor)
          .anchor("end");
      const yticks = tickLines(
        this._yTickLength,
        ymin,
        ymax + this._yTickSep,
        this._yTickSep,
        "y",
        yTickFormat,
      );
      yticks.forEach((tick) => {
        this._children.push(
          tick.tick.stroke(this._axisColor),
          tick.txt
            .anchor("end")
            .translate(-this._yTickLength, -this._yTickLength / 2)
            .fontSize(this._tickFontSize),
        );
      });
    }
    return this;
  }
  bordered(color: string) {
    const top = line([this._xmin, this._ymax], [this._xmax, this._ymax]);
    const right = line([this._xmax, this._ymax], [this._xmax, this._ymin]);
    const bottom = line([this._xmax, this._ymin], [this._xmin, this._ymin]);
    const left = line([this._xmin, this._ymin], [this._xmin, this._ymax]);
    this.children([top, right, bottom, left].map((b) => b.stroke(color)));
    return this;
  }
  end() {
    return this.fit();
  }
}

export function plane(
  domain: [number, number] = [-10, 10],
  range: [number, number] = [-10, 10],
) {
  return new Plane(domain, range);
}

// ============================================================ 2d function plot

export class Plot2D extends Plane {
  f: string;
  _samples: number = 200;
  samples(value: number) {
    this._samples = value;
    return this;
  }
  _integrate?: [number, number];
  _integralFill?: string;
  _integralOpacity?: number;
  integrate(data: {
    bounds: [number, number];
    fill?: string;
    opacity?: number;
  }) {
    this._integrate = data.bounds;
    this._integralFill = data.fill;
    this._integralOpacity = data.opacity;
    return this;
  }
  constructor(f: string) {
    super();
    this.f = "fn " + f + ";";
    this._domain = [-10, 10];
    this._range = [-10, 10];
    const d = 400;
    this._width = d;
    this._height = d;
    const m = 10;
    this._margins = [m, m, m, m];
  }

  private cartesian(f: string) {
    const out: PathCommand[] = [];
    const xmin = this._domain[0];
    const xmax = this._domain[1];
    const ymin = this._range[0];
    const ymax = this._range[1];
    const e = engine(f);
    const fn = e.execute();
    if (!(fn instanceof Fn)) {
      return this;
    }
    const dataset: [number, number][] = [];
    for (let i = -this._samples; i < this._samples; i++) {
      const x = (i / this._samples) * xmax;
      const _y = fn.call(e.compiler, [x]);
      if (typeof _y !== "number") continue;
      const y = _y;
      const point: [number, number] = [x, y];
      if ($isNaN(y) || y < ymin || ymax < y) point[1] = NaN;
      if (x < xmin || xmax < x) continue;
      else dataset.push(point);
    }
    if (this._integrate) {
      const [a, b] = this._integrate;
      const area = area2D();
      let moved = false;
      for (let i = 0; i < dataset.length; i++) {
        const [x, y] = dataset[i];
        if (inRange(a, x, b)) {
          if (!isNaN(y)) {
            if (!moved) {
              area.push(M(x, y, 1));
              moved = true;
            } else {
              area.push(L(x, y, 1));
            }
          } else {
            const next = dataset[i + 1];
            if (next !== undefined && !isNaN(next[1])) {
              out.push(M(next[0], next[1], 1));
            }
          }
        }
      }
      area.close();
      (this._integralFill) && area.fill(this._integralFill);
      (this._integralOpacity) && area.opacity(this._integralOpacity);
      this.and(area);
    }
    let moved = false;
    for (let i = 0; i < dataset.length; i++) {
      const datum = dataset[i];
      if (!isNaN(datum[1])) {
        if (!moved) {
          out.push(M(datum[0], datum[1], 1));
          moved = true;
        } else {
          out.push(L(datum[0], datum[1], 1));
        }
      } else {
        const next = dataset[i + 1];
        if (next !== undefined && !isNaN(next[1])) {
          out.push(M(next[0], next[1], 1));
        }
      }
    }
    const p = path(out[0]._end._x, out[0]._end._y, out[0]._end._z)
      .stroke(this._stroke)
      .strokeWidth(this._strokeWidth);
    for (let i = 1; i < out.length; i++) {
      p._commands.push(out[i]);
    }
    this._children.push(p);
    return this;
  }

  end() {
    this.axis("x");
    this.axis("y");
    this.cartesian(this.f);
    return this.fit();
  }
}

export function plot2D(f: string) {
  return new Plot2D(f);
}

function mapKeys<K, V>(x: Map<K, V>) {
  const out = [];
  for (const [key] of x) {
    out.push(key);
  }
  return out;
}

// ==================================================================== metadata
class Metadata {
  _data: Record<(string | number), number>;
  _kvMax: [string | number, number];
  _kvMin: [string | number, number];
  _keys: (string | number)[] = [];
  _values: number[] = [];
  _entryCount: number;
  _mostPrecise: number;
  constructor(data: Record<(string | number), number>) {
    this._data = data;
    const entries = Object.entries(data);
    this._entryCount = entries.length;
    let max = -Infinity;
    let maxPair: [string | number, number] = ["", max];
    let min = Infinity;
    let minPair: [string | number, number] = ["", min];
    entries.forEach(([key, value]) => {
      if (max < value) {
        max = value;
        maxPair = [key, value];
      }
      if (min > value) {
        min = value;
        minPair = [key, value];
      }
      this._keys.push(key);
      this._values.push(value);
    });
    this._kvMax = maxPair;
    this._kvMin = minPair;
    this._mostPrecise = mostPrecise(this._values);
  }
  frequencyTable() {
    const out: Record<(string | number), number> = {};
    for (let i = 0; i < this._values.length; i++) {
      const key = this._values[i];
      if (out[key] !== undefined) {
        const value = out[key];
        out[key] = value + 1;
      } else {
        out[key] = 1;
      }
    }
    return out;
  }
  get _yRange(): [number, number] {
    return [0, this._entryCount];
  }
  get _xRange(): [number, number] {
    return [0, this._kvMax[1]];
  }
  kvPairs() {
    const out: ([string | number, number])[] = [];
    for (let i = 0; i < this._entryCount; i++) {
      const key = this._keys[i];
      const value = this._values[i];
      out.push([key, value]);
    }
    return out;
  }
}

function meta(data: Record<(string | number), number>) {
  return new Metadata(data);
}

// =================================================================== line plot

class LinePlot extends CONTEXT {
  _data: Metadata;
  constructor(data: Record<string | number, number>) {
    super();
    this._data = meta(data);
  }
  _xTickLength: number = 0.2;
  xTickLength(length: number) {
    this._xTickLength = length;
    return this;
  }
  _yTickLength: number = 0.2;
  yTickLength(length: number) {
    this._yTickLength = length;
    return this;
  }
  _lineColor: string = "black";
  lineColor(color: string) {
    this._lineColor = color;
    return this;
  }
  _lineWidth: number = 2;
  lineWidth(w: number) {
    this._lineWidth = w;
    return this;
  }
  _fontSize: number = 9;
  end() {
    const count = this._data._entryCount - 1;
    this._domain = [0, count];
    this._range = [0, count];
    const d = path();
    d._commands = [];
    const datapoints = this._data.kvPairs();
    const keys = this._data._keys;
    const values = this._data._values;
    const xmin = this._data._kvMin[1];
    const xmax = this._data._kvMax[1];
    const fInterp = interpolator([xmin, xmax], [0, xmax]);
    const xdomain = this._data._xRange;
    const xrange = this._domain;
    const xf = interpolator(xdomain, xrange);
    const xt = this._xTickLength;
    const yt = this._yTickLength;
    datapoints.forEach(([_, value], index) => {
      value = xf(value);
      if (index === 0) {
        d._commands.push(M(index, value));
      } else {
        d._commands.push(L(index, value));
      }
      const xtick = line([index, -xt], [index, xt])
        .stroke(this._stroke);
      const xlabel = text(keys[index])
        .at(index, -xt * 2)
        .dy(-0.2)
        .anchor("middle")
        .fontSize(this._fontSize)
        .fontColor(this._stroke);
      this.and(xlabel);
      this.and(xtick);
      const ytick = line([-yt, index], [yt, index])
        .stroke(this._stroke);
      const t = fInterp(values[index]);
      let tx = t.toPrecision(4);
      if (t === 0) {
        tx = "0";
      }
      const ylabel = text(tx)
        .at(-yt, index)
        .dx(-0.1)
        .dy(-0.1)
        .anchor("end")
        .fontSize(this._fontSize)
        .fontColor(this._stroke);
      this.and(ylabel);
      this.and(ytick);
    });
    this.and(
      d.stroke(this._lineColor)
        .strokeWidth(this._lineWidth),
    );
    const xline = line([0, 0], [count, 0]).stroke("white");
    this.and(xline);
    const yline = line([0, 0], [0, count]).stroke("white");
    this.and(yline);
    this.fit();
    return this;
  }
}

export function linePlot(data: Record<string | number, number>) {
  return new LinePlot(data);
}

// ==================================================================== dot plot
class DotPlot<T extends (string | number) = any> extends CONTEXT {
  _data: Map<T, number> = new Map();
  constructor(data: T[]) {
    super();
    this._data = frequencyTable(data);
  }
  _xTickLength: number = 0.05;
  xTickLength(n: number) {
    this._xTickLength = n;
    return this;
  }
  _xTickSep: number = 1;
  xTickSep(n: number) {
    this._xTickSep = n;
    return this;
  }
  _dotSep: number = 0.5;
  dotSep(n: number) {
    this._dotSep = n;
    return this;
  }
  _dotFill: string = "initial";
  dotFill(color: string) {
    this._dotFill = color;
    return this;
  }
  _dotStroke: string = "initial";
  dotStroke(color: string) {
    this._dotStroke = color;
    return this;
  }
  end() {
    const keycount = this._data.size;
    const keys = mapKeys(this._data);
    this._domain = [-1, keycount];
    let ymax = -Infinity;
    for (const [_, value] of this._data) {
      (value > ymax) && (ymax = value);
    }
    this._range = [0, ymax];
    const x_axis = line([this._xmin, this._ymin], [this._xmax, this._ymin])
      .stroke(this._stroke);
    this.and(x_axis);
    const xticks = tickLines(
      this._xTickLength,
      this._xmin,
      this._xmax,
      this._xTickSep,
      "x",
      (_, i) => text(keys[i - 1]).anchor("middle").fontColor(this._stroke),
    );
    xticks.forEach((t) =>
      this.and(t.tick.stroke(this._stroke), t.txt.dy(-this._dotSep / 2))
    );
    let i = 0;
    this._data.forEach((value) => {
      const k = interpolator([0, 1], [0, this._dotSep]);
      range(0, value, 1).forEach((n) => {
        this.and(
          circle(this._dotRadius).at(i, k(n + this._dotSep))
            .fill(this._dotFill)
            .stroke(this._dotStroke),
        );
      });
      i++;
    });
    return this.fit();
  }
  _dotRadius: number = 0.2;
  dotRadius(r: number) {
    this._dotRadius = r;
    return this;
  }
}

export function dotPlot<T extends string | number>(data: T[]) {
  return new DotPlot(data);
}

// ==================================================================== bar plot

class BarPlot extends CONTEXT {
  _data: Map<string, number> = new Map();
  constructor(data: Record<string, number>) {
    super();
    Object.entries(data).forEach(([key, value]) => {
      this._data.set(key, value);
    });
  }
  _barWidth: number = 0.5;
  barWidth(n: number) {
    this._barWidth = n;
    return this;
  }
  _xTickLength: number = 0.01;
  xTickLength(length: number) {
    this._xTickLength = length;
    return this;
  }
  _yTickLength: number = 0.1;
  yTickLength(length: number) {
    this._yTickLength = length;
    return this;
  }
  _barColor: string = "initial";
  _barStroke: string = "initial";
  barStroke(color: string) {
    this._barStroke = color;
    return this;
  }
  barColor(color: string) {
    this._barColor = color;
    return this;
  }
  end() {
    const xlabels = mapKeys(this._data);
    const xmin = 0;
    const xmax = xlabels.length + this._barWidth;
    let ymin = Infinity;
    let ymax = -Infinity;
    let i = 0;
    const data: { x: number; y: number; key: string }[] = [];
    for (const [key, value] of this._data) {
      (value < ymin) && (ymin = value);
      (value > ymax) && (ymax = value);
      const d = { x: i, y: value, key };
      data.push(d);
      i++;
    }
    this._domain = [xmin, xmax];
    this._range = [xmin, ymax];
    const xline = line([this._xmin, this._ymin], [this._xmax, this._ymin]);
    const yline = line([this._xmin, this._ymin], [this._xmin, this._ymax]);
    data.forEach((d) => {
      const r = quad(this._barWidth, d.y)
        .at(d.x + this._barWidth, d.y)
        .stroke(this._barStroke)
        .fill(this._barColor);
      this.and(r);
    });
    this.and(xline.stroke(this._stroke), yline.stroke(this._stroke));
    const xticks = tickLines(
      this._xTickLength,
      xmin,
      xmax,
      1,
      "x",
      (_, i) => text(xlabels[i]),
    );
    xticks.forEach((tick) =>
      this.and(
        tick.txt.anchor("end").dx(1).dy(-1).fontColor(this._stroke),
      )
    );
    const yticks = tickLines(
      this._yTickLength,
      0,
      ymax,
      1,
      "y",
    );
    yticks.forEach((tick) => {
      this.and(
        tick.tick.stroke(this._stroke),
        tick.txt.fontColor(this._stroke).anchor("end").dx(-0.05).dy(-0.2),
      );
    });
    return this.fit();
  }
}
export function barPlot(data: Record<string, number>) {
  return new BarPlot(data);
}

// =================================================================== histogram

class Histogram extends CONTEXT {
  _data: Map<[number, number], number> = new Map();
  constructor(data: number[]) {
    super();
    this.data(data);
  }
  _barColor: string = "black";
  barColor(color: string) {
    this._barColor = color;
    return this;
  }
  _xTickSep: number = 0.5;
  xTickSep(n: number) {
    this._xTickSep = n;
    return this;
  }
  _xTickLength: number = 0.004;
  xTickLength(length: number) {
    this._xTickLength = length;
    return this;
  }
  _yTickLength: number = 0.8;
  yTickLength(length: number) {
    this._yTickLength = length;
    return this;
  }
  _yTickSep: number = 0.05;
  yTickSep(n: number) {
    this._yTickSep = n;
    return this;
  }

  end() {
    let i = 0;
    for (const [key, value] of this._data) {
      const [a, b] = key;
      const width = b - a;
      const barwidth = this._xTickSep / 2;
      const bar = quad(width * barwidth, -value)
        .fill(this._barColor)
        .stroke(this._stroke)
        .at(i * width * barwidth, 0);
      (value !== 0) && this._children.push(bar);
      i++;
    }
    const xs: number[] = [];
    for (const [key] of this._data) {
      xs.push(key[0]);
    }
    const x_axis = line([this._xmin, this._ymin], [this._xmax, this._ymin])
      .stroke(this._stroke);
    const y_axis = line([this._xmin, this._ymin], [this._xmin, this._ymax])
      .stroke(this._stroke);

    this.and(x_axis, y_axis);

    const xticks = tickLines(
      this._xTickLength,
      this._xmin,
      this._xmax,
      this._xTickSep,
      "x",
      (_, i) => text(xs[i]),
    );
    xticks.forEach((t) =>
      this.and(
        t.tick.stroke(this._stroke),
        t.txt
          .dy(-.015)
          .fontColor(this._stroke)
          .anchor("middle"),
      )
    );
    const yticks = tickLines(
      this._yTickLength,
      this._ymin,
      this._ymax,
      this._yTickSep,
      "y",
      (n) => text(n.toPrecision(2)),
    );
    yticks.forEach((t) =>
      this.and(
        t.tick.stroke(this._stroke),
        t.txt
          .fontColor(this._stroke)
          .anchor("end")
          .dx(-1)
          .dy(-0.005),
      )
    );
    return this.fit();
  }
  _barCount: number = 5;
  barCount(n: number) {
    this._barCount = n;
    return this;
  }
  data(dataset: number[]) {
    this._data = relativeFrequencyTable(
      dataset,
    );
    const x_min = minimum(dataset);
    const x_max = maximum(dataset);
    this._domain = [Math.min(x_min, 0), x_max];
    const values: number[] = [];
    for (const [_, value] of this._data) {
      values.push(value);
    }
    const y_min = minimum(values);
    const y_max = maximum(values);
    this._range = [Math.min(y_min, 0), y_max];
    return this;
  }
}
export function histogram(dataset: number[]) {
  return new Histogram(dataset);
}
type KEY = string | number;

class Point2D {
  _x: number;
  _y: number;
  _label: KEY = "";
  constructor(x: number, y: number) {
    this._x = x;
    this._y = y;
  }
  label(t: KEY) {
    this._label = t;
    return this;
  }
  circle(r: number) {
    return circle(r).at(this._x, this._y);
  }
  y(y: number) {
    this._y = y;
    return this;
  }
  x(x: number) {
    this._x = x;
    return this;
  }
}
function p2D(x: number, y: number) {
  return new Point2D(x, y);
}

class Mapping extends CONTEXT {
  _data: Record<KEY, KEY[]>;
  constructor(_data: Record<KEY, KEY[]>) {
    super();
    this._data = _data;
    this._domain = [-5, 5];
    this._range = [-5, 5];
  }
  _pointRadius: number = .2;
  _setSep: number = 1;
  setSep(sep: number) {
    this._setSep = sep;
    return this;
  }
  pointRadius(r: number) {
    this._pointRadius = r;
    return this;
  }
  end() {
    const entries = Object.entries(this._data).reverse();
    const sep = this._pointRadius * 2;
    let c = 0;
    const x0 = this._xmid - this._setSep;
    const x1 = this._xmid + this._setSep;
    entries.forEach(([key, values], i) => {
      const domainValue = p2D(x0, i);
      const vs = values.reverse();
      vs.forEach((r) => {
        const rangeValue = p2D(x1, c);
        this.and(
          line([domainValue._x, domainValue._y], [
            rangeValue._x,
            rangeValue._y,
          ]).stroke(this._stroke),
        );
        this.and(
          rangeValue.circle(this._pointRadius).fill(this._fill).stroke(
            this._stroke,
          ),
        );
        this.and(
          text(r).fontColor("white").at(x1 + sep, c),
        );
        c++;
      });
      this.and(
        domainValue.circle(this._pointRadius).fill(this._fill).stroke(
          this._stroke,
        ),
      );
      this.and(
        text(key).fontColor("white").at(x0 - sep, i),
      );
    });
    return this.fit();
  }
}

export function mapping(data: Record<KEY, KEY[]>) {
  return new Mapping(data);
}

// ================================================================ SCATTER PLOT

const SCATTER_PLOT = contextual(colorable(BASE));

class ScatterPlot<T = any> extends SCATTER_PLOT {
  _data: T[] = [];
  _x: (d: T) => number;
  _y: (d: T) => number;
  _children: Shape[] = [];
  constructor(x: (d: T) => number, y: (d: T) => number) {
    super();
    this._x = x;
    this._y = y;
    this._fill = "black";
    this._pointStroke = "black";
    this._stroke = "black";
  }
  _pointStroke: string;
  pointStroke(color: string) {
    this._pointStroke = color;
    return this;
  }
  _xTickLength: number = 0.05;
  _yTickLength: number = 0.05;
  _xTickSep: number = 0.5;
  _yTickSep: number = 0.5;
  end() {
    this._data.forEach((d) => {
      const x = this._x(d);
      const y = this._y(d);
      this._children.push(
        circle(0.08).at(x, y).fill(this._fill).stroke(this._pointStroke),
      );
    });
    const x_axis = line([this._xmin, this._ymin], [this._xmax, this._ymin])
      .stroke(this._stroke);
    this.and(x_axis);
    const y_axis = line([this._xmin, this._ymin], [this._xmin, this._ymax])
      .stroke(this._stroke);
    this.and(y_axis);
    const xticks = tickLines(
      this._xTickLength,
      this._xmin,
      this._xmax,
      this._xTickSep,
      "x",
    );
    xticks.forEach((t) =>
      this.and(
        t.tick.stroke(this._stroke),
        t.txt.fontColor(this._stroke).anchor("middle").dy(-0.2),
      )
    );
    const yticks = tickLines(
      this._yTickLength,
      this._ymin,
      this._ymax,
      this._yTickSep,
      "y",
    );
    yticks.forEach((t) =>
      this.and(
        t.tick.stroke(this._stroke),
        t.txt.fontColor(this._stroke).anchor("end").dx(-0.05).dy(-0.05),
      )
    );
    return this.fit();
  }
  data(d: T[]) {
    this._data = d;
    let min = Infinity;
    let max = -Infinity;
    for (let i = 0; i < d.length; i++) {
      const datum = this._x(d[i]);
      (datum < min) && (min = datum);
      (datum > max) && (max = datum);
    }
    this._domain = [Math.min(0, min), max];
    min = Infinity;
    max = -Infinity;
    for (let i = 0; i < d.length; i++) {
      const datum = this._y(d[i]);
      (datum < min) && (min = datum);
      (datum > max) && (max = datum);
    }
    this._range = [Math.min(0, min), max];
    return this;
  }
}
export function scatterPlot<T>(
  accessorX: (d: T) => number,
  accessorY: (d: T) => number,
) {
  return new ScatterPlot(accessorX, accessorY);
}

// ======================================================================= GROUP

const GROUP = colorable(BASE);

export class Group extends GROUP {
  _children: Shape[];
  _locked: boolean = false;
  lock() {
    this._locked = true;
    return this;
  }
  end() {
    this._children = this._children.map((x) => x.end());
    return this;
  }
  constructor(children: Shape[]) {
    super();
    this._children = children;
  }
  private tmap(op: (x: Shape) => Shape): Group {
    this._children = this._children.map((x) => {
      const out = op(x);
      return out;
    });
    return this;
  }
  rotateZ(angle: number): Group {
    return this.tmap((x) => x.rotateZ(angle));
  }
  rotateY(angle: number): Group {
    return this.tmap((x) => x.rotateY(angle));
  }
  rotateX(angle: number): Group {
    return this.tmap((x) => x.rotateX(angle));
  }
  scale(x: number, y: number): Group {
    return this.tmap((p) => p.scale(x, y));
  }
  translateZ(z: number): Group {
    return this.tmap((p) => p.translateZ(z));
  }
  translateY(y: number): Group {
    return this.tmap((p) => p.translateY(y));
  }
  translateX(x: number): Group {
    return this.tmap((p) => p.translateX(x));
  }
  shearZ(dx: number, dy: number): Group {
    return this.tmap((p) => p.shearZ(dx, dy));
  }
  shearX(dx: number, dy: number): Group {
    return this.tmap((p) => p.shearX(dx, dy));
  }
  translate(x: number, y: number): Group {
    return this.tmap((p) => p.translate(x, y));
  }
  interpolate(
    domain: [number, number],
    range: [number, number],
    dimensions: [number, number],
  ): Group {
    return this.tmap((p) => p.interpolate(domain, range, dimensions));
  }
  toString(): string {
    return this._children.map((s) => s.toString()).join("");
  }
}

export function group(children: Shape[]) {
  return new Group(children);
}

// ================================================================== CONTEXTUAL

interface Contextual {
  _children: Shape[];
  _domain: [number, number];
  domain(x: number, y: number): this;
  _range: [number, number];
  range(x: number, y: number): this;
  _margins: [number, number, number, number];
  margins(top: number, right?: number, bottom?: number, left?: number): this;
  marginTop(n: number): this;
  marginBottom(n: number): this;
  marginLeft(n: number): this;
  marginRight(n: number): this;
  _width: number;
  width(w: number): this;
  _height: number;
  height(h: number): this;
  get _my(): number;
  get _mx(): number;
  get _vw(): number;
  get _vh(): number;
  get _xmin(): number;
  get _xmid(): number;
  get _ymid(): number;
  get _xmax(): number;
  get _ymin(): number;
  get _ymax(): number;
  and(...shape: Shape[]): this;
  children(shape: (Shape | Shape[])[]): this;
  fit(domain?: [number, number], range?: [number, number]): this;
  _markers: Markers[];
}

function contextual<CLASS extends Klass>(klass: CLASS): And<CLASS, Contextual> {
  return class extends klass {
    _children: Shape[] = [];
    _markers: Markers[] = [];
    _domain: [number, number] = [-10, 10];
    domain(x: number, y: number) {
      if (x < y) this._domain = [x, y];
      return this;
    }
    marginTop(n: number) {
      this._margins[0] = n;
      return this;
    }
    marginBottom(n: number) {
      this._margins[2] = n;
      return this;
    }
    marginLeft(n: number) {
      this._margins[3] = n;
      return this;
    }
    marginRight(n: number) {
      this._margins[1] = n;
      return this;
    }
    fit(domain?: [number, number], range?: [number, number]) {
      const DOMAIN = domain ? domain : this._domain;
      const RANGE = range ? range : this._range;
      this._children = this._children.map((x) => x.end());
      this._children = this._children.map((x) => {
        if (x._locked) {
          return x;
        }
        const out = x.interpolate(
          DOMAIN,
          RANGE,
          [this._vw, this._vh],
        );
        return out;
      });
      this._children.forEach((child) => {
        if (child instanceof Line) {
          if (child._arrowEnd) {
            const arrow = child._arrowEnd;
            this._markers.push(arrow);
          }
          if (child._arrowStart) {
            const arrow = child._arrowStart;
            this._markers.push(arrow);
          }
        }
      });
      return this;
    }
    _range: [number, number] = [-10, 10];
    range(x: number, y: number) {
      if (x < y) this._range = [x, y];
      return this;
    }
    _margins: [number, number, number, number] = [50, 50, 50, 50];
    margins(
      top: number,
      right: number = top,
      bottom: number = top,
      left: number = right,
    ) {
      this._margins = [top, right, bottom, left];
      return this;
    }
    children(shapes: (Shape | Shape[])[]) {
      shapes.forEach((shape) => {
        if ($isArray(shape)) {
          shape.forEach((s) => this._children.push(s));
        } else {
          this._children.push(shape);
        }
      });
      return this;
    }
    and(...shapes: Shape[]) {
      this._children.push(...shapes);
      return this;
    }
    get _xmid() {
      return (this._xmin + this._xmax) / 2;
    }
    get _ymid() {
      return (this._ymin + this._ymax) / 2;
    }
    get _xmin() {
      return this._domain[0];
    }
    get _xmax() {
      return this._domain[1];
    }
    get _ymin() {
      return this._range[0];
    }
    get _ymax() {
      return this._range[1];
    }

    get _vw() {
      return this._width - (this._mx);
    }
    get _vh() {
      return this._height - (this._my);
    }

    get _my() {
      return this._margins[0] + this._margins[2];
    }
    get _mx() {
      return this._margins[1] + this._margins[3];
    }
    _width: number = 500;
    width(w: number) {
      this._width = w;
      return this;
    }
    _height: number = 500;
    height(h: number) {
      this._height = h;
      return this;
    }
  };
}

export class Space3D extends CONTEXT {
  constructor() {
    super();
    this._stroke = "white";
  }
  end() {
    const angle = -3 * PI / 4;
    const x_axis = line([this._xmin, 0], [this._xmax, 0]).stroke(this._stroke);
    const y_axis = line([0, this._ymin], [0, this._ymax]).stroke(this._stroke);
    const z_axis = line([0, this._ymin], [0, this._ymax])
      .rotateZ(angle)
      .stroke(this._stroke);
    this.and(x_axis, y_axis, z_axis);
    const tick = .3;
    const f = (x: number, y: number) => (x ** 2) + (y ** 2);
    const p = path();
    for (let i = this._domain[0]; i < this._domain[1]; i += tick) {
      for (let j = this._range[0]; j < this._range[1]; j += tick) {
        let z = f(i, j);
        if (isNaN(z)) {
          continue;
        }
        if (i === -5) {
          p._commands[0] = M(i, j, z);
        } else {
          p.L(i, j, z);
        }
      }
    }
    p.translateZ(0);
    this.and(p.stroke("tomato").opacity(0.8));
    this._children.forEach((c) => c.rotateZ(-PI / 4));
    return this.fit();
  }
}
export function space3D() {
  return new Space3D();
}

type EdgeType = "--" | "->";

class Vertex<T = any> {
  _id: string;
  _data: T | null;
  constructor(id: string | number, data: T | null = null) {
    this._id = `${id}`;
    this._data = data;
  }
  /** Returns a copy of this vertex. */
  copy() {
    const out = new Vertex(this._id, this._data);
    return out;
  }
  /** Sets this vertex’s data. */
  data(data: T) {
    const out = this.copy();
    out._data = data;
    return out;
  }
  /** Sets this vertex’s id. */
  id(value: string | number) {
    const out = this.copy();
    out._id = `${value}`;
    return out;
  }
}

export function vtx<T>(id: string | number, data: T | null = null) {
  return (
    new Vertex(id, data)
  );
}

class Edge<T = any, K = any> {
  _source: Vertex<T>;
  _target: Vertex<T>;
  _direction: EdgeType;
  _id: string;
  _meta: K | null;
  constructor(
    source: Vertex<T>,
    target: Vertex<T>,
    direction: EdgeType,
    meta: K | null = null,
  ) {
    this._source = source;
    this._target = target;
    this._direction = direction;
    this._id = `${source._id}${direction}${target._id}`;
    this._meta = meta;
  }
  /**
   * Returns true if this edge is equivalent to the other
   * edge. Where:
   *
   * - 𝑆₁ is the source id of this edge,
   * - 𝑆₂ is the source id of the other edge,
   * - 𝑇₁ is the target id of this edge, and
   * - 𝑇₂ is the target id of the other edge,
   *
   * the equivalence relation is defined as follows:
   * 1. If the edges are of different directions (`--` and `->` or vice versa), the
   *    edges are not equivalent.
   * 2. If the edges are both directed (`--`), the edges are equivalent
   *    only if:
   *    ~~~
   *    (𝑆₁ = 𝑆₂) AND (𝑇₁ = 𝑇₂).
   *    ~~~
   * 3. If the edges are undirected, the edges are equivalent only if:
   *    ~~~
   *    ((𝑆₁ = 𝑆₂) AND (𝑇₁ = 𝑇₂))  OR  ((𝑆₁ = 𝑇₂) AND (𝑇₁ = 𝑆₂))
   *    ~~~
   * @example
   * ~~~
   * // a and b are equivalent since they’re undirected:
   * // 1--2 and 2--1
   * const a = edge(1,2)
   * const b = edge(2,1)
   *
   * // c and d are equivalent since 1->2 and 1->2.
   * // e is not equivalent to either since it’s the directed
   * // edge 2->1
   * const c = link(1,2)
   * const d = link(1,2)
   * const e = link(2,1)
   * ~~~
   */
  isEquivalent(other: Edge) {
    const s1 = this._source._id;
    const t1 = this._target._id;
    const s2 = other._source._id;
    const t2 = other._target._id;
    if (this._direction === "->" && other._direction === "->") {
      return (s1 === s2) && (t1 === t2);
    }
    if (this._direction === "--" && other._direction === "--") {
      return ((s1 === s2 && t1 === t2) || (s1 === t2 && t1 === s2));
    }
    return false;
  }
  reverse() {
    const out = new Edge(this._target, this._source, this._direction);
    out._meta = this._meta;
    out._id = `${this._target._id}${this._direction}${this._source._id}`;
    return out;
  }
  metamap<X>(callback: (metadata: K) => X) {
    const metadata = this._meta;
    if (metadata === null) {
      return this as any as Edge<T, X>;
    }
    const m = callback(metadata);
    return new Edge(this._source, this._target, this._direction, m);
  }

  get isUndirected() {
    return this._direction === "--";
  }
  get isDirected() {
    return this._direction === "->";
  }
  get revid() {
    return `${this._target._id}${this._direction}${this._source._id}`;
  }
  copy() {
    const out = new Edge(this._source, this._target, this._direction);
    return out;
  }
  undirected() {
    if (!this.isDirected) return this;
    return new Edge(this._source, this._target, "--", this._meta);
  }
  direct() {
    if (this.isDirected) return this;
    return new Edge(this._source, this._target, "->", this._meta);
  }
}

export function edge(
  source: string | number | Vertex,
  target: string | number | Vertex,
) {
  return (
    new Edge(
      (typeof source === "string" || typeof source === "number")
        ? vtx(source)
        : source,
      (typeof target === "string" || typeof target === "number")
        ? vtx(target)
        : target,
      "--",
    )
  );
}

export function link(
  source: string | number | Vertex,
  target: string | number | Vertex,
) {
  return (
    new Edge(
      (typeof source === "string" || typeof source === "number")
        ? vtx(source)
        : source,
      (typeof target === "string" || typeof target === "number")
        ? vtx(target)
        : target,
      "->",
    )
  );
}

class Graph<T = any, K = any> {
  _adjacency: Map<string | number, Vertex<T>[]>;
  _vertices: Map<string | number, Vertex<T>>;
  _edges: Map<string, Edge<T, K>>;
  constructor() {
    this._adjacency = new Map();
    this._vertices = new Map();
    this._edges = new Map();
  }

  /** Returns all the neighbors of the given vertex. */
  neighbors(vertex: Vertex) {
    const out: Vertex[] = [];
    this._edges.forEach((e) => {
      if (e._source._id === vertex._id) out.push(e._target);
      else if (e._target._id === vertex._id) out.push(e._source);
    });
    return out;
  }

  /** Returns true if given source (referred to by id) is adjacent to the given target (by id). The edge type must be provided to ensure a correct result. */
  adjacent(
    sourceId: string | number,
    direction: EdgeType,
    targetId: string | number,
  ) {
    const st = `${sourceId}${direction}${targetId}`;
    const ts = `${targetId}${direction}${sourceId}`;
    return (
      this._edges.has(st) ||
      this._edges.has(ts)
    );
  }

  /** Returns the degree of the given vertex (referred to by id). */
  deg(id: string | number) {
    let degree = 0;
    this._edges.forEach((e) => {
      const sourceId = e._source._id;
      if (sourceId === id) {
        degree++;
      }
    });
    return degree;
  }

  /** Returns all the edges of this graph as an array. */
  edgeList() {
    const out: Edge[] = [];
    this._edges.forEach((e) => {
      out.push(e);
    });
    return out;
  }

  /** Returns all the vertices of this graph as an array. */
  vertexList() {
    const out: Vertex[] = [];
    this._vertices.forEach((v) => {
      out.push(v);
    });
    return out;
  }

  /** Returns true if this graph contains the given vertex (referred to by id). Otherwise, returns false. */
  hasVertex(vertexID: string | number): boolean {
    return this._adjacency.has(vertexID);
  }

  /** Appends the given vertex, alongside its data, to this graph. */
  vertex<T>(value: string | number | Vertex, data: T | null = null) {
    const v = typeof value === "string" || typeof value === "number"
      ? vtx(value, data)
      : value;
    if (!this.hasVertex(v._id)) {
      this._adjacency.set(v._id, []);
    }
    this._vertices.set(v._id, v);
    return v;
  }

  /** Appends the given edge to this graph. */
  E(edge: Edge) {
    const source = this.vertex(edge._source);
    const target = this.vertex(edge._target);
    this._adjacency.get(source._id)!.push(this._vertices.get(target._id)!);
    this._adjacency.get(target._id)!.push(this._vertices.get(source._id)!);
    this._edges.set(edge._id, edge);
    const rev = edge.reverse();
    this._edges.set(rev._id, rev);
    return this;
  }

  /** Creates a new edge from the given `sourceID` and `targetID`, then appends the resulting edge to this graph. */
  edge(sourceID: string | number | Vertex, targetID: string | number | Vertex) {
    const E = edge(sourceID, targetID);
    this.E(E);
    return this;
  }
}

/** Returns a new graph. */
export function graph(adjacencyList?: Record<string, (string | number)[]>) {
  const G = new Graph();
  if (adjacencyList) {
    Object.keys(adjacencyList).forEach((source) => {
      const targets = adjacencyList[source];
      const src = vtx(source);
      targets.forEach((target) => {
        const tar = vtx(target);
        const e = edge(src, tar);
        G.E(e);
      });
    });
  }
  return G;
}

class Particle {
  /** The particle’s position vector. */
  _p: Vector;

  /** The particle’s velocity vector. */
  _v: Vector = v2D(0, 0);

  /** The particle’s force vector. */
  _f: Vector = v2D(0, 0);

  /** The particle’s unique identifier. */
  _id: string | number;

  /** The particle’s radius. */
  _r: number = 3;
  constructor(id: string | number, position: Vector) {
    this._p = position;
    this._id = id;
  }
}

/** Returns a new particle. */
function pt(id: string | number, position: Vector) {
  return new Particle(id, position);
}

const FORCE_GRAPH = contextual(BASE);

export class ForceGraph extends FORCE_GRAPH {
  private _particles: Map<(string | number), Particle>;
  private _graph: Graph;
  _iterations: number = 100;
  iterations(x: number) {
    this._iterations = x;
    return this;
  }
  _epsilon: number = 0.5;
  epsilon(e: number) {
    this._epsilon = e;
    return this;
  }
  _stable: boolean = false;
  _repulsion: number = 20;
  repulsion(n: number) {
    this._repulsion = n;
    return this;
  }
  _attraction: number = 0.04;
  attraction(n: number) {
    this._attraction = n;
    return this;
  }
  _decay: number = 0.6;
  decay(n: number) {
    this._decay = n;
    return this;
  }
  _children: Shape[] = [];
  constructor(graph: Graph) {
    super();
    this._graph = graph;
    this._particles = new Map();
  }

  private forEachPt(callback: (particle: Particle) => void) {
    this._particles.forEach((p) => callback(p));
  }

  _domain: [number, number] = [125, 250];
  _range: [number, number] = [125, 250];
  private layout() {
    const MIN_X = this._domain[0];
    const MAX_X = this._domain[1];
    const MIN_Y = this._range[0];
    const MAX_Y = this._range[1];
    for (let i = 0; i < this._iterations; i++) {
      this.iterate(MIN_X, MAX_X, MIN_Y, MAX_Y);
      if (this._stable) break;
    }
  }

  private iterate(
    MIN_X: number,
    MAX_X: number,
    MIN_Y: number,
    MAX_Y: number,
  ) {
    const rsq = (v: Vector, u: Vector) => (
      ((v._x - u._x) ** 2) + ((v._y - u._y) ** 2)
    );
    this.forEachPt((v) => {
      v._f = v2D(0, 0);
      this.forEachPt((u) => {
        if (v._id !== u._id) {
          let d2 = rsq(v._p, u._p);
          if (d2 === 0) d2 = 0.001;
          const c = this._repulsion / d2;
          const f = (v._p.sub(u._p)).mul(c);
          v._f = v._f.add(f);
        }
      });
    });
    this._graph._edges.forEach((e) => {
      const u = this._particles.get(e._source._id);
      const v = this._particles.get(e._target._id);
      if (u && v) {
        const f = (u._p.sub(v._p)).mul(this._attraction);
        v._f = v._f.add(f);
      }
    });
    let displacement = 0;
    this.forEachPt((v) => {
      v._v = (v._v.add(v._f)).mul(this._decay);
      displacement += (Math.abs(v._v._x)) + Math.abs(v._v._y);
      v._p = v._p.add(v._v);
      v._p._x = clamp(MIN_X, v._p._x, MAX_X);
      v._p._y = clamp(MIN_Y, v._p._y, MAX_Y);
    });
    this._stable = displacement < this._epsilon;
  }

  /** Sets the initial position of all particles. By default, particles are initially placed randomly. */
  scatter() {
    this._graph._vertices.forEach((v) => {
      const x = randInt(-2, 2);
      const y = randInt(-2, 2);
      this._particles.set(v._id, pt(v._id, v2D(x, y)));
    });
  }

  _styles: {
    _nodes: Partial<
      {
        _fill: string;
        _radius: number;
        _fontColor: string;
        _fontSize: number;
        _fontFamily: string;
      }
    >;
    _edges: Partial<{ _stroke: string }>;
  } = {
    _nodes: { _fill: "white", _radius: 5 },
    _edges: { _stroke: "grey" },
  };

  get _nodeFontFamily() {
    return this._styles._nodes._fontFamily ?? "inherit";
  }
  nodeFontFamily(font: string) {
    this._styles._nodes._fontFamily = font;
    return this;
  }

  get _nodeFontSize() {
    return this._styles._nodes._fontSize ?? 12;
  }
  nodeFontSize(fontSize: number) {
    this._styles._nodes._fontSize = fontSize;
    return this;
  }

  get _nodeFontColor() {
    return this._styles._nodes._fontColor ?? "initial";
  }

  nodeFontColor(color: string) {
    this._styles._nodes._fontColor = color;
    return this;
  }

  get _nodeRadius() {
    return this._styles._nodes._radius ? this._styles._nodes._radius : 5;
  }

  get _nodeColor() {
    return this._styles._nodes._fill ? this._styles._nodes._fill : "white";
  }

  get _edgeColor() {
    return this._styles._edges._stroke ?? "grey";
  }

  edgeColor(color: string) {
    this._styles._edges._stroke = color;
    return this;
  }

  /** Sets the radius for all nodes in this graph. */
  nodeRadius(r: number) {
    this._styles._nodes._radius = r;
    return this;
  }

  /** Sets the color for all nodes in this graph. */
  nodeColor(value: string) {
    this._styles._nodes._fill = value;
    return this;
  }

  /**
   * Begins drawing the force graph.
   */
  end() {
    this.scatter();
    this.layout();
    const ids = new Set<string>();
    this._graph._edges.forEach((e) => {
      const source = this._particles.get(e._source._id);
      const target = this._particles.get(e._target._id);
      if (source && target && !ids.has(e._id)) {
        const x1 = source._p._x;
        const y1 = source._p._y;
        const x2 = target._p._x;
        const y2 = target._p._y;
        const l = line([x1, y1], [x2, y2]).stroke(this._edgeColor);
        this._children.push(l);
      }
      ids.add(e._id);
      ids.add(e.revid);
    });
    this._particles.forEach((p) => {
      const t = p._id;
      const c = circle(this._nodeRadius)
        .at(p._p._x, p._p._y)
        .fill(this._nodeColor);
      this.and(c);
      const label = text(t)
        .at(p._p._x, p._p._y + p._r)
        .fontFamily(this._nodeFontFamily)
        .fontSize(this._nodeFontSize)
        .fontColor(this._nodeFontColor);
      this.and(label);
    });
    return this.fit();
  }
}

/** Returns a new force layout graph. */
export function forceGraph(graph: Graph) {
  return (
    new ForceGraph(graph)
  );
}

const TNODE = colorable(renderable(BASE));

class TNode extends TNODE {
  _thread: TreeChild | null = null;
  _parent: Fork | null = null;
  _children: TreeChild[] = [];
  _index: number = 0;
  _change: number = 0;
  _shift: number = 0;
  _leftmostSibling: TreeChild | null = null;
  _name: string | number;
  _dx: number = 0;
  _dy: number = 0;
  _id: string | number = uid(5);
  get _x() {
    return this._commands[0]._end._x;
  }
  get _y() {
    return this._commands[0]._end._y;
  }
  get _z() {
    return this._commands[0]._end._z;
  }
  set _x(x: number) {
    this._commands = [M(x, this._y, this._z)];
  }
  set _y(y: number) {
    this._commands = [M(this._x, y, this._z)];
  }
  set _z(z: number) {
    this._commands = [M(this._x, this._y, z)];
  }
  constructor(name: string | number, parent?: Fork) {
    super();
    this._name = name;
    this._parent = parent ? (parent) : null;
    this._commands = [M(0, 0, 1)];
  }
  sketch(depth: number = 0) {
    this._x = -1;
    this._y = depth;
    this._dx = 0;
    this._change = 0;
    this._shift = 0;
    this._thread = null;
    this._leftmostSibling = null;
  }
  left(): TreeChild | null {
    if (this._thread) {
      return this._thread;
    } else if (this._children.length) {
      return this._children[0];
    } else return null;
  }
  right(): TreeChild | null {
    if (this._thread) {
      return this._thread;
    } else if (this._children.length) {
      return (this._children[this._children.length - 1]);
    } else return null;
  }
  get _hasNoChildren() {
    return this._degree === 0;
  }
  get _hasChildren() {
    return !this._hasNoChildren;
  }
  get _degree() {
    return this._children.length;
  }
  hasChild(id: string | number) {
    if (this._hasNoChildren) return false;
    for (let i = 0; i < this._degree; i++) {
      if (this._children[i]._id === id) return true;
    }
    return false;
  }
}

class Leaf extends TNode {
  _ancestor: TreeChild;
  constructor(name: string | number, parent?: Fork) {
    super(name, parent);
    this._x = -1;
    this._ancestor = this;
  }
  height() {
    return 1;
  }
  onLastChild(_: (node: TreeChild) => void) {
    return;
  }
  onFirstChild(_: (node: TreeChild) => void) {
    return;
  }
  nodes(nodes: TreeChild[]) {
    return this;
  }
  child(child: TreeChild) {
    return this;
  }
  inorder(f: (node: TreeChild, index: number) => void) {
    return this;
  }
  preorder(f: (node: TreeChild, index: number) => void) {
    return this;
  }
  postorder(f: (node: TreeChild, index: number) => void) {
    return this;
  }
  bfs(f: (node: TreeChild, level: number) => void) {
    return this;
  }

  childOf(parent: Fork) {
    this._parent = parent;
    this._ancestor = parent._ancestor;
    return this;
  }
}

export function leaf(name: string | number) {
  return new Leaf(name);
}

class Fork extends TNode {
  _ancestor: TreeChild;
  childOf(parent: Fork) {
    this._parent = parent;
    this._ancestor = parent._ancestor;
    return this;
  }
  height() {
    let height = -Infinity;
    this._children.forEach((c) => {
      const h = c.height();
      if (h > height) height = h;
    });
    return height + 1;
  }
  nodes(nodes: TreeChild[]) {
    nodes.forEach((node) => {
      node._index = this._degree;
      this._children.push(node.childOf(this));
    });
    return this;
  }
  onLastChild(callback: (node: TreeChild) => void) {
    const c = this._children[this._children.length - 1];
    if (c) callback(c);
    return this;
  }
  onFirstChild(callback: (node: TreeChild) => void) {
    const c = this._children[0];
    if (c) callback(c);
    return this;
  }
  child(child: TreeChild) {
    this._children.push(child.childOf(this));
    return this;
  }
  inorder(f: (node: TreeChild, index: number) => void) {
    let i = 0;
    const t = (tree: TreeChild) => {
      const [left, right] = arraySplit(tree._children);
      (left.length) && (left.forEach((c) => t(c)));
      f(tree, i++);
      (right.length) && (right.forEach((c) => t(c)));
    };
    t(this);
    return this;
  }
  preorder(f: (node: TreeChild, index: number) => void) {
    let i = 0;
    const t = (tree: TreeChild) => {
      const [left, right] = arraySplit(tree._children);
      f(tree, i++);
      (left.length) && (left.forEach((c) => t(c)));
      (right.length) && (right.forEach((c) => t(c)));
    };
    t(this);
    return this;
  }
  postorder(f: (node: TreeChild, index: number) => void) {
    let i = 0;
    const t = (tree: TreeChild) => {
      const [left, right] = arraySplit(tree._children);
      (left.length) && (left.forEach((c) => t(c)));
      (right.length) && (right.forEach((c) => t(c)));
      f(tree, i++);
    };
    t(this);
    return this;
  }
  bfs(f: (node: TreeChild, level: number) => void) {
    const queue = linkedList<TreeChild>(this);
    let count = queue.length;
    let level = 0;
    while (queue.length > 0) {
      const tree = queue.shift();
      count--;
      if (tree._tag === "None") continue;
      f(tree.value, level);
      tree.value._children.forEach((c) => queue.push(c));
      if (count === 0) {
        level++;
        count = queue.length;
      }
    }
    queue.clear();
    return this;
  }
  constructor(name: string | number) {
    super(name);
    this._ancestor = this;
  }
}

type TreeChild = Leaf | Fork;

/** Returns true if the given argument is a `Leaf`. Else, false. */
function isLeaf(x: any): x is Leaf {
  return x instanceof Leaf;
}

export function subtree(name: string | number) {
  return new Fork(name);
}

type TreeLayout =
  | "knuth"
  | "wetherell-shannon"
  | "buccheim-unger-leipert"
  | "hv"
  | "reingold-tilford";

type Traversal =
  | "preorder"
  | "inorder"
  | "postorder"
  | "bfs";

type LinkFunction = (line: Line, source: TNode, target: TNode) => Line;
const TREE = contextual(colorable(BASE));

class Tree extends TREE {
  _tree: Fork;
  _layout: TreeLayout = "knuth";
  layout(option: TreeLayout) {
    this._layout = option;
    return this;
  }
  private _edgenotes: Partial<Record<Traversal, LinkFunction>> = {};
  edges(of: Traversal, callback: LinkFunction) {
    this._edgenotes[of] = callback;
    return this;
  }
  nodes(nodes: TreeChild[]) {
    nodes.forEach((n) => this._tree.child(n));
    return this;
  }
  private HV() {
    const largerToRight = (parent: TreeChild) => {
      const left = parent.left();
      const right = parent.right();
      if ((left === null) || (right === null)) return;
      const sh = 2;
      if (isLeaf(left) && isLeaf(right)) {
        right._x = parent._x + 1;
        right._y = parent._y;
        left._x = parent._x;
        left._y = parent._y - sh;
        parent._dx = 1;
      } else {
        const L = left._degree;
        const R = right._degree;
        if (L >= R) {
          left._x = parent._x + R + 1;
          left._y = parent._y;
          right._x = parent._x;
          right._y = parent._y - 2;
          parent._dx += left._x;
        } else if (L < R) {
          right._x = parent._x + L + 1;
          right._y = parent._y;
          left._x = parent._x;
          left._y = parent._y - sh;
          parent._dx += right._x;
        }
      }
      parent._children.forEach((c) => largerToRight(c));
    };
    const xmin = this._xmin;
    const ymax = this._ymax;
    this._tree._x = xmin;
    this._tree._y = ymax;
    largerToRight(this._tree);
    return this;
  }
  private buccheim() {
    const leftBrother = (self: TreeChild) => {
      let n = null;
      if (self._parent) {
        for (const node of self._parent._children) {
          if (node._id === self._id) return n;
          else n = node;
        }
      }
      return n;
    };
    const get_lmost_sibling = (self: TreeChild) => {
      if (
        !self._leftmostSibling &&
        self._parent &&
        self._id !== self._parent._children[0]._id
      ) {
        self._leftmostSibling = self._parent._children[0];
        return self._parent._children[0];
      }
      return self._leftmostSibling;
    };
    const movesubtree = (
      wl: TreeChild,
      wr: TreeChild,
      shift: number,
    ) => {
      const st = wr._index - wl._index;
      wr._change -= shift / st;
      wr._shift += shift;
      wl._change += shift / st;
      wr._x += shift;
      wr._dx += shift;
    };
    const ancestor = (
      vil: TreeChild,
      v: TreeChild,
      default_ancestor: TreeChild,
    ) => {
      if (v._parent && v._parent.hasChild(vil._id)) {
        return vil._ancestor;
      }
      return default_ancestor;
    };
    const apportion = (
      v: TreeChild,
      default_ancestor: TreeChild,
      distance: number,
    ) => {
      const w = leftBrother(v);
      let vol = get_lmost_sibling(v);
      if (w !== null && vol !== null) {
        let vir = v;
        let vor = v;
        let vil = w;
        let sir = v._dx;
        let sor = v._dx;
        let sil = vil._dx;
        let sol = vol._dx;
        let VIL: TreeChild | null = vil;
        let VIR: TreeChild | null = vir;
        let VOL: TreeChild | null = vol;
        let VOR: TreeChild | null = vor;
        while (VIL?.right() && VIR?.left()) {
          VIL = vil.right();
          if (VIL) vil = VIL;
          VIR = vir.left();
          if (VIR) vir = VIR;
          VOL = vol.left();
          if (VOL) vol = VOL;
          VOR = vor.right();
          if (VOR) {
            vor = VOR;
            // @ts-ignore
            vor._ancestor = v;
          }
          let shift = (vil._x + sil) - (vir._x + sir) + distance;
          if (shift > 0) {
            let a = ancestor(vil, v, default_ancestor);
            movesubtree(a, v, shift);
            sir = sir + shift;
            sor = sor + shift;
          }
          sil += vil._dx;
          sir += vir._dx;
          sol += vol._dx;
          sor += vor._dx;
        }
        if (vil.right() && !vor.right()) {
          vor._thread = vil.right();
          vor._dx += sil - sor;
        } else {
          if (vir.left() && !vol.left()) {
            vol._thread = vir.left();
            vol._dx += sir - sol;
          }
          default_ancestor = v;
        }
      }
      return default_ancestor;
    };
    const execShifts = (v: TreeChild) => {
      let shift = 0;
      let change = 0;
      for (const w of v._children) {
        w._x += shift;
        w._dx += shift;
        change += w._change;
        shift += w._shift + change;
      }
    };
    const firstwalk = (
      v: TreeChild,
      distance: number = 1,
    ) => {
      if (v._children.length === 0) {
        if (v._leftmostSibling) {
          const lb = leftBrother(v);
          if (lb) v._x = lb._x + distance;
        } else v._x = 0;
      } else {
        let default_ancestor = v._children[0];
        for (const w of v._children) {
          firstwalk(w);
          default_ancestor = apportion(
            w,
            default_ancestor,
            distance,
          );
        }
        execShifts(v);
        const L = v._children[0];
        const R = v._children[v._children.length - 1];
        let midpoint = (L._x + R._x) / 2;
        const w = leftBrother(v);
        if (w) {
          v._x = w._x + distance;
          v._dx = v._x - midpoint;
        } else {
          v._x = midpoint;
        }
      }
      return v;
    };
    const secondwalk = (
      v: TreeChild,
      m: number = 0,
      depth: number = 0,
      min: number | null = null,
    ): number => {
      v._x += m;
      v._y = -depth;
      if (min === null || v._x < min) {
        min = v._x;
      }
      for (const w of v._children) {
        min = secondwalk(w, m + v._dx, depth + 1, min);
      }
      return min;
    };
    const thirdwalk = (tree: TreeChild, n: number) => {
      tree._x += n;
      for (const w of tree._children) {
        thirdwalk(w, n);
      }
    };
    const buccheim = () => {
      this._tree.sketch();
      firstwalk(this._tree);
      const min = secondwalk(this._tree);
      if (min < 0) {
        thirdwalk(this._tree, -min);
      }
    };
    buccheim();
    buccheim();
    const x = this._tree._x;
    const y = this._tree.height() / 2;
    this._tree.bfs((n) => {
      n._x -= x;
      n._y += y;
    });
    return this;
  }
  private knuth() {
    this._tree.bfs((node, level) => {
      const y = 0 - level;
      node._y = y;
    });
    this._tree.inorder((node, index) => {
      node._x = index;
    });
    const x = this._tree._x;
    this._tree.bfs((node) => {
      node._x -= x;
    });
    return this;
  }
  private reingoldTilford() {
    const contour = (
      left: TreeChild,
      right: TreeChild,
      max_offset: number | null = null,
      left_offset: number = 0,
      right_offset: number = 0,
      left_outer: TreeChild | null = null,
      right_outer: TreeChild | null = null,
    ): [
      TreeChild | null,
      TreeChild | null,
      number,
      number,
      number,
      TreeChild,
      TreeChild,
    ] => {
      let delta = left._x + left_offset - (right._x + right_offset);
      if (max_offset === null || delta > max_offset) {
        max_offset = delta;
      }
      if (left_outer === null) left_outer = left;
      if (right_outer === null) right_outer = right;
      let lo = left_outer.left();
      let li = left.right();
      let ri = right.left();
      let ro = right_outer.right();
      if (li && ri) {
        left_offset += left._dx;
        right_offset += right._dx;
        return contour(
          li,
          ri,
          max_offset,
          left_offset,
          right_offset,
          lo,
          ro,
        );
      }
      const out = tuple(
        li,
        ri,
        max_offset,
        left_offset,
        right_offset,
        left_outer,
        right_outer,
      );
      return out;
    };
    const fixSubtrees = (
      left: TreeChild,
      right: TreeChild,
    ) => {
      let [li, ri, diff, loffset, roffset, lo, ro] = contour(left, right);
      diff += 1;
      diff += (right._x + diff + left._x) % 2;
      right._dx = diff;
      right._x += diff;
      if (right._children.length) {
        roffset += diff;
      }
      if (ri && !li) {
        lo._thread = ri;
        lo._dx = roffset - loffset;
      } else if (li && !ri) {
        ro._thread = li;
        ro._dx = loffset - roffset;
      }
      const out = Math.floor((left._x + right._x) / 2);
      return out;
    };
    const addmods = (tree: TreeChild, mod: number = 0) => {
      tree._x += mod;
      tree._children.forEach((c) => addmods(c, mod + tree._dx));
      return tree;
    };
    const setup = (tree: TreeChild, depth: number = 0) => {
      tree.sketch(-depth);
      if (tree._children.length === 0) {
        tree._x = 0;
        return tree;
      }
      if (tree._children.length === 1) {
        tree._x = setup(tree._children[0], depth + 1)._x;
        return tree;
      }
      const left = setup(tree._children[0], depth + 1);
      const right = setup(tree._children[1], depth + 1);
      tree._x = fixSubtrees(left, right);
      return tree;
    };
    setup(this._tree);
    addmods(this._tree);
    const x = this._tree._x;
    const y = this._tree.height() / 2;
    this._tree.bfs((n) => {
      n._x -= x;
      n._y += y;
    });
    return this;
  }
  private wetherellShannon() {
    const lay = (
      tree: TreeChild,
      depth: number,
      nexts: number[] = [0],
      offsets: number[] = [0],
    ) => {
      tree._children.forEach((c) => {
        lay(c, depth + 1, nexts, offsets);
      });
      tree._y = -depth;
      if ($isNothing(nexts[depth])) {
        nexts[depth] = 0;
      }
      if ($isNothing(offsets[depth])) {
        offsets[depth] = 0;
      }
      let x = nexts[depth];
      if (tree._degree === 0) {
        x = nexts[depth];
      } else if (tree._degree === 1) {
        x = tree._children[0]._x + 1;
      } else {
        let lx = 0;
        tree.onFirstChild((n) => {
          lx = n._x;
        });
        let rx = 0;
        tree.onLastChild((n) => {
          rx = n._x;
        });
        const xpos = lx + rx;
        x = xpos / 2;
      }
      offsets[depth] = max(offsets[depth], nexts[depth] - x);
      if (tree._degree === 0) {
        const d = x + offsets[depth];
        tree._x = d;
      } else {
        tree._x = x;
      }
      nexts[depth] += 2;
      tree._dx = offsets[depth];
    };
    const addDxs = (tree: TreeChild, sum: number = 0) => {
      tree._x = tree._x + sum;
      sum += tree._dx;
      tree._children.forEach((c) => addDxs(c, sum));
    };
    lay(this._tree, 0);
    addDxs(this._tree);
    const x = this._tree._x;
    this._tree.bfs((n) => {
      n._x -= x;
    });
    return this;
  }
  private lay() {
    // deno-fmt-ignore
    switch (this._layout) {
      case 'buccheim-unger-leipert': return this.buccheim();
      case 'hv': return this.HV();
      case 'knuth': return this.knuth();
      case 'reingold-tilford': return this.reingoldTilford();
      case 'wetherell-shannon': return this.wetherellShannon();
    }
  }
  _nodeRadius: number = 0.5;
  nodeRadius(r: number) {
    this._nodeRadius = r;
    return this;
  }
  _edgeStroke: string = "black";
  edgeStroke(color: string) {
    this._edgeStroke = color;
    return this;
  }
  _nodeFill: string = "black";
  nodeFill(color: string) {
    this._nodeFill = color;
    return this;
  }
  _textFn?: (t: Text) => Text;
  textMap(callback: (t: Text) => Text) {
    this._textFn = callback;
    return this;
  }
  _nodeFn?: (t: TreeChild) => Shape;
  nodeMap(callback: (t: TreeChild) => Shape) {
    this._nodeFn = callback;
    return this;
  }
  _edgeFn?: (source: TreeChild, target: TreeChild) => Shape;
  edgeMap(callback: (source: TreeChild, target: TreeChild) => Shape) {
    this._edgeFn = callback;
    return this;
  }
  end() {
    this.lay();
    this._tree.bfs((node) => {
      const p = node._parent;
      if (p) {
        const l = line([p._x, p._y], [node._x, node._y]);
        this.and(l);
      }
    });
    const nodes: Shape[] = [];
    const labels: Text[] = [];
    this._tree.bfs((node) => {
      const x = node._x;
      const y = node._y;
      let c = this._nodeFn
        ? (this._nodeFn(node))
        : circle(this._nodeRadius).at(x, y);
      nodes.push(c);
      let t = text(node._name).at(x, y).anchor("middle").dy(this._nodeRadius);
      (this._textFn) && (t = this._textFn(t));
      labels.push(t);
    });
    nodes.forEach((c) => this.and(c));
    labels.forEach((c) => this.and(c));
    this._children = this._children.map((c) => c.end());
    this._children = this._children.map((c) => {
      const o = c.interpolate(
        this._domain,
        this._range,
        [this._vw, this._vh],
      );
      if (o instanceof Circle) {
        o._fill = this._nodeFill;
        o._stroke = this._edgeStroke;
      }
      if (o instanceof Line) {
        o._stroke = this._edgeStroke;
      }
      return o;
    });
    return this;
  }
  constructor(tree: Fork) {
    super();
    this._tree = tree;
    const m = 30;
    this._domain = [-5, 5];
    this._range = [-5, 5];
    this._margins = [m, m, m, m];
    this._width = 300;
    this._height = 300;
  }
}

export function tree(t: Fork) {
  return new Tree(t);
}

// deno-fmt-ignore
export type Parent =
| ForceGraph
| Space3D
| ScatterPlot
| Histogram
| Plot2D
| Tree
| BarPlot
| PolarPlot2D
| Plane
| LinePlot
| Mapping
| DotPlot;

export type Shape = Group | Circle | Line | Path | Text | Quad | Area2D;
export type Markers = ArrowHead;

// ========================================================= end graphics module

class BigRat {
  N: bigint;
  D: bigint;
  constructor(N: bigint, D: bigint) {
    this.N = N;
    this.D = D;
  }
  get _isZero() {
    return this.N !== 0n;
  }
  toString() {
    return `#${this.N}|${this.D}`;
  }
}

function bigRat(N: bigint, D: bigint) {
  return new BigRat(N, D);
}

const $isFraction = (
  value: any,
): value is Fraction => (value instanceof Fraction);

function ratio(n: number, d: number) {
  return new Fraction(n, d);
}

/**
 * Utility function for printing the AST.
 */
function objectTree<T extends Object>(
  Obj: T,
  initial: string = ".",
  cbfn?: (node: any) => void,
): string {
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
    prevstack: ([T, boolean])[],
    cb: (str: string) => any,
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
    if (root instanceof Expr || root instanceof Statement) {
      // @ts-ignore
      root["node"] = nodekind[root.kind];
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
  if (Obj instanceof Statement) {
    // @ts-ignore
    initial = nodekind[Obj.kind];
  }
  grow(
    initial as keyof T,
    obj,
    false,
    [],
    (line: string) => (output += line + "\n"),
  );
  return output;
}

/** At the parsing stage, all parsed node results are kept in an `Either` type (either an AST node) or an Err (error) object. We want to avoid throwing as much as possible for optimal parsing. */
type Either<A, B> = Left<A> | Right<B>;

/** A box type corresponding to failure. */
class Left<T> {
  private value: T;
  constructor(value: T) {
    this.value = value;
  }
  map<A>(f: (x: never) => A): Either<T, never> {
    return this as any;
  }
  isLeft(): this is Left<T> {
    return true;
  }
  isRight(): this is never {
    return false;
  }
  chain<X, S>(f: (x: never) => Either<X, S>): Left<T> {
    return this;
  }
  read<K>(value: K): K {
    return value;
  }
  flatten(): Left<T> {
    return this;
  }
  unwrap() {
    return this.value;
  }
  ap<B, E>(f: Either<T, E>): Either<never, B> {
    return this as any;
  }
}

/** A box type corresponding success. */
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
  chain<N, X>(f: (x: T) => Either<N, X>): Either<never, X> {
    return f(this.value) as Either<never, X>;
  }
  flatten(): Right<(T extends Right<(infer T)> ? T : never)> {
    return ((this.value instanceof Right ||
        this.value instanceof Left)
      ? this.value
      : this) as Right<(T extends Right<(infer T)> ? T : never)>;
  }
  read<K>(_: K): T {
    return this.value;
  }
  unwrap() {
    return this.value;
  }
  ap<B, E>(f: Either<E, (x: T) => B>): Either<never, B> {
    if (f.isLeft()) return f as any as Right<B>;
    return this.map(f.value);
  }
}

/** Returns a new left. */
const left = <T>(x: T): Left<T> => new Left(x);

/** Returns a new right. */
const right = <T>(x: T): Right<T> => new Right(x);

// deno-fmt-ignore
type ErrorType =
| "lexical-error"
| "syntax-error"
| "type-error"
| "runtime-error"
| "resolver-error"
| "environment-error"
| "algebraic-error";

/** An object containing an error report. */
class Err extends Error {
  message: string;
  errorType: ErrorType;
  phase: string;
  location: Location;
  recommendation: string;
  constructor(
    message: string,
    errorType: ErrorType,
    phase: string,
    line: number,
    column: number,
    recommendation: string = "none",
  ) {
    super(message);
    this.errorType = errorType;
    this.phase = phase;
    this.location = { line, column };
    this.message = message;
    this.recommendation = recommendation;
  }
  report() {
    return formattedError(
      this.message,
      this.phase,
      this.errorType,
      this.location.line,
      this.location.column,
      this.recommendation,
    );
  }
}

/** Ensures all errors have the same format. */
function formattedError(
  message: string,
  phase: string,
  errorType: ErrorType,
  line: number,
  column: number,
  recommendation: string,
) {
  let moduleName = "module";
  switch (errorType) {
    case "lexical-error":
      moduleName = "scanner";
      break;
    case "algebraic-error":
      moduleName = "algebraic tree transfomer";
      break;
    case "environment-error":
      moduleName = "environment";
      break;
    case "resolver-error":
      moduleName = "resolver";
      break;
    case "runtime-error":
      moduleName = "interpreter";
      break;
    case "syntax-error":
      moduleName = "parser";
      break;
    case "type-error":
      moduleName = "typechecker";
  }
  return (`${errorType.toUpperCase()}.\nWhile ${phase}, an error occurred on\nline ${line}, column ${column}.\nReporting from the ${moduleName}:\n${message} \nRecommendation: ${recommendation}`);
}

/** Returns a new lexical error. A lexical error is raised if an error occured during scanning. */
function lexicalError(
  message: string,
  phase: string,
  token: Token,
) {
  return new Err(message, "lexical-error", phase, token.L, token.C);
}

/** Returns a new syntax error. A syntax error is raised if an error occured during parsing. */
function syntaxError(
  message: string,
  phase: string,
  token: Token,
) {
  return new Err(message, "syntax-error", phase, token.L, token.C);
}

/** Returns a new runtime error. A runtime error is raised if an error occured during interpretation. */
function runtimeError(
  message: string,
  phase: string,
  token: Token,
  rec: string = "none",
) {
  return new Err(message, "runtime-error", phase, token.L, token.C, rec);
}

/** Returns a new environment error. An environment error is raised if an error occured during an environment lookup. */
function envError(
  message: string,
  phase: string,
  token: Token,
  rec: string,
) {
  return new Err(message, "environment-error", phase, token.L, token.C, rec);
}

/** Returns a new resolver error. A resolver error is raised if an error occured during resolution. */
function resolverError(
  message: string,
  phase: string,
  token: Token,
) {
  return new Err(message, "resolver-error", phase, token.L, token.C);
}

/** Returns a new algebra error. An algebra error is raised if an error occured during a symbolic operation. Because symbolic operations are handled by the symbol engine, these errors must be handled separately. */
function algebraError(
  message: string,
  phase: string,
  token: Token,
) {
  return new Err(message, "algebraic-error", phase, token.L, token.C);
}

enum nodekind {
  class_statement,
  block_statement,
  string_binex,
  grouped_expression,
  expression_statement,
  function_declaration,
  branching_statement,
  print_statement,
  return_statement,
  variable_declaration,
  vector_binex,
  loop_statement,
  algebra_string,
  tuple_expression,
  vector_expression,
  matrix_expression,
  assignment_expression,
  native_call,
  algebraic_unary,
  algebraic_infix,
  logical_unary,
  call,
  nil,
  fraction_expression,
  numeric_constant,
  integer,
  float,
  bool,
  string,
  symbol,
  logical_infix,
  let_expression,
  get_expression,
  set_expression,
  super_expression,
  this_expression,
  relation,
  indexing_expression,
  big_number,
  big_rational,
}

interface Mapper<T> {
  integer(node: Integer): T;
  numericConstant(node: NumericConstant): T;
  vectorExpr(node: VectorExpr): T;
  vectorBinaryExpr(node: VectorBinaryExpr): T;
  matrixExpr(node: MatrixExpr): T;
  indexingExpr(node: IndexingExpr): T;
  bigNumber(node: BigNumber): T;
  fractionExpr(node: FractionExpr): T;
  bigRational(node: RationalExpr): T;
  float(node: Float): T;
  bool(node: Bool): T;
  tupleExpr(node: TupleExpr): T;
  getExpr(node: GetExpr): T;
  setExpr(node: SetExpr): T;
  superExpr(node: SuperExpr): T;
  thisExpr(node: ThisExpr): T;
  string(node: StringLiteral): T;
  stringBinaryExpr(node: StringBinaryExpr): T;
  algebraicString(node: AlgebraicString): T;
  nil(node: Nil): T;
  variable(node: Variable): T;
  assignExpr(node: AssignExpr): T;
  algebraicBinaryExpr(node: AlgebraicBinaryExpr): T;
  algebraicUnaryExpr(node: AlgebraicUnaryExpr): T;
  logicalBinaryExpr(node: LogicalBinaryExpr): T;
  logicalUnaryExpr(node: LogicalUnaryExpr): T;
  relationalExpr(node: RelationalExpr): T;
  callExpr(node: CallExpr): T;
  nativeCall(node: NativeCall): T;
  groupExpr(node: GroupExpr): T;
  blockStmt(node: BlockStmt): T;
  exprStmt(node: ExprStmt): T;
  fnStmt(node: FnStmt): T;
  ifStmt(node: IfStmt): T;
  classStmt(node: ClassStmt): T;
  printStmt(node: PrintStmt): T;
  returnStmt(node: ReturnStmt): T;
  letStmt(node: VariableStmt): T;
  whileStmt(node: WhileStmt): T;
  int(node: Int): T;
  real(node: Real): T;
  sym(node: Sym): T;
  constant(node: Constant): T;
  sum(node: Sum): T;
  product(node: Product): T;
  quotient(node: Quotient): T;
  fraction(node: Fraction): T;
  power(node: Power): T;
  difference(node: Difference): T;
  factorial(node: Factorial): T;
  algebraicFn(node: AlgebraicFn): T;
}

interface Visitor<T> {
  integer(node: Integer): T;
  numericConstant(node: NumericConstant): T;
  vectorExpr(node: VectorExpr): T;
  vectorBinaryExpr(node: VectorBinaryExpr): T;
  matrixExpr(node: MatrixExpr): T;
  indexingExpr(node: IndexingExpr): T;
  bigNumber(node: BigNumber): T;
  fractionExpr(node: FractionExpr): T;
  bigRational(node: RationalExpr): T;
  float(node: Float): T;
  bool(node: Bool): T;
  tupleExpr(node: TupleExpr): T;
  getExpr(node: GetExpr): T;
  setExpr(node: SetExpr): T;
  superExpr(node: SuperExpr): T;
  thisExpr(node: ThisExpr): T;
  string(node: StringLiteral): T;
  stringBinaryExpr(node: StringBinaryExpr): T;
  algebraicString(node: AlgebraicString): T;
  nil(node: Nil): T;
  variable(node: Variable): T;
  assignExpr(node: AssignExpr): T;
  algebraicBinaryExpr(node: AlgebraicBinaryExpr): T;
  algebraicUnaryExpr(node: AlgebraicUnaryExpr): T;
  logicalBinaryExpr(node: LogicalBinaryExpr): T;
  logicalUnaryExpr(node: LogicalUnaryExpr): T;
  relationalExpr(node: RelationalExpr): T;
  callExpr(node: CallExpr): T;
  nativeCall(node: NativeCall): T;
  groupExpr(node: GroupExpr): T;
  blockStmt(node: BlockStmt): T;
  exprStmt(node: ExprStmt): T;
  fnStmt(node: FnStmt): T;
  ifStmt(node: IfStmt): T;
  classStmt(node: ClassStmt): T;
  printStmt(node: PrintStmt): T;
  returnStmt(node: ReturnStmt): T;
  letStmt(node: VariableStmt): T;
  whileStmt(node: WhileStmt): T;
}

abstract class TREENODE {
  abstract get kind(): nodekind;
}

abstract class ASTNode extends TREENODE {
  abstract accept<T>(visitor: Visitor<T>): T;
  abstract trust<T>(mapper: Mapper<T>): T;
  abstract toString(): string;
  abstract isStatement(): this is Statement;
  abstract isExpr(): this is Expr;
}

abstract class Statement extends ASTNode {
  isStatement(): this is Statement {
    return true;
  }
  isExpr(): this is Expr {
    return false;
  }
  toString(): string {
    return "";
  }
}

class ClassStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.classStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.classStmt(this);
  }

  get kind(): nodekind {
    return nodekind.class_statement;
  }
  name: Token;
  methods: FnStmt[];
  constructor(name: Token, methods: FnStmt[]) {
    super();
    this.name = name;
    this.methods = methods;
  }
}

/** Returns a new ASTNode of type ClassStmt. */
function classStmt(name: Token, methods: FnStmt[]) {
  return new ClassStmt(name, methods);
}

class BlockStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.blockStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.blockStmt(this);
  }

  get kind(): nodekind {
    return nodekind.block_statement;
  }
  statements: Statement[];
  constructor(statements: Statement[]) {
    super();
    this.statements = statements;
  }
}

/** Returns true if the given ASTNode is a BlockStmt. */
function isBlock(node: ASTNode): node is BlockStmt {
  return node.kind === nodekind.block_statement;
}

/** Returns an ASTNode of type BlockStmt. */
function block(statements: Statement[]) {
  return new BlockStmt(statements);
}

class ExprStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.exprStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.exprStmt(this);
  }

  get kind(): nodekind {
    return nodekind.expression_statement;
  }
  expression: Expr;
  line: number;
  constructor(expression: Expr, line: number) {
    super();
    this.expression = expression;
    this.line = line;
  }
}

function exprStmt(expression: Expr, line: number) {
  return new ExprStmt(expression, line);
}

class FnStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.fnStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.fnStmt(this);
  }

  get kind(): nodekind {
    return nodekind.function_declaration;
  }
  name: Token<tt.symbol>;
  params: Variable[];
  body: Statement[];
  constructor(
    name: Token<tt.symbol>,
    params: Token<tt.symbol>[],
    body: Statement[],
  ) {
    super();
    this.name = name;
    this.params = params.map((p) => variable(p));
    this.body = body;
  }
}

/** Returns a new FnStmt. */
function functionStmt(
  name: Token<tt.symbol>,
  params: Token<tt.symbol>[],
  body: Statement[],
) {
  return new FnStmt(name, params, body);
}

class IfStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.ifStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.ifStmt(this);
  }

  get kind(): nodekind {
    return nodekind.branching_statement;
  }
  keyword: Token;
  condition: Expr;
  then: Statement;
  alt: Statement;
  constructor(
    keyword: Token,
    condition: Expr,
    then: Statement,
    alt: Statement,
  ) {
    super();
    this.keyword = keyword;
    this.condition = condition;
    this.then = then;
    this.alt = alt;
  }
}

/** Returns a new IfStmt. */
function ifStmt(
  keyword: Token,
  condition: Expr,
  then: Statement,
  alt: Statement,
) {
  return new IfStmt(keyword, condition, then, alt);
}

class PrintStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.printStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.printStmt(this);
  }

  get kind(): nodekind {
    return nodekind.print_statement;
  }
  keyword: Token;
  expression: Expr;
  constructor(keyword: Token, expression: Expr) {
    super();
    this.expression = expression;
    this.keyword = keyword;
  }
}

/** Returns a new PrintStmt. */
function printStmt(keyword: Token, expression: Expr) {
  return new PrintStmt(keyword, expression);
}

class ReturnStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.returnStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.returnStmt(this);
  }

  get kind(): nodekind {
    return nodekind.return_statement;
  }
  value: Expr;
  keyword: Token;
  constructor(value: Expr, keyword: Token) {
    super();
    this.value = value;
    this.keyword = keyword;
  }
}

/** Returns a new ReturnStmt. */
function returnStmt(value: Expr, keyword: Token) {
  return new ReturnStmt(value, keyword);
}

class VariableStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.letStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.letStmt(this);
  }

  get kind(): nodekind {
    return nodekind.variable_declaration;
  }
  variable: Variable;
  value: Expr;
  mutable: boolean;
  constructor(name: Token<tt.symbol>, value: Expr, mutable: boolean) {
    super();
    this.variable = variable(name);
    this.value = value;
    this.mutable = mutable;
  }
  get name() {
    return this.variable.name;
  }
}

function varStmt(name: Token<tt.symbol>, value: Expr) {
  return new VariableStmt(name, value, true);
}

function letStmt(name: Token<tt.symbol>, value: Expr) {
  return new VariableStmt(name, value, false);
}

class WhileStmt extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.whileStmt(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.whileStmt(this);
  }

  get kind(): nodekind {
    return nodekind.loop_statement;
  }
  keyword: Token;
  condition: Expr;
  body: Statement;
  constructor(keyword: Token, condition: Expr, body: Statement) {
    super();
    this.keyword = keyword;
    this.condition = condition;
    this.body = body;
  }
}

/**
 * Returns a new {@link WhileStmt|while-statement}.
 */
function whileStmt(keyword: Token, condition: Expr, body: Statement) {
  return new WhileStmt(keyword, condition, body);
}

abstract class Expr extends ASTNode {
  isStatement(): this is Statement {
    return false;
  }
  isExpr(): this is Expr {
    return true;
  }
}

class IndexingExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.indexingExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.indexingExpr(this);
  }

  toString(): string {
    return "";
  }
  get kind(): nodekind {
    return nodekind.indexing_expression;
  }
  op: Token;
  list: Expr;
  index: Expr;
  constructor(list: Expr, index: Expr, op: Token) {
    super();
    this.list = list;
    this.index = index;
    this.op = op;
  }
}

function indexingExpr(list: Expr, index: Expr, op: Token) {
  return new IndexingExpr(list, index, op);
}

class AlgebraicString extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraicString(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.algebraicString(this);
  }

  toString(): string {
    return "";
  }
  get kind(): nodekind {
    return nodekind.algebra_string;
  }
  expression: Expr;
  op: Token;
  constructor(expression: Expr, op: Token) {
    super();
    this.expression = expression;
    this.op = op;
  }
}

function algebraicString(expression: Expr, op: Token) {
  return new AlgebraicString(expression, op);
}

/** Returns true if `node` is an ASTNode of subtype expression. Else, false. */
function isExpr(node: ASTNode): node is Expr {
  return node instanceof Expr;
}

class TupleExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.tupleExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.tupleExpr(this);
  }

  get kind(): nodekind {
    return nodekind.tuple_expression;
  }
  elements: Expr[];
  constructor(elements: Expr[]) {
    super();
    this.elements = elements;
  }
  toString(): string {
    const elements = this.elements.map((e) => e.toString()).join(",");
    return `(${elements})`;
  }
}

class VectorExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.vectorExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.vectorExpr(this);
  }

  get kind(): nodekind {
    return nodekind.vector_expression;
  }
  op: Token;
  elements: Expr[];
  constructor(elements: Expr[], op: Token) {
    super();
    this.elements = elements;
    this.op = op;
  }
  toString(): string {
    const elements = this.elements.map((e) => e.toString()).join(",");
    return `[${elements}]`;
  }
}

/** Returns a new ASTNode of subtype VectorExpr (a vector expression). */
function vectorExpr(elements: Expr[], op: Token) {
  return new VectorExpr(elements, op);
}

/** Returns true if `node` is a VectorExpr (an ASTNode). Else, false. */
function isVectorExpr(node: ASTNode): node is VectorExpr {
  return node.kind === nodekind.vector_expression;
}

class MatrixExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.matrixExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.matrixExpr(this);
  }

  toString(): string {
    const vectors = this.vectors.map((v) => v.toString()).join(",");
    return `[${vectors}]`;
  }
  get kind(): nodekind {
    return nodekind.matrix_expression;
  }
  vectors: VectorExpr[];
  rows: number;
  cols: number;
  constructor(
    vectors: VectorExpr[],
    rows: number,
    columns: number,
  ) {
    super();
    this.vectors = vectors;
    this.rows = rows;
    this.cols = columns;
  }
}

function matrixExpr(vectors: VectorExpr[], rows: number, columns: number) {
  return new MatrixExpr(vectors, rows, columns);
}

/**
 * Returns a new {@link TupleExpr|tuple expression}.
 */
function tupleExpr(elements: Expr[]) {
  return new TupleExpr(elements);
}
class BigNumber extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.bigNumber(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.bigNumber(this);
  }

  toString(): string {
    return `#${this.value}`;
  }
  get kind(): nodekind {
    return nodekind.big_number;
  }
  value: bigint;
  constructor(value: bigint) {
    super();
    this.value = value;
  }
}

/** Returns a new BigNumber. */
function bigNumber(value: bigint) {
  return new BigNumber(value);
}

class RationalExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.bigRational(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.bigRational(this);
  }

  toString(): string {
    return this.value.toString();
  }
  get kind(): nodekind {
    return nodekind.big_rational;
  }
  value: BigRat;
  constructor(N: bigint, D: bigint) {
    super();
    this.value = bigRat(N, D);
  }
}

/** Returns a new RationalExpr */
function bigRational(N: bigint, D: bigint) {
  return new RationalExpr(N, D);
}

class AssignExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.assignExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.assignExpr(this);
  }

  toString(): string {
    return `${this.name} = ${this.value.toString()}`;
  }
  get kind(): nodekind {
    return nodekind.assignment_expression;
  }
  variable: Variable;
  value: Expr;
  constructor(variable: Variable, value: Expr) {
    super();
    this.variable = variable;
    this.value = value;
  }
  get name() {
    return this.variable.name;
  }
}

/** Returns a new ASTNode corresponding to an assignment expression. */
function assign(name: Variable, value: Expr) {
  return new AssignExpr(name, value);
}

// deno-fmt-ignore
type NativeUnary =
| "ceil"
| "floor"
| "sin"
| "cos"
| "cosh"
| "tan"
| "lg"
| "ln"
| "!"
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
| "deriv"
| "simplify"
| "subex"
| "arccosh";

/** A native function that takes more than 1 argument. */
type NativePolyAry = "max" | "min";

type NativeFn = NativeUnary | NativePolyAry;

class NativeCall extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.nativeCall(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.nativeCall(this);
  }

  toString(): string {
    return `${this.name}(${this.args.map((x) => x.toString()).join(",")})`;
  }
  get kind(): nodekind {
    return nodekind.native_call;
  }
  name: Token<tt.native, string, NativeFn>;
  args: Expr[];
  constructor(
    name: Token<tt.native, string, NativeFn>,
    args: Expr[],
  ) {
    super();
    this.name = name;
    this.args = args;
  }
}

/** Returns a new ASTNode corresponding to a native function call. */
function nativeCall(
  name: Token<tt.native, string, NativeFn>,
  args: Expr[],
) {
  return new NativeCall(name, args);
}

// deno-fmt-ignore
type AlgebraicUnaryOperator =
| tt.plus
| tt.minus
| tt.bang;

class AlgebraicUnaryExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraicUnaryExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.algebraicUnaryExpr(this);
  }

  toString(): string {
    return `${this.op.lexeme}${this.arg.toString()}`;
  }
  get kind(): nodekind {
    return nodekind.algebraic_unary;
  }
  op: Token<AlgebraicUnaryOperator>;
  arg: Expr;
  constructor(op: Token<AlgebraicUnaryOperator>, arg: Expr) {
    super();
    this.op = op;
    this.arg = arg;
  }
}

/** Returns a new algebraic unary expression. */
function algebraicUnary(op: Token<AlgebraicUnaryOperator>, arg: Expr) {
  return new AlgebraicUnaryExpr(op, arg);
}

type LogicalUnaryOperator = tt.not;

class LogicalUnaryExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.logicalUnaryExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.logicalUnaryExpr(this);
  }

  toString(): string {
    return `${this.op.lexeme}(${this.arg.toString()})`;
  }
  get kind(): nodekind {
    return nodekind.logical_unary;
  }
  op: Token<LogicalUnaryOperator>;
  arg: Expr;
  constructor(op: Token<LogicalUnaryOperator>, arg: Expr) {
    super();
    this.op = op;
    this.arg = arg;
  }
}

/** Returns a new ASTNode corresponding to a logical unary expression. */
function logicalUnary(op: Token<LogicalUnaryOperator>, arg: Expr) {
  return new LogicalUnaryExpr(op, arg);
}

type StringBinop = tt.amp;

class StringBinaryExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.stringBinaryExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.stringBinaryExpr(this);
  }
  toString(): string {
    const left = this.left.toString();
    const op = this.op.lexeme;
    const right = this.right.toString();
    return `${left} ${op} ${right}`;
  }
  get kind(): nodekind {
    return nodekind.string_binex;
  }
  left: Expr;
  op: Token<StringBinop>;
  right: Expr;
  constructor(left: Expr, op: Token<StringBinop>, right: Expr) {
    super();
    this.left = left;
    this.op = op;
    this.right = right;
  }
}

function stringBinex(left: Expr, op: Token<StringBinop>, right: Expr) {
  return (
    new StringBinaryExpr(left, op, right)
  );
}

type VectorBinaryOP = tt.dot_add | tt.dot_minus | tt.dot_star | tt.dot_caret;

class VectorBinaryExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.vectorBinaryExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.vectorBinaryExpr(this);
  }
  toString(): string {
    const left = this.left.toString();
    const op = this.op.lexeme;
    const right = this.right.toString();
    return `${left} ${op} ${right}`;
  }
  get kind(): nodekind {
    return nodekind.vector_binex;
  }
  left: Expr;
  op: Token<VectorBinaryOP>;
  right: Expr;
  constructor(left: Expr, op: Token<VectorBinaryOP>, right: Expr) {
    super();
    this.left = left;
    this.op = op;
    this.right = right;
  }
}

function vectorBinaryExpr(left: Expr, op: Token<VectorBinaryOP>, right: Expr) {
  return (
    new VectorBinaryExpr(left, op, right)
  );
}

// deno-fmt-ignore
type ArithmeticOperator =
| tt.plus
| tt.star
| tt.caret
| tt.slash
| tt.minus
| tt.rem
| tt.mod
| tt.percent
| tt.div;

class AlgebraicBinaryExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraicBinaryExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.algebraicBinaryExpr(this);
  }

  get kind(): nodekind {
    return nodekind.algebraic_infix;
  }
  left: Expr;
  op: Token<ArithmeticOperator>;
  right: Expr;
  toString(): string {
    const left = this.left.toString();
    const right = this.right.toString();
    switch (this.op.type) {
      case tt.plus:
        return `${left} + ${right}`;
      case tt.star: {
        if (
          isLatinGreek(right) || isMathSymbol(right) && isNumericString(left)
        ) {
          return `${left}${right}`;
        } else {
          return `${left} * ${right}`;
        }
      }
      case tt.caret:
        return `${left}^${right}`;
      case tt.slash:
        return `${left}/${right}`;
      case tt.minus:
        return `${left} - ${right}`;
      case tt.rem:
        return `${left} rem ${right}`;
      case tt.mod:
        return `${left} mod ${right}`;
      case tt.percent:
        return `${left}% ${right}`;
      case tt.div:
        return `${left} div ${right}`;
    }
  }
  constructor(left: Expr, op: Token<ArithmeticOperator>, right: Expr) {
    super();
    this.left = left;
    this.op = op;
    this.right = right;
  }
}

/** Returns a new AlgebraicBinaryExpr. */
function binex(left: Expr, op: Token<ArithmeticOperator>, right: Expr) {
  return new AlgebraicBinaryExpr(left, op, right);
}

class CallExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.callExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.callExpr(this);
  }

  toString(): string {
    const f = this.callee.toString();
    const args = this.args.map((x) => x.toString()).join(",");
    return `${f}(${args})`;
  }
  get kind(): nodekind {
    return nodekind.call;
  }
  callee: Expr;
  paren: Token;
  args: Expr[];
  constructor(callee: Expr, args: Expr[], paren: Token) {
    super();
    this.callee = callee;
    this.args = args;
    this.paren = paren;
  }
}

/** Returns a new CallExpr. */
function call(callee: Expr, args: Expr[], paren: Token) {
  return new CallExpr(callee, args, paren);
}

class GroupExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.groupExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.groupExpr(this);
  }

  toString(): string {
    return `(${this.expression.toString()})`;
  }
  get kind(): nodekind {
    return nodekind.grouped_expression;
  }
  expression: Expr;
  constructor(expression: Expr) {
    super();
    this.expression = expression;
  }
}

function isGroupExpr(node: ASTNode): node is GroupExpr {
  return node.kind === nodekind.grouped_expression;
}

/**
 * Returns a new {@link GroupExpr|group expression}.
 */
function grouped(expression: Expr) {
  return new GroupExpr(expression);
}

class Nil extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.nil(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.nil(this);
  }

  toString(): string {
    return `nil`;
  }
  get kind(): nodekind {
    return nodekind.nil;
  }
  value: null;
  constructor() {
    super();
    this.value = null;
  }
}

function nil() {
  return new Nil();
}

class FractionExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.fractionExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.fractionExpr(this);
  }

  toString(): string {
    return this.value.toString();
  }
  get kind(): nodekind {
    return nodekind.fraction_expression;
  }
  value: Fraction;
  constructor(n: number, d: number) {
    super();
    this.value = ratio(n, d);
  }
}

function rational(n: number, d: number) {
  return new FractionExpr(n, d);
}

type CoreConstant = "NAN" | "Inf" | "pi" | "e";

class NumericConstant extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.numericConstant(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.numericConstant(this);
  }

  toString(): string {
    return `${this.sym}`;
  }
  get kind(): nodekind {
    return nodekind.numeric_constant;
  }
  value: number;
  sym: CoreConstant;
  constructor(value: number, sym: CoreConstant) {
    super();
    this.value = value;
    this.sym = sym;
  }
}

function numericConstant(value: number, sym: CoreConstant) {
  return new NumericConstant(value, sym);
}

class Integer extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.integer(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.integer(this);
  }
  toString(): string {
    return `${this.value}`;
  }
  get kind(): nodekind {
    return nodekind.integer;
  }
  value: number;
  constructor(value: number) {
    super();
    this.value = value;
  }
}

/** Returns a new {@link Integer|integer node}. */
function integer(n: number) {
  return new Integer(n);
}

class Float extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.float(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.float(this);
  }
  toString(): string {
    return `${this.value}`;
  }
  get kind(): nodekind {
    return nodekind.float;
  }
  value: number;
  constructor(value: number) {
    super();
    this.value = value;
  }
}

/** Returns a new {@link Float|float node}. */
function float(n: number) {
  return new Float(n);
}

class Bool extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.bool(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.bool(this);
  }

  toString(): string {
    return `${this.value}`;
  }
  get kind(): nodekind {
    return nodekind.bool;
  }
  value: boolean;
  constructor(value: boolean) {
    super();
    this.value = value;
  }
}

/** Returns a new {@link Bool|boolean node}. */
function bool(value: boolean) {
  return new Bool(value);
}

class StringLiteral extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.string(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.string(this);
  }

  toString(): string {
    return this.value;
  }
  get kind(): nodekind {
    return nodekind.string;
  }
  value: string;
  constructor(value: string) {
    super();
    this.value = value;
  }
}

/** Returns a new StringLiteral. */
function string(value: string) {
  return new StringLiteral(value);
}

class Variable extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.variable(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.variable(this);
  }

  toString(): string {
    return this.name.lexeme;
  }
  get kind(): nodekind {
    return nodekind.symbol;
  }
  name: Token<tt.symbol>;
  constructor(name: Token<tt.symbol>) {
    super();
    this.name = name;
  }
}

function isVariable(node: ASTNode): node is Variable {
  return node.kind === nodekind.symbol;
}

/** Returns a new Variable. */
function variable(name: Token<tt.symbol>) {
  return new Variable(name);
}

// deno-fmt-ignore
type BinaryLogicalOperator =
| tt.and
| tt.nand
| tt.nor
| tt.xnor
| tt.xor
| tt.or;

class LogicalBinaryExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.logicalBinaryExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.logicalBinaryExpr(this);
  }

  toString(): string {
    const left = this.left.toString();
    const right = this.right.toString();
    return `${left} ${this.op.lexeme} ${right}`;
  }
  get kind(): nodekind {
    return nodekind.logical_infix;
  }
  left: Expr;
  op: Token<BinaryLogicalOperator>;
  right: Expr;
  constructor(left: Expr, op: Token<BinaryLogicalOperator>, right: Expr) {
    super();
    this.left = left;
    this.op = op;
    this.right = right;
  }
}

/** Returns a new {@link LogicalBinaryExpr|logical binary expression}. */
function logicalBinex(
  left: Expr,
  op: Token<BinaryLogicalOperator>,
  right: Expr,
) {
  return new LogicalBinaryExpr(left, op, right);
}

type RelationalOperator = tt.lt | tt.gt | tt.deq | tt.neq | tt.geq | tt.leq;

class GetExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.getExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.getExpr(this);
  }

  toString(): string {
    return ``;
  }
  get kind(): nodekind {
    return nodekind.get_expression;
  }
  object: Expr;
  name: Token;
  constructor(object: Expr, name: Token, loc: Location) {
    super();
    this.object = object;
    this.name = name;
  }
}

function isGetExpr(node: ASTNode): node is GetExpr {
  return node.kind === nodekind.get_expression;
}

function getExpr(object: Expr, name: Token, loc: Location) {
  return new GetExpr(object, name, loc);
}

class SetExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.setExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.setExpr(this);
  }

  toString(): string {
    return "";
  }
  get kind(): nodekind {
    return nodekind.set_expression;
  }
  object: Expr;
  name: Token;
  value: Expr;
  constructor(object: Expr, name: Token, value: Expr, loc: Location) {
    super();
    this.object = object;
    this.name = name;
    this.value = value;
  }
}

function setExpr(object: Expr, name: Token, value: Expr, loc: Location) {
  return new SetExpr(object, name, value, loc);
}

/** A node corresponding to a `super` expression. */
class SuperExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.superExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.superExpr(this);
  }

  toString(): string {
    return "";
  }
  get kind(): nodekind {
    return nodekind.super_expression;
  }
  method: Token;
  loc: Location;
  constructor(method: Token, loc: Location) {
    super();
    this.method = method;
    this.loc = loc;
  }
}

/** A `this` expression AST node. */
class ThisExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.thisExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.thisExpr(this);
  }

  toString(): string {
    return ``;
  }
  get kind(): nodekind {
    return nodekind.this_expression;
  }
  keyword: Token;
  constructor(keyword: Token) {
    super();
    this.keyword = keyword;
  }
}

/** Returns a new AST node corresponding to a `this` expression. */
function thisExpr(keyword: Token) {
  return new ThisExpr(keyword);
}

/** An AST node corresponding to a relational expression. */
class RelationalExpr extends Expr {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.relationalExpr(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.relationalExpr(this);
  }

  toString(): string {
    const left = this.left.toString();
    const right = this.right.toString();
    return `${left} ${this.op.lexeme} ${right}`;
  }
  get kind(): nodekind {
    return nodekind.relation;
  }
  left: Expr;
  op: Token<RelationalOperator>;
  right: Expr;
  constructor(left: Expr, op: Token<RelationalOperator>, right: Expr) {
    super();
    this.left = left;
    this.op = op;
    this.right = right;
  }
}

/** Returns a new AST node for a relational expression. */
function relation(left: Expr, op: Token<RelationalOperator>, right: Expr) {
  return new RelationalExpr(left, op, right);
}

/** The `core` enum is an enumeration of constant strings that ensures the core operation symbols are consistent throughought the code base. */
enum core {
  int = "int",
  real = "real",
  complex = "complex",
  fraction = "fraction",
  symbol = "symbol",
  constant = "constant",
  sum = "+",
  difference = "-",
  product = "*",
  quotient = "/",
  power = "^",
  factorial = "!",
  undefined = "Undefined",
  paren = "()",
}

interface ExpressionVisitor<T> {
  int(node: Int): T;
  real(node: Real): T;
  sym(node: Sym): T;
  constant(node: Constant): T;
  sum(node: Sum): T;
  product(node: Product): T;
  quotient(node: Quotient): T;
  fraction(node: Fraction): T;
  power(node: Power): T;
  difference(node: Difference): T;
  factorial(node: Factorial): T;
  algebraicFn(node: AlgebraicFn): T;
}

// =================================================================== CAM nodes

/** A node corresponding to an algebraic expression. */
abstract class AlgebraicExpression {
  abstract accept<T>(visitor: ExpressionVisitor<T>): T;
  abstract trust<T>(mapper: Mapper<T>): T;
  abstract argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression;
  /**
   * Returns true if this expression is syntactically
   * equal to the provided expression. Otherwise,
   * returns false.
   */
  abstract equals(other: AlgebraicExpression): boolean;
  abstract get _args(): AlgebraicExpression[];
  abstract set _args(args: AlgebraicExpression[]);
  abstract isRNE(): boolean;
  /**
   * Returns this expression as a string.
   */
  abstract toString(): string;
  /**
   * Returns a copy of this expression.
   */
  abstract copy(): AlgebraicExpression;
  /**
   * Returns the ith operand of this expression.
   * If this expression is not a compound expression,
   * returns {@link Undefined}.
   */
  abstract operand(i: number): AlgebraicExpression;
  /**
   * Returns the number of operands of this expression.
   * If this expression is not a compound expression,
   * returns 0.
   */
  abstract get _arity(): number;
  abstract isAlgebraic(): boolean;
  /**
   * This expressions operator.
   */
  readonly _op: string;
  constructor(op: string) {
    this._op = op;
  }
  _parenLevel: number = 0;
  parend() {
    this._parenLevel++;
    return this;
  }
}

/** An atom is any expression that cannot be reduced further. This includes integers, reals, and symbols. */
abstract class Atom extends AlgebraicExpression {
  constructor(op: string) {
    super(op);
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    return this;
  }
  set _args(args: AlgebraicExpression[]) {}
  get _args(): AlgebraicExpression[] {
    return [];
  }
  get _arity(): number {
    return 0;
  }
  operand(i: number): UNDEFINED {
    return Undefined(`Asked for the operand ${i}, but atoms have no operands.`);
  }
}

/** Returns true if `u` is an atomic expression. Else, false. */
function isAtom(u: AlgebraicExpression): u is Atom {
  return u instanceof Atom;
}

class Complex {
  _r: number;
  _i: number;
  constructor(r: number, i: number) {
    this._r = r;
    this._i = i;
  }
}

function cpx(real: number, imaginary: number) {
  return new Complex(real, imaginary);
}

/** An atomic value corresponding to an integer. */
class Int extends Atom {
  isRNE(): boolean {
    return true;
  }
  isAlgebraic(): boolean {
    return true;
  }
  toFrac() {
    return frac(this._n, this._d);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.int(this);
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.int(this);
  }
  copy(): Int {
    const out = int(this._n);
    return out;
  }
  equals(other: AlgebraicExpression): boolean {
    if (!isInt(other)) return false;
    return (other._n === this._n);
  }
  toString(): string {
    return `${this._n}`;
  }
  _n: number;
  get _d() {
    return 1;
  }
  constructor(n: number) {
    super(core.int);
    this._n = n;
  }
  get _isNegative() {
    return this._n < 0;
  }
  get _isPositive() {
    return this._n > 0;
  }
  /**
   * Returns true if this integer is 1.
   * False otherwise.
   */
  get _isOne() {
    return this._n === 1;
  }
  /**
   * Returns true if this integer is 0.
   * False otherwise.
   */
  get _isZero() {
    return this._n === 0;
  }
}

/** Returns true if the given expression `u` is an Int. Else, false. */
function isInt(u: AlgebraicExpression): u is Int {
  return !$isNothing(u) && (u._op === core.int);
}

/** Returns a new `Int`. */
export function int(n: number) {
  return (new Int(n));
}

/** An atomic value corresponding to a floating point number. */
class Real extends Atom {
  isRNE(): boolean {
    return false;
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.real(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.real(this);
  }
  isAlgebraic(): boolean {
    return true;
  }
  copy(): Real {
    const out = real(this._n);
    return out;
  }
  equals(other: AlgebraicExpression): boolean {
    if (!isReal(other)) {
      return false;
    } else {
      return (this._n === other._n);
    }
  }
  toString(): string {
    return `${this._n}`;
  }
  _n: number;
  constructor(n: number) {
    super(core.real);
    this._n = n;
  }
}

/** Returns true if the given expression `u` is a Real. Else, false. */
function isReal(u: AlgebraicExpression): u is Real {
  return !$isNothing(u) && (u._op === core.real);
}

/** Returns a new Real. */
export function real(r: number) {
  return (new Real(r));
}

/** An atomic value corresponding to a symbol. */
class Sym<X extends string = string> extends Atom {
  isRNE(): boolean {
    return false;
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.sym(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.sym(this);
  }
  isAlgebraic(): boolean {
    return true;
  }
  copy(): Sym {
    const out = sym(this._s);
    return out;
  }
  equals(other: AlgebraicExpression): boolean {
    if (!isSymbol(other)) {
      return false;
    } else {
      return (this._s === other._s);
    }
  }
  toString(): string {
    return `${this._s}`;
  }
  _s: X;
  constructor(s: X) {
    const type = (s === core.undefined) ? core.undefined : core.symbol;
    super(type);
    this._s = s;
  }
}

/** Returns a new symbol. */
export function sym(s: string) {
  return new Sym(s);
}

/** Type predicate. Claims and returns true if the given expression `u` is a Sym. False otherwise. Note that this will return true if `u` is `Undefined`, since `Undefined` is a symbol by definition. */
function isSymbol(u: AlgebraicExpression): u is Sym {
  return !$isNothing(u) && ((u._op === core.symbol) ||
    (u._op === core.undefined));
}

/** A node corresponding a numeric constant. */
class Constant<
  P extends (number | null) = (number | null),
  X extends string = string,
> extends Atom {
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.constant(this);
  }
  isRNE(): boolean {
    return false;
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.constant(this);
  }
  isAlgebraic(): boolean {
    return true;
  }
  equals(other: AlgebraicExpression): boolean {
    if (!isConstant(other)) {
      return false;
    } else {
      return (other._value === this._value) && (other._c === this._c);
    }
  }
  get _isNegative() {
    if (this._value === null) {
      return false;
    }
    return this._value < 0;
  }
  get _isPositive() {
    if (this._value === null) {
      return false;
    }
    return this._value > 0;
  }
  get _isZero() {
    return false;
  }
  get _isOne() {
    return false;
  }
  toString(): string {
    if (this._value === null) {
      return `Undefined`;
    } else {
      return `${this._value}`;
    }
  }
  copy() {
    const out = new Constant(this._c, this._value);
    return out;
  }
  _note: string = "";
  note(note: string) {
    this._note = note;
    return this;
  }
  _c: X;
  _value: P;
  constructor(c: X, value: P) {
    super(c === core.undefined ? core.undefined : core.constant);
    this._c = c;
    this._value = value;
  }
}

/** Returns true if the given expression is a constant, false otherwise.*/
function isConstant(u: AlgebraicExpression): u is Constant<number> {
  return !$isNothing(u) && (u._op === core.constant);
}

/** Returns a new Undefined. */
export function Undefined(message: string): UNDEFINED {
  return new Constant(core.undefined, null).note(message);
}

type UNDEFINED = Constant<null, core.undefined>;

/** Claims and returns true if the given expression `u` is the global symbol Undefined (an instance of `Sym`, not the JavaScript `undefined`). False otherwise. Note that constant `Undefined` maps to the literal null. */
function isUndefined(
  u: AlgebraicExpression,
): u is Constant<null, core.undefined> {
  return (u instanceof Constant) && (u._value === null);
}

/** Returns a new numeric constant. */
function constant(c: string, value: number) {
  return new Constant(c, value);
}

/** An AlgebraicExpression node corresponding to a compound expression. Compound expressions are AlgebraicExpressions with an operator (via its `_op` property) and operands (via its non-empty array property `_args`).  */
abstract class Compound extends AlgebraicExpression {
  _op: string;
  _args: AlgebraicExpression[];
  constructor(op: string, args: AlgebraicExpression[]) {
    super(op);
    this._op = op;
    this._args = args;
  }
  get _arity(): number {
    return this._args.length;
  }
  toString(): string {
    const op = this._op;
    const args = this._args.map((x) => x.toString()).join(` ${op} `);
    return args;
  }
  equals(other: AlgebraicExpression): boolean {
    if (!(other instanceof Compound)) {
      return false;
    } else if (this._op !== other._op) {
      return false;
    } else if (this._args.length !== other._args.length) {
      return false;
    } else {
      for (let i = 0; i < this._args.length; i++) {
        const a = this._args[i];
        const b = other._args[i];
        if (!a.equals(b)) {
          return false;
        }
      }
      return true;
    }
  }
}

// deno-fmt-ignore
type AlgOP =
| core.sum
| core.difference
| core.product
| core.quotient
| core.power
| core.factorial
| core.fraction;

/**
---
* A node corresponding to an algebraic operation.
* Algebraic operations comprise of:
*
* 1. `+`
* 2. `-`
* 3. `*`
* 4. `^`
* 5. `!`
* 6. `fraction`
---
*/
abstract class AlgebraicOp<OP extends AlgOP = AlgOP> extends Compound {
  _op: OP;
  _args: AlgebraicExpression[];
  abstract copy(): AlgebraicOp;
  isAlgebraic(): boolean {
    return true;
  }
  constructor(op: OP, args: AlgebraicExpression[]) {
    super(op, args);
    this._op = op;
    this._args = args;
  }
  /**
   * Returns the last operand of this operation.
   */
  last(): AlgebraicExpression {
    const out = this._args[this._args.length - 1];
    if (out === undefined) return Undefined("No last argument exists.");
    return out;
  }
  /**
   * The first operand of this operation.
   */
  head(): AlgebraicExpression {
    const out = this._args[0];
    if (out === undefined) return Undefined("No first argument exists.");
    return out;
  }
  /**
   * This operation’s operands, without the
   * first operand.
   */
  tail(): AlgebraicExpression[] {
    const out: AlgebraicExpression[] = [];
    for (let i = 1; i < this._args.length; i++) {
      out.push(this._args[i]);
    }
    return out;
  }
  operand(i: number): AlgebraicExpression {
    const out = this._args[i - 1];
    if (out === undefined) {
      const L = this._args.length;
      return Undefined(
        `Expression comprises ${L} operands, but index ${i} was passed. This is out of bounds.`,
      );
    } else {
      return out;
    }
  }
  /**
   * Returns a copy of this algebraic operation's
   * arguments.
   */
  argsCopy(): AlgebraicExpression[] {
    return this._args.map((x) => x.copy());
  }
}

/** An algebrac expression corresponding to an n-ary sum. */
class Sum extends AlgebraicOp<core.sum> {
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.sum(this);
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const args = this._args.map((x) => callback(x));
    return new Sum(args);
  }
  isRNE(): boolean {
    if (this._args.length === 1 || this._args.length === 2) {
      return this._args.reduce((p, c) => p && c.isRNE(), true);
    } else {
      return false;
    }
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.sum(this);
  }
  copy(): Sum {
    const out = sum(this.argsCopy());
    return out;
  }
  constructor(args: AlgebraicExpression[]) {
    super(core.sum, args);
  }
}

/** Returns a new algebraic sum. */
export function sum(args: AlgebraicExpression[]) {
  return new Sum(args);
}

/** Type predicate. Returns true if `u` is a, false otherwise. */
function isSum(u: AlgebraicExpression): u is Sum {
  return u instanceof Sum;
}

/** An algebraic expression corresponding to an n-ary product. */
class Product extends AlgebraicOp<core.product> {
  isRNE(): boolean {
    if (this._args.length === 2) {
      return this._args.reduce((p, c) => p && c.isRNE(), true);
    } else {
      return false;
    }
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.product(this);
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const args = this._args.map((x) => callback(x));
    return new Product(args);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.product(this);
  }
  copy(): Product {
    const out = product(this.argsCopy());
    return out;
  }
  toString(): string {
    const args = this._args;
    if (args.length === 2) {
      const [a, b] = args;
      if (isInt(a) && a._n === -1) {
        return `-${b.toString()}`;
      }
      if (
        (isConst(a) && isSymbol(b)) ||
        (isConst(a) && isAlgebraicFn(b))
      ) {
        return `${a.toString()}${b.toString()}`;
      }
    }
    const out: string[] = [];
    for (let i = 0; i < args.length; i++) {
      const a = args[i];
      out.push(a.toString());
    }
    const expr = out.join("");
    return expr;
  }
  constructor(args: AlgebraicExpression[]) {
    super(core.product, args);
  }
}

/** Returns a new product expression. */
export function product(args: AlgebraicExpression[]) {
  return new Product(args);
}

/** Returns true if the given algebraic expression is a product, otherwise, false. */
function isProduct(u: AlgebraicExpression): u is Product {
  return !$isNothing(u) && (u._op === core.product);
}

/** A node corresponding to a quotient. */
class Quotient extends AlgebraicOp<core.quotient> {
  isRNE(): boolean {
    return this.dividend.isRNE() && this.divisor.isRNE();
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.quotient(this);
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const left = callback(this.dividend);
    const right = callback(this.divisor);
    return new Quotient(left, right);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.quotient(this);
  }
  _args: [AlgebraicExpression, AlgebraicExpression];
  copy(): Quotient {
    const left = this.dividend.copy();
    const right = this.divisor.copy();
    const out = quotient(left, right);
    return out;
  }
  constructor(dividend: AlgebraicExpression, divisor: AlgebraicExpression) {
    super(core.quotient, [dividend, divisor]);
    this._args = [dividend, divisor];
  }
  /**
   * Returns this quotient as a {@link Product|product}.
   * @example
   * const q = quotient(1,x) // 1/x
   * const p = q.asProduct() // 1 * x^-1
   */
  asProduct(): Product {
    const left = this.divisor.copy();
    const right = power(this.dividend.copy(), int(-1));
    const out = product([left, right]);
    return out;
  }
  /**
   * @property The divisor of this quotient.
   * @example
   * const q = quotient(sym('x'),sym('y')) // q => x/y
   * const d = q.divisor // d => sym('x')
   */
  get divisor() {
    return (this._args[1]);
  }
  /**
   * @property The dividend of this quotient.
   * @example
   * const q = quotient(sym('x'), sym('y')) // q => x/y
   * const d = q.dividend // d => sym('y')
   */
  get dividend() {
    return (this._args[0]);
  }
}

/** Returns a new Quotient. */
export function quotient(
  dividend: AlgebraicExpression,
  divisor: AlgebraicExpression,
) {
  return new Quotient(dividend, divisor);
}

/** Returns true if `u` is a Quotient, false otherwise. */
function isQuotient(u: AlgebraicExpression): u is Quotient {
  return !$isNothing(u) && (u._op === core.quotient);
}

/** A node corresponding to a fraction. Fractions are defined as a pair of integers `[a,b]`, where `b ≠ 0`. */
class Fraction extends AlgebraicOp<core.fraction> {
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.fraction(this);
  }
  isRNE(): boolean {
    return true;
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    return this;
  }
  toFrac() {
    return this;
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.fraction(this);
  }
  asFloat() {
    return (this._n / this._d);
  }
  get _n() {
    return this.numerator._n;
  }
  get _d() {
    return this.denominator._n;
  }
  asInt() {
    return floor(this._n / this._d);
  }
  static from(numberValue: number | Fraction) {
    if (!$isNumber(numberValue)) {
      return numberValue;
    }
    if (Number.isInteger(numberValue)) return Fraction.of(numberValue, 1);
    let eps = 1.0E-15;
    let h, h1, h2, k, k1, k2, a, x;
    x = numberValue;
    a = floor(x);
    h1 = 1;
    k1 = 0;
    h = a;
    k = 1;
    while (x - a > eps * k * k) {
      x = 1 / (x - a);
      a = floor(x);
      h2 = h1;
      h1 = h;
      k2 = k1;
      k1 = k;
      h = h2 + a * h1;
      k = k2 + a * k1;
    }
    return Fraction.of(h, abs(k));
  }
  static of(n: number, d: number) {
    return new Fraction(n, abs(d));
  }

  toString(): string {
    const n = this.numerator._n;
    const d = this.denominator._n;
    return `${n}|${d}`;
  }
  _args: [Int, Int];
  copy(): Fraction {
    const n = this._args[0]._n;
    const d = this._args[1]._n;
    const out = frac(n, d);
    return out;
  }
  lt(other: Fraction) {
    return this.leq(other) && !this.equals(other);
  }
  pos() {
    const n = +this._n;
    const d = this._d;
    return new Fraction(n, d);
  }
  neg() {
    const n = -this._n;
    const d = this._d;
    return new Fraction(n, d);
  }
  gt(other: Fraction) {
    return !this.leq(other);
  }
  geq(other: Fraction) {
    return this.gt(other) || this.equals(other);
  }
  leq(other: Fraction) {
    const F1 = Fraction.simple(
      this._n,
      this._d,
    );
    const F2 = Fraction.simple(
      other._n,
      other._d,
    );
    return F1._n * F2._d <= F2._n * F1._d;
  }
  sub(x: Fraction) {
    return Fraction.simple(
      this._n * x._d - x._n * this._d,
      this._d * x._d,
    );
  }
  add(x: Fraction) {
    return Fraction.simple(
      this._n * x._d + x._n * this._d,
      this._d * x._d,
    );
  }
  div(x: Fraction) {
    return Fraction.simple(
      this._n * x._d,
      this._d * x._n,
    );
  }
  times(x: Fraction) {
    return Fraction.simple(
      x._n * this._n,
      x._d * this._d,
    );
  }
  equals(other: Fraction) {
    const a = Fraction.simple(this._n, this._d);
    const b = Fraction.simple(other._n, other._d);
    return (
      a._n === b._n &&
      a._d === b._d
    );
  }
  static simple(n: number, d: number) {
    const sgn = sign(n) * sign(d);
    const N = abs(n);
    const D = abs(d);
    const f = gcd(n, d);
    return Fraction.of((sgn * N) / f, D / f);
  }
  constructor(numerator: number, denominator: number) {
    const N = int(numerator);
    const D = int(denominator);
    super(core.fraction, [N, D]);
    this._args = [N, D];
  }
  get _isZero() {
    return (this._rawFloat === 0) || (this._n === 0);
  }
  get _isOne() {
    return this._rawFloat === 1;
  }
  get _isPositive() {
    return this._rawFloat > 0;
  }
  get _isNegative() {
    return this._rawFloat < 0;
  }
  get _rawFloat() {
    return this._n / this._d;
  }
  /**
   * @property The numerator of this fraction (an {@link Int|integer}).
   * @example
   * frac(1,2).numerator // 1
   */
  get numerator() {
    return this._args[0];
  }
  /**
   * @property The denominator of this fraction (an {@link Int|integer}).
   * @example
   * frac(1,2).denominator // 2
   */
  get denominator() {
    return this._args[1];
  }
  /**
   * @property This fraction’s numerator and
   *           denominator in pair form.
   * @example
   * const a = frac(1,2);
   * const b = a.pair // [1,2]
   */
  get pair() {
    return tuple(this.numerator._n, this.denominator._n);
  }
}

/** Returns true if `u` is a Fraction, false otherwise.. */
function isFrac(u: AlgebraicExpression): u is Fraction {
  return u instanceof Fraction;
}

/** Returns a new Fraction. */
export function frac(numerator: number, denominator: number) {
  return new Fraction(numerator, denominator);
}

/** An algebraic expression mapping to a power. */
class Power extends AlgebraicOp<core.power> {
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.power(this);
  }
  isRNE(): boolean {
    const base = this.base;
    const exponent = this.exponent;
    return base.isRNE() && exponent.isRNE();
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const base = callback(this.base);
    const exponent = callback(this.exponent);
    return new Power(base, exponent);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.power(this);
  }
  copy(): Power {
    const b = this.base.copy();
    const e = this.base.copy();
    const out = power(b, e);
    return out;
  }
  _args: [AlgebraicExpression, AlgebraicExpression];
  constructor(base: AlgebraicExpression, exponent: AlgebraicExpression) {
    super(core.power, [base, exponent]);
    this._args = [base, exponent];
  }
  toString(): string {
    const base = this.base.toString();
    let exponent = this.exponent.toString();
    if (!isInt(this.exponent) || !isSymbol(this.exponent)) {
      exponent = `(${exponent})`;
    }
    const out = `${base}^${exponent}`;
    return out;
  }
  /**
   * @property The base of this power.
   * @example
   * e^x // base is 'e'
   */
  get base() {
    return this._args[0];
  }
  /**
   * @property The exponent of this power.
   * @example
   * e^x // exponent is 'x'
   */
  get exponent() {
    return this._args[1];
  }
}

/** Returns a new power expression. @param base - The power expression’s base, which may be any algebraic expression. @param exponent - The power expression’s exponent, which may be any algebraic expression. */
export function power(
  base: AlgebraicExpression,
  exponent: AlgebraicExpression,
) {
  return new Power(base, exponent);
}

/** Returns true if `u` is a Power expression. */
function isPower(u: AlgebraicExpression): u is Power {
  return !$isNothing(u) && (u._op === core.power);
}

/** A node corresponding to a difference. */
class Difference extends AlgebraicOp<core.difference> {
  isRNE(): boolean {
    return this.left.isRNE() && this.right.isRNE();
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const left = callback(this.left);
    const right = callback(this.right);
    return new Difference([left, right]);
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.difference(this);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.difference(this);
  }
  _args: [AlgebraicExpression, AlgebraicExpression] | [AlgebraicExpression];
  copy(): Difference {
    const left = this.left.copy();
    const right = this.right.copy();
    const out = difference([left, right]);
    return out;
  }
  constructor(
    args: [AlgebraicExpression] | [AlgebraicExpression, AlgebraicExpression],
  ) {
    super(core.difference, args);
    this._args = args;
  }
  /**
   * Returns the left minuend of this difference.
   * @example
   * a - b // left is 'a'
   */
  get left() {
    if (this._args.length === 1) {
      return product([int(-1), this._args[0]]);
    } else {
      return this._args[0];
    }
  }
  /**
   * Returns the right minuend of this difference.
   * @example
   * a - b // right is 'b'
   */
  get right() {
    const out = this._args[1];
    if (out === undefined) {
      return product([int(-1), this._args[0]]);
    } else {
      return out;
    }
  }
  /**
   * Returns this difference as a sum. I.e., where L is the lefthand minuend
   * and R is the righthand minuend:
   *
   * ~~~ts
   * L - R becomes L + (-1 * R)
   * ~~~
   */
  toSum() {
    const left = this.left;
    const right = product([int(-1), this.right]);
    return sum([left, right]);
  }
}

/** Returns an expression corresponding to the difference. */
export function difference(
  args: [AlgebraicExpression] | [AlgebraicExpression, AlgebraicExpression],
) {
  return new Difference(args);
}

/** Returns true if `u` is difference, false otherwise. */
function isDifference(u: AlgebraicExpression): u is Difference {
  return !$isNothing(u) && (u._op === core.difference);
}

/** Returns the provided algebraic expression `u`, negated. */
export function negate(u: AlgebraicExpression) {
  return product([int(-1), u]);
}

/** A node corresponding to the mathematical factorial. The factorial is always a unary operation. */
class Factorial extends AlgebraicOp<core.factorial> {
  isRNE(): boolean {
    return false;
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.factorial(this);
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const out = callback(this.arg);
    return new Factorial(out);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.factorial(this);
  }
  _args: [AlgebraicExpression];
  copy(): Factorial {
    const arg = this.arg.copy();
    const out = factorial(arg);
    return out;
  }
  constructor(arg: AlgebraicExpression) {
    super(core.factorial, [arg]);
    this._args = [arg];
  }
  /**
   * Returns the argument of this factorial.
   * @example
   * x! // arg is 'x'
   */
  get arg() {
    return this._args[0];
  }
  toString(): string {
    return `${this.arg.toString()}!`;
  }
}

/** Returns a new factorial expression. */
export function factorial(of: AlgebraicExpression) {
  return new Factorial(of);
}

function isFactorial(u: AlgebraicExpression): u is Factorial {
  return u instanceof Factorial;
}

/** A node corresponding to any function that takes arguments of type algebraic expression. */
class AlgebraicFn extends Compound {
  isRNE(): boolean {
    return false;
  }
  accept<T>(visitor: ExpressionVisitor<T>): T {
    return visitor.algebraicFn(this);
  }
  argmap(
    callback: (x: AlgebraicExpression) => AlgebraicExpression,
  ): AlgebraicExpression {
    const out = this._args.map((x) => callback(x));
    return new AlgebraicFn(this._op, out);
  }
  trust<T>(mapper: Mapper<T>): T {
    return mapper.algebraicFn(this);
  }

  isAlgebraic(): boolean {
    return true;
  }
  _args: AlgebraicExpression[];
  copy(): AlgebraicFn {
    const out = fn(this._op, this._args.map((c) => c.copy()));
    return out;
  }
  operand(i: number): AlgebraicExpression {
    const out = this._args[i - 1];
    if (out === undefined) {
      const L = this._args.length;
      return Undefined(
        `Expression comprises ${L} operands, but index ${i} was passed. This is out of bounds.`,
      );
    } else {
      return out;
    }
  }
  constructor(op: string, args: AlgebraicExpression[]) {
    super(op, args);
    this._op = op;
    this._args = args;
  }
  toString(): string {
    const name = this._op;
    const args = this._args.map((x) => x.toString()).join(",");
    return `${name}(${args})`;
  }
}

/** Returns a new set. */
function setof<T>(...args: T[]) {
  return new Set(args);
}

/** Returns a new algebraic function. */
export function fn(name: string, args: AlgebraicExpression[]) {
  return new AlgebraicFn(name, args);
}

/** Type predicate. Returns true if the given expression `u` is an algebraic function, false otherwise. */
function isAlgebraicFn(u: AlgebraicExpression): u is AlgebraicFn {
  return u instanceof AlgebraicFn;
}

/** Returns true if the given expression is a constant. */
function isConst(
  u: AlgebraicExpression,
): u is Int | Fraction | Constant<number> {
  return (
    !$isNothing(u) && ((u._op === core.int) ||
      (u._op === core.fraction) ||
      (u._op === core.constant)) &&
    (
      !isUndefined(u)
    )
  );
}

// ----------------------------------------------------------------- Twine Nodes
/**
---
* The following nodes correspond to nodes used by Twine.
* These are treated as separate nodes from the algebraic expressions,
* since their semantics and error-handling needs substantially differ
* from the algebraic expression nodes.
---
*/

/** A token type output by Twine’s lexers. */
enum tt {
  // Utility tokens - - - - - - - - - - - - - - - - - - - - - - - -

  /** A utility token indicating the end of input. */
  END,

  /** A utility token indicating an error. */
  ERROR,

  /** A utility token mapped to the empty token. */
  EMPTY,

  // Paired Delimiters - - - - - - - - - - - - - - - - - - - - - - -

  /** Lexeme: `"("` */
  lparen,

  /** Lexeme: `")"` */
  rparen,

  /** Lexeme: `"{"` */
  lbrace,

  /** Lexeme: `"}"` */
  rbrace,

  /** Lexeme: `"["` */
  lbracket,

  /** Lexeme: `"]"` */
  rbracket,

  // Strict Delimiters - - - - - - - - - - - - - - - - - - - - - - -

  /** Lexeme: `";"` */
  semicolon,

  /** Lexeme: `":"` */
  colon,

  /** Lexeme: `"."` */
  dot,

  /** Lexeme: `","` */
  comma,

  // Operator delimiters - - - - - - - - - - - - - - - - - - - - - -

  /** Lexeme: `"+"` */
  plus,

  /** Lexeme: `"-"` */
  minus,

  /** Lexeme: `"*"` */
  star,

  /** Lexeme: `"/"` */
  slash,

  /** Lexeme: `"^"` */
  caret,

  /** Lexeme: `"%"` */
  percent,

  /** Lexeme `"!"`. */
  bang,

  /** Lexeme: `"&"` */
  amp,

  /** Lexeme: `"~"` */
  tilde,

  /** Lexeme: `"|"` */
  vbar,

  /** Lexeme: `"="` */
  eq,

  /** Lexeme: `"<"` */
  lt,

  /** Lexeme: `">"` */
  gt,

  // Operative Dipthongs - - - - - - - - - - - - - - - - - - - - - - -

  /** Lexeme: `"!="` */
  neq,

  /** Lexeme: `"<="` */
  leq,

  /** Lexeme: `">="` */
  geq,

  /** Lexeme: `"=="` */
  deq,

  /** Lexeme: `"++"` */
  plus_plus,

  /** Lexeme: `"--"` */
  minus_minus,

  // Literals - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  symbol,
  string,
  bool,
  int,
  float,
  bignumber,
  bigfraction,
  scientific,
  fraction,
  nan,
  inf,
  nil,
  numeric_constant,
  algebra_string,

  // Vector operators
  /** Lexeme: `.+` - Vector addition */
  dot_add,
  /** Lexeme: `.*` - Vector multiplication */
  dot_star,
  /** Lexeme: `.-` - Vector difference */
  dot_minus,
  /** Lexeme: `.^` - Pairwise division */
  dot_caret,
  /** Lexeme: `@` - Dot product */

  // Matrix Operators
  /** Lexeme: `#+` - Matrix addition */
  /** Lexeme: `#-` - Matrix subtraction */
  /** Lexeme: `#*` - Matrix multiplication */

  // Native Calls - - - - - - - - - - - - - - - - - - - - - - - - - -
  native,

  // Keywords - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  and,
  or,
  not,
  nand,
  xor,
  xnor,
  nor, // Logical operators
  if,
  else, // predicators
  fn, // function declarative
  let, // variable declarative
  var,
  return, // return particle
  while, // while-loop particle
  for, // for-loop particle
  class, // struct declarative
  print, // print statement
  super,
  this,

  /* operator `"rem"` */
  rem,
  /* operator `"mod"` */
  mod,
  /* operator `"div"` */
  div,
}

/** The union of token types corresponding to numeric literals. */
// deno-fmt-ignore
type NumberTokenType =
| tt.int
| tt.float
| tt.scientific
| tt.bignumber
| tt.bigfraction
| tt.fraction;

/** The union of runtime values.  */
// deno-fmt-ignore
type LIT =
| number
| boolean
| string
| bigint
| null
| [number, number]
| [bigint, bigint]
| Err;

/** An object corresponding to the location of a line of code. */
type Location = { line: number; column: number };

/** Returns a new Location. */
function location(line: number, column: number): Location {
  return ({ line, column });
}

class Token<T extends tt = tt, L extends LIT = LIT, S extends string = string> {
  /** This token’s {@link tt|type}. */
  type: T;
  /** This token’s lexeme. */
  lexeme: S;
  /** This token’s literal value. */
  literal: L = null as any;
  /** The line where this token was recognized. */
  L: number;
  /** The column where this token was recognized. */
  C: number;
  loc(): Location {
    return location(this.L, this.C);
  }
  static empty: Token<tt, any> = new Token(tt.EMPTY, "", -1, -1);
  static END: Token<tt, any> = new Token(tt.END, "END", -1, -1);
  isEmpty() {
    return this.type === tt.EMPTY;
  }
  static of<X extends tt>(type: X, lexeme: string) {
    return new Token(type, lexeme, 0, 0);
  }
  guard<X extends tt, L extends LIT>(
    type: X,
    is: (literal: LIT) => literal is L,
  ): this is Token<X, L> {
    return (
      // @ts-ignore
      (this.type === type) && (is(this.literal))
    );
  }
  isArithmeticOperator(): this is Token<ArithmeticOperator> {
    return (
      this.type === tt.plus ||
      this.type === tt.star ||
      this.type === tt.caret ||
      this.type === tt.minus ||
      this.type === tt.rem ||
      this.type === tt.mod ||
      this.type === tt.percent ||
      this.type === tt.div
    );
  }
  among(types: tt[]) {
    for (let i = 0; i < types.length; i++) {
      if (this.type === types[i]) {
        return true;
      }
    }
    return false;
  }
  isAlgebraString(): this is Token<tt.algebra_string, string> {
    return (
      this.type === tt.algebra_string
    );
  }
  /**
   * Returns true if this token maps to the error token.
   */
  isError(): this is Token<tt.ERROR, Err> {
    return this.type === tt.ERROR;
  }
  isVariable(): this is Token<tt.symbol> {
    return (this.type === tt.symbol);
  }

  toString() {
    return this.lexeme;
  }

  is<x extends tt>(type: x): this is Token<x> {
    return (this.type === type as any);
  }

  constructor(
    type: T,
    lexeme: S,
    line: number,
    column: number,
    literal: L = null as any,
  ) {
    this.type = type;
    this.lexeme = lexeme;
    this.L = line;
    this.C = column;
    this.literal = literal;
  }
  /**
   * Returns true if this token is a
   * right-delimiter token. That is,
   * either a `)`, `]`, or `}`.
   */
  isRPD() {
    return (
      this.type === tt.rparen ||
      this.type === tt.rbrace ||
      this.type === tt.rbracket
    );
  }
  /**
   * Sets this token’s lexeme.
   */
  lex(lexeme: string) {
    return new Token(
      this.type,
      lexeme,
      this.L,
      this.C,
      this.literal,
    );
  }
  /**
   * Sets this token’s type.
   */
  entype<X extends tt>(type: X) {
    return new Token(
      type,
      this.lexeme,
      this.L,
      this.C,
      this.literal,
    );
  }

  /**
   * Sets this token’s column.
   */
  encolumn(columnNumber: number) {
    return new Token(
      this.type,
      this.lexeme,
      this.L,
      columnNumber,
      this.literal,
    );
  }
  enline(lineNumber: number) {
    return new Token(
      this.type,
      this.lexeme,
      lineNumber,
      this.C,
      this.literal,
    );
  }
  lit<X extends LIT>(value: X) {
    return new Token(
      this.type,
      this.lexeme,
      this.L,
      this.C,
      value,
    );
  }
  /**
   * Returns true if this token maps
   * to `true` or `false`.
   */
  isBoolean(): this is Token<T, boolean> {
    return (
      $isBoolean(this.literal)
    );
  }
  /**
   * Returns true if this token maps
   * to an integer or float token.
   */
  isNumber(): this is Token<T, number> {
    return (
      $isNumber(this.literal)
    );
  }
  isNumLike() {
    return this.among([
      tt.int,
      tt.float,
      tt.bignumber,
      tt.bigfraction,
      tt.scientific,
      tt.fraction,
      tt.nan,
      tt.inf,
      tt.numeric_constant,
    ]);
  }
  /**
   * Returns true if this token maps
   * to a big integer token.
   */
  isBigNumber(): this is Token<T, bigint> {
    return (
      $isBigInt(this.literal)
    );
  }
  /**
   * Returns true if this token maps
   * to a big rational token.
   */
  isBigRational(): this is Token<tt.bigfraction, [bigint, bigint]> {
    return (
      this.type === tt.bigfraction
    );
  }
  /**
   * Returns true if this token maps
   * to a scientific number token.
   */
  isScientific(): this is Token<tt.scientific, [number, number]> {
    return (this.type === tt.scientific);
  }
  /**
   * Returns true if this token maps
   * to a fraction token.
   */
  isFraction(): this is Token<tt.fraction, [number, number]> {
    return (this.type === tt.fraction);
  }
  /**
   * Returns true if this token maps
   * to a big fraction token.
   */
  isBigFraction(): this is Token<tt.bigfraction, [bigint, bigint]> {
    return (this.type === tt.bigfraction);
  }
  /**
   * Returns a copy of this token.
   */
  copy() {
    const type = this.type;
    const lexeme = this.lexeme;
    const line = this.L;
    const literal = this.literal;
    const column = this.C;
    return new Token(type, lexeme, line, column, literal);
  }
}

/**
---
* Returns a new token.
* @parameter type - The token’s {@link tt|type}.
* @parameter lexeme - The token’s lexeme.
* @parameter line - The line where this token was recognized.
* @parameter column - The column where this token was recognized.
---
 */
function token<X extends tt>(
  type: X,
  lexeme: string,
  line: number,
  column: number,
) {
  return new Token(type, lexeme, line, column);
}

/** Returns true if the string `char` is a Latin or Greek character. */
function isLatinGreek(char: string) {
  return /^[a-zA-Z_$\u00C0-\u02AF\u0370-\u03FF\u2100-\u214F]$/.test(char);
}

/** Returns true if the given string `char` is within the unicode range `∀-⋿`. Else, returns false. */
function isMathSymbol(char: string) {
  return /^[∀-⋿]/u.test(char);
}

/** Returns true if the given string `char` is a Latin/Greek character or a math symbol. Else, returns false. */
function isValidName(char: string) {
  return (isLatinGreek(char) || isMathSymbol(char));
}

/** Returns true if the given string `char` is a digit. Else, returns false. */
function isDigit(char: string) {
  return "0" <= char && char <= "9";
}

/** Returns true if the given character is a greek letter name. */
function isGreekLetterName(c: string) {
  return /^(alpha|beta|gamma|delta|epsilon|zeta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|upsilon|phi|chi|psi|omega)/
    .test(c.toLowerCase());
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
  postfix,
  call,
}

/** @internal A Pratt parsing function. */
type Parslet<T> = (current: Token, lastNode: T) => Either<Err, T>;

/** @internal An entry within parser’s BP table. The first element is a prefix parslet, the second element is an infix parslet, and the last element is the binding power of the operator. */
type ParsletEntry<T> = [Parslet<T>, Parslet<T>, bp];

/** @internal A record of parslet entries, where each key is a token type (`tt`). */
type BPTable<T> = Record<tt, ParsletEntry<T>>;

// ============================================================= Runtime Objects
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
    isInitializer: boolean,
  ) {
    this.declaration = declaration;
    this.closure = closure;
    this.isInitializer = isInitializer;
  }
  arity() {
    return this.declaration.params.length;
  }
  toString() {
    return `fn ${this.declaration.name}(...) {...}`;
  }
  bind(instance: Obj) {
    const environment = runtimeEnv(this.closure);
    environment.define("this", instance, true);
    return new Fn(this.declaration, environment, this.isInitializer);
  }
  call(interpreter: Compiler, args: Primitive[]) {
    const environment = runtimeEnv(this.closure);
    for (let i = 0; i < this.declaration.params.length; i++) {
      environment.define(
        this.declaration.params[i].name.lexeme,
        args[i],
        false,
      );
    }
    try {
      const out = interpreter.executeBlock(this.declaration.body, environment);
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
  isInitializer: boolean,
) {
  return new Fn(declaration, closure, isInitializer);
}

/** Returns true if `x` is an `Fn` oobject, false otherwise. */
function $isFn(x: any): x is Fn {
  return x instanceof Fn;
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
    if (this.fields.has(name.lexeme)) {
      return this.fields.get(name.lexeme)!;
    }
    const method = this.klass.findMethod(name.lexeme);
    if (method !== null) {
      return method.bind(this);
    }
    throw runtimeError(
      `User accessed a non-existent property “${name}”.`,
      `evaluating a property of “${this.klass.name}”`,
      name,
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
  private scopes: (Map<string, boolean>)[] = [];
  private scopesIsEmpty() {
    return this.scopes.length === 0;
  }
  private currentFunction: functionType = functionType.none;
  private currentClass: classType = classType.none;
  private beginScope() {
    this.scopes.push(new Map());
  }
  private endScope() {
    this.scopes.pop();
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
    return this.scopes[this.scopes.length - 1];
  }
  indexingExpr(node: IndexingExpr) {
    this.resolve(node.list);
    this.resolve(node.index);
    return;
  }
  algebraicString(node: AlgebraicString): void {
    return;
  }
  private declare(name: Token) {
    if (this.scopes.length === 0) return;
    const scope = this.peek();
    if (scope.has(name.lexeme)) {
      throw resolverError(
        `Encountered a name collision. The variable “${name.lexeme}” has already been declared in the current scope.`,
        `resolving a declaration`,
        name,
      );
    }
    scope.set(name.lexeme, false);
  }

  private define(name: string) {
    if (this.scopes.length === 0) return;
    const peek = this.peek();
    peek.set(name, true);
  }

  private resolveFn(node: FnStmt, type: functionType) {
    const enclosingFunction = this.currentFunction;
    this.currentFunction = type;
    this.beginScope();
    for (let i = 0; i < node.params.length; i++) {
      this.declare(node.params[i].name);
      this.define(node.params[i].name.lexeme);
    }
    this.resolveEach(node.body);
    this.endScope();
    this.currentFunction = enclosingFunction;
  }

  resolveLocal(node: Expr, name: string) {
    for (let i = this.scopes.length - 1; i >= 0; i--) {
      const scope = this.scopes[i];
      if (scope !== undefined && scope.has(name)) {
        this.client.resolve(node, this.scopes.length - 1 - i);
        return;
      }
    }
  }
  client: T;
  constructor(client: T) {
    this.client = client;
  }
  thisExpr(node: ThisExpr): void {
    if (this.currentClass === classType.none) {
      throw resolverError(
        `Encountered the keyword “this” outside of a class definition. This syntax has no semantic, since “this” points to nothing.`,
        `resolving “this”`,
        node.keyword,
      );
    }
    this.resolveLocal(node, "this");
    return;
  }
  superExpr(node: SuperExpr): void {
    throw new Error(`superExpr not implemented`);
  }
  setExpr(node: SetExpr): void {
    this.resolve(node.value);
    this.resolve(node.object);
    return;
  }
  getExpr(node: GetExpr): void {
    this.resolve(node.object);
    return;
  }
  integer(node: Integer): void {
    return;
  }
  numericConstant(node: NumericConstant): void {
    return;
  }
  vectorExpr(node: VectorExpr): void {
    this.resolveEach(node.elements);
    return;
  }
  matrixExpr(node: MatrixExpr): void {
    this.resolveEach(node.vectors);
    return;
  }
  bigNumber(node: BigNumber): void {
    return;
  }
  fractionExpr(node: FractionExpr): void {
    return;
  }
  bigRational(node: RationalExpr): void {
    return;
  }
  float(node: Float): void {
    return;
  }
  bool(node: Bool): void {
    return;
  }
  tupleExpr(node: TupleExpr): void {
    this.resolveEach(node.elements);
    return;
  }
  string(node: StringLiteral): void {
    return;
  }
  nil(node: Nil): void {
    return;
  }
  variable(node: Variable): void {
    const name = node.name;
    if (!this.scopesIsEmpty() && this.peek().get(name.lexeme) === false) {
      throw resolverError(
        `The user is attempting to read the variable “${node.name}” from its own initializer. This syntax has no semantic.`,
        `resolving the variable ${node.name}`,
        node.name,
      );
    }
    this.resolveLocal(node, node.name.lexeme);
    return;
  }
  assignExpr(node: AssignExpr): void {
    this.resolve(node.value);
    this.resolveLocal(node, node.name.lexeme);
    return;
  }
  stringBinaryExpr(node: StringBinaryExpr): void {
    this.resolve(node.left);
    this.resolve(node.right);
    return;
  }
  vectorBinaryExpr(node: VectorBinaryExpr): void {
    this.resolve(node.left);
    this.resolve(node.right);
    return;
  }
  algebraicBinaryExpr(node: AlgebraicBinaryExpr): void {
    this.resolve(node.left);
    this.resolve(node.right);
    return;
  }
  algebraicUnaryExpr(node: AlgebraicUnaryExpr): void {
    this.resolve(node.arg);
    return;
  }
  logicalBinaryExpr(node: LogicalBinaryExpr): void {
    this.resolve(node.left);
    this.resolve(node.right);
    return;
  }
  logicalUnaryExpr(node: LogicalUnaryExpr): void {
    this.resolve(node.arg);
    return;
  }
  relationalExpr(node: RelationalExpr): void {
    this.resolve(node.left);
    this.resolve(node.right);
    return;
  }
  callExpr(node: CallExpr): void {
    this.resolve(node.callee);
    this.resolveEach(node.args);
    return;
  }
  nativeCall(node: NativeCall): void {
    this.resolveEach(node.args);
    return;
  }
  groupExpr(node: GroupExpr): void {
    this.resolve(node.expression);
    return;
  }
  blockStmt(node: BlockStmt): void {
    this.beginScope();
    this.resolveEach(node.statements);
    this.endScope();
    return;
  }
  exprStmt(node: ExprStmt): void {
    this.resolve(node.expression);
    return;
  }
  classStmt(node: ClassStmt): void {
    const enclosingClass = this.currentClass;
    this.currentClass = classType.class;
    this.declare(node.name);
    this.define(node.name.lexeme);
    this.beginScope();
    const peek = this.peek();
    peek.set("this", true);
    const methods = node.methods;
    for (let i = 0; i < methods.length; i++) {
      const method = methods[i];
      let declaration = functionType.method;
      if (method.name.lexeme === "init") {
        declaration = functionType.initializer;
      }
      this.resolveFn(method, declaration);
    }
    this.endScope();
    this.currentClass = enclosingClass;
    return;
  }
  fnStmt(node: FnStmt): void {
    this.declare(node.name);
    this.define(node.name.lexeme);
    this.resolveFn(node, functionType.function);
    return;
  }
  ifStmt(node: IfStmt): void {
    this.resolve(node.condition);
    this.resolve(node.then);
    this.resolve(node.alt);
    return;
  }
  printStmt(node: PrintStmt): void {
    this.resolve(node.expression);
    return;
  }
  returnStmt(node: ReturnStmt): void {
    if (this.currentFunction === functionType.none) {
      throw resolverError(
        `Encountered the “return” keyword at the top-level. This syntax has no semantic.`,
        `resolving a return-statement`,
        node.keyword,
      );
    }
    if (this.currentFunction === functionType.initializer) {
      throw resolverError(
        `Encounterd the “return” keyword within an initializer.`,
        `resolving a return-statement`,
        node.keyword,
      );
    }
    this.resolve(node.value);
    return;
  }
  letStmt(node: VariableStmt): void {
    this.declare(node.name);
    this.resolve(node.value);
    this.define(node.name.lexeme);
    return;
  }
  whileStmt(node: WhileStmt): void {
    this.resolve(node.condition);
    this.resolve(node.body);
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
}

function resolvable(client: Resolvable) {
  return new Resolver(client);
}

class Environment<T> {
  values: Map<string, T>;
  enclosing: Environment<T> | null;
  mutables: Set<string>;
  constructor(enclosing: Environment<T> | null) {
    this.values = new Map<string, T>();
    this.enclosing = enclosing;
    this.mutables = new Set();
  }
  ancestor(distance: number) {
    // @ts-ignore
    let env = this;
    for (let i = 0; i < distance; i++) {
      // @ts-ignore
      env = this.enclosing;
    }
    return env;
  }
  assignAt(distance: number, name: string, value: T): T {
    this.ancestor(distance).values.set(name, value);
    return value;
  }
  getAt(distance: number, name: string): T {
    return this.ancestor(distance).values.get(name)!;
  }
  /**
   * Assigns a new value to the given name.
   * If no such name exists, throws a new resolver error.
   * The name provided must be a {@link Token|token} to
   * ensure line and column numbers are reported.
   */
  assign(name: Token, value: T): T {
    if (this.values.has(name.lexeme)) {
      if (this.mutables.has(name.lexeme)) {
        this.values.set(name.lexeme, value);
        return value;
      }
      throw envError(
        `The variable “${name.lexeme}” is not a mutable variable.`,
        `assigning a new value to a variable`,
        name,
        `Declare “${name.lexeme}” with “var”.`,
      );
    }
    if (this.enclosing !== null) {
      return this.enclosing.assign(name, value);
    }
    throw envError(
      `The variable “${name.lexeme}” is not defined and only defined variables may be assigned.`,
      `assigning a new value to a variable`,
      name,
      `define ${name.lexeme} with “let” or “var” before line ${name.L}.`,
    );
  }
  /**
   * Binds a new value to the given name.
   */
  define(name: string, value: T, mutable: boolean): T {
    this.values.set(name, value);
    if (mutable) {
      this.mutables.add(name);
    }
    return value;
  }
  /**
   * Retrieves the value bound to the given name.
   * If no such name exists, throws a new resolver error.
   * The name provided must be a {@link Token|token} to ensure
   * line and column numbers are reported.
   */
  get(name: Token): T {
    if (this.values.has(name.lexeme)) {
      return this.values.get(name.lexeme)!;
    }
    if (this.enclosing !== null) {
      return this.enclosing.get(name);
    }
    throw envError(
      `The variable “${name.lexeme}” is not defined, and only defined variables may be read.`,
      `reading a variable`,
      name,
      `Declare “${name.lexeme}” with “let” or “var” before usage.`,
    );
  }
}

function runtimeEnv(enclosing: Environment<Primitive> | null) {
  return new Environment<Primitive>(enclosing);
}

// deno-fmt-ignore
type Primitive =
| number
| boolean
| null
| string
| bigint
| Fraction
| BigRat
| Vector
| Matrix
| Fn
| Class
| Obj
| Primitive[]
| AlgebraicExpression;

function stringify(value: Primitive): string {
  if (value === null) {
    return `nil`;
  } else if (value === undefined) {
    return `undefined`;
  } else if ($isArray(value)) {
    const elems = value.map((v) => stringify(v)).join(", ");
    return `(${elems})`;
  } else if ($isNumber(value)) {
    if ($isNaN(value)) {
      return `NaN`;
    } else if ($isInfinity(value)) {
      return `Inf`;
    } else if ($isInt(value)) {
      return `${value}`;
    } else {
      return `${value}`;
    }
  } else {
    return value.toString();
  }
}

function truthy(x: Primitive) {
  if ($isBoolean(x)) return x;
  if ($isArray(x) || $isString(x)) return x.length !== 0;
  if (x === null || x === undefined) return false;
  if (x instanceof BigRat || x instanceof Fraction) return !x._isZero;
  if (x instanceof Vector) return x.length !== 0;
  if (x instanceof Matrix) return x._R !== 0 && x._C !== 0;
  return (
    x !== 0 &&
    !Number.isNaN(x) &&
    true
  );
}

class Simplifier implements Visitor<AlgebraicExpression> {
  op: Token;
  place(op: Token) {
    this.op = op;
    return this;
  }
  constructor(op: Token) {
    this.op = op;
  }
  private unsupportedError(nodetype: string) {
    return this.error(`${nodetype} cannot be used in algebraic strings.`);
  }
  private error(message: string) {
    return algebraError(
      message,
      "transforming to an algebraic expression",
      this.op,
    );
  }
  vectorBinaryExpr(node: VectorBinaryExpr): AlgebraicExpression {
    throw this.unsupportedError("Vector binary expressions");
  }
  reduce(node: ASTNode) {
    return node.accept(this);
  }
  reduceEach(nodes: ASTNode[]) {
    const args = [];
    for (let i = 0; i < nodes.length; i++) {
      args.push(this.reduce(nodes[i]));
    }
    return args;
  }
  integer(node: Integer): AlgebraicExpression {
    return int(node.value);
  }
  numericConstant(node: NumericConstant): AlgebraicExpression {
    if (node.sym === "NAN") {
      throw this.unsupportedError("NAN");
    } else {
      return constant(node.sym, node.value);
    }
  }
  vectorExpr(node: VectorExpr): AlgebraicExpression {
    throw this.unsupportedError(`Vectors`);
  }
  matrixExpr(node: MatrixExpr): AlgebraicExpression {
    throw this.unsupportedError(`Matrices`);
  }
  indexingExpr(node: IndexingExpr): AlgebraicExpression {
    throw this.unsupportedError(`Indexing expressions`);
  }
  bigNumber(node: BigNumber): AlgebraicExpression {
    throw this.unsupportedError(`Big numbers`);
  }
  fractionExpr(node: FractionExpr): AlgebraicExpression {
    return frac(node.value._n, node.value._d);
  }
  bigRational(node: RationalExpr): AlgebraicExpression {
    throw this.unsupportedError(`Big rationals`);
  }
  float(node: Float): AlgebraicExpression {
    return real(node.value);
  }
  bool(node: Bool): AlgebraicExpression {
    throw this.unsupportedError(`Booleans`);
  }
  tupleExpr(node: TupleExpr): AlgebraicExpression {
    throw this.unsupportedError(`Tuples`);
  }
  getExpr(node: GetExpr): AlgebraicExpression {
    throw this.unsupportedError(`Get expressions`);
  }
  setExpr(node: SetExpr): AlgebraicExpression {
    throw this.unsupportedError(`Set expressions`);
  }
  superExpr(node: SuperExpr): AlgebraicExpression {
    throw this.unsupportedError(`Super expressions`);
  }
  thisExpr(node: ThisExpr): AlgebraicExpression {
    throw this.unsupportedError(`This expressions`);
  }
  string(node: StringLiteral): AlgebraicExpression {
    throw this.unsupportedError(`Strings expressions`);
  }
  algebraicString(node: AlgebraicString): AlgebraicExpression {
    throw this.error(`Algebraic strings cannot be used within themselves`);
  }
  nil(node: Nil): AlgebraicExpression {
    return Undefined("null");
  }
  variable(node: Variable): AlgebraicExpression {
    return sym(node.name.lexeme);
  }
  assignExpr(node: AssignExpr): AlgebraicExpression {
    throw this.unsupportedError(`Assignment expressions`);
  }
  algebraicBinaryExpr(node: AlgebraicBinaryExpr): AlgebraicExpression {
    const left = this.reduce(node.left);
    const right = this.reduce(node.right);
    const op = node.op.type;
    switch (op) {
      case tt.caret: {
        return power(left, right);
      }
      case tt.plus: {
        return sum([left, right]);
      }
      case tt.minus: {
        return difference([left, right]);
      }
      case tt.star: {
        return product([left, right]);
      }
      case tt.slash: {
        return quotient(left, right);
      }
    }
    const lexeme = node.op.lexeme;
    throw this.error(
      `The “${lexeme}” operator is not supported in algebraic expressions`,
    );
  }
  algebraicUnaryExpr(node: AlgebraicUnaryExpr): AlgebraicExpression {
    const arg = this.reduce(node.arg);
    const op = node.op.type;
    switch (op) {
      case tt.plus: {
        return sum([arg]);
      }
      case tt.minus: {
        if (isInt(arg) || isReal(arg)) {
          return int(-arg._n);
        } else {
          return product([int(-1), arg]);
        }
      }
      case tt.bang: {
        return factorial(arg);
      }
    }
  }
  stringBinaryExpr(node: StringBinaryExpr): AlgebraicExpression {
    throw this.unsupportedError(`String binary expressions`);
  }
  logicalBinaryExpr(node: LogicalBinaryExpr): AlgebraicExpression {
    throw this.unsupportedError(`Logical binary expressions`);
  }
  logicalUnaryExpr(node: LogicalUnaryExpr): AlgebraicExpression {
    throw this.unsupportedError(`Logical unary expressions`);
  }
  relationalExpr(node: RelationalExpr): AlgebraicExpression {
    throw this.unsupportedError(`Relational expressions`);
  }
  callExpr(node: CallExpr): AlgebraicExpression {
    throw this.unsupportedError(`User defined functions`);
  }
  nativeCall(node: NativeCall): AlgebraicExpression {
    const args = this.reduceEach(node.args);
    if (args.length === 0) {
      throw this.error(`Encountered a native call with no arguments.`);
    }
    const name = node.name.lexeme;
    switch (name) {
      case "deriv": {
        const expr = args[0];
        const variable = args[1];
        if (variable === undefined || !isSymbol(variable)) {
          throw this.error(
            `Derivatives require a second symbol parameter.\n I.e., the derivative with respect to what variable?`,
          );
        } else {
          return derivative(expr, variable);
        }
      }
      case "simplify": {
        return simplify(args[0]);
      }
      default: {
        return fn(name, args);
      }
    }
  }
  groupExpr(node: GroupExpr): AlgebraicExpression {
    const out = this.reduce(node.expression);
    return out;
  }
  blockStmt(node: BlockStmt): AlgebraicExpression {
    throw this.unsupportedError(`Block statements`);
  }
  exprStmt(node: ExprStmt): AlgebraicExpression {
    throw this.unsupportedError(`Expression statements`);
  }
  fnStmt(node: FnStmt): AlgebraicExpression {
    throw this.unsupportedError(`Function declarations`);
  }
  ifStmt(node: IfStmt): AlgebraicExpression {
    throw this.unsupportedError(`If statements`);
  }
  classStmt(node: ClassStmt): AlgebraicExpression {
    throw this.unsupportedError(`Class statements`);
  }
  printStmt(node: PrintStmt): AlgebraicExpression {
    throw this.unsupportedError(`Print statements`);
  }
  returnStmt(node: ReturnStmt): AlgebraicExpression {
    throw this.unsupportedError(`Return statements`);
  }
  letStmt(node: VariableStmt): AlgebraicExpression {
    throw this.unsupportedError(`Let statements`);
  }
  whileStmt(node: WhileStmt): AlgebraicExpression {
    throw this.unsupportedError(`While statements`);
  }
}

class Compiler implements Visitor<Primitive> {
  environment: Environment<Primitive>;
  globals: Environment<Primitive>;
  locals: Map<Expr, number>;
  mode: "log-plain" | "log-latex" | "exec" = "exec";
  prints: string[] = [];
  simplifier: Simplifier;
  evaluate(node: ASTNode): Primitive {
    return node.accept(this);
  }
  setmode(mode: "log-plain" | "log-latex" | "exec") {
    this.mode = mode;
    return this;
  }
  constructor() {
    this.globals = runtimeEnv(null);
    this.environment = this.globals;
    this.locals = new Map();
    this.simplifier = new Simplifier(Token.empty);
  }
  vectorBinaryExpr(node: VectorBinaryExpr): Primitive {
    const op = node.op;
    const left = this.evaluate(node.left);
    const right = this.evaluate(node.right);
    if (!$isVector(left) || !$isVector(right)) {
      throw runtimeError(
        `The operator “${node.op.lexeme}” is restricted to vectors`,
        "interpreting an infix vector expression",
        op,
      );
    }
    switch (op.type) {
      case tt.dot_add: {
        return left.add(right);
      }
      case tt.dot_minus: {
        return left.sub(right);
      }
      case tt.dot_star: {
        return left.mul(right);
      }
      case tt.dot_caret: {
        return left.pow(right);
      }
    }
  }
  algebraicString(node: AlgebraicString): Primitive {
    const expression = node.expression;
    return this.simplifier.place(node.op).reduce(expression);
  }
  lookupVariable(name: Token, expr: Expr) {
    const distance = this.locals.get(expr);
    if (distance !== undefined) {
      return this.environment.getAt(distance, name.lexeme);
    } else {
      return this.globals.get(name);
    }
  }
  indexingExpr(node: IndexingExpr) {
    const L = this.evaluate(node.list);
    const I = this.evaluate(node.index) as number;
    if (!$isNumber(I)) {
      throw runtimeError(
        `Expected a number index, but got “${stringify(I)}”`,
        `evaluating an index expression`,
        node.op,
      );
    }
    if ($isVector(L) || $isMatrix(L)) {
      const out = L.element(I);
      if (out === null) {
        throw runtimeError(
          `Encountered an out-of-bounds index.\nThe provided index exceeds the length of the targeted sequential.`,
          `evaluating an indexing expression`,
          node.op,
          "Ensure the index value is less than the sequential’s length.",
        );
      } else {
        return out;
      }
    } else if ($isArray(L)) {
      const out = L[I - 1];
      if (out === undefined) {
        return null;
      } else {
        return out;
      }
    } else {
      throw runtimeError(
        `Expected a sequential for indexing, but got “${stringify(L)}”`,
        `evaluating an indexing expression`,
        node.op,
      );
    }
  }
  resolve(expression: Expr, depth: number) {
    this.locals.set(expression, depth);
  }
  thisExpr(node: ThisExpr): Primitive {
    return this.lookupVariable(node.keyword, node);
  }
  stringBinaryExpr(node: StringBinaryExpr): Primitive {
    const left = stringify(this.evaluate(node.left));
    const right = stringify(this.evaluate(node.right));
    const op = node.op.type;
    switch (op) {
      case tt.amp:
        return left + right;
    }
  }
  superExpr(node: SuperExpr): Primitive {
    throw new Error(`superExpr not implemented`);
  }
  setExpr(node: SetExpr): Primitive {
    const obj = this.evaluate(node.object);
    if (!$isKlassInstance(obj)) {
      throw runtimeError(
        `Only instances have fields`,
        `interpreting a field set`,
        node.name,
      );
    }
    const value = this.evaluate(node.value);
    return obj.set(node.name.lexeme, value);
  }
  getExpr(node: GetExpr): Primitive {
    const obj = this.evaluate(node.object);
    if ($isKlassInstance(obj)) {
      return obj.get(node.name);
    }
    throw runtimeError(
      `Properties only exist on instances of classes. “${
        stringify(obj)
      }” is not a class instance.`,
      `interpreting a property access`,
      node.name,
    );
  }
  matrixExpr(node: MatrixExpr): Primitive {
    const vectors = node.vectors.map((v) => this.evaluate(v)) as Vector[];
    return new Matrix(vectors, node.rows, node.cols);
  }
  vectorExpr(node: VectorExpr): Vector {
    const nums: number[] = [];
    const elements = node.elements;
    for (let i = 0; i < elements.length; i++) {
      const n = this.evaluate(elements[i]);
      if (typeof n !== "number") {
        throw runtimeError(
          `Vectors must only contain either numbers or expressions that reduce to numbers. The value ${
            stringify(n)
          } is not a number.`,
          `interpreting a vector expression`,
          node.op,
        );
      }
      nums.push(n);
    }
    return vector(nums);
  }

  integer(node: Integer): Primitive {
    return node.value;
  }
  numericConstant(node: NumericConstant): Primitive {
    return node.value;
  }
  bigNumber(node: BigNumber): Primitive {
    return node.value;
  }
  fractionExpr(node: FractionExpr): Primitive {
    return node.value;
  }
  bigRational(node: RationalExpr): Primitive {
    return node.value;
  }
  float(node: Float): Primitive {
    return node.value;
  }
  bool(node: Bool): Primitive {
    return node.value;
  }
  tupleExpr(node: TupleExpr): Primitive {
    const elements = node.elements.map((e) => this.evaluate(e));
    return elements;
  }
  string(node: StringLiteral): Primitive {
    return node.value;
  }
  nil(node: Nil): Primitive {
    return node.value;
  }
  variable(node: Variable): Primitive {
    return this.lookupVariable(node.name, node);
  }
  assignExpr(node: AssignExpr): Primitive {
    const value = this.evaluate(node.value);
    const distance = this.locals.get(node);
    if (distance !== undefined) {
      this.environment.assignAt(distance, node.name.lexeme, value);
    } else {
      this.globals.assign(node.name, value);
    }
    return value;
  }
  algebraicBinaryExpr(node: AlgebraicBinaryExpr): Primitive {
    let L = this.evaluate(node.left) as any;
    let R = this.evaluate(node.right) as any;
    const op = node.op.type;
    if (($isNumber(L) && $isFraction(R)) || ($isNumber(R) && $isFraction(L))) {
      L = Fraction.from(L);
      R = Fraction.from(R);
    }
    if ($isFraction(L) && $isFraction(R)) {
      // deno-fmt-ignore
      switch (op) {
        case tt.star: return L.times(R);
        case tt.slash: return L.div(R);
        case tt.plus: return L.add(R);
        case tt.minus: return L.sub(R);
        case tt.percent: return percent(L.asFloat(), R.asFloat());
        case tt.rem: return (L.asInt()) % (R.asInt());
        case tt.mod: return mod(L.asInt(), R.asInt());
        case tt.div: return floor(L.asInt()/R.asInt());
        case tt.caret: return (L.asInt() ** R.asInt());
      }
    }
    // deno-fmt-ignore
    switch (op) {
      case tt.plus: return L + R;
      case tt.star: return L * R;
      case tt.caret: return L ** R;
      case tt.slash: return L / R;
      case tt.minus: return L - R;
      case tt.rem: return L % R;
      case tt.mod: return mod(L, R);
      case tt.percent: return percent(L, R);
      case tt.div: return floor(L / R);
    }
  }
  algebraicUnaryExpr(node: AlgebraicUnaryExpr): Primitive {
    const arg = this.evaluate(node.arg) as any;
    const op = node.op.type;
    if ($isFraction(arg)) {
      // deno-fmt-ignore
      switch (op) {
        case tt.plus: return arg.pos();
        case tt.minus: return arg.neg();
        case tt.bang: {
          throw runtimeError(
            `The factorial operation is not defined on rationals`,
            `interpreting a factorial expression`,
						node.op,
          )
        }
      }
    }
    // deno-fmt-ignore
    switch (op) {
      case tt.plus: return +arg;
      case tt.minus: return -arg;
      case tt.bang: return factorialize(arg);
    }
  }
  logicalBinaryExpr(node: LogicalBinaryExpr): Primitive {
    const L = truthy(this.evaluate(node.left));
    const R = truthy(this.evaluate(node.right));
    const op = node.op.type;

    // deno-fmt-ignore
    switch (op) {
      case tt.and: return L && R;
      case tt.or: return L || R;
      case tt.nand: return !(L && R);
      case tt.nor: return !(L||R);
      case tt.xnor: return L === R;
      case tt.xor: return L !== R;
    }
  }
  logicalUnaryExpr(node: LogicalUnaryExpr): Primitive {
    const arg = this.evaluate(node.arg);
    return !(truthy(arg));
  }
  relationalExpr(node: RelationalExpr): Primitive {
    let L = this.evaluate(node.left) as any;
    let R = this.evaluate(node.right) as any;
    const op = node.op.type;
    if (($isNumber(L) && $isFraction(R)) || ($isNumber(R) && $isFraction(L))) {
      L = Fraction.from(L);
      R = Fraction.from(R);
    }
    if ($isFraction(L) && $isFraction(R)) {
      // deno-fmt-ignore
      switch (op) {
        case tt.lt: return L.lt(R);
        case tt.gt: return L.gt(R);
        case tt.deq: return L.equals(R);
        case tt.neq: return !L.equals(R);
        case tt.geq: return L.geq(R);
        case tt.leq: return L.leq(R);
      }
    }
    // deno-fmt-ignore
    switch (op) {
      case tt.lt: return L < R;
      case tt.gt: return L > R;
      case tt.deq: return L === R;
      case tt.neq: return L !== R;
      case tt.geq: return L >= R;
      case tt.leq: return L <= R;
    }
  }
  groupExpr(node: GroupExpr): Primitive {
    return this.evaluate(node.expression);
  }
  callExpr(node: CallExpr): Primitive {
    const callee = this.evaluate(node.callee);
    const args: Primitive[] = [];
    for (let i = 0; i < node.args.length; i++) {
      args.push(this.evaluate(node.args[i]));
    }
    if ($isKlass(callee)) {
      return callee.call(this, args);
    }
    if ($isFn(callee)) {
      return callee.call(this, args);
    }
    // deno-fmt-ignore
    throw runtimeError(
      `“${stringify(callee)}” is neither a function nor a class. Only functions and classes may be called.`,
      `evaluating a call expression`,
			node.paren
    );
  }
  nativeCall(node: NativeCall): Primitive {
    const val = node.args.map((v) => this.evaluate(v)) as any[];
    // deno-fmt-ignore
    switch (node.name.lexeme) {
      case 'deriv': {
        const arg = val[0];
        let x = val[1];
        if ($isString(x)) {
          x = sym(x);
        }
        if (!(arg instanceof AlgebraicExpression)) {
          throw runtimeError(
            `Only algebraic expressions may be passed to “deriv”`,
            `evaluating a native call`,
						node.name,
          )
        }
        if (!isSymbol(x)) {
          throw runtimeError(
            `“deriv” requires a symbol as its second argument`,
            `evaluating a native call`,
						node.name,
          )
        }
        return derivative(arg, x)
      }
      case 'gcd': {
        const a = floor(val[0]);
        const b = floor(val[1]);
        return gcd(a,b);
      }
      case 'simplify': {
        const arg = val[0];
        if (!(arg instanceof AlgebraicExpression)) {
          throw runtimeError(
            `Only algebraic expressions may be passed to “simplify”`,
            `evaluating a native call`,
						node.name,
          )
        }
        return simplify(arg);
      }
      case 'subex': {
        if (!(val[0] instanceof AlgebraicExpression)) {
          throw runtimeError(
            `subex may only be called with an algebraic expressions`,
            `evaluating a native call`,
						node.name,
          )
        }
        return subex(val[0]);
      }
      case "!": return factorialize(val[0]);
      case 'tanh': return tanh(val[0]);
      case 'sqrt': return sqrt(val[0]);
      case 'sinh': return sinh(val[0]);
      case 'exp': return Math.exp(val[0]);
      case 'cosh': return cosh(val[0]);
      case 'floor': return floor(val[0]);
      case 'ceil': return ceil(val[0]);
      case 'arctan': return arctan(val[0]);
      case 'arcsinh': return arcsinh(val[0]);
      case 'arcsin': return arcsin(val[0]);
      case 'arccosh': return arccosh(val[0]);
      case 'arccos': return arccos(val[0]);
      case "cos": return cos(val[0]);
      case "sin": return sin(val[0]);
      case "tan": return tan(val[0]);
      case "lg": return lg(val[0]);
      case 'ln': return ln(val[0]);
      case 'log': return log(val[0]);
      case 'max': return max(...val);
      case 'min': return min(...val);
      case 'avg': return avg(...val);
    }
  }
  executeBlock(statements: Statement[], environment: Environment<Primitive>) {
    const previous = this.environment;
    try {
      this.environment = environment;
      let result: Primitive = null;
      const L = statements.length;
      for (let i = 0; i < L; i++) {
        result = this.evaluate(statements[i]);
      }
      return result;
    } finally {
      this.environment = previous;
    }
  }
  classStmt(node: ClassStmt): Primitive {
    this.environment.define(node.name.lexeme, null, true);
    const methods = new Map<string, Fn>();
    for (let i = 0; i < node.methods.length; i++) {
      const method = node.methods[i];
      const f = callable(
        method,
        this.environment,
        method.name.lexeme === "init",
      );
      methods.set(method.name.lexeme, f);
    }
    const klass = klassObj(node.name.lexeme, methods);
    this.environment.assign(node.name, klass);
    return null;
  }
  blockStmt(node: BlockStmt): Primitive {
    const env = runtimeEnv(this.environment);
    return this.executeBlock(node.statements, env);
  }
  exprStmt(node: ExprStmt): Primitive {
    return this.evaluate(node.expression);
  }
  fnStmt(node: FnStmt): Primitive {
    const f = callable(node, this.environment, false);
    this.environment.define(node.name.lexeme, f, false);
    return f;
  }
  ifStmt(node: IfStmt): Primitive {
    if (truthy(this.evaluate(node.condition))) {
      return this.evaluate(node.then);
    } else {
      return this.evaluate(node.alt);
    }
  }
  printStmt(node: PrintStmt): Primitive {
    const s = this.evaluate(node.expression);
    const out = stringify(s);
    if (this.mode === "log-plain") {
      this.prints.push(out);
    } else if (this.mode === "log-latex") {
      const expr = latexify(node.expression);
      const reduction = latexify(s);
      const res = latex.join(expr, latex.to(), reduction);
      this.prints.push(res);
    } else {
      console.log(out);
    }
    return out;
  }
  returnStmt(node: ReturnStmt): Primitive {
    const value = this.evaluate(node.value);
    throw returnValue(value);
  }
  letStmt(node: VariableStmt): Primitive {
    const value = this.evaluate(node.value);
    this.environment.define(node.name.lexeme, value, node.mutable);
    return value;
  }
  maxLoops: number = Infinity;
  loopcap(n: number) {
    this.maxLoops = n;
    return this;
  }
  whileStmt(node: WhileStmt): Primitive {
    let out: Primitive = null;
    let i = 0;
    while (truthy(this.evaluate(node.condition))) {
      out = this.evaluate(node.body);
      i++;
      if (i > this.maxLoops) {
        throw runtimeError(
          `Iterations exceed this environment’s safety ceiling`,
          "interpreting a while-loop",
          node.keyword,
        );
      }
    }
    return out;
  }
  interpret(statements: Statement[]) {
    try {
      let result: Primitive = null;
      const L = statements.length;
      for (let i = 0; i < L; i++) {
        result = this.evaluate(statements[i]);
      }
      return right(result);
    } catch (error) {
      return left(error as Err);
    }
  }
}

class Latexer implements Mapper<string> {
  reduce(node: ASTNode | AlgebraicExpression) {
    return node.trust(this);
  }
  integer(node: Integer): string {
    return `${node.value}`;
  }
  numericConstant(node: NumericConstant): string {
    const x = node.sym;
    switch (x) {
      case "Inf":
        return latex.esc("infty");
      case "NAN":
        return latex.txt(`NAN`);
      case "e":
        return "e";
      case "pi":
        return latex.esc("pi");
    }
  }
  vectorExpr(node: VectorExpr): string {
    return node.toString();
  }
  vectorBinaryExpr(node: VectorBinaryExpr): string {
    const left = this.reduce(node.left);
    const right = this.reduce(node.right);
    let op = node.op.type;
    if (isVectorExpr(node.left) && isVectorExpr(node.right)) {
      const l = node.left.elements;
      const r = node.right.elements;
      if (l.length !== r.length) {
        throw runtimeError(
          `Unequal vectors encountered`,
          "rendering a latex string",
          node.op,
        );
      }
      const op = node.op.lexeme.slice(1);
      const out = [];
      for (let i = 0; i < l.length; i++) {
        const left = this.reduce(l[i]);
        const right = this.reduce(r[i]);
        const L = latex.brace(left);
        const R = latex.brace(right);
        const e = latex.join(L, op, R);
        out.push(e);
      }
      return latex.surround(out.join(",~~"), "[", "]");
    }
    switch (op) {
      case tt.dot_add:
        return latex.join(left, "+", right);
      case tt.dot_minus:
        return latex.join(left, "-", right);
      case tt.dot_star:
        return latex.join(left, latex.esc("cdot"), right);
      case tt.dot_caret:
        return latex.join(left, "^", latex.brace(right));
    }
  }
  matrixExpr(node: MatrixExpr): string {
    const open = latex.esc("begin{bmatrix}");
    let body = "";
    const vectors = node.vectors;
    const maxRow = vectors.length - 1;
    vectors.forEach((d, i) => {
      const maxCol = d.elements.length - 1;
      d.elements.forEach((e, j) => {
        const element = this.reduce(e);
        body += element;
        if (j !== maxCol) {
          body += ` & `;
        }
      });
      if (i !== maxRow) {
        body += latex.linebreak();
      }
    });
    const close = latex.esc("end{bmatrix}");
    return open + body + close;
  }
  indexingExpr(node: IndexingExpr): string {
    const target = this.reduce(node.list);
    const index = this.reduce(node.index);
    return latex.join(target, "_", latex.brace(index));
  }
  bigNumber(node: BigNumber): string {
    return `${node.value}`;
  }
  fractionExpr(node: FractionExpr): string {
    const n = node.value._n;
    const d = node.value._d;
    return latex.frac(n, d);
  }
  bigRational(node: RationalExpr): string {
    const n = node.value.N;
    const d = node.value.D;
    return latex.frac(n.toString(), d.toString());
  }
  float(node: Float): string {
    return `${node.value}`;
  }
  bool(node: Bool): string {
    return latex.txt(`${node.value}`);
  }
  tupleExpr(node: TupleExpr): string {
    const out = node.elements.map((e) => this.reduce(e)).join(",");
    return latex.surround(out, "(", ")");
  }
  getExpr(node: GetExpr): string {
    const target = this.reduce(node.object);
    const prop = latex.txt(node.name.lexeme);
    return latex.join(target, "_", latex.brace(prop));
  }
  setExpr(node: SetExpr): string {
    const target = this.reduce(node.object);
    const prop = latex.txt(node.name.lexeme);
    const value = this.reduce(node.value);
    return latex.join(target, "_", latex.brace(prop), "=", value);
  }
  superExpr(node: SuperExpr): string {
    return latex.join(latex.txt("super"), "(", ")");
  }
  thisExpr(node: ThisExpr): string {
    return latex.txt("this");
  }
  string(node: StringLiteral): string {
    return latex.dquoted(node.value);
  }
  stringBinaryExpr(node: StringBinaryExpr): string {
    const left = this.reduce(node.left);
    const right = this.reduce(node.right);
    const op = node.op.type;
    switch (op) {
      case tt.amp:
        return latex.join(left, latex.esc("&"), right);
    }
  }
  simplifier: Simplifier = new Simplifier(Token.empty);
  algebraicString(node: AlgebraicString): string {
    const exp = this.simplifier.place(node.op).reduce(node.expression);
    return this.reduce(exp);
  }
  nil(node: Nil): string {
    return latex.esc("varnothing");
  }
  variable(node: Variable): string {
    const name = node.name.lexeme;
    if (isGreekLetterName(name)) {
      return latex.esc(name);
    } else {
      return name;
    }
  }
  assignExpr(node: AssignExpr): string {
    const left = this.reduce(node.variable);
    const value = this.reduce(node.value);
    return latex.tie(left, "=", value);
  }
  algebraicBinaryExpr(node: AlgebraicBinaryExpr): string {
    const left = this.reduce(node.left);
    const right = this.reduce(node.right);
    const op = node.op.type;
    switch (op) {
      case tt.star:
        return latex.tie(left, latex.esc("times"), right);
      case tt.slash:
        return latex.frac(left, right);
      case tt.plus:
        return latex.join(left, "+", right);
      case tt.minus:
        return latex.join(left, "-", right);
      case tt.percent:
        return latex.join(
          left + latex.percent(),
          "~" + latex.txt("of") + "~",
          right,
        );
      case tt.rem:
        return latex.tie(left, latex.txt("rem"), right);
      case tt.mod:
        return latex.tie(left, latex.esc("bmod"), right);
      case tt.div:
        return latex.tie(left, latex.txt("div"), right);
      case tt.caret:
        return latex.join(left, "^", latex.brace(right));
    }
  }
  algebraicUnaryExpr(node: AlgebraicUnaryExpr): string {
    const value = this.reduce(node.arg);
    const op = node.op.type;
    switch (op) {
      case tt.plus:
        return latex.join("+", value);
      case tt.minus:
        return latex.join("-", value);
      case tt.bang:
        return latex.join(value, "!");
    }
  }
  logicalBinaryExpr(node: LogicalBinaryExpr): string {
    const left = this.reduce(node.left);
    const right = this.reduce(node.right);
    const op = node.op.type;
    switch (op) {
      case tt.and:
        return latex.tie(left, latex.and(), right);
      case tt.nand:
        return latex.tie(left, latex.nand(), right);
      case tt.nor:
        return latex.tie(left, latex.nor(), right);
      case tt.xnor:
        return latex.tie(left, latex.xnor(), right);
      case tt.xor:
        return latex.tie(left, latex.xor(), right);
      case tt.or:
        return latex.tie(left, latex.or(), right);
    }
  }
  logicalUnaryExpr(node: LogicalUnaryExpr): string {
    const arg = this.reduce(node.arg);
    return latex.join(latex.esc("neg"), " ", arg);
  }
  relationalExpr(node: RelationalExpr): string {
    const left = this.reduce(node.left);
    const right = this.reduce(node.right);
    const op = node.op.type;
    switch (op) {
      case tt.lt:
        return latex.tie(left, latex.esc("lt "), right);
      case tt.gt:
        return latex.tie(left, latex.esc("gt "), right);
      case tt.deq:
        return latex.tie(left, latex.esc("equiv"), right);
      case tt.neq:
        return latex.tie(left, latex.esc("neq"), right);
      case tt.geq:
        return latex.tie(left, latex.esc("geq"), right);
      case tt.leq:
        return latex.tie(left, latex.esc("leq"), right);
    }
  }
  callExpr(node: CallExpr): string {
    const name = this.reduce(node.callee);
    const args = latex.surround(
      node.args.map((x) => this.reduce(x)).join(",~"),
      "(",
      ")",
    );
    return latex.join(name, args);
  }
  nativeCall(node: NativeCall): string {
    const arg = node.args.map((x) => this.reduce(x)).join(",~");
    const parend = (args: string) => latex.surround(args, "(", ")");
    const name = node.name.lexeme;
    switch (name) {
      case "!":
        return latex.join(arg, "!");
      case "arccos":
      case "arccosh":
      case "arcsin":
      case "arcsinh":
      case "arctan":
      case "avg":
      case "cos":
      case "cosh":
      case "exp":
      case "gcd":
      case "lg":
      case "ln":
      case "log":
      case "max":
      case "min":
      case "simplify":
      case "sin":
      case "sinh":
      case "subex":
      case "tan":
      case "tanh":
        return latex.join(latex.txt(name), parend(arg));
      case "ceil":
        return latex.surround(arg, latex.esc("lfloor"), latex.esc("rfloor"));
      case "deriv": {
        const v = this.reduce(node.args[1]);
        const d = latex.frac("d", "d" + v);
        return latex.join(d, arg);
      }
      case "sqrt":
        return latex.join(latex.esc("sqrt"), latex.brace(arg));
      case "floor":
        return latex.surround(arg, latex.esc("lceil"), latex.esc("rceil"));
    }
  }
  groupExpr(node: GroupExpr): string {
    return latex.surround(this.reduce(node.expression), "(", ")");
  }
  blockStmt(node: BlockStmt): string {
    return latex.txt("void");
  }
  exprStmt(node: ExprStmt): string {
    return latex.txt("void");
  }
  fnStmt(node: FnStmt): string {
    return latex.txt("void");
  }
  ifStmt(node: IfStmt): string {
    return latex.txt("void");
  }
  classStmt(node: ClassStmt): string {
    return latex.txt("void");
  }
  printStmt(node: PrintStmt): string {
    return latex.txt("void");
  }
  returnStmt(node: ReturnStmt): string {
    return latex.txt("void");
  }
  letStmt(node: VariableStmt): string {
    return latex.txt("void");
  }
  whileStmt(node: WhileStmt): string {
    return latex.txt("void");
  }
  int(node: Int): string {
    return `${node._n}`;
  }
  real(node: Real): string {
    return `${node._n}`;
  }
  sym(node: Sym<string>): string {
    const name = node._s;
    if (isGreekLetterName(name)) {
      return latex.esc(name);
    } else {
      return name;
    }
  }
  constant(node: Constant<number | null, string>): string {
    return `${node._c}`;
  }
  sum(node: Sum): string {
    let expressions = node._args.map((a) => this.reduce(a)).join("+");
    return expressions;
  }
  product(node: Product): string {
    if (node._args.length === 2) {
      const left = this.reduce(node._args[0]);
      const right = this.reduce(node._args[1]);
      if (isInt(node._args[0]) && isSymbol(node._args[1])) {
        return latex.tie(left, right);
      }
    }
    const out = [];
    for (let i = 0; i < node._args.length; i++) {
      const left = node._args[i];
      out.push(this.reduce(left));
    }
    let expr = out.join("");
    return expr;
  }
  quotient(node: Quotient): string {
    if (node._args.length === 2) {
      const n = this.reduce(node._args[0]);
      const d = this.reduce(node._args[1]);
      return latex.frac(n, d);
    } else {
      let expressions = node._args.map((a) => this.reduce(a)).join("/");
      return expressions;
    }
  }
  fraction(node: Fraction): string {
    return latex.frac(node._n, node._d);
  }
  power(node: Power): string {
    const base = this.reduce(node.base);
    const exp = this.reduce(node.exponent);
    let expressions = latex.join(
      base,
      "^",
      latex.surround(latex.brace(exp), "(", ")"),
    );
    return expressions;
  }
  difference(node: Difference): string {
    let expressions = node._args.map((a) => this.reduce(a)).join("-");
    return expressions;
  }
  factorial(node: Factorial): string {
    let expressions = node._args.map((a) => this.reduce(a)).join("!");
    return expressions;
  }
  algebraicFn(node: AlgebraicFn): string {
    const name = node._op;
    const args = node._args.map((x) => this.reduce(x)).join(",~");
    return latex.tie(name, latex.surround(args, "(", ")"));
  }
}

export function latexify(x: ASTNode | AlgebraicExpression | Primitive): string {
  if (x instanceof ASTNode || x instanceof AlgebraicExpression) {
    const d = new Latexer();
    return d.reduce(x);
  } else if ($isNumber(x) || $isBigInt(x)) {
    return `${x}`;
  } else if ($isNothing(x)) {
    return latex.esc("varnothing");
  } else if ($isFraction(x)) {
    return latex.frac(x._n, x._d);
  } else if ($isBoolean(x)) {
    return latex.txt(x ? "true" : "false");
  } else if ($isString(x)) {
    return latex.dquoted(x);
  } else if ($isMatrix(x)) {
    return x.toLatex();
  } else if ($isVector(x)) {
    return x.toLatex();
  } else if ($isArray(x)) {
    const out = x.map((e) => latexify(e)).join(",~");
    return latex.surround(out, "(", ")");
  } else {
    return latex.txt("ERROR");
  }
}

type EngineSettings = {
  /**
   * Indicates whether implicit multiplication is permitted.
   * Defaults to true.
   */
  implicitMultiplication: boolean;
};

function lexical(input: string) {
  /**
   * All variables prefixed with a `$` are
   * stateful variables.
   */

  /** The current line. */
  let $line = 1;

  /** The current column. */
  let $column = 1;

  /**
   * Points to the first character
   * of the lexeme currently being
   * scanned.
   */
  let $start = 0;

  /**
   * Points at the character currently
   * being read.
   */
  let $current = 0;

  /**
   * Error indicator defaulting to null.
   * If initialized, scanning will cease (per the condition
   * in {@link atEnd}).
   */
  let $error: null | Err = null;

  /**
   * Returns true if the scanner has reached
   * the end of input.
   */
  const atEnd = () => ($current >= input.length) || ($error !== null);

  /**
   * Consumes and returns the next character
   * in the input expression.
   */
  const tick = () => input[$current++];

  /**
   * Returns the input substring from
   * start to current.
   */
  const slice = () => input.slice($start, $current);

  /**
   * Returns a new token.
   * An optional lexeme may be passed.
   */
  const tkn = (type: tt, lexeme: string | null = null) => {
    lexeme = lexeme ? lexeme : slice();
    return token(type, lexeme, $line, $column);
  };

  /**
   * Returns an error token. If called,
   * sets the mutable error variable.
   */
  const errorTkn = (message: string, phase: string): Token<tt.ERROR, Err> => {
    const errToken = token(tt.ERROR, "", $line, $column);
    $error = lexicalError(message, phase, errToken);
    return token(tt.ERROR, "", $line, $column).lit($error);
  };

  /**
   * Returns the current character.
   */
  const peek = () => atEnd() ? "" : input[$current];

  /**
   * Returns the character just
   * head of the current character.
   */
  const peekNext = () => atEnd() ? "" : input[$current + 1];

  /**
   * Returns the character
   * n places ahead of current.
   */
  const lookup = (n: number) => atEnd() ? "" : input[$current + n];

  /**
   * If the provided expected string
   * matches, increments the current
   * pointer and returns true.
   * Otherwise returns false without
   * increment.
   */
  const match = (expected: string) => {
    if (atEnd()) return false;
    if (input[$current] !== expected) return false;
    $current++;
    return true;
  };

  /**
   * Returns true if the current peek (the character
   * pointed at by `current`) matches the provided
   * number.
   */
  const peekIs = (c: string) => (peek() === c);

  /**
   * Consumes all whitespice while
   * moving the scanner’s `current`
   * pointer forward.
   */
  const skipws = () => {
    while (!atEnd()) {
      const c = peek();
      // deno-fmt-ignore
      switch (c) {
        case ' ':
        case '\r':
        case '\t': 
          tick();
          $column++;
          break;
        case '\n':
          $line++;
          $column=0;
          tick();
          break;
        default:
          return;
      }
    }
  };

  const numToken = (
    numberString: string,
    type: NumberTokenType,
    hasSeparators: boolean,
  ): Token => {
    const n = hasSeparators ? numberString.replaceAll("_", "") : numberString;
    switch (type) {
      case tt.int: {
        const num = Number.parseInt(n);
        if (num > MAX_INT) {
          return errorTkn(
            `Encountered an integer overflow. Consider rewriting “${numberString}” as a bignumber: “#${numberString}”. If “${numberString}” is to be used symbolically, consider rewriting “${numberString}” as a scientific number.`,
            "scanning an integer literal",
          );
        } else {
          return tkn(type).lit(num);
        }
      }
      case tt.float: {
        const num = Number.parseFloat(n);
        if (num > Number.MAX_VALUE) {
          return errorTkn(
            `Encountered a floating point overflow. Consider rewriting "${n}" as a fraction or bigfraction. If "${n}" is to be used symbolically, consider rewriting "${n}" as a scientific number.`,
            "scanning a floating point literal",
          );
        }
        return tkn(tt.float).lit(num);
      }
      case tt.scientific: {
        const [a, b] = n.split("E");
        const base = Number.parseFloat(a);
        const exponent = Number.parseInt(b);
        return tkn(type).lit(tuple(base, exponent));
      }
      case tt.fraction: {
        const [a, b] = n.split("|");
        const N = Number.parseInt(a);
        const D = Number.parseInt(b);
        if (N > MAX_INT || D > MAX_INT) {
          return tkn(tt.bigfraction).lit(tuple(
            BigInt(N),
            BigInt(D),
          ));
        } else {
          return tkn(type).lit(tuple(N, D));
        }
      }
      default: {
        return errorTkn(`Unknown number type`, `scanning a literal number`);
      }
    }
  };

  const number = (initialType: NumberTokenType) => {
    let type = initialType;
    let scannedSeparators = false;
    // scanning integer
    while (isDigit(peek()) && !atEnd()) {
      tick();
    }
    if (peekIs("_") && isDigit(peekNext())) {
      tick(); // eat the '_'
      const phase = `scanning a number with separators`;
      scannedSeparators = true;
      // scan separators
      let digits = 0;
      while (isDigit(peek()) && !atEnd()) {
        tick();
        digits++;
        if (peekIs("_") && isDigit(peekNext())) {
          if (digits === 3) {
            tick();
            digits = 0;
          } else {
            return errorTkn(
              `There must be 3 ASCII digits before the numeric separator “_”.`,
              phase,
            );
          }
        }
      }
      if (digits !== 3) {
        return errorTkn(
          `There must be 3 ASCII digits after the numeric separator “_”.`,
          phase,
        );
      }
    }
    if (peekIs(".") && isDigit(peekNext())) {
      tick();
      type = tt.float;
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
    }

    /**
     * FractionExpr numbers take the form:
     * ~~~ts
     * [int] '|' [int]
     * // e.g., 1|2
     * ~~~
     * Both sides must be integers.
     */
    if (peekIs("|")) {
      if (type !== tt.int) {
        return errorTkn(
          `Expected an integer before “|”`,
          "scanning a fraction",
        );
      }
      type = tt.fraction;
      tick(); // eat the '|'
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
      return numToken(slice(), type, scannedSeparators);
    }

    // scientific
    /**
     * Syntax is: [float] 'E' ('+'|'-') [int]
     * The exponent must always be an integer.
     */
    if (peekIs("E")) {
      if (isDigit(peekNext())) {
        // This is a scientific with the form [float] E [int]
        type = tt.scientific;
        tick(); // eat the 'E'
        while (isDigit(peek())) tick();
      } else if (
        ((peekNext() === "+") || (peekNext() === "-")) && isDigit(lookup(2))
      ) {
        // This is a scientific with the form [float] E (+|-) [int]
        type = tt.scientific;
        tick(); // eat the 'E'
        tick(); // eat the '+' or '-'
        while (isDigit(peek())) tick();
      }
    }
    return numToken(slice(), type, scannedSeparators);
  };

  /**
   * Record of native functions. Each key corresponds
   * to the native function name. The number mapped to
   * by the key is the function’s arity (the number
   * of arguments the function takes).
   */
  const nativeFunctions: Record<NativeFn, number> = {
    deriv: 1,
    avg: 1,
    gcd: 1,
    simplify: 1,
    subex: 1,
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
    "!": 1,
    max: 1,
    min: 1,
  };

  /**
   * Scans a single-quoted variable.
   */
  const algebraicString = () => {
    while ((peek() !== `'`) && !atEnd()) {
      if (peek() === `\n`) {
        $line++;
        $column = 0;
      } else {
        $column++;
      }
      tick();
    }
    if (atEnd()) {
      return errorTkn(
        `Unterminated algebraic string`,
        "scanning an algebraic string",
      );
    }
    tick(); // eat the ':'
    const s = slice().replaceAll(`'`, "");
    return tkn(tt.algebra_string).lit(s);
  };

  const stringLiteral = () => {
    while (peek() !== `"` && !atEnd()) {
      if (peek() === `\n`) {
        $line++;
        $column = 0;
      } else {
        $column++;
      }
      tick();
    }
    if (atEnd()) return errorTkn(`Infinite string`, "scanning a string");
    tick();
    const lex = slice().slice(1, -1);
    return tkn(tt.string, lex);
  };

  /**
   * Scans a word. Word is defined as
   * either a user-defined symbol (the token `SYM`)
   * or a reserved word.
   */
  const word = () => {
    while ((isValidName(peek()) || isDigit(peek())) && (!atEnd())) {
      tick();
    }
    const string = slice();
    const native = nativeFunctions[string as NativeFn];
    if (native !== undefined) {
      return tkn(tt.native);
    }
    // deno-fmt-ignore
    switch (string) {
      case 'this': return tkn(tt.this);
      case 'super': return tkn(tt.super);
      case 'class': return tkn(tt.class);
      case 'false': return tkn(tt.bool).lit(false);
      case 'true': return tkn(tt.bool).lit(true);
      case 'NAN': return tkn(tt.nan).lit(NaN);
      case 'Inf': return tkn(tt.inf).lit(Infinity);
      case 'pi': return tkn(tt.numeric_constant).lit(PI);
      case 'e': return tkn(tt.numeric_constant).lit(E);
      case 'return': return tkn(tt.return);
      case 'while': return tkn(tt.while);
      case 'for': return tkn(tt.for);
      case 'let': return tkn(tt.let);
      case 'var': return tkn(tt.var);
      case 'fn': return tkn(tt.fn);
      case 'if': return tkn(tt.if);
      case 'else': return tkn(tt.else);
      case 'print': return tkn(tt.print);
      case 'rem': return tkn(tt.rem);
      case 'mod': return tkn(tt.mod);
      case 'div': return tkn(tt.div);
      case 'nil': return tkn(tt.nil).lit(null);
      case 'and': return tkn(tt.and);
      case 'or': return tkn(tt.or);
      case 'nor': return tkn(tt.nor);
      case 'xor': return tkn(tt.xor);
      case 'xnor': return tkn(tt.xnor);
      case 'not': return tkn(tt.not);
      case 'nand': return tkn(tt.nand);
    }
    return tkn(tt.symbol);
  };

  const isHexDigit = (char: string) => (
    (("0" <= char) && (char <= "9")) ||
    (("a" <= char) && (char <= "f")) ||
    (("A" <= char) && (char <= "F"))
  );

  const isOctalDigit = (char: string) => (
    "0" <= char && char <= "7"
  );

  const hexNumber = () => {
    if (!(isHexDigit(peek()))) {
      return errorTkn(
        `Expected hexadecimals after “0x”`,
        "scanning a hexadecimal",
      );
    }
    while (isHexDigit(peek()) && !atEnd()) {
      tick();
    }
    const s = slice().replace("0x", "");
    const n = Number.parseInt(s, 16);
    return tkn(tt.int).lit(n);
  };

  const octalNumber = () => {
    if (!(isOctalDigit(peek()))) {
      return errorTkn(
        `Expected octal digits after “0o”`,
        "scanning an octal number",
      );
    }
    while (isOctalDigit(peek()) && !atEnd()) {
      tick();
    }
    const s = slice().replace("0o", "");
    const n = Number.parseInt(s, 8);
    return tkn(tt.int).lit(n);
  };

  const binaryNumber = () => {
    if (!(peekIs("0") || peekIs("1"))) {
      return errorTkn(
        `Expected binary digits after “0b”`,
        "scanning a binary number",
      );
    }
    while ((peekIs("0") || peekIs("1")) && !atEnd()) {
      tick();
    }
    const s = slice().replace("0b", "");
    const n = Number.parseInt(s, 2);
    return tkn(tt.int).lit(n);
  };
  const scanBigNumber = () => {
    let didSeeVBAR = false;
    while (isDigit(peek()) && !atEnd()) {
      tick();
    }
    if (peekIs("|") && isDigit(peekNext())) {
      tick(); // eat the '|'
      didSeeVBAR = true;
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
    }
    const n = slice().replace("#", "");
    if (didSeeVBAR) {
      const [a, b] = n.split("|");
      const N = BigInt(a);
      const D = BigInt(b);
      return tkn(tt.bigfraction).lit([N, D]);
    }
    return tkn(tt.bignumber).lit(BigInt(n));
  };

  /** Scans a token. */
  const scan = (): Token => {
    skipws();
    $start = $current;
    if (atEnd()) {
      return tkn(tt.END, "END");
    }
    const c = tick();
    if (isValidName(c)) {
      return word();
    }
    if (c === "#") {
      if (!isDigit(peek())) {
        return errorTkn(`Expected digits after “#”`, `scanning a bignumber`);
      } else {
        const out = scanBigNumber();
        return out;
      }
    }
    if (isDigit(c)) {
      if (c === "0" && match("b")) {
        return binaryNumber();
      } else if (c === "0" && match("o")) {
        return octalNumber();
      } else if (c === "0" && match("x")) {
        return hexNumber();
      }
      return number(tt.int);
    }
    // deno-fmt-ignore
    switch (c) {
      case ":": return tkn(tt.colon);
      case "&": return tkn(tt.amp);
      case "~": return tkn(tt.tilde);
      case "|": return tkn(tt.vbar);
      case "(": return tkn(tt.lparen);
      case ")": return tkn(tt.rparen);
      case "[": return tkn(tt.lbracket);
      case "]": return tkn(tt.rbracket);
      case "{": return tkn(tt.lbrace);
      case "}": return tkn(tt.rbrace);
      case ",": return tkn(tt.comma);
      case ".": {
        if (match('+')) {
          return tkn(tt.dot_add);
        } else if (match('-')) {
          return tkn(tt.dot_minus);
        } else if (match('*')) {
          return tkn(tt.dot_star);
        } else if (match('^')) {
          return tkn(tt.dot_caret);
        } else {
          return tkn(tt.dot);
        }
      }
      case "-": {
        if (peek()==='-' && peekNext()==='-') {
          while (peek()!=='\n' && !atEnd()) {
            tick();
          }
          return Token.empty;
        } else {
          return tkn(match('-') ? tt.minus_minus : tt.minus);
        }
      }
      case "+": return tkn(match('+') ? tt.plus_plus : tt.plus);
      case "*": return tkn(tt.star);
      case ";": return tkn(tt.semicolon);
      case '%': return tkn(tt.percent);
      case "/": return tkn(tt.slash);
      case "^": return tkn(tt.caret);
      case '!': return tkn(match('=') ? tt.neq : tt.bang);
      case '=': {
        if (peek()==='=' && peekNext()==='=') {
          while (peek()==='=') {
            tick();
          }
          while (!atEnd()) {
            tick();
            if (peek()==='=' && peekNext()==='=' && lookup(2)==='=') {
              break;
            }
          }
          if (atEnd()) {
            return errorTkn(`Unterminated block comment`, `scanning a “=”`);
          }
          while (peek()==='=') {
            tick();
          }
          return Token.empty;
        } else {
          return tkn(match('=') ? tt.deq : tt.eq);
        }
      }
      case '<': return tkn(match('=') ? tt.leq : tt.lt);
      case '>': return tkn(match('=') ? tt.geq : tt.gt);
      case `"`: return stringLiteral();
      case `'`: return algebraicString();
    }
    return errorTkn(`Unknown token: “${c}”`, "scanning");
  };
  const stream = () => {
    const out: Token[] = [];
    let prev = Token.empty;
    let now = scan();
    if (!now.isEmpty()) {
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
      if (k.isEmpty()) {
        continue;
      } else {
        peek = k;
      }
      // remove trailing commas
      if (prev.isRPD() && now.is(tt.comma) && peek.isRPD()) {
        continue;
      }
      out.push(now);
    }
    out.push(peek);
    return right(out);
  };

  const isDone = () => ($current >= input.length);

  return {
    stream,
    scan,
    isDone,
  };
}

class ParserState<STMT extends TREENODE, EXPR extends TREENODE> {
  /**
   * Property bound to the current error status.
   * If this variable is not bound to null, then
   * an error occurred. This variable should only
   * be modified through the panic method or
   * through the error method.
   */
  ERROR: null | Err = null;
  panic(error: Err) {
    this.ERROR = error;
    return this;
  }
  private lexer!: ReturnType<typeof lexical>;
  init(source: string) {
    this.lexer = lexical(source);
    this.next();
    return this;
  }
  prev: Token = Token.empty;
  cursor: number = -1;
  peek: Token = Token.empty;
  current: Token = Token.empty;
  lastExpression: EXPR;
  currentExpression: EXPR;
  lastStmt: nodekind;
  currentStmt: nodekind;
  source: string = "";
  constructor(nil: EXPR, emptyStmt: STMT) {
    this.lastExpression = nil;
    this.currentExpression = nil;
    this.lastStmt = emptyStmt.kind;
    this.currentStmt = emptyStmt.kind;
  }
  implicitSemicolonOK() {
    return (
      this.peek.is(tt.END) ||
      this.atEnd()
    );
  }
  newExpression<E extends EXPR>(expression: E) {
    const prev = this.currentExpression;
    this.currentExpression = expression;
    this.lastExpression = prev;
    return right(expression);
  }
  newStmt<S extends STMT>(statement: S) {
    const prev = this.currentStmt;
    this.currentStmt = statement.kind;
    this.lastStmt = prev;
    const out = right(statement);
    return out;
  }
  semicolonOK() {
    return (
      this.peek.is(tt.END) ||
      this.atEnd()
    );
  }
  next() {
    this.cursor++;
    this.current = this.peek;
    const nxtToken = this.lexer.scan();
    if (nxtToken.isError()) {
      this.ERROR = nxtToken.literal;
      return Token.END;
    }
    this.peek = nxtToken;
    return this.current;
  }
  atEnd() {
    return (this.lexer.isDone()) || (this.ERROR !== null);
  }
  error(message: string, phase: string) {
    const e = syntaxError(message, phase, this.current);
    this.ERROR = e;
    return left(e);
  }
  check(type: tt) {
    if (this.atEnd()) {
      return false;
    } else {
      return this.peek.is(type);
    }
  }
  nextIs(type: tt) {
    if (this.peek.is(type)) {
      this.next();
      return true;
    }
    return false;
  }
}

const enstate = <EXPR extends TREENODE, STMT extends TREENODE>(
  nil: EXPR,
  emptyStmt: STMT,
) => (new ParserState(nil, emptyStmt));

/** Parses the given string. */
function syntax(source: string) {
  const state = enstate<Expr, Statement>(nil(), exprStmt(nil(), -1))
    .init(
      source,
    );
  const this_expression = (t: Token) => {
    return state.newExpression(thisExpr(t));
  };
  const number: Parslet<Expr> = (t) => {
    if (t.isNumber()) {
      const out = t.is(tt.int)
        ? state.newExpression(integer(t.literal))
        : state.newExpression(float(t.literal));
      const peek = state.peek;
      if (peek.is(tt.lparen) || peek.is(tt.native) || peek.is(tt.symbol)) {
        const r = expr(bp.imul);
        if (r.isLeft()) {
          return r;
        }
        const right = r.unwrap();
        const star = token(tt.star, "*", peek.L, peek.C);
        const left = out.unwrap();
        return state.newExpression(
          grouped(binex(left, star, right)),
        );
      }
      return out;
    } else {
      return state.error(
        `Expected an integer, but got “${t.lexeme}”`,
        "parsing an integer",
      );
    }
  };

  const string_literal: Parslet<Expr> = (t) => {
    return state.newExpression(string(t.lexeme));
  };

  const scientific_number: Parslet<Expr> = (t) => {
    if (t.isScientific()) {
      const [a, b] = t.literal;
      const lhs = float(a);
      const rhs = binex(
        integer(10),
        token(tt.caret, "^", t.L, t.C),
        integer(b),
      );
      return state.newExpression(binex(
        lhs,
        token(tt.star, "*", t.L, t.C),
        rhs,
      ));
    } else {
      return state.error(
        `Unexpected scientific number`,
        "parsing a scientific number",
      );
    }
  };

  /**
   * Parses a {@link RelationalExpr|relational expression}.
   */
  const compare = (op: Token, lhs: Expr): Either<Err, RelationalExpr> => {
    const p = precof(op.type);
    return expr(p).chain((rhs) => {
      return state.newExpression(
        relation(lhs, op as Token<RelationalOperator>, rhs),
      );
    });
  };

  /**
   * Parses a right-associative
   * {@link AlgebraicBinaryExpr|algebraic binary expression}.
   */
  const rinfix = (op: Token, lhs: Expr): Either<Err, AlgebraicBinaryExpr> => {
    return expr(precof(op.type)).chain((rhs) => {
      const out = binex(lhs, op as Token<ArithmeticOperator>, rhs);
      return state.newExpression(out);
    });
  };

  /**
   * Parses an {@link AlgebraicBinaryExpr|algebraic binary expression}.
   */
  const infix = (
    op: Token,
    lhs: Expr,
  ): Either<Err, AlgebraicBinaryExpr | AssignExpr> => {
    if (state.nextIs(tt.eq)) {
      if (isVariable(lhs)) {
        const name = lhs;
        const r = expr();
        if (r.isLeft()) {
          return r;
        }
        const rhs = r.unwrap();
        const value = binex(lhs, op as Token<ArithmeticOperator>, rhs);
        return state.newExpression(assign(name, value));
      } else {
        return state.error(
          `Invalid lefthand side of assignment. Expected a variable to the left of “${op.lexeme}=”, but got “${lhs.toString()}".`,
          `parsing the complex assignment “${op.lexeme}=”`,
        );
      }
    }
    const p = precof(op.type);
    const RHS = expr(p);
    if (RHS.isLeft()) {
      return RHS;
    }
    const rhs = RHS.unwrap();

    const out = binex(lhs, op as Token<ArithmeticOperator>, rhs);
    return state.newExpression(out);
  };

  /**
   * Parses a {@link FractionExpr|rational number}.
   */
  const fraction = (op: Token): Either<Err, FractionExpr> => {
    if (op.isFraction()) {
      const [N, D] = op.literal;
      return state.newExpression(rational(floor(N), floor(abs(D))));
    } else {
      return state.error(
        `Unexpected rational number`,
        "parsing a rational number",
      );
    }
  };

  /**
   * Parses a {@link LogicalBinaryExpr|logical infix expression}.
   */
  const logic_infix = (
    op: Token,
    lhs: Expr,
  ): Either<Err, LogicalBinaryExpr> => {
    const p = precof(op.type);
    return expr(p).chain((rhs) => {
      return state.newExpression(
        logicalBinex(lhs, op as Token<BinaryLogicalOperator>, rhs),
      );
    });
  };

  /**
   * Parses a {@link BigNumber|big number}.
   */
  const big_number = (op: Token): Either<Err, BigNumber> => {
    if (op.isBigNumber()) {
      return state.newExpression(bigNumber(op.literal));
    } else {
      return state.error(
        `Unexpected big number literal`,
        `parsing an expression`,
      );
    }
  };

  /**
   * Parses a {@link RationalExpr|big rational}.
   */
  const big_rational = (op: Token): Either<Err, RationalExpr> => {
    if (op.isBigFraction()) {
      const [a, b] = op.literal;
      return state.newExpression(bigRational(a, b));
    } else {
      return state.error(
        `Unexpected big rational literal`,
        `parsing an expression`,
      );
    }
  };

  /**
   * Parses a {@link Bool|boolean literal}.
   */
  const boolean_literal = (op: Token): Either<Err, Bool> => {
    if (op.isBoolean()) {
      return state.newExpression(bool(op.literal));
    } else {
      return state.error(`Unexpected boolean literal`, `parsing an expression`);
    }
  };

  /**
   * Parses a {@link NumericConstant|numeric constant} or {@link Nil|nil}.
   */
  const constant = (op: Token): Either<Err, NumericConstant | Nil> => {
    const type = op.type;
    const erm = `Unexpected constant “${op.lexeme}”`;
    const src = `parsing an expression`;
    // deno-fmt-ignore
    switch (type) {
      case tt.nan: return state.newExpression(numericConstant(NaN, 'NAN'));
      case tt.inf: return state.newExpression(numericConstant(Infinity, 'Inf'));
      case tt.nil: return state.newExpression(nil());
      case tt.numeric_constant: {
        switch (op.lexeme) {
          case "pi": return state.newExpression(numericConstant(PI, "pi"));
          case 'e': return state.newExpression(numericConstant(E, 'e'))
          default: return state.error(erm, src);
        }
      }
      default: return state.error(erm, src);
    }
  };

  /**
   * Parses a {@link GroupExpr|parenthesized expression}.
   */
  const primary = (op: Token) => {
    const innerExpression = expr();
    if (innerExpression.isLeft()) {
      return innerExpression;
    }
    if (state.nextIs(tt.comma)) {
      const elements: Expr[] = [innerExpression.unwrap()];
      do {
        const e = expr();
        if (e.isLeft()) {
          return e;
        }
        elements.push(e.unwrap());
      } while (state.nextIs(tt.comma));
      if (!state.nextIs(tt.rparen)) {
        return state.error(
          `Expected “)” to close the tuple`,
          `parsing a tuple`,
        );
      }
      return state.newExpression(tupleExpr(elements));
    }
    if (!state.nextIs(tt.rparen)) {
      return state.error(
        `Expected closing “)”`,
        "parsing a parenthesized expression",
      );
    }
    return innerExpression.map((e) => grouped(e));
  };

  const allowImplicit = (kind: nodekind) => (
    kind === nodekind.algebraic_infix ||
    kind === nodekind.algebraic_unary ||
    kind === nodekind.float ||
    kind === nodekind.numeric_constant ||
    kind === nodekind.native_call ||
    kind === nodekind.integer ||
    kind === nodekind.grouped_expression
  );

  const function_call = (
    op: Token,
    node: Expr,
  ) => {
    const callee = node;
    if (isGroupExpr(callee) && allowImplicit(callee.expression.kind)) {
      const left = callee.expression;
      const r = expr();
      if (r.isLeft()) return r;
      if (!state.nextIs(tt.rparen)) {
        return state.error(
          `Expected a “)” to close the expression`,
          "parsing an implicit multiplication",
        );
      }
      const right = r.unwrap();
      const star = token(tt.star, "*", op.L, op.C);
      return state.newExpression(binex(left, star, right));
    }
    let args: Expr[] = [];
    if (!state.check(tt.rparen)) {
      const arglist = comma_separated_list(
        isExpr,
        `Expected expression`,
        "call",
      );
      if (arglist.isLeft()) return arglist;
      args = arglist.unwrap();
    }
    const paren = state.next();
    if (!paren.is(tt.rparen)) {
      return state.error(`Expected “)” to close args`, "call");
    }
    const out = call(callee, args, op);
    return state.newExpression(out);
  };

  /**
   * Parses a {@link NativeCall|native function call}.
   */
  const native_call: Parslet<Expr> = (op): Either<Err, NativeCall> => {
    const lex = op.lexeme;
    const src = `parsing a native call “${lex}”`;
    if (!state.nextIs(tt.lparen)) {
      return state.error(`Expected “(” to open the argument list`, src);
    }
    let args: Expr[] = [];
    if (!state.check(tt.rparen)) {
      const arglist = comma_separated_list(
        isExpr,
        `Expected expression`,
        src,
      );
      if (arglist.isLeft()) {
        return arglist;
      }
      args = arglist.unwrap();
    }
    if (!state.nextIs(tt.rparen)) {
      return state.error(`Expected “)” to close the argument list`, src);
    }
    return state.newExpression(
      nativeCall(op as Token<tt.native, string, NativeFn>, args),
    );
  };

  /**
   * Parses a variable name.
   */
  const variable_name: Parslet<Expr> = (op) => {
    if (op.isVariable()) {
      const out = variable(op);
      return state.newExpression(out);
    } else {
      return state.error(
        `Unexpected variable “${op.lexeme}”`,
        "parsing expression",
      );
    }
  };

  /**
   * Parses a logical not expression.
   */
  const logical_not: Parslet<Expr> = (op) => {
    const p = precof(op.type);
    return expr(p).chain((arg) =>
      state.newExpression(logicalUnary(op as Token<tt.not>, arg))
    );
  };

  /**
   * Parses an {@link AssignExpr|assignment expression}.
   */
  const assignment = (
    op: Token,
    node: Expr,
  ): Either<Err, AssignExpr | SetExpr> => {
    const src = `parsing an assignment`;
    if (isVariable(node)) {
      return expr().chain((n) => {
        return state.newExpression(assign(node, n));
      });
    } else if (isGetExpr(node)) {
      const rhs = expr();
      if (rhs.isLeft()) {
        return rhs;
      }
      return state.newExpression(
        setExpr(node.object, node.name, rhs.unwrap(), op.loc()),
      );
    } else {
      return state.error(
        `Expected a valid assignment target, but got “${node.toString()}”`,
        src,
      );
    }
  };

  const comma_separated_list = <K extends Expr>(
    filter: (e: Expr) => e is K,
    errorMessage: string,
    src: string,
  ) => {
    const elements: K[] = [];
    do {
      const e = expr();
      if (e.isLeft()) return e;
      const element = e.unwrap();
      if (!filter(element)) {
        return state.error(errorMessage, src);
      }
      elements.push(element);
    } while (state.nextIs(tt.comma));
    return right(elements);
  };

  const vector_expression = (prev: Token) => {
    const elements: Expr[] = [];
    const vectors: VectorExpr[] = [];
    const src = `parsing a vector expression`;
    let rows = 0;
    let columns = 0;
    if (!state.check(tt.rbracket)) {
      do {
        const elem = expr();
        if (elem.isLeft()) {
          return elem;
        }
        const element = elem.unwrap();
        if (isVectorExpr(element)) {
          rows++;
          columns = element.elements.length;
          vectors.push(element);
        } else {
          elements.push(element);
        }
      } while (state.nextIs(tt.comma) && !state.atEnd());
    }
    if (!state.nextIs(tt.rbracket)) {
      return state.error(
        `Expected a right bracket “]” to close the vector`,
        src,
      );
    }
    if (vectors.length !== 0) {
      if (vectors.length !== columns) {
        return state.error(
          `Encountered a jagged matrix. Jagged matrices are not permitted. For jagged lists, consider using nested tuples.`,
          src,
        );
      }
      return state.newExpression(matrixExpr(vectors, rows, columns));
    }
    return state.newExpression(vectorExpr(elements, prev));
  };

  const get_expression = (op: Token, lhs: Expr) => {
    const src = `parsing a get expression`;
    const nxt = state.next();
    if (!nxt.isVariable()) {
      return state.error(`Expected property name`, src);
    }
    let exp = getExpr(lhs, nxt, op.loc());
    if (state.nextIs(tt.lparen)) {
      const args: Expr[] = [];
      if (!state.check(tt.rparen)) {
        do {
          const x = expr();
          if (x.isLeft()) {
            return x;
          }
          const arg = x.unwrap();
          args.push(arg);
        } while (state.nextIs(tt.comma));
      }
      const rparen = state.next();
      if (!rparen.is(tt.rparen)) {
        return state.error(`Expected “)” to after method arguments`, src);
      }
      return state.newExpression(call(exp, args, op));
    }
    return state.newExpression(exp);
  };

  const indexing_expression: Parslet<Expr> = (op, lhs) => {
    const index = expr();
    if (index.isLeft()) {
      return index;
    }
    const rbracket = state.next();
    if (!rbracket.is(tt.rbracket)) {
      return state.error(
        `Expected a right bracket “]” to close the accessor`,
        `parsing an index accessor`,
      );
    }
    return state.newExpression(indexingExpr(lhs, index.unwrap(), rbracket));
  };

  const factorial_expression = (op: Token, node: Expr) => {
    return state.newExpression(
      algebraicUnary(op as Token<AlgebraicUnaryOperator>, node),
    );
  };

  const implicitMUL: Parslet<Expr> = (op, left) => {
    if (op.is(tt.symbol)) {
      const right = variable(op);
      const star = token(tt.star, "*", op.L, op.C);
      return state.newExpression(binex(
        left,
        star,
        right,
      ));
    } else {
      return state.error(
        `Expected a symbol for implicit multiplication, but got “${op.lexeme}”`,
        "parsing implicit multiplication",
      );
    }
  };

  const decrement = (op: Token, node: Expr) => {
    if (isVariable(node)) {
      const right = binex(
        node,
        op.entype(tt.minus).lex("-"),
        integer(1),
      );
      return state.newExpression(assign(node, right));
    } else {
      return state.error(
        `Expected the lefthand side of “--” to be either a variable or a property accessor, but got “${node.toString()}”`,
        `parsing a decrement “--”`,
      );
    }
  };

  const increment = (op: Token, node: Expr) => {
    if (isVariable(node)) {
      const right = binex(
        node,
        op.entype(tt.plus).lex("+"),
        integer(1),
      );
      return state.newExpression(assign(node, right));
    } else {
      return state.error(
        `Expected the lefthand side of “++” to be either a variable or a property accessor, but got “${node.toString()}”`,
        `parsing an increment “++”`,
      );
    }
  };

  /**
   * The “blank” parslet. This parslet is used as a placeholder.
   * If the {@link expr|expression parser} calls this parslet,
   * then the {@link error} variable is set and parsing shall cease.
   */
  const ___: Parslet<Expr> = (t) => {
    if (state.ERROR !== null) {
      return left(state.ERROR);
    } else {
      return state.error(`Unexpected lexeme: ${t.lexeme}`, `expression`);
    }
  };

  const vector_infix: Parslet<Expr> = (op, left) => {
    const p = precof(op.type);
    return expr(p).chain((right) => {
      return state.newExpression(
        vectorBinaryExpr(left, op as Token<VectorBinaryOP>, right),
      );
    });
  };

  const string_infix: Parslet<Expr> = (op, left) => {
    const p = precof(op.type);
    return expr(p).chain((right) => {
      return state.newExpression(
        stringBinex(left, op as Token<StringBinop>, right),
      );
    });
  };

  const algebraic_string: Parslet<Expr> = (op) => {
    if (op.isAlgebraString()) {
      const tkns = op.literal;
      const t = token(tt.algebra_string, "", op.L, op.C);
      const result = syntax(tkns).expression();
      if (result.isLeft()) {
        return result;
      }
      const expression = result.unwrap();
      return state.newExpression(algebraicString(expression, t));
    } else {
      return state.error(
        `Unexpected algebraic string`,
        `parsing an expression`,
      );
    }
  };

  const prefix: Parslet<Expr> = (op) => {
    const p = precof(op.type);
    return expr(p).chain((arg) => {
      if (op.is(tt.minus)) {
        return state.newExpression(algebraicUnary(op, arg));
      } else if (op.is(tt.plus)) {
        return state.newExpression(algebraicUnary(op, arg));
      } else {
        return state.error(
          `Unknown prefix operator “${op.lexeme}”`,
          `parsing a prefix operation`,
        );
      }
    });
  };

  /**
   * The “blank” binding power. This particular binding power
   * is bound either (1) the {@link ___|blank parslet}
   * or (2) parlsets that should not trigger recursive calls.
   */
  const ___o = bp.nil;

  /**
   * The rules table comprises mappings from every
   * {@link tt|token type} to a triple `(Prefix, Infix, B)`,
   * where `Prefix` and `Infix` are {@link Parslet|parslets} (small
   * parsers that handle a single grammar rule), and `B` is a
   * {@link bp|binding power}.
   */
  const rules: BPTable<Expr> = {
    [tt.END]: [___, ___, ___o],
    [tt.ERROR]: [___, ___, ___o],
    [tt.EMPTY]: [___, ___, ___o],
    [tt.lparen]: [primary, function_call, bp.call], // done
    [tt.rparen]: [___, ___, ___o],
    [tt.lbrace]: [___, ___, ___o],
    [tt.rbrace]: [___, ___, ___o],
    [tt.lbracket]: [vector_expression, indexing_expression, bp.call], // done
    [tt.rbracket]: [___, ___, ___o],
    [tt.semicolon]: [___, ___, ___o],
    [tt.colon]: [___, ___, ___o],
    [tt.dot]: [___, get_expression, bp.call], // done
    [tt.comma]: [___, ___, ___o],
    [tt.super]: [___, ___, ___o],

    [tt.amp]: [___, string_infix, bp.stringop], // done
    [tt.tilde]: [___, ___, ___o],
    [tt.vbar]: [___, ___, ___o],
    [tt.eq]: [___, assignment, bp.assign], // done
    [tt.bang]: [___, factorial_expression, bp.postfix], // done
    [tt.plus_plus]: [___, increment, bp.postfix], // done
    [tt.minus_minus]: [___, decrement, bp.postfix], // done
    
    // vector expressions
    [tt.dot_add]: [___, vector_infix, bp.sum], // done
    [tt.dot_minus]: [___, vector_infix, bp.sum], // done
    [tt.dot_star]: [___, vector_infix, bp.product], // done
    [tt.dot_caret]: [___, vector_infix, bp.power], // done

    // algebraic expressions
    [tt.plus]: [prefix, infix, bp.sum], // done
    [tt.minus]: [prefix, infix, bp.difference], // done
    [tt.star]: [___, infix, bp.product], // done
    [tt.slash]: [___, infix, bp.quotient], // done
    [tt.caret]: [___, rinfix, bp.power], // done
    [tt.percent]: [___, infix, bp.quotient], // done
    [tt.rem]: [___, infix, bp.quotient], // done
    [tt.mod]: [___, infix, bp.quotient], // done
    [tt.div]: [___, infix, bp.quotient], // done

    // comparison expressions
    [tt.lt]: [___, compare, bp.rel], // done
    [tt.gt]: [___, compare, bp.rel], // done
    [tt.neq]: [___, compare, bp.rel], // done
    [tt.leq]: [___, compare, bp.rel], // done
    [tt.geq]: [___, compare, bp.rel], // done
    [tt.deq]: [___, compare, bp.rel], // done

    // logical binary expressions
    [tt.nand]: [___, logic_infix, bp.nand], // done
    [tt.xor]: [___, logic_infix, bp.xor], // done
    [tt.xnor]: [___, logic_infix, bp.xnor], // done
    [tt.nor]: [___, logic_infix, bp.nor], // done
    [tt.and]: [___, logic_infix, bp.and], // done
    [tt.or]: [___, logic_infix, bp.or], // done
    [tt.not]: [logical_not, ___, bp.not], // done

    // literals
    [tt.symbol]: [variable_name, implicitMUL, bp.atom], // done
    [tt.string]: [string_literal, ___, bp.atom], // done
    [tt.bool]: [boolean_literal, ___, bp.atom], // done
    [tt.int]: [number, ___, bp.atom], // done
    [tt.float]: [number, ___, bp.atom], // done
    [tt.bignumber]: [big_number, ___, bp.atom],
    [tt.bigfraction]: [big_rational, ___, bp.atom],
    [tt.scientific]: [scientific_number, ___, bp.atom],
    [tt.fraction]: [fraction, ___, bp.atom], // done
    [tt.nan]: [constant, ___, bp.atom], // done
    [tt.inf]: [constant, ___, bp.atom], // done
    [tt.nil]: [constant, ___, bp.atom], // done
    [tt.numeric_constant]: [constant, ___, bp.atom], // done
    [tt.this]: [this_expression, ___, bp.atom],
    [tt.algebra_string]: [algebraic_string, ___, bp.atom], // done

    // native calls
    [tt.native]: [native_call, ___, bp.call], // done

    [tt.if]: [___, ___, ___o],
    [tt.else]: [___, ___, ___o],
    [tt.fn]: [___, ___, ___o],
    [tt.let]: [___, ___, ___o],
    [tt.var]: [___, ___, ___o],
    [tt.return]: [___, ___, ___o],
    [tt.while]: [___, ___, ___o],
    [tt.for]: [___, ___, ___o],
    [tt.class]: [___, ___, ___o],
    [tt.print]: [___, ___, ___o],
  };
  /**
   * Returns the prefix parsing rule mapped to by the given
   * token type.
   */
  const prefixRule = (t: tt): Parslet<Expr> => rules[t][0];

  /**
   * Returns the infix parsing rule mapped to by the given
   * token type.
   */
  const infixRule = (t: tt): Parslet<Expr> => rules[t][1];

  /**
   * Returns the {@link bp|precedence} of the given token type.
   */
  const precof = (t: tt): bp => rules[t][2];

  /**
   * Parses an {@link Expr|conventional expression} via
   * Pratt parsing.
   */
  const expr = (minbp: number = bp.lowest): Either<Err, Expr> => {
    let token = state.next();
    const pre = prefixRule(token.type);
    let lhs = pre(token, nil());
    if (lhs.isLeft()) {
      return lhs;
    }
    while (minbp < precof(state.peek.type)) {
      if (state.atEnd()) {
        break;
      }
      token = state.next();
      const r = infixRule(token.type);
      const rhs = r(token, lhs.unwrap());
      if (rhs.isLeft()) {
        return rhs;
      }
      lhs = rhs;
    }
    return lhs;
  };

  const PRINT = () => {
    const current = state.current;
    // print eaten in STMT
    const arg = EXPRESSION();
    return arg.map((x) => printStmt(current, x.expression));
  };

  /**
   * Parses an {@link IfStmt|if-statement}.
   */
  const IF = (): Either<Err, IfStmt> => {
    const keyword = state.current;
    const c = expr();
    const src = `parsing an if-statement`;
    if (c.isLeft()) {
      return c;
    }
    const condition = c.unwrap();
    if (!state.nextIs(tt.lbrace)) {
      return state.error(
        `Expected a left brace “{” to begin the consequent block.`,
        src,
      );
    }
    const consequent = BLOCK();
    if (consequent.isLeft()) {
      return consequent;
    }
    const thenBranch = consequent.unwrap();
    let elseBranch: Statement = returnStmt(
      nil(),
      state.current,
    );
    if (state.nextIs(tt.else)) {
      const _else = STMT();
      if (_else.isLeft()) {
        return _else;
      }
      elseBranch = _else.unwrap();
    }
    return state.newStmt(ifStmt(keyword, condition, thenBranch, elseBranch));
  };

  /**
   * Parses a {@link FnStmt|function statement} (i.e., a function declaration).
   */
  const FN = (): Either<Err, FnStmt> => {
    // fn eaten in STMT
    const name = state.next();
    const src = `parsing a function a declaration`;
    if (!name.isVariable()) {
      return state.error(
        `Expected a valid identifier for the function’s name, but got “${name.lexeme}”.`,
        src,
      );
    }
    if (!state.nextIs(tt.lparen)) {
      return state.error(
        `Expected a left parenthesis “(” to begin the parameter list`,
        src,
      );
    }
    const params: Token<tt.symbol>[] = [];
    if (!state.peek.is(tt.rparen)) {
      do {
        const expression = state.next();
        if (!expression.isVariable()) {
          return state.error(
            `Expected a valid identifier as a parameter, but got “${expression.lexeme}”`,
            src,
          );
        }
        params.push(expression);
      } while (state.nextIs(tt.comma));
    }
    if (!state.nextIs(tt.rparen)) {
      return state.error(
        `Expected a right parenthesis “)” to close the parameter list`,
        src,
      );
    }
    if (state.nextIs(tt.eq)) {
      const body = EXPRESSION();
      return body.chain((b) => state.newStmt(functionStmt(name, params, [b])));
    }
    if (!state.nextIs(tt.lbrace)) {
      return state.error(
        `Expected a left-brace “{” to open the function’s body. If this function’s body is composed of a single statement, consider using the assignment operator “=”`,
        src,
      );
    }
    const body = BLOCK();
    return body.chain((b) =>
      state.newStmt(functionStmt(name, params, b.statements))
    );
  };

  const WHILE = () => {
    const current = state.current;
    const src = `parsing a while loop`;
    const loopCondition = expr();
    if (loopCondition.isLeft()) {
      return loopCondition;
    }
    if (!state.nextIs(tt.lbrace)) {
      return state.error(`Expected a block after the condition`, src);
    }
    const body = BLOCK();
    if (body.isLeft()) {
      return body;
    }
    return body.chain((loopBody) =>
      state.newStmt(whileStmt(current, loopCondition.unwrap(), loopBody))
    );
  };

  /**
   * Parses a {@link BlockStmt|block statement}.
   */
  const BLOCK = (): Either<Err, BlockStmt> => {
    const statements: Statement[] = [];
    while (!state.atEnd() && !state.check(tt.rbrace)) {
      const stmt = STMT();
      if (stmt.isLeft()) {
        return stmt;
      }
      statements.push(stmt.unwrap());
    }
    if (!state.nextIs(tt.rbrace)) {
      return state.error(
        `Expected a right brace “}” to close the block`,
        `parsing a block`,
      );
    }
    return state.newStmt(block(statements));
  };

  /**
   * Parses a {@link VariableStmt|let statement}.
   */
  const VAR = (prev: tt.let | tt.var): Either<Err, VariableStmt> => {
    const src = `parsing a variable declaration`;
    const name = state.next();
    if (!name.isVariable()) {
      return state.error(`Expected a valid identifier`, src);
    }
    if (!state.nextIs(tt.eq)) {
      return state.error(`Expected an assignment operator “=”`, src);
    }
    const init = EXPRESSION();
    if (init.isLeft()) {
      return init;
    }
    const value = init.unwrap();
    return state.newStmt(
      (prev === tt.let ? letStmt : varStmt)(name, value.expression),
    );
  };

  /**
   * Parses an expression statement.
   */
  const EXPRESSION = (): Either<Err, ExprStmt> => {
    const out = expr();
    if (out.isLeft()) {
      return out;
    }
    const expression = out.unwrap();
    const line = state.peek.L;
    if (state.nextIs(tt.semicolon) || state.implicitSemicolonOK()) {
      return state.newStmt(exprStmt(expression, line));
    }
    return state.error(
      `Expected “;” to end the statement`,
      "expression-statement",
    );
  };

  const RETURN = (): Either<Err, ReturnStmt> => {
    const c = state.current;
    const out = EXPRESSION();
    return out.chain((e) => state.newStmt(returnStmt(e.expression, c)));
  };

  const FOR = (): Either<Err, Statement> => {
    // keyword 'for' eaten by STMT
    const current = state.current;
    const src = `parsing a for-loop`;
    const preclauseToken = state.next();
    if (!preclauseToken.is(tt.lparen)) {
      return state.error(
        `Expected a left parentheses “(” after the keyword “for” to begin the loop’s clauses, but got “${preclauseToken.lexeme}”.`,
        src,
      );
    }
    let init: Statement | null = null;
    if (state.nextIs(tt.semicolon)) {
      init = init;
    } else if (state.nextIs(tt.var)) {
      const initializer = VAR(tt.var);
      if (initializer.isLeft()) {
        return initializer;
      }
      init = initializer.unwrap();
    } else {
      const exp = EXPRESSION();
      if (exp.isLeft()) {
        return exp;
      }
      init = exp.unwrap();
    }
    let condition: Expr | null = null;
    if (!state.check(tt.semicolon)) {
      const c = expr();
      if (c.isLeft()) {
        return c;
      }
      condition = c.unwrap();
    }
    const postConditionToken = state.next();
    if (!postConditionToken.is(tt.semicolon)) {
      return state.error(
        `Expected a semicolon “;” after the for-loop condition, but got “${postConditionToken.lexeme}”.`,
        src,
      );
    }
    let increment: Expr | null = null;
    if (!state.check(tt.rparen)) {
      const inc = expr();
      if (inc.isLeft()) {
        return inc;
      }
      increment = inc.unwrap();
    }
    const postIncrementToken = state.next();
    if (!postIncrementToken.is(tt.rparen)) {
      return state.error(
        `Expected a right “)” to close the for-loop’s clauses, but got “${postIncrementToken.lexeme}”`,
        src,
      );
    }
    const b = STMT();
    if (b.isLeft()) {
      return b;
    }
    const bodyLine = state.current.L;
    let body: Statement = b.unwrap();
    if (increment !== null) {
      if (isBlock(body)) {
        body.statements.push(exprStmt(increment, bodyLine));
      } else {
        body = block([body, exprStmt(increment, bodyLine)]);
      }
    }
    let loopCondition: Expr = bool(true);
    if (condition !== null) {
      loopCondition = condition;
    }
    body = whileStmt(current, loopCondition, body);
    if (init !== null) {
      body = block([init, body]);
    }
    return state.newStmt(body);
  };

  const CLASS = () => {
    // class keyword eaten in Stmt
    const src = `parsing a class declaration`;
    const name = state.next();
    if (!name.isVariable()) {
      return state.error(
        `Expected a class name after “class”, but got “${name.lexeme}”`,
        src,
      );
    }
    const lbrace = state.next();
    if (!lbrace.is(tt.lbrace)) {
      return state.error(
        `Expected a left-brace “{” to begin the body of class “${name.lexeme}”, but got “${lbrace.lexeme}”`,
        src,
      );
    }
    const methods = [];
    while (!state.check(tt.rbrace) && !state.atEnd()) {
      const f = FN();
      if (f.isLeft()) {
        return f;
      }
      methods.push(f.unwrap());
    }
    const postMethodsToken = state.next();
    if (!postMethodsToken.is(tt.rbrace)) {
      return state.error(
        `Expected a right brace “}” after the body of class “${name.lexeme}”, but got “${postMethodsToken.lexeme}”.`,
        src,
      );
    }
    return state.newStmt(classStmt(name, methods));
  };

  /**
   * Parses a statement.
   */
  const STMT = (): Either<Err, Statement> => {
    if (state.nextIs(tt.var)) {
      return VAR(tt.var);
    } else if (state.nextIs(tt.let)) {
      return VAR(tt.let);
    } else if (state.nextIs(tt.fn)) {
      return FN();
    } else if (state.nextIs(tt.lbrace)) {
      return BLOCK();
    } else if (state.nextIs(tt.if)) {
      return IF();
    } else if (state.nextIs(tt.return)) {
      return RETURN();
    } else if (state.nextIs(tt.while)) {
      return WHILE();
    } else if (state.nextIs(tt.for)) {
      return FOR();
    } else if (state.nextIs(tt.print)) {
      return PRINT();
    } else if (state.nextIs(tt.class)) {
      return CLASS();
    } else {
      return EXPRESSION();
    }
  };

  return {
    /**
     * Returns a syntax analysis of
     * a single expression.
     */
    expression() {
      // The error is not null if
      // an error occurred during
      // scanning. In that case we
      // immediately return the scanner’s
      // reported error.
      if (state.ERROR !== null) {
        return left(state.ERROR);
      }
      const out = expr();
      return out;
    },
    /**
     * Returns a syntax analysis of the entire
     * program with statements.
     */
    statements() {
      // Similar to analyzeExpression, we
      // immediately return the scanner’s
      // reported error.
      if (state.ERROR !== null) {
        return left(state.ERROR);
      }
      const stmts: Statement[] = [];
      while (!state.atEnd()) {
        const stmt = STMT();
        if (stmt.isLeft()) {
          return stmt;
        }
        stmts.push(stmt.unwrap());
      }
      return right(stmts);
    },
  };
}

/** Parses an algebraic expression. */
function exp(source: string) {
  const lexer = lexical(source);
  let $peek = Token.empty;
  let $current = Token.empty;
  let $error: null | Err = null;
  let $lastExpression: AlgebraicExpression = Undefined("null");
  let $cursor = 0;
  const error = (message: string, phase: string, token: Token = $current) => {
    const e = syntaxError(message, phase, token);
    $error = e;
    return left(e);
  };

  const next = () => {
    $cursor++;
    $current = $peek;
    const token = lexer.scan();
    if (token.isError()) {
      $error = error(token.lexeme, "scanner", token).unwrap();
    }
    $peek = token;
    return $current;
  };

  const atEnd = () => (lexer.isDone()) || ($error !== null);

  const node = (of: AlgebraicExpression) => {
    $lastExpression = of;
    return right(of);
  };

  const ___: Parslet<AlgebraicExpression> = (t) => {
    if ($error !== null) {
      return left($error);
    } else {
      return error(`Unexpected lexeme: ${t.lexeme}`, `algebraic-expression`);
    }
  };

  const ___o = bp.nil;

  const NUMBER: Parslet<AlgebraicExpression> = (t) => {
    if (t.isNumber()) {
      const n = t.literal;
      const out = t.is(tt.int) ? int(n) : real(n);
      if ($peek.is(tt.lparen) || $peek.is(tt.native) || $peek.is(tt.symbol)) {
        const r = expr(bp.imul);
        if (r.isLeft()) return r;
        const rhs = r.unwrap();
        const lhs = out;
        if (isProduct(rhs)) {
          return node(product([lhs, ...rhs._args]));
        } else {
          return node(product([lhs, rhs]));
        }
      }
      return node(out);
    } else {
      return error(`Unexpected number “${t.lexeme}”`, "parsing a number", t);
    }
  };
  const nextIs = (t: tt) => {
    if ($peek.is(t)) {
      next();
      return true;
    } else {
      return false;
    }
  };

  const PRIMARY: Parslet<AlgebraicExpression> = (op) => {
    const innerExpr = expr();
    if (innerExpr.isLeft()) {
      return innerExpr;
    }
    if (!nextIs(tt.rparen)) {
      return error(
        `Expected “)” to close the expression`,
        `parsing an algebraic expression`,
      );
    }
    const out = node(innerExpr.unwrap().parend());
    return out;
  };

  const PRODUCT: Parslet<AlgebraicExpression> = (op, lhs) => {
    const p = precof(op.type);
    const RHS = expr(p);
    if (RHS.isLeft()) return RHS;
    const rhs = RHS.unwrap();
    let args: AlgebraicExpression[] = [];
    if (isProduct(rhs) && isProduct(lhs)) {
      lhs._args.forEach((x) => args.push(x));
      rhs._args.forEach((x) => args.push(x));
      return node(product(args));
    }
    if (isProduct(rhs) && !isProduct(lhs)) {
      args.push(lhs);
      rhs._args.forEach((x) => args.push(x));
      return node(product(args));
    }
    if (!isProduct(rhs) && isProduct(lhs)) {
      lhs._args.forEach((x) => args.push(x));
      args.push(rhs);
      return node(product(args));
    }
    return node(product([lhs, rhs]));
  };

  const SUM: Parslet<AlgebraicExpression> = (op, lhs) => {
    const p = precof(op.type);
    const RHS = expr(p);
    if (RHS.isLeft()) return RHS;
    const rhs = RHS.unwrap();
    let args: AlgebraicExpression[] = [];
    if (isSum(rhs) && isSum(lhs)) {
      lhs._args.forEach((x) => args.push(x));
      rhs._args.forEach((x) => args.push(x));
      return node(sum(args));
    }
    if (isSum(rhs) && !isSum(lhs)) {
      args.push(lhs);
      rhs._args.forEach((x) => args.push(x));
      return node(sum(args));
    }
    if (!isSum(rhs) && isSum(lhs)) {
      lhs._args.forEach((x) => args.push(x));
      args.push(rhs);
      return node(sum(args));
    }
    return node(sum([lhs, rhs]));
  };

  const DIFFERENCE: Parslet<AlgebraicExpression> = (op, lhs) => {
    const p = precof(op.type);
    const RHS = expr(p);
    if (RHS.isLeft()) return RHS;
    const rhs = RHS.unwrap();
    return node(difference([lhs, rhs]));
  };

  const QUOTIENT: Parslet<AlgebraicExpression> = (op, lhs) => {
    const p = precof(op.type);
    const RHS = expr(p);
    if (RHS.isLeft()) return RHS;
    const rhs = RHS.unwrap();
    return node(quotient(lhs, rhs));
  };

  const POWER: Parslet<AlgebraicExpression> = (op, lhs) => {
    const RHS = expr(bp.power);
    if (RHS.isLeft()) return RHS;
    const rhs = RHS.unwrap();
    return node(power(lhs, rhs));
  };

  const SYMBOL: Parslet<AlgebraicExpression> = (op) => {
    if (op.isVariable()) {
      const out = sym(op.lexeme);
      return node(out);
    } else {
      return error(
        `Unexpected variable “${op.lexeme}”`,
        "parsing an expression",
        op,
      );
    }
  };

  const FRACTION: Parslet<AlgebraicExpression> = (op) => {
    if (op.isFraction()) {
      const [a, b] = op.literal;
      return node(frac(a, b));
    } else {
      return error(
        `Unexpected fraction “${op.lexeme}”`,
        `parsing an expression`,
        op,
      );
    }
  };

  const NEGATE: Parslet<AlgebraicExpression> = (op) => {
    const e = expr(op.type);
    return e.chain((x) => node(negate(x)));
  };

  const rules: BPTable<AlgebraicExpression> = {
    [tt.END]: [___, ___, ___o],
    [tt.ERROR]: [___, ___, ___o],
    [tt.EMPTY]: [___, ___, ___o],
    [tt.lparen]: [___, ___, bp.call],
    [tt.rparen]: [___, ___, ___o],
    [tt.lbrace]: [___, ___, ___o],
    [tt.rbrace]: [___, ___, ___o],
    [tt.lbracket]: [___, ___, ___o],
    [tt.rbracket]: [___, ___, ___o],
    [tt.semicolon]: [___, ___, ___o],
    [tt.colon]: [___, ___, ___o],
    [tt.dot]: [___, ___, ___o],
    [tt.comma]: [___, ___, ___o],
    [tt.plus]: [___, SUM, bp.sum],
    [tt.minus]: [NEGATE, DIFFERENCE, bp.sum],
    [tt.star]: [___, PRODUCT, bp.product],
    [tt.slash]: [___, QUOTIENT, bp.quotient],
    [tt.caret]: [___, POWER, bp.power],
    [tt.percent]: [___, ___, ___o],
    [tt.bang]: [___, ___, ___o],
    [tt.amp]: [___, ___, ___o],
    [tt.tilde]: [___, ___, ___o],
    [tt.vbar]: [___, ___, ___o],
    [tt.eq]: [___, ___, ___o],
    [tt.lt]: [___, ___, ___o],
    [tt.gt]: [___, ___, ___o],
    [tt.neq]: [___, ___, ___o],
    [tt.leq]: [___, ___, ___o],
    [tt.geq]: [___, ___, ___o],
    [tt.deq]: [___, ___, ___o],
    [tt.plus_plus]: [___, ___, ___o],
    [tt.minus_minus]: [___, ___, ___o],
    [tt.symbol]: [SYMBOL, ___, bp.atom],
    [tt.string]: [___, ___, ___o],
    [tt.bool]: [___, ___, ___o],
    [tt.int]: [NUMBER, ___, bp.atom],
    [tt.float]: [NUMBER, ___, bp.atom],
    [tt.bignumber]: [___, ___, ___o],
    [tt.bigfraction]: [___, ___, ___o],
    [tt.scientific]: [___, ___, ___o],
    [tt.fraction]: [FRACTION, ___, bp.atom],
    [tt.nan]: [___, ___, ___o],
    [tt.inf]: [___, ___, ___o],
    [tt.nil]: [___, ___, ___o],
    [tt.numeric_constant]: [___, ___, ___o],
    [tt.algebra_string]: [___, ___, ___o],
    [tt.dot_add]: [___, ___, ___o],
    [tt.dot_star]: [___, ___, ___o],
    [tt.dot_minus]: [___, ___, ___o],
    [tt.dot_caret]: [___, ___, ___o],
    [tt.native]: [___, ___, ___o],
    [tt.and]: [___, ___, ___o],
    [tt.or]: [___, ___, ___o],
    [tt.not]: [___, ___, ___o],
    [tt.nand]: [___, ___, ___o],
    [tt.xor]: [___, ___, ___o],
    [tt.xnor]: [___, ___, ___o],
    [tt.nor]: [___, ___, ___o],
    [tt.if]: [___, ___, ___o],
    [tt.else]: [___, ___, ___o],
    [tt.fn]: [___, ___, ___o],
    [tt.let]: [___, ___, ___o],
    [tt.var]: [___, ___, ___o],
    [tt.return]: [___, ___, ___o],
    [tt.while]: [___, ___, ___o],
    [tt.for]: [___, ___, ___o],
    [tt.class]: [___, ___, ___o],
    [tt.print]: [___, ___, ___o],
    [tt.super]: [___, ___, ___o],
    [tt.this]: [___, ___, ___o],
    [tt.rem]: [___, ___, ___o],
    [tt.mod]: [___, ___, ___o],
    [tt.div]: [___, ___, ___o],
  };
  const prefixRule = (t: tt): Parslet<AlgebraicExpression> => rules[t][0];
  const infixRule = (t: tt): Parslet<AlgebraicExpression> => rules[t][1];
  const precof = (t: tt): bp => rules[t][2];

  const expr = (
    minbp: number = bp.lowest,
  ): Either<Err, AlgebraicExpression> => {
    let token = next();
    const pre = prefixRule(token.type);
    let lhs = pre(token, Undefined("null"));
    if (lhs.isLeft()) {
      return lhs;
    }
    while (minbp < precof($peek.type)) {
      if (atEnd()) {
        break;
      }
      token = next();
      const r = infixRule(token.type);
      const rhs = r(token, lhs.unwrap());
      if (rhs.isLeft()) {
        return rhs;
      }
      lhs = rhs;
    }
    return lhs;
  };

  return {
    /** Returns the AlgebraicExpression tree corresponding to the expression.*/
    parse: () => {
      next();
      const out = expr();
      return out;
    },
  };
}

/** Reduces the given fraction or integer to either a fraction in standard form or an integer.  */
export function simplifyRationalNumber(
  u: AlgebraicExpression,
): Fraction | Int | UNDEFINED {
  if (!isInt(u) && !isFrac(u)) {
    return Undefined(
      `In call to simplifyRationalNumber, the argument ${u} is neither an Int nor a Fraction.`,
    );
  }
  if (isInt(u)) {
    return u;
  } else {
    const n = u._n;
    const d = u._d;
    if (rem(n, d) === 0) {
      return int(quot(n, d));
    } else {
      const g = gcd(n, d);
      if (d > 0) {
        return frac(quot(n, g), quot(d, g));
      } else if (d < 0) {
        return frac(quot(-n, g), quot(-d, g));
      } else {
        return u;
      }
    }
  }
}
function numeratorOf(u: Int_OR_Frac) {
  return u._n;
}
function denominatorOf(u: Int_OR_Frac) {
  return u._d;
}

type Int_OR_Frac = Int | Fraction;
type Product_OR_Sum = Product | Sum;

function isIntorFrac(u: AlgebraicExpression): u is Int_OR_Frac {
  return (isInt(u) || isFrac(u));
}
function isProductOrSum(u: AlgebraicExpression): u is Product_OR_Sum {
  return (isProduct(u) || isSum(u));
}

/** Evaluates an integer or fraction quotient. */
export function evaluateQuotient(v: Int_OR_Frac, w: Int_OR_Frac) {
  if (numeratorOf(w) === 0) {
    return Undefined(
      `In call to evaluateQuotient, the numerator of argument “w” is 0.`,
    );
  } else {
    const N = numeratorOf(v) * denominatorOf(w);
    const D = numeratorOf(w) * denominatorOf(v);
    return frac(N, D);
  }
}

/** Evaluates an integer or fraction product. */
export function evaluateProduct(v: Int_OR_Frac, w: Int_OR_Frac) {
  if (numeratorOf(w) === 0) {
    return Undefined(
      `In call to evaluateProduct, the numerator of argument “w” is 0.`,
    );
  } else {
    const N = numeratorOf(v) * numeratorOf(w);
    const D = denominatorOf(v) * denominatorOf(w);
    return frac(N, D);
  }
}

/** Evaluates an integer or fraction sum. */
export function evaluateSum(v: Int_OR_Frac, w: Int_OR_Frac) {
  if (numeratorOf(w) === 0) {
    return Undefined(
      `In call to evaluateSum, the numerator of argument “w” is 0.`,
    );
  } else {
    const v_n = numeratorOf(v);
    const v_d = denominatorOf(v);
    const w_n = numeratorOf(w);
    const w_d = denominatorOf(w);
    const N = (v_n * w_d) + (w_n * v_d);
    const D = v_d * w_d;
    return frac(N, D);
  }
}

/** Evaluates an integer or fraction difference. */
export function evaluateDiff(v: Int_OR_Frac, w: Int_OR_Frac) {
  if (numeratorOf(w) === 0) {
    return Undefined(
      `In call to evaluateDiff, the numerator of argument “w” is 0.`,
    );
  } else {
    const v_n = numeratorOf(v);
    const v_d = denominatorOf(v);
    const w_n = numeratorOf(w);
    const w_d = denominatorOf(w);
    const N = (v_n * w_d) - (w_n * v_d);
    const D = v_d * w_d;
    return frac(N, D);
  }
}

/** Evaluates an integer or fraction power. */
export function evaluatePower(v: Int_OR_Frac, n: Int) {
  const v_n = numeratorOf(v);
  if (v_n !== 0) {
    if (n._n > 0) {
      const N = numeratorOf(v);
      const D = denominatorOf(v);
      const P = n._n;
      return frac(N ** P, D ** P);
    } else if (n._n === 0) {
      return int(1);
    } else if (n._n === -1) {
      const N = numeratorOf(v);
      const D = denominatorOf(v);
      return frac(D, N);
    } else {
      const N = numeratorOf(v) ** -n._n;
      const D = denominatorOf(v) ** -n._n;
      return frac(D, N);
    }
  } else {
    if (n._n >= 1) {
      return int(0);
    } else {
      return Undefined(
        `In call to evaluatePower, the exponent is not an integer. Exponents must be integers when evaluating rational expressions.`,
      );
    }
  }
}

/** Simplifies a rational number expression. */
export function simplifyRNE(
  u: AlgebraicExpression,
): Int | Fraction | UNDEFINED {
  if (!u.isRNE()) {
    return Undefined(
      `In call to simplifyRNE, argument “u” is not an RNE (rational number expression).`,
    );
  }
  // @ts-ignore
  const f = (u: AlgebraicExpression): Int | Fraction | UNDEFINED => {
    if (isInt(u)) {
      return u;
    } else if (isFrac(u)) {
      const N = numeratorOf(u);
      if (N === 0) {
        return Undefined(
          `In call to simplifyRNE’s recursive simplifier, the numerator of argument “u” is zero.`,
        );
      } else {
        return u;
      }
    } else if (u._args.length === 1) {
      const v = f(u.operand(1));
      if (isUndefined(v) || isSum(v)) {
        return v;
      }
      if (isDifference(u)) {
        return evaluateProduct(int(-1), v);
      }
    } else if (u._args.length === 2) {
      if (isSum(u) || isProduct(u) || isDifference(u) || isQuotient(u)) {
        const v = f(u.operand(1));
        const w = f(u.operand(2));
        if (isUndefined(v) || isUndefined(w)) {
          const notes = ["\n"];
          if (isUndefined(v)) notes.push(v._note);
          if (isUndefined(w)) notes.push(w._note);
          const msgs = notes.join("\n");
          return Undefined(
            `In call to simplifyRNE’s recursive simplifier, the numerator of arguments “v” and “w” are undefined. Tracing:` +
              msgs,
          );
        } else if (isSum(u)) {
          return evaluateSum(v, w);
        } else if (isDifference(u)) {
          return evaluateDiff(v, w);
        } else if (isProduct(u)) {
          return evaluateProduct(v, w);
        } else if (isQuotient(u)) {
          return evaluateQuotient(v, w);
        }
      } else if (isPower(u)) {
        const v = f(u.base);
        if (isUndefined(v)) {
          return v;
        } else if (isInt(u.exponent)) {
          return evaluatePower(v, u.exponent);
        } else if (isFrac(u.exponent)) {
          return Undefined(
            `In call to simplifyRNE’s recursive simplifier, the exponent ${u.exponent.toString()} is not an integer. Rational power expressions must have integer exponents.`,
          );
        } else {
          return Undefined(
            `In call to simplifyRNE’s recursive simplifier, unrecognized operator “${u._op}”`,
          );
        }
      }
    } else {
      return Undefined(
        `In call to simplifyRNE’s recursive simplifier, argument “u” is not an RNE (rational number expression.)`,
      );
    }
  };
  const v = f(u);
  if (isUndefined(v)) {
    const msg = v._note;
    return Undefined(`In simplifyRNE: ${msg}`);
  } else {
    return simplifyRationalNumber(v);
  }
}

/** Returns true if the expression `e1` precedes `e2`, false otherwise. */
export function order(e1: AlgebraicExpression, e2: AlgebraicExpression) {
  const filter8 = (
    u: AlgebraicExpression,
  ): u is Sym | Power | Sum | Factorial | AlgebraicFn => {
    return (
      isSymbol(u) ||
      isPower(u) ||
      isSum(u) ||
      isFactorial(u) ||
      isAlgebraicFn(u)
    );
  };

  const filter9 = (
    u: AlgebraicExpression,
  ): u is Sum | Factorial | AlgebraicFn | Sym => {
    return (
      isSum(u) ||
      isFactorial(u) ||
      isAlgebraicFn(u) ||
      isSymbol(u)
    );
  };

  const filter10 = (
    u: AlgebraicExpression,
  ): u is Factorial | AlgebraicFn | Sym => {
    return (
      isFactorial(u) ||
      isAlgebraicFn(u) ||
      isSymbol(u)
    );
  };

  const filter11 = (u: AlgebraicExpression): u is AlgebraicFn | Sym => {
    return (
      isAlgebraicFn(u) ||
      isSymbol(u)
    );
  };

  const order1 = (u: Int_OR_Frac, v: Int_OR_Frac) => {
    const A = u.toFrac();
    const B = v.toFrac();
    return A.lt(B);
  };

  const order2 = (u: Sym, v: Sym) => {
    const A = u._s;
    const B = v._s;
    return A < B;
  };

  const order3 = (u: Product_OR_Sum, v: Product_OR_Sum) => {
    if (u._op !== v._op) {
      return false;
    }
    if (!(u.last().equals(v.last()))) {
      return ORDER(u.last(), v.last());
    }
    const m = u._args.length;
    const n = v._args.length;
    const k = min(n, m) - 1;
    if (1 <= k) {
      for (let j = 0; j <= k; j++) {
        const o1 = u.operand(m - j);
        const o2 = v.operand(n - j);
        if (!o1.equals(o2)) {
          return ORDER(o1, o2);
        }
      }
    }
    return m < n;
  };

  const order4 = (u: Power, v: Power): boolean => {
    if (!u.base.equals(v.base)) {
      return ORDER(u.base, v.base);
    } else {
      return ORDER(u.exponent, v.exponent);
    }
  };

  const order5 = (u: Factorial, v: Factorial): boolean => {
    return ORDER(u.arg, v.arg);
  };

  const order6 = (u: AlgebraicFn, v: AlgebraicFn): boolean => {
    if (u._op !== v._op) {
      return u._op < v._op;
    } else {
      const uOp1 = u.operand(1);
      const uOp2 = u.operand(1);
      if (!uOp1.equals(uOp2)) {
        return ORDER(uOp1, uOp2);
      }
      const m = u._args.length;
      const n = v._args.length;
      const k = min(n, m) - 1;
      if (1 <= k) {
        for (let j = 0; j <= k - 1; j++) {
          const o1 = u.operand(m - j);
          const o2 = u.operand(n - j);
          if (!o1.equals(o2)) {
            return ORDER(o1, o2);
          }
        }
      }
      return m < n;
    }
  };

  const order8 = (
    u: Product,
    v: Power | Sum | Factorial | AlgebraicFn | Sym,
  ) => {
    if (!u.equals(v)) {
      return ORDER(u.last(), v);
    } else {
      return true;
    }
  };

  const order9 = (u: Power, v: Sum | Factorial | AlgebraicFn | Sym) => {
    return ORDER(u, power(v, int(1)));
  };

  const order10 = (u: Sum, v: Factorial | AlgebraicFn | Sym) => {
    if (!u.equals(v)) {
      return ORDER(u, sum([v]));
    } else {
      return true;
    }
  };

  const order11 = (u: Factorial, v: AlgebraicFn | Sym) => {
    if (u.operand(1).equals(v)) {
      return false;
    } else {
      return ORDER(u, factorial(v));
    }
  };

  const order12 = (u: AlgebraicFn, v: Sym) => {
    if (u._op === v._s) {
      return false;
    } else {
      return ORDER(sym(u._op), v);
    }
  };

  const ORDER = (u: AlgebraicExpression, v: AlgebraicExpression): boolean => {
    if (isIntorFrac(u) && isIntorFrac(v)) {
      return order1(u, v);
    } else if (isSymbol(u) && isSymbol(v)) {
      return order2(u, v);
    } else if (isProductOrSum(u) && isProductOrSum(v)) {
      return order3(u, v);
    } else if (isPower(u) && isPower(v)) {
      return order4(u, v);
    } else if (isFactorial(u) && isFactorial(v)) {
      return order5(u, v);
    } else if (isAlgebraicFn(u) && isAlgebraicFn(v)) {
      return order6(u, v);
    } else if (isIntorFrac(u)) {
      return true;
    } else if (isProduct(u) && filter8(v)) {
      return order8(u, v);
    } else if (isPower(u) && filter9(v)) {
      return order9(u, v);
    } else if (isSum(u) && filter10(v)) {
      return order10(u, v);
    } else if (isFactorial(u) && filter11(v)) {
      return order11(u, v);
    } else if (isAlgebraicFn(u) && isSymbol(v)) {
      return order12(u, v);
    } else {
      return false;
    }
  };
  return ORDER(e1, e2);
}

/** Returns all complete subexpressions of the given expression. */
function subex(expression: AlgebraicExpression) {
  const out: AlgebraicExpression[] = [];
  const set = setof<string>();
  const f = (u: AlgebraicExpression) => {
    if (isAtom(u)) {
      const s = u.toString();
      if (!set.has(s)) {
        out.push(u);
        set.add(s);
      }
      return null;
    } else {
      const s = u.toString();
      if (!set.has(s)) {
        out.push(u);
        u._args.forEach((x) => f(x));
        set.add(s);
      }
      return null;
    }
  };
  f(expression);
  return out;
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

/** Returns the derivative of a given algebraic expression. */
function derivative(expression: AlgebraicExpression, variable: string | Sym) {
  return expression;
}

/** Sorts the given list of expressions. */
function sortex(expressions: AlgebraicExpression[]) {
  const out = [];
  for (let i = 0; i < expressions.length; i++) {
    out.push(expressions[i].copy());
  }
  return out.sort((a, b) => order(a, b) ? 1 : -1);
}

// deno-fmt-ignore
function $integerPower(v: AlgebraicExpression, n: Int): AlgebraicExpression {
  /** SINTPOW-1 */
  if (isIntorFrac(v)) {
    return simplifyRNE(power(v, n));
  }
  
  /** SINTPOW-2 */
  else if (n._isZero) {
    return int(1);
  }
  
  /** SINTPOW-3 */
  else if (n._isOne) {
    return v;
  }
  
  /** SINTPOW-4 */
  else if (isPower(v)) {
    const r = v.base;
    const s = v.exponent;
    const p = $product(product([s,n]));
    if (isInt(p)) {
      return $integerPower(r,p);
    } else {
      return power(r,p);
    }
  }

  /** SINTPOW-5 */
  else if (isProduct(v)) {
    const args:AlgebraicExpression[] = [];
    const v_args = v._args;
    for (let i = 0; i < v_args.length; i++) {
      const v = v_args[i];
      const r = $integerPower(v, n);
      args.push(r);
    }
    const p = product(args);
    return $product(p);
  }
  
  /** SINTPOW-6 */
  else {
    return power(v,n);
  }
}
function $e(source: string, message: string) {
  return `In call to [${source}], ${message}.`;
}

/** Returns the base of the given AlgebraicExpression. */
function baseOf(u: AlgebraicExpression) {
  if (
    isSum(u) ||
    isSymbol(u) ||
    isProduct(u) ||
    isFactorial(u) ||
    isAlgebraicFn(u)
  ) {
    return u;
  } else if (isPower(u)) {
    return u.base;
  } else {
    return Undefined(`${u.toString()} has no base`);
  }
}

/** Returns the exponent of the given AlgebraicExpression. */
function exponentOf(u: AlgebraicExpression) {
  if (
    isSum(u) ||
    isSymbol(u) ||
    isProduct(u) ||
    isFactorial(u) ||
    isAlgebraicFn(u)
  ) {
    return int(1);
  } else if (isPower(u)) {
    return u.exponent;
  } else {
    return Undefined(`${u.toString()} has no exponent`);
  }
}

/** Returns true if the AlgebraicExpression `u` is an Int, Fraction, Real, or Constant. This function is primarily used by the automatic simplification algorithm. */
function isC(u: AlgebraicExpression): u is Int | Fraction | Real | Constant {
  return (
    isIntorFrac(u) ||
    isReal(u) ||
    isConstant(u)
  );
}

/** Returns the term of the given AlgebraicExpression. */
function termOf(u: AlgebraicExpression) {
  if (
    isSum(u) ||
    isPower(u) ||
    isSymbol(u) ||
    isFactorial(u) ||
    isAlgebraicFn(u)
  ) {
    return u;
  } else if (isProduct(u)) {
    if (isC(u._args[0])) {
      const newOperands: AlgebraicExpression[] = [];
      for (let i = 1; i < u._args.length; i++) {
        newOperands.push(u._args[i]);
      }
      return product(newOperands);
    } else {
      return u;
    }
  } else {
    return Undefined($e("termOf", `${u.toString()} has no term`));
  }
}

/** Returns the constant of the given AlgebraicExpression. */
function constOf(u: AlgebraicExpression) {
  if (
    isSymbol(u) ||
    isSum(u) ||
    isPower(u) ||
    isFactorial(u) ||
    isAlgebraicFn(u)
  ) {
    return int(1);
  } else if (isProduct(u)) {
    if (isC(u._args[0])) {
      return u._args[0];
    } else {
      return int(1);
    }
  } else {
    return Undefined($e("constOf", `${u.toString()} has no constant.`));
  }
}

/** Given the array of AlgebraicExpressions `exprs2`, returns a new array with the AlgebraicExpression `expr` as the first element, with the elements of `exprs2`’s elements following. */
function adjoin(expr: AlgebraicExpression, exprs2: AlgebraicExpression[]) {
  const out: AlgebraicExpression[] = [expr];
  for (let j = 0; j < exprs2.length; j++) {
    out.push(exprs2[j]);
  }
  return out;
}

/** Given the array of expressions, returns a new array with all but the first element. */
function rest(u: AlgebraicExpression[]): AlgebraicExpression[] {
  const out: AlgebraicExpression[] = [];
  if (2 <= u.length) {
    for (let i = 1; i < u.length; i++) {
      out.push(u[i]);
    }
  }
  return out;
}

/** Simplifies a power expression. */
// deno-fmt-ignore
function $power(u: Power): AlgebraicExpression {
  const v = u.base;
  const w = u.exponent;

  /** SPOW-1 */
  if (isUndefined(v) || isUndefined(w)) {
    return Undefined($e('$power', 'a base or exponent is undefined.'));
  }
  
  /** SPOW-2 */
  else if (isIntorFrac(v) && v._isZero) {
    if (isIntorFrac(w) && w._isPositive) {
      return int(0);
    } else {
      return Undefined(
        $e('$power', 'the base is zero, but the exponent is either (1) neither an integer nor a fraction, or (2) the exponent is non-positive'),
      );
    }
  }
  
  /** SPOW-3 */
  else if (isIntorFrac(v) && v._isOne) {
    return int(1);
  }
  
  /** SPOW-4 */
  else if (isInt(w)) {
    return $integerPower(v, w);
  }
  
  /** SPOW-5 */
  else {
    return u;
  }
}

// deno-fmt-ignore
function mergeProducts(
  p: AlgebraicExpression[],
  q: AlgebraicExpression[],
): AlgebraicExpression[] {
  /** MPRD-1 */
  if (p.length === 0) {
    return q;
  }

  /** MPRD-2 */
  else if (q.length === 0) {
    return p;
  }
  
  /** MPRD-3 */
  else {
    const p1 = p[0];
    const q1 = q[0];
    const h = $productREC([p1, q1]);
    
    /** MPRD-3.1 */
    if (h.length === 0) {
      return mergeProducts(rest(p), rest(q));
    }
    
    /** MPRD-3.2 */
    else if (h.length === 1) {
      return adjoin(h[0], mergeProducts(rest(p), rest(q)));
    }

    /** MPRD-3.3 */
    else if (h.length===2 && h[0].equals(p1) && h[1].equals(q1)) {
      return adjoin(p1, mergeProducts(rest(p), q));
    }
    else {
      return adjoin(q1, mergeProducts(p, rest(q)));
    }
  }
}

/** Simplifies the operands of a product recursively. */
// deno-fmt-ignore
function $productREC(L: AlgebraicExpression[]): AlgebraicExpression[] {
  if (L.length === 2 && !isProduct(L[0]) && !isProduct(L[1])) {
    const [u1, u2] = L;

    /** SPRDREC-1(1) */
    if (isIntorFrac(u1) && isIntorFrac(u2)) {
      const P = simplifyRNE(product([u1, u2]));
      if (isIntorFrac(P) && P._isOne) {
        return [];
      } else {
        return [P];
      }
    }
    
    /** SPRDREC-1(2)(a) */
    else if (isIntorFrac(u1) && u1._isOne) {
      return [u2];
    }
    
    /** SPRDREC-1(2)(b) */
    else if (isIntorFrac(u2) && u2._isOne) {
      return [u1];
    }

    /** SPRDREC-1(3) */
    else if (baseOf(u1).equals(baseOf(u2))) {
      const S = $sum(sum([exponentOf(u1), exponentOf(u2)]));
      const P = $power(power(baseOf(u1), S));
      if (isIntorFrac(P) && P._isOne) {
        return [];
      } else {
        return [P];
      }
    }
    
    /** SPRDREC-1(4) */
    else if (order(u2,u1)) {
      return [u2, u1];
    }
    
    /** SPRDREC-1(5) */
    else {
      return L;
    }
  } 

  else if (L.length===2 && (isProduct(L[0]) || isProduct(L[1]))) {
    const [u1,u2]=L;
    if (isProduct(u1) && isProduct(u2)) {
      return mergeProducts(u1._args, u2._args);
    }
    else if (isProduct(u1) && !isProduct(u2)) {
      return mergeProducts(u1._args, [u2]);
    }
    else {
      return mergeProducts([u1], u2._args);
    }
  }
  
  /** SPRDREC-3 */
  else if (L.length >= 2) {
    const w = $productREC(rest(L));

    /** SPRDREC-3.1 */
    if (isProduct(w[0])) {
      return mergeProducts(w[0]._args, w);
    }

    /** SPRDREC-3.2 */
    else {
      return mergeProducts([w[0]], w);
    }
  }

  else {
    return L;
  }
}

/** Simplifies a product expression. */
function $product(u: Product): AlgebraicExpression {
  const L = u._args;

  for (let i = 0; i < L.length; i++) {
    const arg = L[i];

    /** SPRD-1 */
    if (isUndefined(arg)) {
      return Undefined($e("$product", "Undefined operand encountered"));
    }

    /** SPRD-2 */
    if ((isIntorFrac(arg) && arg._isZero)) {
      return int(0);
    }

    /** SPRD-3 */
    if (L.length === 1) {
      return arg;
    }
  }

  /** SPRD-4 */
  const v = $productREC(L);
  if (v.length === 1) {
    return v[0];
  } else if (v.length >= 2) {
    return product(v);
  } else {
    return int(1);
  }
}

function mergeSums(
  p: AlgebraicExpression[],
  q: AlgebraicExpression[],
): AlgebraicExpression[] {
  throw new Error(`$mergeSums unimplemented`);
}

// deno-fmt-ignore
function $sumREC(L: AlgebraicExpression[]): AlgebraicExpression[] {
  if (L.length === 2 && !isSum(L[0]) && !isSum(L[1])) {
    const [u1,u2]=L;
    
    if (isIntorFrac(u1) && isIntorFrac(u2)) {
      const P = simplifyRNE(sum([u1,u2]));
      if (isIntorFrac(P) && P._isZero) {
        return [];
      } else {
        return [P];
      }
    }

    else if (isIntorFrac(u1) && u1._isZero) {
      return [u2];
    }
    
    else if (isIntorFrac(u2) && u2._isZero) {
      return [u1];
    }

    else if (termOf(u1).equals(termOf(u2))) {
      const S = $sum(sum([u1, u2]));
      if (isIntorFrac(S) && S._isZero) {
        return [];
      } else {
        return [S];
      }
    }
    
    else if (order(u2,u1)) {
      return [u2,u1];
    }

    else {
      return L;
    }
  }
  
  else if (L.length===2 && (isSum(L[0]) || isSum(L[1]))) {
    const [u1,u2]=L;
    if (isSum(u1) && isSum(u2)) {
      return mergeSums(u1._args, u2._args);
    }
    else if (isSum(u1) && !isSum(u2)) {
      return mergeSums(u1._args, [u2]);
    }
    else {
      return mergeSums([u1], u2._args);
    }
  }
  
  else if (L.length >= 2) {
    const w = $sumREC(rest(L));
    if (isSum(w[0])) {
      return mergeSums(w[0]._args, w);
    }
    else {
      return mergeSums([w[0]], w);
    }
  }

  else {
    return L;
  }
}

function $sum(u: Sum): AlgebraicExpression {
  const L = u._args;
  for (let i = 0; i < L.length; i++) {
    const arg = L[i];
    if (isUndefined(arg)) {
      return Undefined($e("$sum", "Undefined operator encountered"));
    }
    if (L.length === 1) {
      return arg;
    }
  }
  const v = $sumREC(L);
  if (v.length === 1) {
    return v[0];
  } else if (v.length >= 2) {
    return sum(v);
  } else {
    return int(0);
  }
}

function $quotient(u: Quotient): AlgebraicExpression {
  throw new Error("method unimplemented");
}

function $difference(u: Difference): AlgebraicExpression {
  throw new Error("method unimplemented");
}

function $factorial(u: Factorial): AlgebraicExpression {
  throw new Error("method unimplemented");
}

function $function(u: AlgebraicFn): AlgebraicExpression {
  throw new Error("method unimplemented");
}

/** Simplifies the given expression. */
function simplify(expression: AlgebraicExpression) {
  const $ = (u: AlgebraicExpression): AlgebraicExpression => {
    if (isInt(u) || isSymbol(u) || isConst(u) || isReal(u)) {
      return u;
    } else if (isFrac(u)) {
      return simplifyRationalNumber(u);
    } else {
      const v = u.argmap($);
      if (isPower(v)) {
        return $power(v);
      } else if (isProduct(v)) {
        return $product(v);
      } else if (isSum(v)) {
        return $sum(v);
      } else if (isQuotient(v)) {
        return $quotient(v);
      } else if (isDifference(v)) {
        return $difference(v);
      } else if (isFactorial(v)) {
        return $factorial(v);
      } else {
        return $function(v);
      }
    }
  };
  return $(expression);
}

export function engine(source: string) {
  let settings: EngineSettings = {
    implicitMultiplication: true,
  };
  const compiler = new Compiler();

  /** Parses the source code. */
  const parse = () => syntax(source).statements();

  /** Compiles the source code. */
  const compile = (program: Left<Err> | Right<Statement[]>) => {
    if (program.isLeft()) {
      return program;
    }
    const statements = program.unwrap();
    const interpreter = compiler;
    const resolved = resolvable(interpreter).resolved(statements);
    if (resolved.isLeft()) {
      return resolved;
    }
    return interpreter.interpret(statements);
  };

  return {
    tokens: () => lexical(source).stream(),
    ast: () => objectTree(parse()),
    /** Sets the engine’s settings. */
    engineSettings(options: Partial<EngineSettings>) {
      settings = { ...settings, ...options };
      return this;
    },
    algebraTree() {
      throw new Error(`method unimplemented`);
    },
    /**
     * Executes the given source code with the current
     * engine settings.
     */
    execute() {
      const parsing = parse();
      if (parsing.isLeft()) return parsing.unwrap().message;
      const result = compile(parsing);
      if (result.isLeft()) return result.unwrap().message;
      const out = result.unwrap();
      return out;
    },
    compiler,
    log(mode: "latex" | "plain" = "latex"): Result {
      const program = parse();
      if (program.isLeft()) {
        const error = program.unwrap().report();
        return res([], error);
      }
      const statements = program.unwrap();
      const interpreter = new Compiler().setmode(`log-${mode}`).loopcap(600);
      const resolved = resolvable(interpreter).resolved(statements);
      if (resolved.isLeft()) {
        const error = resolved.unwrap().report();
        return res([], error);
      }
      const result = interpreter.interpret(statements);
      if (result.isLeft()) {
        const error = result.unwrap().report();
        return res([], error);
      } else {
        const prints = interpreter.prints;
        if (prints.length === 0) {
          prints.push(
            mode === "latex" ? latex.txt("OK") : "OK",
          );
        }
        return res(prints);
      }
    },
  };
}

type Result = {
  error?: string;
  result: string[];
};

function res(result: string[], error?: string) {
  return ({ error, result });
}
