// § Utility Functions
/** Returns a pretty-print tree of the given Object `Obj`. */
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
    return out.map((d, i) => g(d, i)).join();
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

// § Primitives

/** An object corresponding to a number of the form `n/d`, where `n` and `d` are integers. */
class Fraction {
  /** The numerator of this fraction. */
  $n: number;
  /** The denominator of this fraction. */
  $d: number;
  constructor(n: number, d: number) {
    this.$n = n;
    this.$d = d;
  }
  toString() {
    return `${this.$n}/${this.$d}`;
  }
}

/**
 * Returns a new Fraction. Both arguments must be integers.
 * If the arguments are not integers, they will be floored.
 * @param n The numerator of this fraction (must be an integer).
 * @param d The denominator of this fraction (must be an integer).
 * @returns A new instance of a fraction.
 */
function fraction(n: number, d: number) {
  return new Fraction(Math.floor(n), Math.floor(d));
}

/** An object corresponding to a number of the form `m x 10^n`. */
class Scientific_Number {
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
}

/**
 * Returns a new instance of a Scientific_Number.
 * @param m The mantissa in `m x 10^n`.
 * @param n The exponent (an integer) in `m x 10^n`.
 * @returns A new scientific number (m x 10^n).
 */
function sci(m: number, n: number) {
  return new Scientific_Number(m, Math.floor(n));
}

/** A value native to Winnow. */
type PRIMITIVE =
  | number
  | string
  | null
  | boolean
  | bigint
  | Scientific_Number
  | Fraction
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
  L extends PRIMITIVE = PRIMITIVE
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
  withLiteral<L2 extends PRIMITIVE>(primitive: L2) {
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
        return tkn(type).withLiteral(sci(base, exponent));
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
  sym(node: Sym): T;
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
function classStmt(name: Token, methods: FnStmt[]) {
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
function blockStmt(statements: Statement[]) {
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
function exprStmt(expression: Expr) {
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
  $params: Sym[];
  $body: Statement[];
  constructor(
    name: Token<token_type.symbol>,
    params: Sym[],
    body: Statement[]
  ) {
    super();
    this.$name = name;
    this.$params = params;
    this.$body = body;
  }
}

/** Returns a new function declaration statement. */
function fnStmt(
  name: Token<token_type.symbol>,
  params: Sym[],
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
function ifStmt(
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
function printStmt(keyword: Token, expression: Expr) {
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
function returnStmt(keyword: Token, expression: Expr) {
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
  $variable: Sym;
  $value: Expr;
  $mutable: boolean;
  constructor(variable: Sym, value: Expr, mutable: boolean) {
    super();
    this.$variable = variable;
    this.$value = value;
    this.$mutable = mutable;
  }
}

/** Returns a new 'var' statement node. */
function varStmt(symbol: Sym, value: Expr) {
  return new VariableStmt(symbol, value, true);
}

/** Returns a new 'let' statement node. */
function letStmt(symbol: Sym, value: Expr) {
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
function whileStmt(keyword: Token, condition: Expr, body: Statement) {
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
function indexExpr(list: Expr, index: Expr, op: Token) {
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
function algebraString(expression: Expr, op: Token) {
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
function tupleExpr(elements: Expr[]) {
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
function vectorExpr(op: Token, elements: Expr[]) {
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
  $vectors: VectorExpr[];
  $rowCount: number;
  $colCount: number;
  constructor(vectors: VectorExpr[], rows: number, columns: number) {
    super();
    this.$vectors = vectors;
    this.$rowCount = rows;
    this.$colCount = columns;
  }
}

/** Returns a new matrix expression. */
function matrixExpr(vectors: VectorExpr[], rowCount: number, colCount: number) {
  return new MatrixExpr(vectors, rowCount, colCount);
}

/** A node corresponding to a symbol. */
class Sym extends Expr {
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
function sym(symbol: Token<token_type.symbol>) {
  return new Sym(symbol);
}

/** Returns true, and asserts, if the given node is a symbol node. */
function isSymbol(node: ASTNode): node is Sym {
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
  $symbol: Sym;
  $value: Expr;
  constructor(symbol: Sym, value: Expr) {
    super();
    this.$symbol = symbol;
    this.$value = value;
  }
}

/** Returns a new assignment expression node. */
function assignmentExpr(symbol: Sym, value: Expr) {
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
function nativeCall(name: Token<token_type.native>, args: Expr[]) {
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
function negExpr(op: Token<token_type.minus>, arg: Expr) {
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
function posExpr(op: Token<token_type.plus>, arg: Expr) {
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
function factorialExpr(op: Token<token_type.bang>, arg: Expr) {
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
function notExpr(op: Token<token_type.not>, arg: Expr) {
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
function vectorBinex(left: Expr, op: Token<VectorBinop>, right: Expr) {
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
function algebraicBinex(left: Expr, op: Token<AlgebraicOp>, right: Expr) {
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
function callExpr(callee: Expr, paren: Token, args: Expr[]) {
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
function parendExpr(innerExpression: Expr) {
  return new ParendExpr(innerExpression);
}

/** Returns true, and asserts, if the given node is a parenthesized-expression node. */
function isParendExpr(node: ASTNode): node is ParendExpr {
  return node.kind() === nodekind.parend_expression;
}

type LiteralTokenType =
  | NumberTokenType
  | token_type.string
  | token_type.boolean
  | token_type.nil
  | token_type.nan
  | token_type.inf;

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
function stringLit(value: string) {
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
function bool(value: boolean) {
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
function nil() {
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
function integer(value: number) {
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
function float(value: number) {
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
function bigInteger(value: bigint) {
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
  $value: Scientific_Number;
  constructor(value: Scientific_Number) {
    super();
    this.$value = value;
  }
}

/** Returns a new scientific number node. */
function scinum(value: Scientific_Number) {
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
  $value: Fraction;
  constructor(value: Fraction) {
    super();
    this.$value = value;
  }
}

/* Returns a new fraction node. */
function frac(value: Fraction) {
  return new Frac(value);
}

/** Returns an empty statement. */
function emptyStmt() {
  return exprStmt(nil());
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
function numConst(symbol: string, value: number) {
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
function logicalBinex(left: Expr, op: Token<BinaryLogicOp>, right: Expr) {
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
function relationExpr(left: Expr, op: Token<RelationOp>, right: Expr) {
  return new RelationExpr(left, op, right);
}

// TODO - Implement Get Expression
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
function getExpr(object: Expr, name: Token) {
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
function stringConcat(
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
function setExpr(object: Expr, name: Token, value: Expr) {
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
function superExpr(method: Token) {
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
function thisExpr(keyword: Token) {
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
    return this.lexer.atEnd() || this.$error !== null;
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
  const state = enstate<Expr, Statement>(nil(), emptyStmt()).init(source);

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
      return state.newExpr(tupleExpr(elements));
    }
    if (!state.nextIs(token_type.right_paren)) {
      return state.error(`Expected a closing ")".`, state.$current.$line);
    }
    return innerExpression.map((e) => parendExpr(e));
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
      return state.newExpr(algebraicBinex(left, star, right));
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
    return state.newExpr(callExpr(callee, op, args));
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
      return state.newExpr(matrixExpr(vectors, rows, columns));
    }
    return state.newExpr(vectorExpr(prev, elements));
  };

  /** Parses an indexing expression. */
  const indexingExpression: Parslet<Expr> = (op, lhs) => {
    const index = expr();
    if (index.isLeft()) return index;
    const rbracket = state.next();
    if (!rbracket.isType(token_type.right_bracket)) {
      return state.error(`Expected a right bracket "]"`, rbracket.$line);
    }
    return state.newExpr(indexExpr(lhs, index.unwrap(), op));
  };

  /** Parses a get expression */
  const getExpression = (op: Token, lhs: Expr) => {
    const nxt = state.next();
    if (!nxt.isType(token_type.symbol)) {
      return state.error("Expected a property name", nxt.$line);
    }
    let exp = getExpr(lhs, nxt);
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
      return state.newExpr(callExpr(exp, op, args));
    }
    return state.newExpr(exp);
  };

  /** Parses an assignment expression */
  const assignment = (op: Token, node: Expr) => {
    if (isSymbol(node)) {
      return expr().chain((n) => state.newExpr(assignmentExpr(node, n)));
    } else if (isGetExpr(node)) {
      const rhs = expr();
      if (rhs.isLeft()) return rhs;
      return state.newExpr(setExpr(node.$object, node.$name, rhs.unwrap()));
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
      return state.newExpr(factorialExpr(op, node));
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
      const right = algebraicBinex(
        node,
        token(tt, operator, op.$line),
        operator === "*" ? node : integer(1)
      );
      return state.newExpr(assignmentExpr(node, right));
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
      state.newExpr(vectorBinex(left, op as Token<VectorBinop>, right))
    );
  };

  /* Parses a comparison expression. */
  const compare = (op: Token, lhs: Expr) => {
    const p = precof(op.$type);
    return expr(p).chain((rhs) => {
      return state.newExpr(relationExpr(lhs, op as Token<RelationOp>, rhs));
    });
  };

  /* Parses a prefix expression. */
  const prefix: Parslet<Expr> = (op) => {
    const p = precof(op.$type);
    const a = expr(p);
    if (a.isLeft()) return a;
    const arg = a.unwrap();
    if (op.isType(token_type.minus)) {
      return state.newExpr(negExpr(op, arg));
    } else if (op.isType(token_type.plus)) {
      return state.newExpr(posExpr(op, arg));
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
        const value = algebraicBinex(lhs, op as Token<AlgebraicOp>, rhs);
        return state.newExpr(assignmentExpr(name, value));
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
    return state.newExpr(algebraicBinex(lhs, op as Token<AlgebraicOp>, rhs));
  };

  /* Parses a logical infix expression. */
  const logicInfix = (op: Token, lhs: Expr) => {
    const p = precof(op.$type);
    return expr(p).chain((rhs) => {
      return state.newExpr(logicalBinex(lhs, op as Token<BinaryLogicOp>, rhs));
    });
  };

  /* Parses a logical not expression. */
  const logicNot = (op: Token) => {
    const p = precof(op.$type);
    return expr(p).chain((arg) =>
      state.newExpr(notExpr(op as Token<token_type.not>, arg))
    );
  };

  /* Parses a symbol. */
  const varname: Parslet<Expr> = (op) => {
    if (op.isType(token_type.symbol)) {
      const out = sym(op);
      return state.newExpr(out);
    } else {
      return state.error(`Unexpected variable "${op.$lexeme}"`, op.$line);
    }
  };

  /* Parses an implicit multiplication. */
  const impMul: Parslet<Expr> = (op, left) => {
    if (op.isType(token_type.symbol)) {
      const right = sym(op);
      const star = token(token_type.star, "*", op.$line);
      return state.newExpr(algebraicBinex(left, star, right));
    } else {
      return state.error(
        `Expected a symbol for implicit multiplication, but got "${op.$lexeme}"`,
        op.$line
      );
    }
  };

  /* Parses a string literal. */
  const stringLiteral: Parslet<Expr> = (t) =>
    state.newExpr(stringLit(t.$lexeme));

  /* Parses a boolean literal. */
  const boolLiteral: Parslet<Expr> = (op) => {
    if (op.isType(token_type.boolean) && typeof op.$literal === "boolean") {
      return state.newExpr(bool(op.$literal));
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
        return state.newExpr(numConst("NaN", NaN));
      case token_type.inf:
        return state.newExpr(numConst("Inf", Infinity));
      case token_type.numeric_constant: {
        switch (op.$lexeme as NativeConstants) {
          case "e":
            return state.newExpr(numConst("e", Math.E));
          case "pi":
            return state.newExpr(numConst("pi", Math.PI));
          case "ln2":
            return state.newExpr(numConst("ln2", Math.LN2));
          case "ln10":
            return state.newExpr(numConst("ln10", Math.LN10));
          case "log10e":
            return state.newExpr(numConst("log10e", Math.LOG10E));
          case "log2e":
            return state.newExpr(numConst("log2e", Math.LOG2E));
          case "sqrt2":
            return state.newExpr(numConst("sqrt2", Math.SQRT2));
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
        ? state.newExpr(integer(t.$literal))
        : state.newExpr(float(t.$literal));
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
        return state.newExpr(parendExpr(algebraicBinex(left, star, right)));
      }
      return out;
    } else {
      return state.error(`Expected a number, but got "${t.$lexeme}"`, t.$line);
    }
  };

  /* Parses a native call expression. */
  const ncall = (op: Token) => {
    const lex = op.$lexeme;
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
    return state.newExpr(nativeCall(op as Token<token_type.native>, args));
  };

  /* Parses a string concatenation expression. */
  const concatenation: Parslet<Expr> = (op: Token, left: Expr) => {
    const p = precof(op.$type);
    return expr(p).chain((right) => {
      return state.newExpr(
        stringConcat(left, op as Token<token_type.ampersand>, right)
      );
    });
  };

  /** Parses a fraction literal. */
  const fract = (op: Token) => {
    if (op.isType(token_type.fraction) && op.$literal instanceof Fraction) {
      return state.newExpr(frac(op.$literal));
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
      return state.newExpr(algebraString(expression, op));
    } else {
      return state.error(`Unexpected algebraic string`, op.$line);
    }
  };

  /* Parses a this expression. */
  const thisExpression = (t: Token) => state.newExpr(thisExpr(t));

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
    let lhs = pre(token, nil());
    if (lhs.isLeft()) return lhs;
    while (minbp < precof(state.$peek.$type)) {
      if (state.atEnd()) break;
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
      return state.newStmt(exprStmt(expression));
    } else {
      return state.error(
        `Expected ";" to end the statement`,
        state.$current.$line
      );
    }
  };

  const STATEMENT = () => {
    return EXPRESSION();
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
