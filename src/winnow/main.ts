type Either<A, B> = Left<A> | Right<B>;

/** An object corresponding to failure. */
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
  unwrap() {
    return this.value;
  }
  chain<X, S>(f: (x: never) => Either<X, S>): Left<T> {
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

enum TOKEN_TYPE {
  // Utility tokens
  END,
  ERROR,
  EMPTY,
  // Paired Delimiters
  LEFT_PAREN,
  RIGHT_PAREN,
  LEFT_BRACE,
  RIGHT_BRACE,
  LEFT_BRACKET,
  RIGHT_BRACKET,
  // Single Delimiters
  SEMICOLON,
  COLON,
  DOT,
  COMMA,
  // Operator Delimiters
  PLUS,
  MINUS,
  STAR,
  SLASH,
  CARET,
  PERCENT,
  BANG,
  AMPERSAND,
  TILDE,
  VBAR,
  EQUAL,
  LESS,
  GREATER,
  LESS_EQUAL,
  GREATER_EQUAL,
  BANG_EQUAL,
  EQUAL_EQUAL,
  PLUS_PLUS,
  MINUS_MINUS,
  STAR_STAR,
  // Vector Operators
  DOT_ADD,
  DOT_STAR,
  DOT_MINUS,
  DOT_CARET,
  AT,
  // Matrix Operators
  POUND_PLUS, // '#+'
  POUND_MINUS, // '#-
  POUND_STAR, // '#*'
  // Literals
  SYMBOL,
  STRING,
  BOOLEAN,
  INTEGER,
  FLOAT,
  FRACTION,
  SCIENTIFIC,
  BIG_NUMBER,
  FALSE,
  TRUE,
  NAN,
  INF,
  NIL,
  NUMERIC_CONSTANT,
  ALGEBRAIC,
  // Keyword Tokens
  AND,
  OR,
  NOT,
  NAND,
  XOR,
  XNOR,
  NOR,
  IF,
  ELSE,
  FN,
  LET,
  VAR,
  RETURN,
  WHILE,
  FOR,
  CLASS,
  PRINT,
  SUPER,
  THIS,
  REM,
  MOD,
  DIV,
}

type NumberTokenType =
  | TOKEN_TYPE.INTEGER
  | TOKEN_TYPE.FLOAT
  | TOKEN_TYPE.BIG_NUMBER
  | TOKEN_TYPE.SCIENTIFIC
  | TOKEN_TYPE.FRACTION;

type ErrorType =
  | "lexical-error"
  | "syntax-error"
  | "type-error"
  | "runtime-error"
  | "environment-error"
  | "resolver-error";

/** An object corresponding to an error in Winnow. */
class Err extends Error {
  /** This error's message. */
  message: string;
  /** This error's type. */
  $type: ErrorType;
  /** The line where this error occurred. */
  $line: number;
  /** The column where this error occurred. */
  $column: number;
  constructor(message: string, type: ErrorType, line: number, column: number) {
    super(message);
    this.message = message;
    this.$type = type;
    this.$line = line;
    this.$column = column;
  }
  toString() {
    return this.message;
  }
}

const errorFactory =
  (type: ErrorType) => (message: string, line: number, column: number) =>
    new Err(message, type, line, column);
const lexicalError = errorFactory("lexical-error");
const syntaxError = errorFactory("syntax-error");
const typeError = errorFactory("type-error");
const runtimeError = errorFactory("runtime-error");
const environmentError = errorFactory("environment-error");
const resolverError = errorFactory("resolver-error");

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
}

/**
 * Returns a new instance of a Scientific_Number.
 * @param m The mantissa in `m x 10^n`.
 * @param n The exponent (an integer) in `m x 10^n`.
 * @returns A new scientific number (m x 10^n).
 */
function scinum(m: number, n: number) {
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

/** Returns true iff `x` is a Scientific_Number. */
function isScientificNumber(x: any): x is Scientific_Number {
  return x instanceof Scientific_Number;
}

/** Returns true iff `x` is null. */
function isNull(x: any): x is null {
  return x === null;
}

/** Returns true iff `x` is a Boolean value. */
function isBoolean(x: any): x is boolean {
  return typeof x === "boolean";
}

/** Returns true iff `x` is a number value. */
function isNumber(x: any): x is number {
  return typeof x === "number";
}

/** Returns true iff `x` is a string value. */
function isString(x: any): x is string {
  return typeof x === "string";
}

/** An object corresponding to a token in Winnow. */
class Token<
  T extends TOKEN_TYPE = TOKEN_TYPE,
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
  /** The column where this token was first encountered. */
  $column: number;
  constructor(type: T, lexeme: string, line: number, column: number) {
    this.$type = type;
    this.$lexeme = lexeme;
    this.$line = line;
    this.$column = column;
  }
  copy() {
    return new Token(this.$type, this.$lexeme, this.$line, this.$column);
  }
  /**
   * Returns a copy of this token with a new token type.
   * @param tokenType The new token type.
   * @returns A copy of this token with the given token type.
   */
  withType<X extends TOKEN_TYPE>(tokenType: X) {
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
   * Returns a copy of this token with a new column number.
   * @param column The new column number.
   * @returns A copy of this token with the given column as its column number.
   */
  withColumn(column: number) {
    const out = this.copy();
    out.$column = column;
    return out;
  }

  isType<K extends T>(type: K): this is Token<K> {
    return this.$type === type;
  }

  isErrorToken(): this is Token<TOKEN_TYPE.ERROR, Err> {
    return this.$type === TOKEN_TYPE.ERROR;
  }

  /**
   * Returns true if and only if this token is a
   * right-delimiter token. That is, either a `)`,
   * `]`, or `}`.
   * @returns a boolean
   */
  isRightDelimiter() {
    return (
      this.$type === TOKEN_TYPE.RIGHT_PAREN ||
      this.$type === TOKEN_TYPE.RIGHT_BRACE ||
      this.$type === TOKEN_TYPE.RIGHT_BRACKET
    );
  }

  static empty: Token<TOKEN_TYPE, any> = new Token(
    TOKEN_TYPE.EMPTY,
    "",
    -1,
    -1
  );
  static end: Token<TOKEN_TYPE, any> = new Token(TOKEN_TYPE.END, "END", -1, -1);

  toString() {
    return `{token: ${TOKEN_TYPE[this.$type]}, lexeme: ${this.$lexeme}, line: ${
      this.$line
    }, column: ${this.$column}, literal: ${this.$literal}}`;
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
function token<X extends TOKEN_TYPE>(
  type: X,
  lexeme: string,
  line: number,
  column: number
): Token<X> {
  return new Token(type, lexeme, line, column);
}

export function lexical(code: string) {
  /**
   * A variable corresponding to the
   * current line the scanner's on.
   */
  let $line: number = 1;

  /**
   * A variable corresponding to the
   * current column the scanner's on.
   */
  let $column: number = 1;

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
  const tkn = (type: TOKEN_TYPE, lexeme: string = ""): Token => {
    lexeme = lexeme ? lexeme : slice();
    return token(type, lexeme, $line, $column);
  };

  /**
   * Returns an error token. If called, sets the
   * mutable $error variable, causing scanning to
   * cease.
   * @param message The error message to accompany
   * the Err object.
   * @returns A new Token of type TOKEN_TYPE.ERROR.
   */
  const errorToken = (message: string): Token<TOKEN_TYPE.ERROR, Err> => {
    const errTkn = token(TOKEN_TYPE.ERROR, "", $line, $column);
    $error = lexicalError(message, $line, $column);
    return token(TOKEN_TYPE.ERROR, "", $line, $column).withLiteral($error);
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
          $column++;
          break;
        case "\n":
          $line++;
          $column = 0;
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

  const dictionary: Record<string, () => Token> = {
    this: () => tkn(TOKEN_TYPE.THIS),
    super: () => tkn(TOKEN_TYPE.SUPER),
    class: () => tkn(TOKEN_TYPE.CLASS),
    false: () => tkn(TOKEN_TYPE.FALSE).withLiteral(false),
    true: () => tkn(TOKEN_TYPE.TRUE).withLiteral(true),
    NaN: () => tkn(TOKEN_TYPE.NAN).withLiteral(NaN),
    Inf: () => tkn(TOKEN_TYPE.INF).withLiteral(Infinity),
    return: () => tkn(TOKEN_TYPE.RETURN),
    while: () => tkn(TOKEN_TYPE.WHILE),
    for: () => tkn(TOKEN_TYPE.FOR),
    let: () => tkn(TOKEN_TYPE.LET),
    var: () => tkn(TOKEN_TYPE.VAR),
    fn: () => tkn(TOKEN_TYPE.FN),
    if: () => tkn(TOKEN_TYPE.IF),
    else: () => tkn(TOKEN_TYPE.ELSE),
    print: () => tkn(TOKEN_TYPE.PRINT),
    rem: () => tkn(TOKEN_TYPE.REM),
    mod: () => tkn(TOKEN_TYPE.MOD),
    div: () => tkn(TOKEN_TYPE.DIV),
    nil: () => tkn(TOKEN_TYPE.NIL),
    and: () => tkn(TOKEN_TYPE.AND),
    or: () => tkn(TOKEN_TYPE.OR),
    nor: () => tkn(TOKEN_TYPE.NOR),
    xor: () => tkn(TOKEN_TYPE.XOR),
    xnor: () => tkn(TOKEN_TYPE.XNOR),
    not: () => tkn(TOKEN_TYPE.NOT),
    nand: () => tkn(TOKEN_TYPE.NAND),
  };

  const wordToken = () => {
    while ((isValidNameChar(peek()) || isDigit(peek())) && !atEnd()) {
      tick();
    }
    const word = slice();
    if (dictionary[word]) {
      return dictionary[word]();
    } else {
      return tkn(TOKEN_TYPE.SYMBOL);
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
    return tkn(TOKEN_TYPE.BIG_NUMBER).withLiteral(BigInt(n));
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
    return tkn(TOKEN_TYPE.INTEGER).withLiteral(integerValue);
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
    return tkn(TOKEN_TYPE.INTEGER).withLiteral(integerValue);
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
    return tkn(TOKEN_TYPE.INTEGER).withLiteral(integerValue);
  };

  const numTkn = (
    numberString: string,
    type: NumberTokenType,
    hasSeparators: boolean
  ) => {
    const n = hasSeparators ? numberString.replaceAll("_", "") : numberString;
    switch (type) {
      case TOKEN_TYPE.INTEGER: {
        const num = Number.parseInt(n);
        if (num > Number.MAX_SAFE_INTEGER) {
          return errorToken(
            `Encountered an integer overflow. Consider rewriting “${numberString}” as a bignumber: “#${numberString}”. If “${numberString}” is to be used symbolically, consider rewriting “${numberString}” as a scientific number.`
          );
        } else {
          return tkn(type).withLiteral(num);
        }
      }
      case TOKEN_TYPE.FLOAT: {
        const num = Number.parseFloat(n);
        if (num > Number.MAX_VALUE) {
          return errorToken(
            `Encountered a floating point overflow. Consider rewriting "${n}" as a fraction or bigfraction. If "${n}" is to be used symbolically, consider rewriting "${n}" as a scientific number.`
          );
        }
      }
      case TOKEN_TYPE.FRACTION: {
        const [a, b] = n.split("|");
        const N = Number.parseInt(a);
        const D = Number.parseInt(b);
        return tkn(type).withLiteral(fraction(N, D));
      }
      case TOKEN_TYPE.SCIENTIFIC: {
        const [a, b] = n.split("E");
        const base = Number.parseFloat(a);
        const exponent = Number.parseInt(b);
        return tkn(type).withLiteral(scinum(base, exponent));
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
              'Expected 3 ASCII digits before the separator "_"'
            );
          }
        }
      }
      if (digits !== 3) {
        return errorToken('Expected 3 ASCII digits before the separator "_"');
      }
    }

    // handle floating point numbers
    if (peekIs(".") && isDigit(peekNext())) {
      tick();
      type = TOKEN_TYPE.FLOAT;
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
    }

    // handle fractions
    if (peekIs("|")) {
      if (type !== TOKEN_TYPE.INTEGER) {
        return errorToken('Expected an integer before "|"');
      }
      type = TOKEN_TYPE.FRACTION;
      tick();
      while (isDigit(peek()) && !atEnd()) {
        tick();
      }
      return numTkn(slice(), type, hasSeparators);
    }

    if (peekIs("E")) {
      if (isDigit(peekNext())) {
        type = TOKEN_TYPE.SCIENTIFIC;
        tick();
        while (isDigit(peek())) tick();
      } else if (
        (peekNext() === "+" || peekNext() === "-") &&
        isDigit(lookup(2))
      ) {
        type = TOKEN_TYPE.SCIENTIFIC;
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
        $column = 0;
      } else {
        $column++;
      }
      tick();
    }
    if (atEnd()) return errorToken("Unterminated string");
    tick();
    const lex = slice().slice(1, -1);
    return tkn(TOKEN_TYPE.STRING, lex);
  };

  const scan = (): Token => {
    // Start by skipping whitespace.
    skipWhitespace();

    // Set the $start and $current pointers
    // to the same characters.
    $start = $current;

    // If we've reached the end of the source code,
    // immediately return an END token.
    if (atEnd()) return tkn(TOKEN_TYPE.END, "END");

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
        return tkn(TOKEN_TYPE.POUND_PLUS);
      } else if (match("-")) {
        return tkn(TOKEN_TYPE.POUND_MINUS);
      } else if (match("*")) {
        return tkn(TOKEN_TYPE.POUND_STAR);
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
        return numberToken(TOKEN_TYPE.INTEGER);
      }
    }
    switch (char) {
      case ":":
        return tkn(TOKEN_TYPE.COLON);
      case "&":
        return tkn(TOKEN_TYPE.AMPERSAND);
      case "~":
        return tkn(TOKEN_TYPE.TILDE);
      case "|":
        return tkn(TOKEN_TYPE.VBAR);
      case "(":
        return tkn(TOKEN_TYPE.LEFT_PAREN);
      case ")":
        return tkn(TOKEN_TYPE.RIGHT_PAREN);
      case "[":
        return tkn(TOKEN_TYPE.LEFT_BRACKET);
      case "]":
        return tkn(TOKEN_TYPE.RIGHT_BRACKET);
      case "{":
        return tkn(TOKEN_TYPE.LEFT_BRACE);
      case "}":
        return tkn(TOKEN_TYPE.RIGHT_BRACE);
      case ",":
        return tkn(TOKEN_TYPE.COMMA);
      case "*":
        return tkn(TOKEN_TYPE.STAR);
      case ";":
        return tkn(TOKEN_TYPE.SEMICOLON);
      case "%":
        return tkn(TOKEN_TYPE.PERCENT);
      case "/":
        return tkn(TOKEN_TYPE.SLASH);
      case "^":
        return tkn(TOKEN_TYPE.CARET);
      case "!":
        return tkn(match("=") ? TOKEN_TYPE.BANG_EQUAL : TOKEN_TYPE.BANG);
      case "<":
        return tkn(match("=") ? TOKEN_TYPE.LESS_EQUAL : TOKEN_TYPE.LESS);
      case ">":
        return tkn(match("=") ? TOKEN_TYPE.GREATER_EQUAL : TOKEN_TYPE.GREATER);
      case '"':
        return stringToken();
      case ".": {
        if (match("+")) {
          return tkn(TOKEN_TYPE.DOT_ADD);
        } else if (match("-")) {
          return tkn(TOKEN_TYPE.DOT_MINUS);
        } else if (match("*")) {
          return tkn(TOKEN_TYPE.DOT_STAR);
        } else if (match("^")) {
          return tkn(TOKEN_TYPE.DOT_CARET);
        } else {
          return tkn(TOKEN_TYPE.DOT);
        }
      }
      case "-": {
        if (peek() === "-" && peekNext() === "-") {
          while (peek() !== "\n" && !atEnd()) {
            tick();
          }
          return Token.empty;
        } else {
          return tkn(match("-") ? TOKEN_TYPE.MINUS_MINUS : TOKEN_TYPE.MINUS);
        }
      }
      case "+": {
        return tkn(match("+") ? TOKEN_TYPE.PLUS_PLUS : TOKEN_TYPE.PLUS);
      }

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
          return tkn(match("=") ? TOKEN_TYPE.EQUAL_EQUAL : TOKEN_TYPE.EQUAL);
        }
      }
    }
    return errorToken(`Unknown token: ${char}`);
  };

  const stream = () => {
    const out: Token[] = [];
    let prev = Token.empty;
    let now = scan();
    if (!now.isType(TOKEN_TYPE.EMPTY)) {
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
      if (k.isType(TOKEN_TYPE.EMPTY)) {
        continue;
      } else {
        peek = k;
      }
      // remove trailing commas
      if (
        prev.isRightDelimiter() &&
        now.isType(TOKEN_TYPE.COMMA) &&
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

interface Visitor<T> {
  // Statements
  expressionStatement(node: ExpressionStatement): T;
  blockStatement(node: BlockStatement): T;
  // Expressions
  integer(node: Integer): T;
  float(node: Float): T;
  bool(node: Bool): T;
  nil(node: Nil): T;
  frac(node: Frac): T;
  string(node: StringLiteral): T;
  variable(node: Variable): T;
  logicalBinaryExpression(node: LogicalBinaryExpression): T;
  relationExpression(node: RelationExpression): T;
  algebraicBinaryExpression(node: AlgebraicBinaryExpression): T;
}

enum NODEKIND {
  // Statements
  EXPRESSION_STATEMENT,
  BLOCK_STATEMENT,
  // Expressions
  INTEGER,
  FLOAT,
  BOOL,
  NIL,
  FRACTION,
  STRING,
  VARIABLE,
  LOGICAL_INFIX,
  ALGEBRAIC_INFIX,
  RELATION,
}

abstract class TREENODE {
  abstract get kind(): NODEKIND;
}

abstract class ASTNode extends TREENODE {
  abstract accept<T>(visitor: Visitor<T>): T;
  abstract toString(): string;
  abstract isStatement(): this is Statement;
  abstract isExpr(): this is Expression;
}

abstract class Statement extends ASTNode {
  isStatement(): this is Statement {
    return true;
  }
  isExpr(): this is Expression {
    return false;
  }
  toString(): string {
    return "";
  }
}

class ExpressionStatement extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.expressionStatement(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.EXPRESSION_STATEMENT;
  }
  $expression: Expression;
  $line: number;
  constructor(expression: Expression, line: number) {
    super();
    this.$expression = expression;
    this.$line = line;
  }
}
const exprStmt = (expression: Expression, line: number) =>
  new ExpressionStatement(expression, line);

class BlockStatement extends Statement {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.blockStatement(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.BLOCK_STATEMENT;
  }
  $statements: Statement[];
  constructor(statements: Statement[]) {
    super();
    this.$statements = statements;
  }
}
const blockStmt = (statements: Statement[]) => new BlockStatement(statements);

abstract class Expression extends ASTNode {
  isStatement(): this is Statement {
    return false;
  }
  isExpr(): this is Expression {
    return true;
  }
}

class Integer extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.integer(this);
  }
  toString(): string {
    return `${this.$value}`;
  }
  get kind(): NODEKIND {
    return NODEKIND.INTEGER;
  }
  $value: number;
  constructor(value: number) {
    super();
    this.$value = value;
  }
}
const integer = (n: number) => new Integer(Math.floor(n));

class Float extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.float(this);
  }
  toString(): string {
    return `${this.$value}`;
  }
  get kind(): NODEKIND {
    return NODEKIND.FLOAT;
  }
  $value: number;
  constructor(value: number) {
    super();
    this.$value = value;
  }
}
const float = (n: number) => new Float(n);

class Bool extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.bool(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.BOOL;
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
const bool = (b: boolean) => new Bool(b);

class Nil extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.nil(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.NIL;
  }
  toString(): string {
    return "nil";
  }
  $value: null = null;
  constructor() {
    super();
  }
}
const nil = () => new Nil();

class Frac extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.frac(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.FRACTION;
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
const frac = (value: Fraction) => new Frac(value);

class StringLiteral extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.string(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.STRING;
  }
  toString(): string {
    return `"${this.$value}"`;
  }
  $value: string;
  constructor(value: string) {
    super();
    this.$value = value;
  }
}

class Variable extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.variable(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.VARIABLE;
  }
  toString(): string {
    return `${this.$name.$lexeme}`;
  }
  $name: Token<TOKEN_TYPE.SYMBOL>;
  constructor(name: Token<TOKEN_TYPE.SYMBOL>) {
    super();
    this.$name = name;
  }
}
const variable = (name: Token<TOKEN_TYPE.SYMBOL>) => new Variable(name);

type BinaryLogicOperator =
  | TOKEN_TYPE.AND
  | TOKEN_TYPE.NAND
  | TOKEN_TYPE.NOR
  | TOKEN_TYPE.XNOR
  | TOKEN_TYPE.XOR
  | TOKEN_TYPE.OR;

class LogicalBinaryExpression extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.logicalBinaryExpression(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.LOGICAL_INFIX;
  }
  toString(): string {
    const left = this.$left.toString();
    const op = this.$op.$lexeme;
    const right = this.$right.toString();
    return `${left} ${op} ${right}`;
  }
  $left: Expression;
  $op: Token<BinaryLogicOperator>;
  $right: Expression;
  constructor(
    left: Expression,
    op: Token<BinaryLogicOperator>,
    right: Expression
  ) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}
const logicalBinex = (
  left: Expression,
  op: Token<BinaryLogicOperator>,
  right: Expression
) => new LogicalBinaryExpression(left, op, right);

type RelationOperator =
  | TOKEN_TYPE.LESS
  | TOKEN_TYPE.GREATER
  | TOKEN_TYPE.EQUAL_EQUAL
  | TOKEN_TYPE.BANG_EQUAL
  | TOKEN_TYPE.GREATER_EQUAL
  | TOKEN_TYPE.LESS_EQUAL;

class RelationExpression extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.relationExpression(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.RELATION;
  }
  toString(): string {
    const left = this.$left.toString();
    const op = this.$op.$lexeme;
    const right = this.$right.toString();
    return `${left} ${op} ${right}`;
  }
  $left: Expression;
  $op: Token<RelationOperator>;
  $right: Expression;
  constructor(
    left: Expression,
    op: Token<RelationOperator>,
    right: Expression
  ) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}
const relation = (
  left: Expression,
  op: Token<RelationOperator>,
  right: Expression
) => new RelationExpression(left, op, right);

type AlgebraicOperator =
  | TOKEN_TYPE.PLUS
  | TOKEN_TYPE.STAR
  | TOKEN_TYPE.CARET
  | TOKEN_TYPE.SLASH
  | TOKEN_TYPE.MINUS
  | TOKEN_TYPE.REM
  | TOKEN_TYPE.MOD
  | TOKEN_TYPE.PERCENT
  | TOKEN_TYPE.DIV;

class AlgebraicBinaryExpression extends Expression {
  accept<T>(visitor: Visitor<T>): T {
    return visitor.algebraicBinaryExpression(this);
  }
  get kind(): NODEKIND {
    return NODEKIND.ALGEBRAIC_INFIX;
  }
  toString(): string {
    const left = this.$left.toString();
    const op = this.$op.$lexeme;
    const right = this.$right.toString();
    return `${left} ${op} ${right}`;
  }
  $left: Expression;
  $op: Token<AlgebraicOperator>;
  $right: Expression;
  constructor(
    left: Expression,
    op: Token<AlgebraicOperator>,
    right: Expression
  ) {
    super();
    this.$left = left;
    this.$op = op;
    this.$right = right;
  }
}
const algebraicBinex = (
  left: Expression,
  op: Token<AlgebraicOperator>,
  right: Expression
) => new AlgebraicBinaryExpression(left, op, right);

class ParserState<STMT extends TREENODE, EXPR extends TREENODE> {
  /**
   * Property bound to the current error status.
   * If this variable is not bound to null, then
   * an error occurred. This variable should only
   * be modified through the panic method or
   * through the error method.
   */
  $ERROR: null | Err = null;
  /**
   * If called, sets the $ERROR property of this parser state
   * to the given error. This will stop any further parsing.
   * @param error An Err object.
   * @returns this ParserState.
   */
  panic(error: Err) {
    this.$ERROR = error;
    return this;
  }
  private lexer!: ReturnType<typeof lexical>;
  init(sourceCode: string) {
    this.lexer = lexical(sourceCode);
    this.next();
    return this;
  }
  $prev: Token = Token.empty;
  $peek: Token = Token.empty;
  $current: Token = Token.empty;
  $cursor: number = -1;
  $lastExpression: EXPR;
  $currentExpression: EXPR;
  $lastStatement: NODEKIND;
  $currentStatement: NODEKIND;
  $source: string = "";
  constructor(nilExpression: EXPR, emptyStatement: STMT) {
    this.$lastExpression = nilExpression;
    this.$currentExpression = nilExpression;
    this.$lastStatement = emptyStatement.kind;
    this.$currentStatement = emptyStatement.kind;
  }
  implicitSemicolonOK() {
    return this.$peek.isType(TOKEN_TYPE.END) || this.atEnd();
  }
  newExpression<E extends EXPR>(expression: E) {
    const prev = this.$currentExpression;
    this.$currentExpression = expression;
    this.$lastExpression = prev;
    return right(expression);
  }
  newStatement<S extends STMT>(statement: S) {
    const prev = this.$currentStatement;
    this.$currentStatement = statement.kind;
    this.$lastStatement = prev;
    return right(statement);
  }
  next() {
    this.$cursor++;
    this.$current = this.$peek;
    const nxtToken = this.lexer.scan();
    if (nxtToken.isErrorToken()) {
      this.$ERROR = nxtToken.$literal;
      return Token.end;
    }
    this.$peek = nxtToken;
    return this.$current;
  }
  atEnd() {
    return this.lexer.atEnd() || this.$ERROR !== null;
  }
  error(message: string) {
    const e = syntaxError(message, this.$current.$line, this.$current.$column);
    this.$ERROR = e;
    return left(e);
  }
  check(tokenType: TOKEN_TYPE) {
    if (this.atEnd()) {
      return false;
    } else {
      return this.$peek.isType(tokenType);
    }
  }
  nextIs(tokenType: TOKEN_TYPE) {
    if (this.$peek.isType(tokenType)) {
      this.next();
      return true;
    }
    return false;
  }
}

const enstate = <EXPR extends TREENODE, STMT extends TREENODE>(
  nilExpression: EXPR,
  emptyStatement: STMT
) => new ParserState(nilExpression, emptyStatement);

enum BP {
  NIL,
  LOWEST,
  STRINGOP,
  ASSIGN,
  ATOM,
  OR,
  NOR,
  AND,
  NAND,
  XOR,
  XNOR,
  NOT,
  EQ,
  REL,
  SUM,
  DIFFERENCE,
  PRODUCT,
  QUOTIENT,
  IMUL,
  POWER,
  POSTFIX,
  CALL,
}
type Parslet<T> = (current: Token, lastNode: T) => Either<Err, T>;
type ParsletEntry<T> = [Parslet<T>, Parslet<T>, BP];
type BPTable<T> = Record<TOKEN_TYPE, ParsletEntry<T>>;
function syntax(source: string) {
  const state = enstate<Expression, Statement>(nil(), exprStmt(nil(), -1)).init(
    source
  );
  
  const ___: Parslet<Expression> = (t) => {
    if (state.$ERROR !== null) return left(state.$ERROR);
    return state.error(`Unexpected token: ${t.$lexeme}`);
  }
  const ___o = BP.NIL;
  
  const rules: BPTable<Expression> = {
    [TOKEN_TYPE.END]: [___, ___, ___o],
    [TOKEN_TYPE.ERROR]: [___, ___, ___o],
    [TOKEN_TYPE.EMPTY]: [___, ___, ___o],
    [TOKEN_TYPE.LEFT_PAREN]: [___, ___, ___o],
    [TOKEN_TYPE.RIGHT_PAREN]: [___, ___, ___o],
    [TOKEN_TYPE.LEFT_BRACE]: [___, ___, ___o],
    [TOKEN_TYPE.RIGHT_BRACE]: [___, ___, ___o],
    [TOKEN_TYPE.LEFT_BRACKET]: [___, ___, ___o],
    [TOKEN_TYPE.RIGHT_BRACKET]: [___, ___, ___o],
    [TOKEN_TYPE.SEMICOLON]: [___, ___, ___o],
    [TOKEN_TYPE.COLON]: [___, ___, ___o],
    [TOKEN_TYPE.DOT]: [___, ___, ___o],
    [TOKEN_TYPE.COMMA]: [___, ___, ___o],
    [TOKEN_TYPE.PLUS]: [___, ___, ___o],
    [TOKEN_TYPE.MINUS]: [___, ___, ___o],
    [TOKEN_TYPE.STAR]: [___, ___, ___o],
    [TOKEN_TYPE.SLASH]: [___, ___, ___o],
    [TOKEN_TYPE.CARET]: [___, ___, ___o],
    [TOKEN_TYPE.PERCENT]: [___, ___, ___o],
    [TOKEN_TYPE.BANG]: [___, ___, ___o],
    [TOKEN_TYPE.AMPERSAND]: [___, ___, ___o],
    [TOKEN_TYPE.TILDE]: [___, ___, ___o],
    [TOKEN_TYPE.VBAR]: [___, ___, ___o],
    [TOKEN_TYPE.EQUAL]: [___, ___, ___o],
    [TOKEN_TYPE.LESS]: [___, ___, ___o],
    [TOKEN_TYPE.GREATER]: [___, ___, ___o],
    [TOKEN_TYPE.LESS_EQUAL]: [___, ___, ___o],
    [TOKEN_TYPE.GREATER_EQUAL]: [___, ___, ___o],
    [TOKEN_TYPE.BANG_EQUAL]: [___, ___, ___o],
    [TOKEN_TYPE.EQUAL_EQUAL]: [___, ___, ___o],
    [TOKEN_TYPE.PLUS_PLUS]: [___, ___, ___o],
    [TOKEN_TYPE.MINUS_MINUS]: [___, ___, ___o],
    [TOKEN_TYPE.STAR_STAR]: [___, ___, ___o],
    [TOKEN_TYPE.DOT_ADD]: [___, ___, ___o],
    [TOKEN_TYPE.DOT_STAR]: [___, ___, ___o],
    [TOKEN_TYPE.DOT_MINUS]: [___, ___, ___o],
    [TOKEN_TYPE.DOT_CARET]: [___, ___, ___o],
    [TOKEN_TYPE.AT]: [___, ___, ___o],
    [TOKEN_TYPE.POUND_PLUS]: [___, ___, ___o],
    [TOKEN_TYPE.POUND_MINUS]: [___, ___, ___o],
    [TOKEN_TYPE.POUND_STAR]: [___, ___, ___o],
    [TOKEN_TYPE.SYMBOL]: [___, ___, ___o],
    [TOKEN_TYPE.STRING]: [___, ___, ___o],
    [TOKEN_TYPE.BOOLEAN]: [___, ___, ___o],
    [TOKEN_TYPE.INTEGER]: [___, ___, ___o],
    [TOKEN_TYPE.FLOAT]: [___, ___, ___o],
    [TOKEN_TYPE.FRACTION]: [___, ___, ___o],
    [TOKEN_TYPE.SCIENTIFIC]: [___, ___, ___o],
    [TOKEN_TYPE.BIG_NUMBER]: [___, ___, ___o],
    [TOKEN_TYPE.FALSE]: [___, ___, ___o],
    [TOKEN_TYPE.TRUE]: [___, ___, ___o],
    [TOKEN_TYPE.NAN]: [___, ___, ___o],
    [TOKEN_TYPE.INF]: [___, ___, ___o],
    [TOKEN_TYPE.NIL]: [___, ___, ___o],
    [TOKEN_TYPE.NUMERIC_CONSTANT]: [___, ___, ___o],
    [TOKEN_TYPE.ALGEBRAIC]: [___, ___, ___o],
    [TOKEN_TYPE.AND]: [___, ___, ___o],
    [TOKEN_TYPE.OR]: [___, ___, ___o],
    [TOKEN_TYPE.NOT]: [___, ___, ___o],
    [TOKEN_TYPE.NAND]: [___, ___, ___o],
    [TOKEN_TYPE.XOR]: [___, ___, ___o],
    [TOKEN_TYPE.XNOR]: [___, ___, ___o],
    [TOKEN_TYPE.NOR]: [___, ___, ___o],
    [TOKEN_TYPE.IF]: [___, ___, ___o],
    [TOKEN_TYPE.ELSE]: [___, ___, ___o],
    [TOKEN_TYPE.FN]: [___, ___, ___o],
    [TOKEN_TYPE.LET]: [___, ___, ___o],
    [TOKEN_TYPE.VAR]: [___, ___, ___o],
    [TOKEN_TYPE.RETURN]: [___, ___, ___o],
    [TOKEN_TYPE.WHILE]: [___, ___, ___o],
    [TOKEN_TYPE.FOR]: [___, ___, ___o],
    [TOKEN_TYPE.CLASS]: [___, ___, ___o],
    [TOKEN_TYPE.PRINT]: [___, ___, ___o],
    [TOKEN_TYPE.SUPER]: [___, ___, ___o],
    [TOKEN_TYPE.THIS]: [___, ___, ___o],
    [TOKEN_TYPE.REM]: [___, ___, ___o],
    [TOKEN_TYPE.MOD]: [___, ___, ___o],
    [TOKEN_TYPE.DIV]: [___, ___, ___o]
  }
}
