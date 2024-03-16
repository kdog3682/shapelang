export { Vector, Group, ORIGIN, Shape, Line, Rect, Circle, Polygon, Triangle, Point, Square, }
import {fixFloatingPoint, ffp, toRadians} from "/home/kdog3682/2024-javascript/js-toolkit/mathkit/index.js"
/* prettier-ignore */ import {must, fooga, assignAllowed, simpleRecursiveWalk, simpleArgument, exprTemplater, stringifyIfNotPrimitive, panic, stringerf, wrapFunction, regexTemplater, iso8601, strftime, walkChildEntries, getFiletype, matchf, isUrl, looksLikeFunction, toArgument2, notNull, CumulativeStorage2, simpleAssign, findAndMatch, infuseObjectArray, regexGetter, splitArg, isJavascriptComment, runTest, everyOther, splitByRange, get_regex, getImports, isJavascriptFile, isValidDateString, WriteObject, equalityf, group2, splitOnce2, dedent4, isLowerCase, looksLikeRegExpString, isRegExpString, getDependencies, camelSplit, toggle3, countf, isLiteralObject, bindMethodsAndState2, call, mget3, looks_like_object_function, create_functions_from_master, toStringArgumentPretty, codeChunks, smart_map, run_tests, hasStartingCallable, mapTemplate, aliaser, fixSelector, htmlTags, removePythonComments, simpleStringBreaker, colonConfig, operations, error, redColon, so2, group, parseSingleLineJson, Items, findDependencies, tryf, find4, localeString, cssComment, colonConfigf, trimArray, parseCallable, bringToLifeTextFix, localBringToLife, getCaller4, getErrorStack, forEach, getBindings, getExports, matchstr, filter4, allEqual, count, isNativeFunction, repeat, getIndentAndText, StopWatch, stringDictSetter, getFunctionInfo, runRef, toLines, hasCallable, getProseWords, tally, paramify, codeSplit, debugConfig, logConfig, blueColon, toStringArgument3, stringCallable, simpleBinding, dashSplit4, appendBelow, appendAbove, removeLineComments, prependIfNecessary, smartDedent4, blueSandwich, walk4, findLineIndex, parseAB, applyTransform, kebabCase, getExcerpt, sortObject, buildFunction, maybeSort, parseFunction, isTypable, frontMatter, dictEntry, insertAfterIndex, State, bindMethods, cpop, tagRE, toggle2, createFunction2, assignIncrementedIndex, ufa, assignArray, regexFromComment, createParsersFromObject, imatch, globalConsoleDebug, bindMethodsAndState, isQuestion, oxfordComma, isUpperCase, getFunctionIdentifier, filter3, match, getMatch, alternatef, reCombine, assertion2, deepEqual, hasDollar, so, deepAssign, Tally, getFunctionNames, throwError, notEqual, tryString, prettyPrintCodeSnippet, prettyPrintErrorStack, iter, quotify, transformerf, assign, defineBinding, jspy, linebreak, stringCall2, reduce3, getClassParameters, assignOnTop2, isIdentifier, ndy, dashSplit3, runFunctionFromRef, equalf, alphabet, stateGetterFromSchema, mreplace, require, topLineComment, isChinese, replacef, ignoref, codeLibrary, splitLines, addArgumentQuotes, getBindings2, addCaret, mget2, getStartingConfig, incrementalEat, strlen, hr, setOnce, unescapeHtml, oxford, breakerf, runTests, map3, dateSplit, transformDict, walk3, toRegExp, tryAgainf, assertNotNull, getArgumentObject, isArgumentObject, typef, requireArg, keyAndValue, assignf, stateGetter3, objectFromArguments2, assignDefaults, transformValue, assign3, assignFresh3, evaluate3, scopedEvaluator, objectFromArguments, enforce, sub, filterObject, extractStartingJsonLikeConfig, unbrackify, newlineIndent2, deleteLine, both, normalizeIndent, getComment, secondComment, isStringRegExp, dashSplit2, clock, warning, errorStringify, alert, labelCase, bottomComment, stringCompose, getAnyIdentifier, chalkf, getNumbers, partitions, has, addUnit, toCallable, unquote, filter2, warn2, join2, caller2, assignOnce, longShort, shortLong, argPop, caller, assignOnTop, toggle, defineWindow, unescapeNewlines, escapeQuotes, unescapeQuotes, escapeNewlines, setAliases, announceCaller, removeStartingComments, smartBind, assignExisting, isObjectWithKey, eatStart, modularIncrementItem, getRegex, runFunction, isObjectLikeArray, itemGetter2, getAllKeys, prefixSlice, hasQuotes, assertion, diff, toggleState, initState, dunder, objectGetter, superTransform, popFilter, testRunner, assert2, insertAtDollar, popEmptyLine, getOptions, mergeSpecs, sortByKeys, map2, strictMessengerAssert, smartSplit, chalk4, typeLog, getFunctionName, Clock, search3, MyError, fuzzyMatch3, debugDisplay, getCaller3, messengerAssert, camelSlice, setPrototype, assignAliases, display, modifyNumber, toDict, setPush, modularIncrementIndex, longstamp, popIndex, toggleOnOff, locWrap, walk2, typeMatch, prettyStringify, getIdentifiers, CustomError, argMatch, brackify2, smartestDedent, modularIncrementNumber, AbstractMethodError, allUnique, Trie, boundarySplit, numberBoundarySplit, nodeLog, getFirst, defineProperty, supermix, partial, timeLog, timestamp, raise, getIdentifier, conditionalPrefix, conditionalSuffix, QueryList, fuzzyMatch2, buildDict, getTextAndCommand, sprawlFactory, getParameters2, pushf, intersection, union, blue, green, sandwich, getLastSpaces, smartDedent3, red, sort, debounce, checkValue, getCodeChunks, logf, boundary, myError, conditional, isStringFunction, toSpaces, objectf, searchAll, difference, singleQuote, itemGetter, slice2, mergeObjects, once, dashSplit, nchalk, coerceToObject, ArrayState, exporter2, indent2, iterator, removeAllComments, countParams, cumulativeSchemaAssign, argKwargSplit, argParse, removeInlineComments, getFrontMatter, hasHtmlSuffix, lazyArray, isThisFunction, escapeHTML, getKwargs2, search2, toStringArgument, createFuzzyMatch, edit2, splice, zip, merge2, argArgsKwargs, fill2, chalk, vueWrap, splitArray, splitArray2, warn, makeRunner2, searchf2, smartDedent2, dedent2, toArray2, stateGetter2, sortByIndex, IndexedCache, argo, curry2, doUntil2, evaluate2, findall2, findIndex2, findItem2, getCaller2, getErrorStack2, isJson, indexGetter2, insert2, pop2, parseError2, remove2, reduce2, testf2, type2, unshift2, waterfall2, xsplit2, Cache, cumulativeAssign, replaceBefore, topComment, isAsyncFunction, mapSort, getFileURI, getQuotes, isClassObject, isInitialized, getFallback, bindingRE, addObjectOrObjectProperty, forDoubles, isCss, log, iterate, backAndForth, round, iteratorWrapper, toJSON, isFromMap, toString, empty, conditionalString, getConfigArg, hasKey, errorWrap, successWrap, check, toPoints, bind, mixinAliases, isPercentage, isBasicType, reducerStrategy, gather, entries, stateGetter, methodCase, vueCase, push2, smarterIndent, lineSplit, Store, isSingleLetter, prepareText, isSymbol, getShortest, slice, KeyError, deepCopy, argsKwargs, isError, isColor, list, objectEditor, matchall, makeFastRunner, announce, hasLetter, filter, reduce, stringCall, capitalizeName, stop, proseCase, lineDitto, mixinSetters, modularIncrement, distinct, definedSort, groupBy, reWrap2, fuzzyMatch, isPlural, Element, parseError, isPrimitiveArray, callableArgsKwargs, waterfall, defineVariable, info, flat2D, splitThePage, handleError, dedent, TypeAssertion, createFunction, pluralize, remove, PageStorage, Storage, UniqueStorage, Watcher, arrayToDict, addProperty, addQuotes, argWrapFactory, assert, abrev, abf, addExtension, assignFresh, antif, atFirst, atSecond, backspace, bindObject, breaker, blockQuote, brackify, bringToLife, comment, countCaptureGroups, capitalizeTitle, classMixin, callableRE, camelToTitle, curry, createVariable, changeExtension, curryStart, curryEnd, capitalize, copy, camelCase, compose, char2n, camelToDash, deepMerge, datestamp, doublef, dictSetter, dictSetter2, dsearch, doUntil, dashCase, doubleQuote, dict, dictGetter, depluralize, dreplace, dictf, endsWithWord, exporter, edit, exists, evaluate, extend, find, flatMap, fill, fixUrl, functionGetter, findall, fixPath, flat, fparse, findIndex, firstLine, ftest, getKwargs, getFirstName, getBindingName, getParameters, getLastWord, getCodeWords, getCodeWords2, getIndent, getExtension, getLast, getLongest, getChunks, getCaller, getStackTrace, getConstructorName, getFirstWord, getWords, getSpaces, hasComma, hasSpaces, hasHtml, hasBracket, hasNewline, hasCaptureGroup, hasEquals, hasValue, hasCamelCase, hasNumber, hackReplace, insert, indexGetter, incrementf, isCallable, isQuote, isEven, isOdd, isLast, isHTML, isNode, interweave, inferLang, isString, isArray, isObject, isDefined, isFunction, isPrimitive, isNumber, isSet, isNestedArray, indent, isNull, isWord, isBoolean, isRegExp, identity, isObjectLiteral, isJsonParsable, isCapitalized, isNewLine, isObjectArray, isStringArray, isClassFunction, joinSpaces, join, keyArrayToObject, lowerCase, linebreakRE, len, lineGetter, lineCount, lastLine, logConsole, makeRunner, mixin, modularf, matchGetter, merge, mget, map, mergeOnTop, mergeToObject, mapFilter, noop, nestPush, no, newlineIndent, n2char, objectWalk, overlap, objectToString, opposite, pipe, parseTopAttrs, pascalCase, partition, parens, push, pop, parseJSON, rigidSort, removeQuotes, rep, removeComments, range, removeExtension, rescape, reverse, reWrap, reduceToString, repeatUntil, swapKey, sayhi, swap, splitMapJoin, splitCamel, smallify, search, stringify, shared, smartDedent, stringBreaker, sleep, split, snakeCase, stringArgument, sorted, splitOnce, searchf, secondLine, titleCase, textOrJson, toNumber, toArgument, toNestedArray, test, type, tail, transformObject, trim, testf, toArray, templater, totalOverlap, upperCase, unique, uncapitalize, unzip, wrap, walk, wrapf, xsplit, yes, zip2} from "/home/kdog3682/2023/utils.js"

class Group {
    constructor(...shapes) {
        this.shapes = flat(shapes)
    }
    toJSON() {
        return this.shapes.map((x) => x.toJSON())
    }
    apply(key, ...args) {
        return new Group(this.shapes.map((x) => x.apply(key, ...args)))
    }
    map(fn) {
        return new Group(this.shapes.map(fn))
    }
    translate(...args) {
        return this.apply('translate', ...args)
    }
    rotate(...args) {
        return this.apply('rotate', ...args)
    }
}

class ShapeAttributes {
    constructor(attrs) {
        this.attrs = exists(attrs) ? copy(attrs) : {}
        this.keys = ['points']
    }

    with(points) {
        return new this.constructor(...points, this.attrs)
    }
    fill(fill) { return this.set('fill', fill) }
    thickness(thickness) { return this.set('thickness', thickness) }
    stroke(stroke) { return this.set('stroke', stroke) }
    get type() {
        return type(this)
    }
    toJSON() {
        if ('json' in this) {
            return this.json()
        }
        const baseKeys = [
            "type",
            "attrs",
        ]
        const keys = this.keys.concat(baseKeys)
        const reducer = (key) => {
            const value = key == 'points'
                ? this[key].map((x) => x.toJSON())
                : this[key]
            return [key, value]
        }
        return reduce(keys, reducer)
    }
    set(k, v) {
        if (this.keys.includes(k)) {
            this[k] = v
        } else {
            this.attrs[k] = v
        }
        return this
    }
}
class Shape extends ShapeAttributes {
    apply(key, ...args) {
        const mapper = (point) => {
            return point[key](...args)
        }
        const points = this.points.map(mapper)
        return this.with(points)
    }
    translate(...args) {
        return this.apply('translate', ...args)
    }
    rotate(...args) {
        return this.apply('rotate', ...args)
    }
}

class Point {
    constructor(x, y) {
        this.x = x
        this.y = y
    }
    copy() {
        return new Point(this.x, this.y)
    }
    translate(o) {
        if (arguments.length == 3) {
            const x = isDefined(arguments[0]) ? arguments[0] : this.x
            const y = isDefined(arguments[1]) ? arguments[1] : this.y
            return new Point(x, y)
        }
        if (arguments.length == 2) {
            return new Point(this.x + arguments[0], this.y + arguments[1])
        }
        if (type(arguments[0]) == 'Vector') {
            return new Point(this.x + arguments[0].x, this.y + arguments[0].y)
        }

        let {dx, dy, angle, length} = o
        if (isDefined(angle)) {
            angle = toRadians(angle)
            const x = this.x + length * Math.cos(angle)
            const y = this.y + length * Math.sin(angle)
            return new Point(x, y)
        }
        if (isDefined(dx)) {
            return new Point(dx, dy)
        }
        panic('{angle, length} or 2 arguments')
    }
    rotate(degrees, pivot) {
        if (!pivot) {
            pivot = ORIGIN
        }

        console.log(degrees, pivot)
        return rotatePointAroundPoint(this, pivot, degrees)

        const radians = toRadians(degrees)
        const cos = Math.cos(radians)
        const sin = Math.sin(radians)
        return pivot.translate(
            (this.x - pivot.x) * cos,
            (this.y - pivot.y) * sin
        )
    }
    toJSON() {
        return { x: this.x, y: this.y }
    }
}

class Circle extends Shape {
    constructor(center, radius, attrs) {
        super(attrs)
        this.center = center // Center is a Point
        this.radius = radius // radius is a number
        this.keys = [
            "center",
            "radius",
        ]
    }
    json() {
        const {x, y} = this.center.toJSON()
        return {
            tag: 'circle',
            attrs: {
                cx: x,
                cy: y,
                r: this.radius,
                ...compute(this.attrs)
            }
        }
    }
    set points(x) {
        this.center = x[0]
    }
    get points() {
        return [this.center]
    }
    with(points) {
        return new Circle(points[0], this.radius, this.attrs)
    }
    get north() {
        return new Point(this.center.x, this.center.y - this.radius)
    }
    get south() {
        return new Point(this.center.x, this.center.y + this.radius)
    }
    get east() {
        return new Point(this.center.x + this.radius, this.center.y)
    }
    get west() {
        return new Point(this.center.x - this.radius, this.center.y)
    }
    get northeast() {
        return this.center.translate(
            this.radius / Math.sqrt(2),
            -this.radius / Math.sqrt(2)
        )
    }

    get southeast() {
        return this.center.translate(
            this.radius / Math.sqrt(2),
            this.radius / Math.sqrt(2)
        )
    }

    get southwest() {
        return this.center.translate(
            -this.radius / Math.sqrt(2),
            this.radius / Math.sqrt(2)
        )
    }

    get northwest() {
        return this.center.translate(
            -this.radius / Math.sqrt(2),
            -this.radius / Math.sqrt(2)
        )
    }
}

class Line extends Shape {

    json() {
        const start = this.start.toJSON()
        const end = this.end.toJSON()
        return {
            tag: 'line',
            attrs: {
                x1: start.x,
                y1: start.y,
                x2: end.x,
                y2: end.y,
                ...compute(this.attrs),
            }
        }
    }

    get length() {
        return Math.sqrt(Math.pow(this.end.x - this.start.x, 2) + Math.pow(this.end.y - this.start.y, 2));
    }

    set length(newLength) {
        const angle = this.angle; // Current angle of the line
        this.end = new Point(
            this.start.x + Math.cos(angle) * newLength,
            this.start.y + Math.sin(angle) * newLength
        );
    }

    // Getter for angle (in radians)
    get angle() {
        return Math.atan2(this.end.y - this.start.y, this.end.x - this.start.x);
    }

    // Setter for angle (in radians)
    set angle(newAngle) {
        const length = this.length; // Current length of the line
        this.end = new Point(
            this.start.x + Math.cos(newAngle) * length,
            this.start.y + Math.sin(newAngle) * length
        );
    }
    get width() {
        return Math.abs(this.b.x - this.a.x);
    }

    // Setter for width
    set width(newWidth) {
        // Assuming width changes occur by adjusting the x-coordinate of point b
        this.b = new Point(this.a.x + newWidth, this.b.y);
    }

    // Getter for height
    get height() {
        return Math.abs(this.b.y - this.a.y);
    }

    // Setter for height
    set height(newHeight) {
        // Assuming height changes occur by adjusting the y-coordinate of point b
        this.b = new Point(this.b.x, this.a.y + newHeight);
    }


    constructor(start, end, attrs) {
        super(attrs)
        this.start = start // Start is start Point
        this.end = end // End is start Point
    }
    set points(points) {
        this.start = points[0]
        this.end = points[1]
    }
    get points() {
        return [this.start, this.end]
    }
}
class Polygon extends Shape {
    constructor(points, attrs) {
        super(attrs);
        this.points = points
    }
    with(points) {
        return new Polygon(points, this.attrs)
    }
    add(point) {
        return this.with(this.points.concat(point))
    }
}


class Triangle extends Shape {
    constructor(c, a, b, attrs) {
        super(attrs);
        this.c = c;
        this.a = a;
        this.b = b;
    }
    get points() {
        return [this.c, this.a, this.b]
    }
    set points(points) {
        ;[this.c, this.a, this.b] = points
    }
}

class Rect extends Shape {
    json() {
        return {
            tag: 'rect',
            attrs: {
                ...this.start.toJSON(),
                width: this.width,
                height: this.height,
                ...compute(this.attrs)
            }
        }
    }
    constructor(a, b, attrs) {
        super(attrs)
        this.a = a // bottom-left corner
        this.b = b // top-right corner
    }
    get points() {
        return [this.a, this.b]
    }
    set points(points) {
        this.a = points[0]
        this.b = points[1]
    }
    get width() {
        return Math.abs(this.b.x - this.a.x)
    }
    get height() {
        return Math.abs(this.b.y - this.a.y)
    }
    get north() {
        return new Point(this.a.x + (this.b.x - this.a.x) / 2, this.b.y)
    }
    get south() {
        return new Point(this.a.x + (this.b.x - this.a.x) / 2, this.a.y)
    }
    get east() {
        return new Point(this.b.x, this.a.y + (this.b.y - this.a.y) / 2)
    }
    get west() {
        return new Point(this.a.x, this.a.y + (this.b.y - this.a.y) / 2)
    }
    get northeast() {
        return this.b
    }
    get northwest() {
        return new Point(this.a.x, this.b.y)
    }
    get southeast() {
        return new Point(this.b.x, this.a.y)
    }
    get southwest() {
        return this.a
    }
    get midpoint() {
        return new Point((this.a.x + this.b.x) / 2, (this.a.y + this.b.y) / 2)
    }
    get center() {
        return this.midpoint
    }
}
class Square extends Rect {
    constructor(point, length, attrs) {
        super(point, point.translate(length, length), attrs)
    }
    get length() {
        return this.b.x - this.a.x
    }
    set length(newLength) {
        this.b = this.a.translate(newLength, newLength)
    }
}

const ORIGIN = new Point(0, 0)


function rotatePointAroundPoint(point, center, angle) {
  // Destructure the points
  const {x: px, y: py} = point;
  const {x: cx, y: cy} = center;

  // Convert angle from degrees to radians
  // const radians = angle * (Math.PI / 180);
    const radians = toRadians(angle)
  // Translate point to origin
  const xTranslated = px - cx;
  const yTranslated = py - cy;

  // Rotate point
  const xRotated = xTranslated * Math.cos(radians) - yTranslated * Math.sin(radians);
  // console.log({xRotated})
  const yRotated = xTranslated * Math.sin(radians) + yTranslated * Math.cos(radians);
  // console.log({yRotated})

  // Translate point back
  const xFinal = xRotated + cx;
  const yFinal = yRotated + cy;
  return new Point(xFinal, yFinal)
}

function compute(styleAttrs) {
    const p = {}
    if (!styleAttrs) {
        return p
    }
    const aliases = {
        'stroke': 'stroke',
        'thickness': 'stroke-width',
        'fill': 'fill',
    }

    for (const [k, v] of entries(styleAttrs)) {
        const key = must(aliases, k)
        p[key] = v
    }

    if (p.stroke && !p.hasOwnProperty('stroke-width')) {
        p['stroke-width'] = 1
    }
    if (p['stroke-width'] && !p.hasOwnProperty('stroke')) {
        p['stroke'] = 'black'
    }
    return p
}


function getPoint(a) {
    return isObject(a[0]) || isPoint(a[0]) ? [a[0].x, a[0].y] : a
}

class Vector {
    constructor(...args) {
        const [x, y] = getPoint(args)
        this.x = x
        this.y = y
    }

    add(other) {
        return new Vector(this.x + other.x, this.y + other.y)
    }

    subtract(other) {
        return new Vector(this.x - other.x, this.y - other.y)
    }

    multiply(scalar) {
        return new Vector(this.x * scalar, this.y * scalar)
    }

    dot(other) {
        return this.x * other.x + this.y * other.y
    }

    magnitude() {
        return Math.sqrt(this.x ** 2 + this.y ** 2)
    }

    flip(x, y) {
        let n = 1
        let m = 1
        if (x || y) {
            if (x) {
                m = -1
            }
            if (y) {
                n = -1
            }
        } else {
            m = -1
            n = -1
        }
        return new Vector(m * this.x, n * this.y)
    }

    toString() {
        return `(${this.x}, ${this.y})`
    }
}

function isPoint(x) {
    return type(x) == 'Point'
}
