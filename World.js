import {asciiGrid} from "/home/kdog3682/2024-javascript/js-toolkit/Ascii.js"
import {fixFloatingPoint, average, sum, ffp, toRadians} from "/home/kdog3682/2024-javascript/js-toolkit/mathkit/index.js"
import {xmlString} from "/home/kdog3682/2024-javascript/js-toolkit/xmlString.js"
import { Palette } from "/home/kdog3682/2024-javascript/shapelang/Palette.js"
import * as variables from "/home/kdog3682/2023/variables.js"
import {Group, Vector, ORIGIN, Shape, Line, Rect, Circle, Polygon, Triangle, Point, Square} from "/home/kdog3682/2024-javascript/shapelang/shapes.js"
export {
    World,
}
/* prettier-ignore */ import * as shapes from "/home/kdog3682/2024-javascript/shapelang/shapes.js"
/* prettier-ignore */ import {fooga, assignAllowed, simpleRecursiveWalk, simpleArgument, exprTemplater, stringifyIfNotPrimitive, panic, stringerf, wrapFunction, regexTemplater, iso8601, strftime, walkChildEntries, getFiletype, matchf, isUrl, looksLikeFunction, toArgument2, notNull, CumulativeStorage2, simpleAssign, findAndMatch, infuseObjectArray, regexGetter, splitArg, isJavascriptComment, runTest, everyOther, splitByRange, get_regex, getImports, isJavascriptFile, isValidDateString, WriteObject, equalityf, group2, splitOnce2, dedent4, isLowerCase, looksLikeRegExpString, isRegExpString, getDependencies, camelSplit, toggle3, countf, isLiteralObject, bindMethodsAndState2, call, mget3, looks_like_object_function, create_functions_from_master, toStringArgumentPretty, codeChunks, smart_map, run_tests, hasStartingCallable, mapTemplate, aliaser, fixSelector, htmlTags, removePythonComments, simpleStringBreaker, colonConfig, operations, error, redColon, so2, group, parseSingleLineJson, Items, findDependencies, tryf, find4, localeString, cssComment, colonConfigf, trimArray, parseCallable, bringToLifeTextFix, localBringToLife, getCaller4, getErrorStack, forEach, getBindings, getExports, matchstr, filter4, allEqual, count, isNativeFunction, repeat, getIndentAndText, StopWatch, stringDictSetter, getFunctionInfo, runRef, toLines, hasCallable, getProseWords, tally, paramify, codeSplit, debugConfig, logConfig, blueColon, toStringArgument3, stringCallable, simpleBinding, dashSplit4, appendBelow, appendAbove, removeLineComments, prependIfNecessary, smartDedent4, blueSandwich, walk4, findLineIndex, parseAB, applyTransform, kebabCase, getExcerpt, sortObject, buildFunction, maybeSort, parseFunction, isTypable, frontMatter, dictEntry, insertAfterIndex, State, bindMethods, cpop, tagRE, toggle2, createFunction2, assignIncrementedIndex, ufa, assignArray, regexFromComment, createParsersFromObject, imatch, globalConsoleDebug, bindMethodsAndState, isQuestion, oxfordComma, isUpperCase, getFunctionIdentifier, filter3, match, getMatch, alternatef, reCombine, assertion2, deepEqual, hasDollar, so, deepAssign, Tally, getFunctionNames, throwError, notEqual, tryString, prettyPrintCodeSnippet, prettyPrintErrorStack, iter, quotify, transformerf, assign, defineBinding, jspy, linebreak, stringCall2, reduce3, getClassParameters, assignOnTop2, isIdentifier, ndy, dashSplit3, runFunctionFromRef, equalf, alphabet, stateGetterFromSchema, mreplace, require, topLineComment, isChinese, replacef, ignoref, codeLibrary, splitLines, addArgumentQuotes, getBindings2, addCaret, mget2, getStartingConfig, incrementalEat, strlen, hr, setOnce, unescapeHtml, oxford, breakerf, runTests, map3, dateSplit, transformDict, walk3, toRegExp, tryAgainf, assertNotNull, getArgumentObject, isArgumentObject, typef, requireArg, keyAndValue, assignf, stateGetter3, objectFromArguments2, assignDefaults, transformValue, assign3, assignFresh3, evaluate3, scopedEvaluator, objectFromArguments, enforce, sub, filterObject, extractStartingJsonLikeConfig, unbrackify, newlineIndent2, deleteLine, both, normalizeIndent, getComment, secondComment, isStringRegExp, dashSplit2, clock, warning, errorStringify, alert, labelCase, bottomComment, stringCompose, getAnyIdentifier, chalkf, getNumbers, partitions, has, addUnit, toCallable, unquote, filter2, warn2, join2, caller2, assignOnce, longShort, shortLong, argPop, caller, assignOnTop, toggle, defineWindow, unescapeNewlines, escapeQuotes, unescapeQuotes, escapeNewlines, setAliases, announceCaller, removeStartingComments, smartBind, assignExisting, isObjectWithKey, eatStart, modularIncrementItem, getRegex, runFunction, isObjectLikeArray, itemGetter2, getAllKeys, prefixSlice, hasQuotes, assertion, diff, toggleState, initState, dunder, objectGetter, superTransform, popFilter, testRunner, assert2, insertAtDollar, popEmptyLine, getOptions, mergeSpecs, sortByKeys, map2, strictMessengerAssert, smartSplit, chalk4, typeLog, getFunctionName, Clock, search3, MyError, fuzzyMatch3, debugDisplay, getCaller3, messengerAssert, camelSlice, setPrototype, assignAliases, display, modifyNumber, toDict, setPush, modularIncrementIndex, longstamp, popIndex, toggleOnOff, locWrap, walk2, typeMatch, prettyStringify, getIdentifiers, CustomError, argMatch, brackify2, smartestDedent, modularIncrementNumber, AbstractMethodError, allUnique, Trie, boundarySplit, numberBoundarySplit, nodeLog, getFirst, defineProperty, supermix, partial, timeLog, timestamp, raise, getIdentifier, conditionalPrefix, conditionalSuffix, QueryList, fuzzyMatch2, buildDict, getTextAndCommand, sprawlFactory, getParameters2, pushf, intersection, union, blue, green, sandwich, getLastSpaces, smartDedent3, red, sort, debounce, checkValue, getCodeChunks, logf, boundary, myError, conditional, isStringFunction, toSpaces, objectf, searchAll, difference, singleQuote, itemGetter, slice2, mergeObjects, once, dashSplit, nchalk, coerceToObject, ArrayState, exporter2, indent2, iterator, removeAllComments, countParams, cumulativeSchemaAssign, argKwargSplit, argParse, removeInlineComments, getFrontMatter, hasHtmlSuffix, lazyArray, isThisFunction, escapeHTML, getKwargs2, search2, toStringArgument, createFuzzyMatch, edit2, splice, zip, merge2, argArgsKwargs, fill2, chalk, vueWrap, splitArray, splitArray2, warn, makeRunner2, searchf2, smartDedent2, dedent2, toArray2, stateGetter2, sortByIndex, IndexedCache, argo, curry2, doUntil2, evaluate2, findall2, findIndex2, findItem2, getCaller2, getErrorStack2, isJson, indexGetter2, insert2, pop2, parseError2, remove2, reduce2, testf2, type2, unshift2, waterfall2, xsplit2, Cache, cumulativeAssign, replaceBefore, topComment, isAsyncFunction, mapSort, getFileURI, getQuotes, isClassObject, isInitialized, getFallback, bindingRE, addObjectOrObjectProperty, forDoubles, isCss, log, iterate, backAndForth, round, iteratorWrapper, toJSON, isFromMap, toString, empty, conditionalString, getConfigArg, hasKey, errorWrap, successWrap, check, bind, mixinAliases, isPercentage, isBasicType, reducerStrategy, gather, entries, stateGetter, methodCase, vueCase, push2, smarterIndent, lineSplit, Store, isSingleLetter, prepareText, isSymbol, getShortest, slice, KeyError, deepCopy, argsKwargs, isError, isColor, list, objectEditor, matchall, makeFastRunner, announce, hasLetter, filter, reduce, stringCall, capitalizeName, stop, proseCase, lineDitto, mixinSetters, modularIncrement, distinct, definedSort, groupBy, reWrap2, fuzzyMatch, isPlural, Element, parseError, isPrimitiveArray, callableArgsKwargs, waterfall, defineVariable, info, flat2D, splitThePage, handleError, dedent, TypeAssertion, createFunction, pluralize, remove, PageStorage, Storage, UniqueStorage, Watcher, arrayToDict, addProperty, addQuotes, argWrapFactory, assert, abrev, abf, addExtension, assignFresh, antif, atFirst, atSecond, backspace, bindObject, breaker, blockQuote, brackify, bringToLife, comment, countCaptureGroups, capitalizeTitle, classMixin, callableRE, camelToTitle, curry, createVariable, changeExtension, curryStart, curryEnd, capitalize, copy, camelCase, compose, char2n, camelToDash, deepMerge, datestamp, doublef, dictSetter, dictSetter2, dsearch, doUntil, dashCase, doubleQuote, dict, dictGetter, depluralize, dreplace, dictf, endsWithWord, exporter, edit, exists, evaluate, extend, find, flatMap, fill, fixUrl, functionGetter, findall, fixPath, flat, fparse, findIndex, firstLine, ftest, getKwargs, getFirstName, getBindingName, getParameters, getLastWord, getCodeWords, getCodeWords2, getIndent, getExtension, getLast, getLongest, getChunks, getCaller, getStackTrace, getConstructorName, getFirstWord, getWords, getSpaces, hasComma, hasSpaces, hasHtml, hasBracket, hasNewline, hasCaptureGroup, hasEquals, hasValue, hasCamelCase, hasNumber, hackReplace, insert, indexGetter, incrementf, isCallable, isQuote, isEven, isOdd, isLast, isHTML, isNode, interweave, inferLang, isString, isArray, isObject, isDefined, isFunction, isPrimitive, isNumber, isSet, isNestedArray, indent, isNull, isWord, isBoolean, isRegExp, identity, isObjectLiteral, isJsonParsable, isCapitalized, isNewLine, isObjectArray, isStringArray, isClassFunction, joinSpaces, join, keyArrayToObject, lowerCase, linebreakRE, len, lineGetter, lineCount, lastLine, logConsole, makeRunner, mixin, modularf, matchGetter, merge, mget, map, mergeOnTop, mergeToObject, mapFilter, noop, nestPush, no, newlineIndent, n2char, objectWalk, overlap, objectToString, opposite, pipe, parseTopAttrs, pascalCase, partition, parens, push, pop, parseJSON, rigidSort, removeQuotes, rep, removeComments, range, removeExtension, rescape, reverse, reWrap, reduceToString, repeatUntil, swapKey, sayhi, swap, splitMapJoin, splitCamel, smallify, search, stringify, shared, smartDedent, stringBreaker, sleep, split, snakeCase, stringArgument, sorted, splitOnce, searchf, secondLine, titleCase, textOrJson, toNumber, toArgument, toNestedArray, test, type, tail, transformObject, trim, testf, toArray, templater, totalOverlap, upperCase, unique, uncapitalize, unzip, wrap, walk, wrapf, xsplit, yes, zip2} from "/home/kdog3682/2023/utils.js"

const baseSvgAttrs = {
"xmlns": "http://www.w3.org/2000/svg",
// "xmlns:xlink": "http://www.w3.org/1999/xlink",
"version": "1.1",
// "preserveAspectRatio": "xMidYMid meet"
}
function str(p) {
    return `(${fixFloatingPoint(p.x)}, ${fixFloatingPoint(p.y)})`
}
class World {
    static from(s) {
        const fn = buildFunction({
            paramString: '{palette, origin, group, circle, rect, line}',
            body: s,
            name: 'lambda',
        }, Function)
        // console.log(fn.toString())
        // console.log(fn)
        // throw fn.toString()
        const world = new World()
        const value = world.canvas(fn)
        return value
        throw value
    }
    debug() {
        const w = this.computedWidth
        const h = this.computedHeight
        const pad = this.options.padding
        const vec = new Vector(1, -1)
        const alter = (p) => {
            return p.translate(null, h - p.y, 'absolute').translate(vec)
        }

        return this.store.map((x) => x.value.points.map(alter).map(ffp).map(str)).flat().join('\n')
        return this.store.map((x) => x.value.points.map(ffp).map(str)).flat().join('\n')
    }
    get contents() {
        return new Group(this.store.map((x) => x.value))
    }

    normalizeCoordinates() {
        return norm(this)
        const mapper = (item) => {
            return item.value.points
        }

        // const pad = this.options.padding
        const pad = 20
        const points = this.store.map(mapper).flat()
        const xpoints = points.map((point) => point.x)
        const ypoints = points.map((point) => point.y)

        let xmin = Math.min(...xpoints)
        let xmax = Math.max(...xpoints)
        let ymin = Math.min(...ypoints)
        let ymax = Math.max(...ypoints)

        this.computedWidth = xmax - xmin
        this.computedHeight = ymax - ymin
        const w = this.computedWidth
        const h = this.computedHeight


        const exmin = xmin < 0 ? -1 * xmin : 0
        const eymin = ymin > 0 ? -1 * xmin : 0
        throw console.log({w, h, xmin, ymin, pad})
        const bottomLeft = new Vector(exmin + pad, eymin - pad)
        console.log({bottomLeft})
        const alter2 = (p) => {
            const p2 = p
                .translate(null, h - p.center.y, 'absolute')
                .translate(bottomLeft)
            return p2
        }

        const alter = (p) => {
            return p
                .translate(null, h - p.y, 'absolute')
                .translate(bottomLeft)
                // .translate(vec)
        }
        console.log('original points', points.map(str).join('  '))
        console.log('new points', points.map(alter).map(str).join('  '))
        // throw ''
        // return

        const width = (w + pad * 2) + xmax + xmin
        // const height = 50
        const height = (h + pad * 2)
        // console.log(width, height)
        // throw ''
        const viewBox = `0 0 ${width} ${height}`
        const contents = this.contents
            .translate(vec)
            .map(alter2)
            .toJSON()

        return {
            tag: 'svg',
            style: { border: '1px solid black', zoom: '200%'},
            attrs: {
                width, 
                height,
                viewBox
            },
            children: contents,
            // children: this.contents.translate(vec).map(alter).toJSON()
        }

    }
    translate(...args) {
        const world = new World(this.width, this.height, this.options)
        world.tally = new Tally(this.tally)
        const mapper = (item) => {
            const value = item.value.translate(...args)
            const next = { ...item, value }
            return next
        }

        world.store = this.store.map(mapper)
        return world
    }

    setSize(width, height) {
        if (width) this.width = width
        if (height) this.height = height
    }
    toVob() {
        const shapes = this.store.map((x) => x.value)
        const children = shapes.map((x) => x.json())
        const w = this.width
        const h = this.height
        const width = w
        const height = h
        const viewBox = `0 0 ${w} ${h}`
        const state = {
            tag: 'svg',
            attrs: {
                viewBox,
                width,
                height,
                ...baseSvgAttrs
            },
            style: { border: '1px solid black' },
            children
        }
        return state
    }
    toSvg(state) {
        const runner = (state) => {
            const children = state.children
            if (children) {
                return xmlString(state, children.map(runner))
            }
            return xmlString(state)
        }
        return runner(state || this.normalizeCoordinates())
    }
    toJSON() {
        const mapper = (item) => {
            return item.value.toJSON()
        }
        const options = this.options
        const contents = this.store.map(mapper)
        return { options, contents }
    }

    normalize(point) {
        let {x, y} = point
        let unit = this.options.unit
        x -= this.options.minX
        y = this.height - y - unit + this.options.minY
        x *= this.options.magnification
        y *= this.options.magnification
        return new Point(x, y)
    }

    constructor(width = 10, height = 10, options = {}) {
        this.setSize(width, height)
        this.options = options
        assignFresh(this.options, {padding: 3})
        this.store = []
        this.tally = new Tally()
        this.get = this.get.bind(this)
        this.register = this.register.bind(this)
    }
    get(key) {
        const baseKeys = [
            "rects",
            "lines",
            "circles",
            "squares",
        ]

        const mapper = (x) => {
            return x.value
        }

        const [a, b, c] = key.split('.')
        if (baseKeys.includes(a)) {
            const key = capitalize(a).slice(0, -1)
            if (b == null) {
                const filter = (x) => {
                    return x.type == key
                }
                return this.store.filter(filter).map(mapper)
            } else {
                const finder = (x) => {
                    return x.type == key && x.accessor == b
                }
                return mapper(this.store.find(finder))
            }
        } else {
            const finder = (x) => {
                return x.accessor == a
            }
            return mapper(this.store.find(finder))
        }
    }
    register(shape) {
        if (isArray(shape)) {
            return shape.forEach(this.register)
        }
        const type = shape.type
        const count = this.tally.add(type)
        const payload = {
            type,
            accessor: n2char(count - 1),
            value: shape,
        }
        this.store.push(payload)
        return shape
    }

    canvas(fn) {
        function parse(s) {
            const m = match(s, /p(-?\d)(-?\d)/)
            assert(m)
            return m.map(Number)
        }

        const transform = (p) => {
            switch (type(p)) {
                case 'Point':
                    return p
                case 'Object':
                    return new Point(p.x, p.y)
                case 'Array':
                    return new Point(...p) 
                case 'String':
                    return new Point(...parse(p))
            }
        }
        const line = (p1, p2, attrs) => {
            const points = [p1, p2].map(transform)
            const line = new Line(...points, attrs)
            return this.register(line)
        }

        const rect = (p1 , p2, attrs) => {
            const points = [p1, p2].map(transform)
            const rect = new Rect(...points, attrs)
            return this.register(rect)
        }

        const circle = (p1, r = 1, attrs = {}) => {
            const circle = new Circle(transform(p1), r, attrs)
            return this.register(circle)
        }

        const group = (items) => {
            const g = new Group(items)
            return g
        }

        const palette = new Palette()
        const point = (x, y) => {
            return new Point(x, y)
        }

        const value = fn({point, palette, group, origin: ORIGIN, rect, circle, line, get: this.get, register: this.register})
        if (isDefined(value)) {
            return value
        }
        return this.toSvg()
    }
}

function compare(a, b, fn) {
	console.log('first')
    console.log('------------')
    console.log(fn ? fn(a) : a)
    console.log(hr())
    console.log(fn ? fn(b) : b)
    console.log('------------')
    console.log('second')
    throw '@compare'
}



function boundingBox(points) {
    const xpoints = points.map((point) => point.x)
    const ypoints = points.map((point) => point.y)
    const xmin = Math.min(...xpoints)
    const xmax = Math.max(...xpoints)
    const ymin = Math.min(...ypoints)
    const ymax = Math.max(...ypoints)
    const x = average(...xpoints)
    const y = average(...ypoints)
    const center = { x, y }
    const width = xmax - xmin
    const height = ymax - ymin
    return {
        xmin, xmax, ymin, ymax, width, height, center
    }
}


function applyMatrix(point, matrix) {
    var x = point.x * matrix[0][0] + point.y * matrix[0][1] + matrix[0][2];
    var y = point.x * matrix[1][0] + point.y * matrix[1][1] + matrix[1][2];
    return { x: x, y: y };
}


function translater(x, y) {
    var translationMatrix = [
        [1, 0, x], // Translate along the x-axis
        [0, 1, y], // Translate along the y-axis
        [0, 0, 1]     // This row is for perspective transformations (not used for translation)
    ];
    return translationMatrix
}
function translate(x, a, b) {
    const translationMatrix = translater(a, b)
    return applyMatrix(x, translationMatrix)
}


function toPoints(s) {
    const r = /(-?\d)(-?\d)/g
    return findall(r, s).map((x) => new Point(...x.map(Number)))
}

function norm(state) {
    // the problem is everything has to be a circle
    const mapper = (item) => {
        return item.value.points
    }

    const r = state.store[0].value.radius
    const p = state.options.padding + r
    const points = state.store.map(mapper).flat()
    const box = boundingBox(points)
    const [w, h, vec] = applyPadding(box, state.options.padding)

    const contents = state.contents
        .translate(vec)
        .toJSON()

    return svgWrapper(w, h, contents)
}

function svgWrapper(width, height, children) {
    const viewBox = `0 0 ${width} ${height}`

    return {
        tag: 'svg',
        style: { oautline: '1px solid black', zoom: '200%'},
        attrs: {
            width, 
            height,
            viewBox
        },
        children
    }
}



function applyPadding(box, padding) {
    let pTop, pRight, pBottom, pLeft;

    if (typeof padding === 'number') {
        pTop = pRight = pBottom = pLeft = padding;
    } else if (typeof padding === 'object') {
        pTop = padding.top !== undefined ? padding.top : 0;
        pRight = padding.right !== undefined ? padding.right : 0;
        pBottom = padding.bottom !== undefined ? padding.bottom : 0;
        pLeft = padding.left !== undefined ? padding.left : 0;
    }

    const h = box.height + pTop + pBottom;
    const w = box.width + pLeft + pRight;
    const vec = new Vector(box.xmin - pLeft, box.ymax + pTop);
    vec.x = Math.abs(vec.x)
    vec.y = h - vec.y
    return [w, h, vec]
}
