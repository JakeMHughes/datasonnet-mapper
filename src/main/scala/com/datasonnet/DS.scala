package com.datasonnet

/*-
 * Copyright 2019-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.math.{BigDecimal, RoundingMode}
import java.net.URL
import java.security.SecureRandom
import java.text.DecimalFormat
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{DateTimeException, Duration, Instant, LocalDateTime, Period, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.function.Function
import java.util.{Base64, Scanner}

import com.datasonnet
import com.datasonnet.document.{DefaultDocument, MediaType}
import com.datasonnet.header.Header
import com.datasonnet.modules.{Crypto, JsonPath, Regex}
import com.datasonnet.spi.{DataFormatService, Library, ujsonUtils}
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import sjsonnet.Expr.Member.Visibility
import sjsonnet.ReadWriter.{ApplyerRead, ArrRead, StringRead}
import sjsonnet.Std.{builtin, builtinWithDefaults, _}
import sjsonnet.{Applyer, Error, EvalScope, Expr, FileScope, Materializer, Val}
import ujson.Value

import scala.collection.mutable
import scala.util.Random
import scala.jdk.CollectionConverters._

object DSLowercase extends Library {

  override def namespace() = "ds"

  override def libsonnets(): java.util.Set[String] = Set("util").asJava

  /** Core */
  override def functions(dataFormats: DataFormatService, header: Header): java.util.Map[String, Val.Func] = Map(
    
    /** Documentation
     * ### `append(array arr, any val)`
     * Given `arr` and `val`, appends `val` to the end of `arr`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     2,
     *     3,
     *     5,
     *     7
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.append(payload, 11)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   2,
     *   3,
     *   5,
     *   7,
     *   11
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("append", "first", "second") {
      (_, _, arr: Val.Arr, second: Val) =>
        val out = collection.mutable.Buffer.empty[Val.Lazy]
        Val.Arr(out.appendAll(arr.value).append(Val.Lazy(second)).toSeq)
    },

    /** Documentation
     * ### `combine(any first, any second)`
     * Combines `first` and `second`. Some values will auto-coerce, e.g. the number 5 will auto coerce to the string "5."
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * {
     *     string: ds.combine("Hello ", "World"),
     *     number: ds.combine(5, 7),
     *     auto1: ds.combine("Hello ", 5),
     *     auto2: ds.combine(5, "10"),
     *     array: ds.combine([1,2], [3,4]),
     *     obj: ds.combine({a:1}, {b:2})
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "string": "Hello World",
     *   "number": "57",
     *   "auto1": "Hello 5",
     *   "auto2": "510",
     *   "array": [
     *     1,
     *     2,
     *     3,
     *     4
     *   ],
     *   "obj": {
     *     "a": 1,
     *     "b": 2
     *   }
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("combine", "first", "second") {
      (ev, fs, first: Val, second: Val) =>
        first match {
          case Val.Str(str) =>
            second match {
              case Val.Str(str2) => Val.Lazy(Val.Str(str.concat(str2))).force
              case Val.Num(num) =>
                Val.Lazy(Val.Str(str.concat(
                  if (Math.ceil(num) == Math.floor(num)) {
                    num.toInt.toString
                  } else {
                    num.toString
                  }
                ))).force
              case i => throw Error.Delegate("Expected String or Number, got: " + i.prettyName)
            }
          case Val.Num(num) =>
            val stringNum = if (Math.ceil(num) == Math.floor(num)) {
              num.toInt.toString
            } else {
              num.toString
            }
            second match {
              case Val.Str(str) => Val.Lazy(Val.Str(stringNum.concat(str))).force
              case Val.Num(num2) =>
                Val.Lazy(Val.Str(stringNum.concat(
                  if (Math.ceil(num2) == Math.floor(num2)) {
                    num2.toInt.toString
                  } else {
                    num2.toString
                  }
                ))).force
              case i => throw Error.Delegate("Expected String or Number, got: " + i.prettyName)
            }
          case Val.Arr(arr) =>
            second match {
              case Val.Arr(arr2) => Val.Arr(arr.concat(arr2))
              case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
            }
          case obj: Val.Obj =>
            val out = scala.collection.mutable.Map[String, Val.Obj.Member]()
            second match {
              case secObj: Val.Obj =>
                out.addAll(obj.getVisibleKeys().map {
                  case (sKey, _) => sKey -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(sKey, -1)(fs, ev))
                }).addAll(secObj.getVisibleKeys().map {
                  case (sKey, _) => sKey -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => secObj.value(sKey, -1)(fs, ev))
                })
                new Val.Obj(out, _ => (), None)
              case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
            }
          case i => throw Error.Delegate(
            "Expected Array, Object, Number, or String, got: " + i.prettyName)
        }
    },


    /** Documentation
     * ### `contains(array|string item, any val)`
     * If `item` is an array, returns true if `item` contains `val`.
     *
     * If `item` is a string, returns true if `item` contains the sub string `val`.
     *
     * *Example*
     *
     * .Payload
     * ----------
     * [
     *     2,
     *     3,
     *     5,
     *     7
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.contains(payload, 2)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("contains", "container", "value") {
      (_, _, container: Val, value: Val) =>
        container match {
          // See: scala.collection.IterableOnceOps.exists
          case Val.Arr(array) =>
            array.exists(_.force == value)
          case Val.Str(s) =>
            value.cast[Val.Str].value.r.findAllMatchIn(s).nonEmpty;
          case i => throw Error.Delegate("Expected Array or String, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `distinctBy(array|object item, function discriminator)`
     * Returns a new object containing only the items that are a unique result from `discriminator`.
     * 
     * The function `discriminator` is expected to take the value as the first parameter (required) and the index as the second (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *   "array": [
     *     1,
     *     1,
     *     2,
     *     2,
     *     3,
     *     3,
     *     4,
     *     4,
     *     5
     *   ],
     *   "obj": {
     *     "a": 1,
     *     "b": 2,
     *     "c": 1
     *   }
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *     array: ds.distinctBy(payload.array, function(item,index) item),
     *     obj: ds.distinctBy(payload.obj, function(value,key) value)
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "array": [
     *     1,
     *     2,
     *     3,
     *     4,
     *     5
     *   ],
     *   "obj": {
     *     "a": 1,
     *     "b": 2
     *   }
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("distinctBy", "container", "funct") {
      (ev, fs, container: Val, funct: Applyer) =>
        container match {
          case Val.Arr(arr) =>
            distinctBy(arr, funct)
          case obj: Val.Obj =>
            distinctBy(obj, funct, ev, fs)
          case i => throw Error.Delegate("Expected Array or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `endsWith(string str, string subStr)`
     * Returns true if `str` ends with `subStr`. Ignores casing.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "name": "Scala",
     *     "version": "1.0"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.endsWith(payload.version, ".0")
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("endsWith", "main", "sub") {
      (_, _, main: String, sub: String) =>
        main.toUpperCase.endsWith(sub.toUpperCase);
    },

    /** Documentation
     * ### `entriesOf(object obj)`
     * Returns an array of objects describing each key value pair of `obj`.
     *
     * *Example*
     *
     * .Payload
     * ----------
     * {
     *     "name": "Scala",
     *     "version": "1.0"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.entriesOf(payload)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   {
     *     "value": "Scala",
     *     "key": "name"
     *   },
     *   {
     *     "value": "1.0",
     *     "key": "version"
     *   }
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("entriesOf", "obj") {
      (ev, fs, obj: Val.Obj) =>
        Val.Arr(obj.getVisibleKeys().keySet.collect({
          case key =>
            val currentObj = scala.collection.mutable.Map[String, Val.Obj.Member]()
            currentObj += ("key" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Lazy(Val.Str(key)).force))
            currentObj += ("value" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev)))

            Val.Lazy(new Val.Obj(currentObj, _ => (), None))
        }).toSeq)
    },

    /** Documentation
     * ### `filter(array arr, function func)`
     * Filters `arr` depending on the result of `func`.
     * 
     * The function `func` is expected to take the value as the first parameter (required) and the index as the second (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     1,
     *     2,
     *     3,
     *     4
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.filter(payload, function(value, index) value < 3)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   1,
     *   2
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("filter", "array", "funct") {
      (_, _, value: Val, funct: Applyer) =>
        value match {
          case Val.Arr(array) => filter(array, funct)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `filterObject(object obj, function func)`
     * Filters `obj` depending on the result of `func`.
     * 
     * The function `func` is expected to take the property value as the first parameter (required), the property key as the second (optional) and the index as the third (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "version": 1.7
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.filterObject(payload, function(value, key, index) value > 1.5)
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "version": 1.7
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("filterObject", "obj", "func") {
      (ev, fs, value: Val, func: Applyer) =>
        value match {
          case obj: Val.Obj => filterObject(obj, func, ev, fs)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `find(string|array item, any val)`
     * Returns an array containing the location where `val` occurs in `item`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "string": "Hello World",
     *     "array": [1,2,3,4]
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *     string: ds.find(payload.string, "World"),
     *     array: ds.find(payload.array, 3)
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "string": [6],
     *   "array": [2]
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("find", "container", "value") {
      (_, _, container: Val, value: Val) =>
        container match {
          case Val.Str(str) =>
            val sub = value.cast[Val.Str].value
            Val.Arr(sub.r.findAllMatchIn(str).map(_.start).map(item => Val.Lazy(Val.Num(item))).toSeq)
          case Val.Arr(s) =>
            Val.Arr(s.zipWithIndex.collect({
              case (v, i) if v.force == value => Val.Lazy(Val.Num(i))
            }))
          case i => throw Error.Delegate("Expected Array or String, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `flatten(array arr)`
     * Given `arr`, which contains one level arrays, creates a flat array.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *   [
     *     1,
     *     2
     *   ],
     *   [
     *     3,
     *     4
     *   ]
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.flatten(payload)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   1,
     *   2,
     *   3,
     *   4
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("flatten", "array") {
      (_, _, array: Val) =>
        array match {
          case Val.Arr(outerArray) =>
            val out = collection.mutable.Buffer.empty[Val.Lazy]
            for (innerArray <- outerArray) {
              innerArray.force match {
                case Val.Null => out.append(Val.Lazy(Val.Null))
                case Val.Arr(v) => out.appendAll(v)
                case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
              }
            }
            Val.Arr(out.toSeq)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `flatMap(array arr, function func)`
     * Given an array of arrays `arr`, creates a flat array using the outcome of `func`.
     * 
     * The function `func` is expected to take the value as the first parameter (required) and the index as the second (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     [
     *         2,
     *         3,
     *         5,
     *         7
     *     ],
     *     [
     *         11,
     *         13,
     *         17,
     *         19
     *     ]
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.flatMap(payload, function(value, index) value)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   2,
     *   3,
     *   5,
     *   7,
     *   11,
     *   13,
     *   17,
     *   19
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("flatMap", "array", "funct") {
      (_, _, array: Val, funct: Applyer) =>
        array match {
          case Val.Arr(s) => flatMap(s, funct)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `foldLeft(array arr, any initVal, function func)`
     * Iterates over `arr`, applying `func` to the previous result. Starts with the value provided in `initVal`.
     * 
     * The function `func` is expected to take the current value as the first parameter (required) and the previous value as the second parameter (required).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     1,
     *     2,
     *     3,
     *     4
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.foldLeft(payload, 1, function(curr, prev) curr * prev)
     * ------------------------
     * .Result
     * ------------------------
     * 24
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("foldLeft", "arr", "init", "func") { (_, _, arr: Val.Arr, init: Val, func: Applyer) =>
      var current = init
      for (item <- arr.value) {
        val c = current
        current = func.apply(Val.Lazy(c), item)
      }
      current
    },

    /** Documentation
     * ### `foldRight(array arr, any initVal, function func)`
     * Iterates backwards over an array, applying `func` to the previous result. Starts with the value provided in `initVal`.
     * 
     * The function `func` is expected to take the current value as the first parameter (required) and the previous value as the second parameter (required).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     1,
     *     2,
     *     3,
     *     4
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.foldRight(payload, 1, function(curr, prev) curr * prev)
     * ------------------------
     * .Result
     * ------------------------
     * 24
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    // TODO: can we do this without reverse? has to traverse the collection twice
    builtin("foldRight", "arr", "init", "func") { (_, _, arr: Val.Arr, init: Val, func: Applyer) =>
      var current = init
      for (item <- arr.value.reverse) {
        val c = current
        current = func.apply(item, Val.Lazy(c))
      }
      current
    },

    /** Documentation
     * ### `groupBy(array|object items, function discriminator)`
     * Groups the provided `items` into an object based on the result of `discriminator`.
     * 
     * The function `discriminator` is expected to take the value as the first parameter (required) and the index as the second (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "array": [
     *         "a",
     *         "b",
     *         "a"
     *     ],
     *     "obj": {
     *         "a":"Alpha",
     *         "b":"Bravo",
     *         "c": "Alpha"
     *     }
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *     array: ds.groupBy(payload.array, function(item,index) item ),
     *     obj: ds.groupBy(payload.obj, function(value,key) value)
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "array": {
     *     "a": [
     *       "a",
     *       "a"
     *     ],
     *     "b": [
     *       "b"
     *     ]
     *   },
     *   "obj": {
     *     "Alpha": {
     *       "a": "Alpha",
     *       "c": "Alpha"
     *     },
     *     "Bravo": {
     *       "b": "Bravo"
     *     }
     *   }
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("groupBy", "container", "funct") {
      (ev, fs, container: Val, funct: Applyer) =>
        container match {
          case Val.Arr(s) =>
            groupBy(s, funct)
          case obj: Val.Obj =>
            groupBy(obj, funct, ev, fs)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Array or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `isArray(any valToCheck)`
     * Accepts any given value as `valToCheck` and checks if it is of type array.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     1,
     *     2,
     *     3,
     *     4
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isArray(payload)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isArray", "v") { (_, _, v: Val) =>
      v.isInstanceOf[Val.Arr]
    },

    /** Documentation
     * ### `isBlank(string strToCheck)`
     * Checks if `strToCheck` is blank. Also returns true if null.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * {
     *     str1: ds.isBlank("     "),
     *     str2: ds.isBlank(""),
     *     'null': ds.isBlank(null)
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "str1": true,
     *   "str2": true,
     *   "null": true
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isBlank", "value") {
      (_, _, value: Val) =>
        value match {
          case Val.Str(s) => s.trim().isEmpty
          case Val.Null => true
          case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `isBoolean(any valToCheck)`
     * Accepts any given value as `valToCheck` and checks if it is of type bool.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *   "name": "Java",
     *   "isObjectOriented": true
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isBoolean(payload.isObjectOriented)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isBoolean", "v") { (_, _, v: Val) =>
      v == Val.True || v == Val.False
    },

    /** Documentation
     * ### `isDecimal(num numToCheck)`
     * Checks that the input number `numToCheck` is a decimal number. Trailing zeros are ignored.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * {
     *     a: ds.isDecimal(2),
     *     b: ds.isDecimal(2.0),
     *     c: ds.isDecimal(2.1),
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "a": false,
     *   "b": false,
     *   "c": true
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isDecimal", "value") {
      (_, _, value: Double) =>
        (Math.ceil(value) != Math.floor(value)).booleanValue()
    },

    /** Documentation
     * ### `isEmpty(any valToCheck)`
     * Checks if `valToCheck` is empty. Does not ignore white space if string. Returns true if null.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * {
     *     "null": ds.isEmpty(null),
     *     str: ds.isEmpty("    "),
     *     array: ds.isEmpty([]),
     *     obj: ds.isEmpty({})
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "null": true,
     *   "str": false,
     *   "array": true,
     *   "obj": true
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isEmpty", "container") {
      (_, _, container: Val) =>
        container match {
          case Val.Null => true
          case Val.Str(s) => s.isEmpty.booleanValue()
          case Val.Arr(s) => s.isEmpty.booleanValue()
          case s: Val.Obj => s.getVisibleKeys().isEmpty.booleanValue()
          case i => throw Error.Delegate("Expected String, Array, or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `isEven(num numToCheck)`
     * 
     * Checks that the input number `numToCheck` is an even number.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "version": 2.0
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isEven(payload.version)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isEven", "num") {
      (_, _, num: Double) =>
        (num % 2) == 0
    },

    /** Documentation
     * ### `isFunction(any valToCheck)`
     * Accepts any given value `valToCheck` and checks if it is of type function.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * ds.isFunction(function() "5")
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isFunction", "v") { (_, _, v: Val) =>
      v.isInstanceOf[Val.Func]
    },

    /** Documentation
     * ### `isInteger(num numToCheck)`
     * Checks that the input number `numToCheck` is an integer. Trailing zeros are ignored.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "version": 2.0
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isInteger(payload.version)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isInteger", "value") {
      (_, _, value: Double) =>
        (Math.ceil(value) == Math.floor(value)).booleanValue()
    },

    /** Documentation
     * ### `isNumber(any valToCheck)`
     * Accepts any given value `valToCheck` and checks if it is of type number.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "age": 5
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isNumber(payload.age)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isNumber", "v") { (_, _, v: Val) =>
      v.isInstanceOf[Val.Num]
    },

    /** Documentation
     * ### `isObject(any valToCheck)`
     * Accepts any given value `valToCheck` and checks if it is of type object.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "language": "Java"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isObject(payload)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isObject", "v") { (_, _, v: Val) =>
      v.isInstanceOf[Val.Obj]
    },

    /** Documentation
     * ### `isOdd(num numToCheck)`
     * Checks that `numToCheck` is an odd number.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "age": 5
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isOdd(payload.age)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isOdd", "num") {
      (_, _, num: Double) =>
        (num % 2) != 0
    },

    /** Documentation
     * ### `isString(any valToCheck)`
     * Accepts any given value `valToCheck` and checks if it is of type string.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "language":"Java"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.isString(payload.language)
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("isString", "v") { (_, _, v: Val) =>
      v.isInstanceOf[Val.Str]
    },

    /** Documentation
     * ### `joinBy(array arr, string separator)`
     * Joins `arr` into a string with the provided `separator`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "versions": [1.0, 1.2, 1.7, 1.8]
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.joinBy(payload.versions, ", ")
     * ------------------------
     * .Result
     * ------------------------
     * "1, 1.2, 1.7, 1.8"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("joinBy", "array", "sep") {
      (_, _, array: Val.Arr, sep: String) =>
        array.value.map({
          _.force match {
            case Val.Str(x) => x
            case Val.True => "true"
            case Val.False => "false"
            case Val.Num(x) => if (!x.isWhole) x.toString else x.intValue().toString
            case i => throw Error.Delegate("Expected String, Number, or Boolean, got: " + i.prettyName)
          }
        }).mkString(sep)
    },

    /** Documentation
     * ### `keysOf(object obj)`
     * Returns an array of all the key names in `obj`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *    "departureDate": "01/20/2019",
     *    "origin": "PHX",
     *    "destination": "SEA"
     *  }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.keysOf(payload)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   "departureDate",
     *   "origin",
     *   "destination"
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("keysOf", "obj") {
      (_, _, obj: Val.Obj) =>
        Val.Arr(obj.getVisibleKeys().keySet.map(item => Val.Lazy(Val.Str(item))).toSeq)
    },

    /** Documentation
     * ### `lower(string str)`
     * Converts `str` to all lower case characters.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *    "origin": "PHX",
     *    "destination": "SEA"
     *  }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.lower(payload.origin)
     * ------------------------
     * .Result
     * ------------------------
     * "phx"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("lower", "str") {
      (_, _, str: String) =>
        str.toLowerCase();
    },

    /** Documentation
     * ### `map(array arr, function func)`
     * Loops through all items in `arr`, applies `func` to each, and returns a new array containing each result. Returns null if `arr` is null.
     * 
     * The function `func` is expected to take the value as the first parameter (required) and the index as the second (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "versions": [1.0, 1.2, 1.7, 1.8]
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.map(payload.versions, function(value, index) value > 1.2)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   false,
     *   false,
     *   true,
     *   true
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("map", "array", "funct") {
      (_, _, array: Val, funct: Applyer) =>
        array match {
          case Val.Arr(seq) =>
            map(seq, funct)
          case Val.Null => Val.Lazy(Val.Null).force
          case i =>throw Error.Delegate("Expected Array, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `mapEntries(object obj, function func)`
     * Loops through all properties in `obj`, applies `func` to each, and returns a new array containing each result.
     * 
     * The function `func` is expected to take the property value as the first parameter (required), the property key as the second (optional) and the index as the third (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *    "origin": "PHX",
     *    "destination": "SEA"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.mapEntries(payload, function(value, key, index) value)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   "PHX",
     *   "SEA"
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("mapEntries", "value", "funct") {
      (ev, fs, value: Val, funct: Applyer) =>
        value match {
          case obj: Val.Obj =>
            mapEntries(obj, funct, ev, fs)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `mapObject(object obj, function func)`
     * Loops through all properties in `obj`, applies `func` to each, and returns a new object containing each result.
     * 
     * The function `func` is expected to take the property value as the first parameter (required), the property key as the second (optional) and the index as the third (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *    "origin": "PHX",
     *    "destination": "SEA"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.mapObject(payload, function(value, key, index) {[key]:value})
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "origin": "PHX",
     *   "destination": "SEA"
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("mapObject", "value", "funct") {
      (ev, fs, value: Val, funct: Applyer) =>
        value match {
          case obj: Val.Obj =>
            mapObject(obj, funct, ev, fs)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `match(string str, string regex)`
     * Executes the regex expression `regex` against `str` and returns an array with the match groups.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "email": "test@server.com"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.match(payload.email, "(.*)@(.*)(.com)")
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   "test@server.com",
     *   "test",
     *   "server",
     *   ".com"
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("match", "string", "regex") {
      (_, _, string: String, regex: String) =>
        val out = collection.mutable.Buffer.empty[Val.Lazy]
        regex.r.findAllMatchIn(string).foreach(
          word => (0 to word.groupCount).foreach(index => out += Val.Lazy(Val.Str(word.group(index))))
        )
        Val.Arr(out.toSeq)
    },

    /** Documentation
     * ### `matches(string str, string regex)`
     * Executes the regex expression `regex` against `str` and returns `true` or `false` if the expression matches the input.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "email": "test@server.com"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.matches(payload.email, "(.*)@(.*)(.com)")
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("matches", "string", "regex") {
      (_, _, string: String, regex: String) =>
        regex.r.matches(string);
    },

    /** Documentation
     * ### `max(array arr)`
     * Returns the max value in `arr`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     5,
     *     2,
     *     7,
     *     3
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.max(payload)
     * ------------------------
     * .Result
     * ------------------------
     * 7
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("max", "array") {
      (_, _, array: Val.Arr) =>
        var value = array.value.head
        for (x <- array.value) {
          value.force.prettyName match {
            case "string" =>
              if (value.force.cast[Val.Str].value < x.force.cast[Val.Str].value) {
                value = x
              }
            case "boolean" =>
              if (x.force == Val.Lazy(Val.True).force) {
                value = x
              }
            case "number" =>
              if (value.force.cast[Val.Num].value < x.force.cast[Val.Num].value) {
                value = x
              }
            case i => throw Error.Delegate(
              "Expected Array of type String, Boolean, or Number, got: Array of type " + i)
          }
        }
        value.force
    },

    /** Documentation
     * ### `maxBy(array arr, function func)`
     * Returns the max result of `func` in `arr`.
     * 
     * The function `func` is expected to take the value as the first parameter (required).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     {"age": 5},
     *     {"age": 7},
     *     {"age": 3}
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.maxBy(payload, function(value) value.age)
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "age": 7
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("maxBy", "array", "funct") {
      (_, _, array: Val.Arr, funct: Applyer) =>
        val seqVal = array.value
          funct.apply(seqVal.head).prettyName match{
          case "string" =>
            seqVal.maxBy(item => funct.apply(item).cast[Val.Str].value).force
          case "boolean" =>
            if(seqVal.forall( item => item.force.prettyName.equals("boolean")))
              if (seqVal.exists(item => item.force == Val.True)) {
                Val.True
              } else { Val.False }
            else throw Error.Delegate("Received a dirty array")
          case "number" =>
            seqVal.maxBy(item => funct.apply(item).cast[Val.Num].value).force
          case i => throw Error.Delegate(
            "Expected Array of type String, Boolean, or Number, got: Array of type " + i)
        }
    },

    /** Documentation
     * ### `min(array arr)`
     * Returns the min value in `arr`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     5,
     *     2,
     *     7,
     *     3
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.min(payload)
     * ------------------------
     * .Result
     * ------------------------
     * 2
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("min", "array") {
      (_, _, array: Val.Arr) =>
        var value = array.value.head
        for (x <- array.value) {
          value.force.prettyName match {
            case "string" =>
              if (value.force.cast[Val.Str].value > x.force.cast[Val.Str].value) {
                value = x
              }
            case "boolean" =>
              if (x.force == Val.Lazy(Val.False).force) {
                value = x
              }
            case "number" =>
              if (value.force.cast[Val.Num].value > x.force.cast[Val.Num].value) {
                value = x
              }
            case i => throw Error.Delegate(
              "Expected Array of type String, Boolean, or Number, got: Array of type " + i)
          }
        }
        value.force
    },

    /** Documentation
     * ### `minBy(array arr, function func)`
     * Returns the max result of `func` in `arr`.
     * 
     * The function `func` is expected to take the value as the first parameter (required).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     {"age": 5},
     *     {"age": 7},
     *     {"age": 3}
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.minBy(payload, function(value) value.age)
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "age": 3
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("minBy", "array", "funct") {
      (_, _, array: Val.Arr, funct: Applyer) =>
        val seqVal = array.value
        funct.apply(seqVal.head).prettyName match{
          case "string" =>
            seqVal.minBy(item => funct.apply(item).cast[Val.Str].value).force
          case "boolean" =>
            if(seqVal.forall( item => item.force.prettyName.equals("boolean")))
              if (seqVal.exists(item => item.force == Val.False)) {
                Val.False
              } else { Val.True }
            else throw Error.Delegate("Received a dirty array")
          case "number" =>
            seqVal.minBy(item => funct.apply(item).cast[Val.Num].value).force
          case i => throw Error.Delegate(
            "Expected Array of type String, Boolean, or Number, got: Array of type " + i)
        }
    },

    /** Documentation
     * ### `orderBy(array|object items, function func)`
     * Reorders the array `items` by the result of `func`.
     * 
     * If `items` is an array: the function `func` is expected to take the value as the first parameter (required).
     * 
     * If `items` is an object: the function `func` is expected to take the value as the first parameter (required) and the key as the second parameter (optional).
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     {"age": 5},
     *     {"age": 7},
     *     {"age": 3}
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.orderBy(payload, function(value) value.age)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   {
     *     "age": 3
     *   },
     *   {
     *     "age": 5
     *   },
     *   {
     *     "age": 7
     *   }
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("orderBy", "value", "funct") {
      (ev, fs, value: Val, funct: Applyer) =>
        value match {
          case Val.Arr(array) =>
            orderBy(array, funct)
          case obj: Val.Obj =>
            orderBy(obj, funct, ev, fs)
          case Val.Null => Val.Lazy(Val.Null).force
          case i => throw Error.Delegate("Expected Array or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `parseDouble(string str)`
     * Parses a string `str` containing a number and returns its decimal value. Trailing zeros are ignored.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "version":"1.5"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.parseDouble(payload.version)
     * ------------------------
     * .Result
     * ------------------------
     * 1.5
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    // migrated from util.libsonnet
    builtin("parseDouble", "str") { (_, _, str: String) =>
      str.toDouble
    },

    /** Documentation
     * ### `parseHex(string str)`
     * Parses a hex value given as a string `str` and returns its decimal value.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "hex":"F"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.parseHex(payload.hex)
     * ------------------------
     * .Result
     * ------------------------
     * 15
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("parseHex", "str") { (_, _, str: String) =>
      Integer.parseInt(str, 16)
    },

    /** Documentation
     * ### `parseInt(string str)`
     * Parses an int value given as a string `str` and returns its integer value.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "number":"50"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.parseInt(payload.number)
     * ------------------------
     * .Result
     * ------------------------
     * 50
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("parseInt", "str") { (_, _, str: String) =>
      str.toInt
    },

    /** Documentation
     * ### `parseOctal(string str)`
     * Parses an octal value given as a string `str` and returns its integer value.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "octal":"107136"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.parseOctal(payload.octal)
     * ------------------------
     * .Result
     * ------------------------
     * 36446
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("parseOctal", "str") { (_, _, str: String) =>
      Integer.parseInt(str, 8)
    },

    /** Documentation
     * ### `prepend(array arr, any val)`
     * Given `arr` and `val`, inserts `val` at the beginning of `arr`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *     2,
     *     3,
     *     4
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.prepend(payload, 1)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   1,
     *   2,
     *   3,
     *   4
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("prepend", "first", "second") {
      (_, _, arr: Val.Arr, second: Val) =>
        val out = collection.mutable.Buffer.empty[Val.Lazy]
        Val.Arr(out.append(Val.Lazy(second)).appendAll(arr.value).toSeq)
    },

    /** Documentation
     * ### `range(number start, number end)`
     * Returns an array with the numbers from the `start` to the `end` of the range, inclusive.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "start": 0,
     *     "end": 3
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.range(payload.start, payload.end)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   0,
     *   1,
     *   2,
     *   3
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    // TODO: add step param
    builtin("range", "begin", "end") {
      (_, _, begin: Int, end: Int) =>
        Val.Arr((begin to end).map(i => Val.Lazy(Val.Num(i))))
    },

    /** Documentation
     * ### `read(string data, string mimeType, object params)`
     * Reads a string `data` as the given `mimetype`.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * ds.read("{\"price\": 8.95}", "application/json", {})
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "price": 8.95
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtinWithDefaults("read",
      "data" -> None,
      "mimeType" -> None,
      "params" -> Some(Expr.Null(0))) { (args, ev) =>
      val data = args("data").cast[Val.Str].value
      val mimeType = args("mimeType").cast[Val.Str].value
      val params = if (args("params") == Val.Null) {
        Library.EmptyObj
      } else {
        args("params").cast[Val.Obj]
      }
      read(dataFormats, data, mimeType, params, ev)
    },

    /** Documentation
     * ### `readUrl(string url)`
     * Reads `url` and returns the content of the url, if it's JSON.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * ds.readUrl("http://httpbin.org/get")
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "args": {},
     *   "headers": {
     *     "Accept": "text/html, image/gif, image/jpeg, *; q=.2",
     *     "Host": "httpbin.org",
     *     "User-Agent": "Java/14.0.1",
     *     "X-Amzn-Trace-Id": "Root=1-5f7f568d-481e623471c21cc2686e53e8"
     *   },
     *   "origin": "69.250.49.68",
     *   "url": "http://httpbin.org/get"
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    //TODO add read mediatype
    builtin("readUrl", "url") {
      (_, _, url: String) =>
        url match {
          case str if str.startsWith("classpath://") =>
            val source = io.Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(str.replaceFirst("classpath://", "")))
            val out =
              try {
                source.mkString
              }
              catch {
                case _: NullPointerException => "null"
              }
            Materializer.reverse(ujsonUtils.parse(out));
          case _ =>
            val out = new Scanner(new URL(url).openStream(), "UTF-8").useDelimiter("\\A").next()
            Materializer.reverse(ujsonUtils.parse(out));
        }
    },

    /** Documentation
     * ### `remove(array|object item, string|array value)`
     * Removes `value` from `item` and returns the remaining array or object.
     * All properties of the object can be removed using a `value` in the array format.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "array": [
     *         1,
     *         2,
     *         3,
     *         4
     *     ],
     *     "obj": {
     *         "a": 1,
     *         "b": 2
     *     }
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *   array: ds.remove(payload.array, 3),
     *   obj: ds.remove(payload.obj, "b"),
     *   emptyObj: ds.remove(payload.obj, ["a","b"])
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "array": [
     *     1,
     *     2,
     *     4
     *   ],
     *   "obj": {
     *     "a": 1
     *   },
     *   "emptyObj": {}
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("remove", "collection", "value") {
      (ev, fs, collection: Val, value: Val) =>
        collection match {
          case Val.Arr(arr) =>
            Val.Arr(arr.collect({
              case arrValue if arrValue.force != value => arrValue
            }))
          case obj: Val.Obj =>
            new Val.Obj(
            value match {
              case Val.Str(str) =>
                scala.collection.mutable.Map(
                  obj.getVisibleKeys().keySet.toSeq.collect({
                    case key if key != str =>
                      key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))
                  }): _*)
              case Val.Arr(arr) =>
                scala.collection.mutable.Map(
                  obj.getVisibleKeys().keySet.toSeq.collect({
                    case key if !arr.exists(item => item.force.cast[Val.Str] == Val.Str(key)) =>
                      key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))
                  }): _*)
              case i => throw Error.Delegate("Expected String or Array, got: " + i.prettyName)
            }, _ => (), None)
          case i => throw Error.Delegate("Expected Array or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `removeMatch(array|object items, any val)`
     * Given an array or an object `items` and `val` of the same type, removes the matching values. If `items` is an object, both key and value must match.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "array": [1,2,3,4],
     *     "obj": {"a":1,"b":2}
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *   array: ds.removeMatch(payload.array, [1,4]),
     *   obj: ds.removeMatch(payload.obj, {a:1,b:3})
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "array": [
     *     2,
     *     3
     *   ],
     *   "obj": {
     *     "b": 2
     *   }
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("removeMatch", "first", "second") {
      (ev, fs, first: Val, second: Val) =>
        first match {
          case Val.Arr(arr) =>
            second match {
              case Val.Arr(arr2) =>
                //unfortunately cannot use diff here because of lazy values
                Val.Arr(arr.filter(arrItem => !arr2.exists(arr2Item => arr2Item.force == arrItem.force)))
              case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
            }
          case obj: Val.Obj =>
            second match {
              case obj2: Val.Obj =>
                new Val.Obj(scala.collection.mutable.Map(
                  obj.getVisibleKeys().keySet.toSeq.collect({
                    case key if !(obj2.containsKey(key) && obj.value(key, -1)(fs, ev) == obj2.value(key, -1)(fs, ev)) =>
                      key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))
                  }): _*), _ => (), None)
              case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
            }
          case i => throw Error.Delegate("Expected Array or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `replace(string phrase, string regex, string replacement)`
     * Replaces the matching `regex` with the `replacement` in the `phrase`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "regex": "Hello",
     *     "replacement": "Goodbye"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.replace("Hello World", payload.regex, payload.replacement)
     * ------------------------
     * .Result
     * ------------------------
     * "Goodbye World"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("replace", "string", "regex", "replacement") {
      (_, _, str: String, reg: String, replacement: String) =>
        reg.r.replaceAllIn(str, replacement)
    },

    /** Documentation
     * ### `reverse(array|object items)`
     * Given an array or object as `items`, reverses the order of the elements.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "array": [
     *         1,
     *         2,
     *         3,
     *         4
     *     ],
     *     "obj": {
     *         "a":1,
     *         "b":2
     *     }
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *   array: ds.reverse(payload.array),
     *   obj: ds.reverse(payload.obj)
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "array": [
     *     4,
     *     3,
     *     2,
     *     1
     *   ],
     *   "obj": {
     *     "b": 2,
     *     "a": 1
     *   }
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("reverse", "collection") {
      (ev, fs, collection: Val) =>
        collection match {
          case Val.Str(str) => Val.Lazy(Val.Str(str.reverse)).force
          case Val.Arr(arr) => Val.Arr(arr.reverse)
          case obj: Val.Obj =>
            var result: Seq[(String, Val.Obj.Member)] = Seq()
            obj.getVisibleKeys().foreach(entry => result = result.prepended(
              entry._1 -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(entry._1, -1)(fs, ev))
            ))
            new Val.Obj(mutable.LinkedHashMap(result: _*), _ => (), None)
          case i => throw Error.Delegate("Expected Array or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `scan(string str, string regex)`
     * Executes the regex expression `regex` against `str` and returns an array with each match as an array.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "email": "test@server.com"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.scan(payload.email, "(.*)@(.*)(.com)")
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   [
     *     "test@server.com",
     *     "test",
     *     "server",
     *     ".com"
     *   ]
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("scan", "str", "regex") {
      (_, _, str: String, regex: String) =>
        Val.Arr(
          regex.r.findAllMatchIn(str).map(item => {
            Val.Lazy(Val.Arr(
              (0 to item.groupCount).map(i => Val.Lazy(Val.Str(item.group(i))))
            ))
          }).toSeq
        )
    },

    /** Documentation
     * ### `select(object obj, string path)`
     * Returns a value inside `obj` by the provided `path`. For nested objects, the path is separated by a dot ('.').
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *   "language": {
     *       "name": "Java",
     *       "version": "1.8"
     *   }
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *   language: ds.select(payload, 'language.name')
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "language": "Java"
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("select", "obj", "path") {
      (ev, fs, obj: Val.Obj, path: String) =>
        select(obj,path,ev,fs)
    },

    /** Documentation
     * ### `sizeOf(any val)`
     * Returns the size of `val`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "array": [1, 2],
     *     "obj": {"prop": 2},
     *     "string": "x"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * {
     *     array: ds.sizeOf(payload.array),
     *     object: ds.sizeOf(payload.obj),
     *     'null': ds.sizeOf(null),
     *     'function': ds.sizeOf(function(a,b,c) 1),
     *     string: ds.sizeOf(payload.string)
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "array": 2,
     *   "object": 1,
     *   "null": 0,
     *   "function": 3,
     *   "string": 1
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("sizeOf", "value") {
      (_, _, value: Val) =>
        value match {
          case Val.Str(s) => s.length()
          case s: Val.Obj => s.getVisibleKeys().size
          case Val.Arr(s) => s.size
          case s: Val.Func => s.params.allIndices.size
          case Val.Null => 0
          case i => throw Error.Delegate("Expected Array, String, or Object, got: " + i.prettyName)
        }
    },

    /** Documentation
     * ### `splitBy(string strToSplit, string regex)`
     * Splits `strToSplit` into an array based on the matching `regex`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "string": "Hello World"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.splitBy(payload.string, " ")
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   "Hello",
     *   "World"
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("splitBy", "str", "regex") {
      (_, _, str: String, regex: String) =>
        Val.Arr(regex.r.split(str).toIndexedSeq.map(item => Val.Lazy(Val.Str(item))))
    },

    /** Documentation
     * ### `startsWith(string str, string subStr)`
     * Checks if `str` starts with `subStr`. Ignores casing.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "string": "Hello World"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.startsWith(payload.string, "hello")
     * ------------------------
     * .Result
     * ------------------------
     * true
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("startsWith", "str1", "str2") {
      (_, _, str1: String, str2: String) =>
        str1.toUpperCase().startsWith(str2.toUpperCase());
    },

    /** Documentation
     * ### `toString(any val)`
     * Returns `val` to a string.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "num": 5
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.toString(payload.num)
     * ------------------------
     * .Result
     * ------------------------
     * "5"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("toString", "value") {
      (_, _, value: Val) =>
        convertToString(value)
    },

    /** Documentation
     * ### `trim(string str)`
     * Removes leading and trailing spaces in `str`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "string": "      Hello World       "
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.trim(payload.string)
     * ------------------------
     * .Result
     * ------------------------
     * "Hello World"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("trim", "str") {
      (_, _, str: String) =>
        str.trim()
    },

    /** Documentation
     * ### `typeOf(any val)`
     * Returns a string describing the type of object `val` is.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * {
     *     string: ds.typeOf(""),
     *     bool: ds.typeOf(true),
     *     "null": ds.typeOf(null),
     *     number: ds.typeOf(0),
     *     "function": ds.typeOf(function() 1),
     *     array: ds.typeOf([]),
     *     object: ds.typeOf({})
     * }
     * ------------------------
     * .Result
     * ------------------------
     * {
     *   "string": "string",
     *   "bool": "boolean",
     *   "null": "null",
     *   "number": "number",
     *   "function": "function",
     *   "array": "array",
     *   "object": "object"
     * }
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("typeOf", "value") {
      (_, _, value: Val) =>
        value match {
          case Val.True | Val.False => "boolean"
          case Val.Null => "null"
          case _: Val.Obj => "object"
          case _: Val.Arr => "array"
          case _: Val.Func => "function"
          case _: Val.Num => "number"
          case _: Val.Str => "string"
        }
    },

    /** Documentation
     * ### `unzip(array arr)`
     * Unzips an array of arrays `arr` and creates a new array of arrays based on their index in `arr`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * [
     *   [
     *     1,
     *     2
     *   ],
     *   [
     *     1,
     *     2
     *   ]
     * ]
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.unzip(payload)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   [
     *     1,
     *     1
     *   ],
     *   [
     *     2,
     *     2
     *   ]
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("unzip", "array") {
      (_, _, array: Val.Arr) =>
        val size = array.value.map(
          _.force match {
            case Val.Arr(arr) => arr.size
            case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
          }
        ).max
        val out = collection.mutable.Buffer.empty[Val.Lazy]
        for (i <- 0 until size) {
          val current = collection.mutable.Buffer.empty[Val.Lazy]
          for (x <- array.value) {
            current.append(x.force.asInstanceOf[Val.Arr].value(i))
          }
          out.append(Val.Lazy(Val.Arr(current.toSeq)))
        }
        Val.Arr(out.toSeq)
    },

    /** Documentation
     * ### `upper(string str)`
     * Converts a string to all uppercase characters.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "string": "HeLlO wOrLd"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.upper(payload.string)
     * ------------------------
     * .Result
     * ------------------------
     * "HELLO WORLD"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("upper", "str") {
      (_, _, str: String) =>
        str.toUpperCase()
    },

    /** Documentation
     * ### `uuid`
     * Generates random alphanumeric uuid.
     * 
     * *Example*
     * 
     * .DataSonnet map:
     * ------------------------
     * ds.uuid
     * ------------------------
     * .Result
     * ------------------------
     * "cj36alpm-8mlt-fm43-8vth-mbd961259lqh"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin0("uuid") {
      (_, _, _) =>
        val n = 36
        val AlphaNumericString = "0123456789" +
          "abcdefghijklmnopqrstuvxyz"
        val sb = new StringBuilder(n)
        for (i <- 0 until n) {
          if (i.equals(8) || i.equals(13) || i.equals(18) || i.equals(23)) {
            sb.append('-')
          }
          else {
            val index = (AlphaNumericString.length * Math.random()).toInt
            sb.append(AlphaNumericString.charAt(index))
          }
        }
        Val.Lazy(Val.Str(sb.toString())).force
    },

    /** Documentation
     * ### `valuesOf(object obj)`
     * Given an object `obj`, returns an array of the values inside `obj`.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *    "origin": "PHX",
     *    "destination": "SEA"
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.valuesOf(payload)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   "PHX",
     *   "SEA"
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("valuesOf", "obj") {
      (ev, fs, obj: Val.Obj) =>
        Val.Arr(obj.getVisibleKeys().keySet.map(key => Val.Lazy(obj.value(key, -1)(fs, ev))).toSeq)
    },

    /** Documentation
     * ### `write(array|object item, string mimeType, object params)`
     * Converts `item` to a string.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "price": 8.95
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.write(payload, "application/json", {})
     * ------------------------
     * .Result
     * ------------------------
     * "{\"price\":8.95}"
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    // moved from dataformats
    builtinWithDefaults("write",
      "data" -> None,
      "mimeType" -> None,
      "params" -> Some(Expr.Null(0))) { (args, ev) =>
      val data = args("data")
      val mimeType = args("mimeType").cast[Val.Str].value
      val params = if (args("params") == Val.Null) {
        Library.EmptyObj
      } else {
        args("params").cast[Val.Obj]
      }
      write(dataFormats, data, mimeType, params, ev)
    },

    /** Documentation
     * ### `zip(array array1, array array2)`
     * Accepts `array1` and `array2` and combines them into one using elements with matching indexes.
     * 
     * *Example*
     * 
     * .Payload
     * ----------
     * {
     *     "firstNames": ["Evelyn", "Herman"],
     *     "lastNames": ["Waugh" , "Melville", "Tolkien"]
     * }
     * ----------
     * .DataSonnet map:
     * ------------------------
     * ds.zip(payload.firstNames, payload.lastNames)
     * ------------------------
     * .Result
     * ------------------------
     * [
     *   [
     *     "Evelyn",
     *     "Waugh"
     *   ],
     *   [
     *     "Herman",
     *     "Melville"
     *   ]
     * ]
     * ------------------------
     *
     * *Version*:
     *
     * Created: 1.0.0
     */
    builtin("zip", "array1", "array2") {
      (_, _, array1: Val.Arr, array2: Val.Arr) =>

        val smallArray = if (array1.value.size <= array2.value.size) array1 else array2
        val bigArray = (if (smallArray == array1) array2 else array1).value
        val out = collection.mutable.Buffer.empty[Val.Lazy]
        for ((v, i) <- smallArray.value.zipWithIndex) {
          val current = collection.mutable.Buffer.empty[Val.Lazy]
          if (smallArray == array1) {
            current.append(v)
            current.append(bigArray(i))
          }
          else {
            current.append(bigArray(i))
            current.append(v)
          }
          out.append(Val.Lazy(Val.Arr(current.toSeq)))
        }
        Val.Arr(out.toSeq)
    }
  ).asJava

  override def modules(dataFormats: DataFormatService, header: Header): java.util.Map[String, Val.Obj] = Map(
    /** XML */
    "xml" -> moduleFrom(
      builtinWithDefaults("flattenContents", "element" -> None, "namespaces" -> Some(Expr.Null(0))) {
        (args, ev) =>
          val element = args("element").cast[Val.Obj]
          val namespaces = if (args("namespaces") == Val.Null) {
            Library.EmptyObj
          } else {
            args("namespaces").cast[Val.Obj]
          }

          val wrapperName = "a"
          val wrapperStop = s"</$wrapperName>"

          val wrapperProperties = scala.collection.mutable.Map[String, Val.Obj.Member]()
          wrapperProperties += ("a" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => element))
          val wrapped = new Val.Obj(wrapperProperties, _ => (), None)

          val xmlProperties = scala.collection.mutable.Map[String, Val.Obj.Member]()
          xmlProperties += ("OmitXmlDeclaration" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Str("true")))
          namespaces.foreachVisibleKey((key, _) => {
            xmlProperties += ("NamespaceDeclarations." + key ->
              Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) =>
                namespaces.value(key, -1)(new FileScope(null, Map.empty), ev)))
          })

          val properties = new Val.Obj(xmlProperties, _ => (), None)

          val written = write(dataFormats, wrapped, "application/xml", properties, ev)

          written.substring(written.indexOf(">") + 1, written.length - wrapperStop.length)
      },
    ),
    /** DateTime */
    /** Documentation
     * This library uses Java's DateTimeFormatter library to format the date to a consistent value using ISO_OFFSET_DATE_TIME.
     * If your datetime is not in this format, you can use the `parse` function to convert it. After you are finished executing your logic,
     * you can use the `format` function to set the output format.
     */
    "datetime" -> moduleFrom(

      /** Documentation
       * ### `atBeginningOfDay(string datetime)`
       * Returns the given datetime at midnight.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.atBeginningOfDay("2020-12-31T23:19:35Z")
       * ------------------------
       * .Result:
       * ------------------------
       * "2020-12-31T00:00:00Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("atBeginningOfDay", "datetime"){
        (_,_,datetime: String) =>
          val date = java.time.ZonedDateTime
            .parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          date.minusHours(date.getHour)
              .minusMinutes(date.getMinute)
              .minusSeconds(date.getSecond)
              .minusNanos(date.getNano)
            .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `atBeginningOfHour(string datetime)`
       * Returns the given datetime with the minutes and seconds set to zero.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.atBeginningOfHour("2020-12-31T23:19:35Z")
       * ------------------------
       * .Result:
       * ------------------------
       * "2020-12-31T23:00:00Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("atBeginningOfHour", "datetime"){
        (_,_,datetime: String) =>
          val date = java.time.ZonedDateTime
            .parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          date.minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano)
            .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `atBeginningOfMonth(string datetime)`
       * Returns the given datetime with the day set to first of the month and the time set to midnight.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.atBeginningOfMonth("2020-12-31T23:19:35Z")
       * ------------------------
       * .Result:
       * ------------------------
       * "2020-12-01T00:00:00Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("atBeginningOfMonth", "datetime"){
        (_,_,datetime: String) =>
          val date = java.time.ZonedDateTime
            .parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          date.minusDays(date.getDayOfMonth-1)
            .minusHours(date.getHour)
            .minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano)
            .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `atBeginningOfWeek(string datetime)`
       * Returns the given datetime at the first of the current week and the time set to midnight
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.atBeginningOfWeek("2020-12-31T23:19:35Z")
       * ------------------------
       * .Result:
       * ------------------------
       * "2020-12-27T00:00:00Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("atBeginningOfWeek", "datetime"){
        (_,_,datetime: String) =>
          val date = java.time.ZonedDateTime
            .parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)

          date.minusDays( if(date.getDayOfWeek.getValue == 7) 0 else date.getDayOfWeek.getValue  )
            .minusHours(date.getHour)
            .minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano)
            .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `atBeginningOfYear(string datetime)`
       * Returns the given datetime at the first of the year
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.atBeginningOfYear("2020-12-31T23:19:35Z")
       * ------------------------
       * .Result:
       * ------------------------
       * "2020-01-01T00:00:00Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("atBeginningOfYear", "datetime"){
        (_,_,datetime: String) =>
          val date = java.time.ZonedDateTime
            .parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          date.minusMonths(date.getMonthValue-1)
            .minusDays(date.getDayOfMonth-1)
            .minusHours(date.getHour)
            .minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano)
            .format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `changeTimeZone(string datetime, string timezone)`
       * Changes the date timezone, retaining the instant. This normally results in a change to the local date-time.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.changeTimeZone("2020-12-31T23:19:35Z", "America/Los_Angeles")
       * ------------------------
       * .Result:
       * ------------------------
       * "2020-12-31T15:19:35-08:00"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("changeTimeZone", "datetime", "timezone") {
        (_, _, datetime: String, timezone: String) =>
          val datetimeObj = java.time.ZonedDateTime.parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          val zoneId = ZoneId.of(timezone)
          val newDateTimeObj = datetimeObj.withZoneSameInstant(zoneId)
          newDateTimeObj.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `compare(string datetime1, string datetime2)`
       * Returns `1` if `datetime1 > datetime2`, `-1` if `datetime1 < datetime2`, and `0` if `datetime1 == datetime2`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.compare("2020-12-31T23:19:35Z","2020-01-01T00:00:00Z")
       * ------------------------
       * .Result
       * ------------------------
       * 1
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("compare", "datetime", "datetwo") { (_, _, datetimeone: String, datetimetwo: String) =>
          val datetimeObj1 = java.time.ZonedDateTime
            .parse(datetimeone, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          val datetimeObj2 = java.time.ZonedDateTime
            .parse(datetimetwo, DateTimeFormatter.ISO_OFFSET_DATE_TIME)

          datetimeObj1.compareTo(datetimeObj2)
      },

      /** Documentation
       * ### `date(object datetime)`
       * This function uses a datetime object to generate a datetime in string format.
       * Every key in the object is an optional number value, except the timezone which is an optional string.
       * 
       * Example structure:
       * ------------------------
       * {
       *     "year": 0,
       *     "month": 0,
       *     "day": 0,
       *     "hour": 0,
       *     "minute": 0,
       *     "second": 0,
       *     "timezone": "Z"
       * }
       * ------------------------
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * local datetime={
       *     "year": 2021,
       *     "timezone": "America/Los_Angeles"
       * };
       * ds.datetime.date(datetime)
       * ------------------------
       * .Result
       * ------------------------
       * "2021-01-01T00:00:00-08:00"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("date", "obj") {
        (ev,fs,obj: Val.Obj) =>
          //year, month, dayOfMonth, hour, minute, second, nanoSecond, zoneId
          val out = mutable.Map[String, Val]()
          obj.foreachVisibleKey( (key,_) => out.addOne(key, obj.value(key,-1)(fs,ev)))
          java.time.ZonedDateTime.of(
            out.getOrElse("year",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toInt,
            out.getOrElse("month",Val.Lazy(Val.Num(1)).force).cast[Val.Num].value.toInt,
            out.getOrElse("day",Val.Lazy(Val.Num(1)).force).cast[Val.Num].value.toInt,
            out.getOrElse("hour",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toInt,
            out.getOrElse("minute",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toInt,
            out.getOrElse("second",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toInt,
            0, //out.getOrElse("nanosecond",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toInt TODO?
            ZoneId.of(out.getOrElse("timezone",Val.Lazy(Val.Str("Z")).force).cast[Val.Str].value)
          ).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `daysBetween(string datetime1, string datetime2)`
       * Returns the number of days between `datetime1` and `datetime2`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * local date1 = "2019-09-20T18:53:41.425Z";
       * local date2 = "2019-09-14T18:53:41.425Z";
       * ds.datetime.daysBetween(date1, date2)
       * ------------------------
       * .Result
       * ------------------------
       * 6
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      // newly added
      builtin("daysBetween", "datetime", "datetwo") {
        (_, _, datetimeone: String, datetimetwo: String) =>
          val dateone = java.time.ZonedDateTime
            .parse(datetimeone, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          val datetwo = java.time.ZonedDateTime
            .parse(datetimetwo, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
          ChronoUnit.DAYS.between(dateone, datetwo).abs.toDouble;
      },

      /** Documentation
       * ### `format(string datetime, string outputFormat)`
       * Given a datetime, will convert it to the specified output format.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.format("2019-09-20T18:53:41.425Z", "yyyy/MM/dd")
       * ------------------------
       * .Result
       * ------------------------
       * "2019/09/20"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("format", "datetime", "outputFormat") { (_, _, datetime: String, outputFormat: String) =>
        val datetimeObj = java.time.ZonedDateTime.parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        datetimeObj.format(DateTimeFormatter.ofPattern(outputFormat))
      },

      /** Documentation
       * ### `isLeapYear(string datetime)`
       * Returns a boolean indicating if `datetime` is a leap year.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.isLeapYear("2019-09-14T18:53:41.425Z")
       * ------------------------
       * .Result
       * ------------------------
       * false
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isLeapYear", "datetime") {
        (_, _, datetime: String) =>
          java.time.ZonedDateTime
            .parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
            .toLocalDate.isLeapYear;
      },

      /** Documentation
       * ### `minus(string datetime, string period)`
       * Subtracts a `period` type from the given datetime.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.minus("2019-09-20T18:53:41Z", "P2D")
       * ------------------------
       * .Result
       * ------------------------
       * "2019-09-18T18:53:41Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("minus", "datetime", "period") { (_, _, date: String, period: String) =>
        val datetime = java.time.ZonedDateTime.parse(date, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        if (period.contains("T")) {
          datetime.minus(Duration.parse(period)).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        } else {
          datetime.minus(Period.parse(period)).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        }
      },

      /** Documentation
       * ### `now()`
       * Returns the current datetime.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.now()
       * ------------------------
       * .Result:
       * ------------------------
       * "2021-01-05T13:09:45.476375-05:00"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("now") { (_, _, _) => ZonedDateTime.now().format(DateTimeFormatter.ISO_OFFSET_DATE_TIME) },

      /** Documentation
       * ### `parse(string|number datetime, string inputFormat)`
       * Parses the datetime using the input format and returns the value in the default format.
       * If an epoch or timestamp value is used as the datetime you can use `"epoch"` or `"timestamp"` as the inputFormat
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.parse("12/31/1990 10:10:10", "MM/dd/yyyy HH:mm:ss")
       * ------------------------
       * .Result:
       * ------------------------
       * "1990-12-31T10:10:10Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("parse", "datetime", "inputFormat") { (_, _, datetime: Val, inputFormat: String) =>
        var datetimeObj : ZonedDateTime = null
        inputFormat.toLowerCase match {
          case "timestamp" | "epoch" =>
            var inst : Instant = null
            datetime match{
              case Val.Str(item) => inst = Instant.ofEpochSecond(item.toInt.toLong)
              case Val.Num(item) => inst = Instant.ofEpochSecond(item.toLong)
              case _ => throw Error.Delegate("Expected datetime to be a string or number, got: " + datetime.prettyName)
            }
            datetimeObj = java.time.ZonedDateTime.ofInstant(inst, ZoneOffset.UTC)
          case _ =>
            datetimeObj = try{ //will catch any errors if zone data is missing and default to Z
              java.time.ZonedDateTime.parse(datetime.cast[Val.Str].value, DateTimeFormatter.ofPattern(inputFormat))
            } catch {
              case e: DateTimeException =>
                LocalDateTime.parse(datetime.cast[Val.Str].value, DateTimeFormatter.ofPattern(inputFormat)).atZone(ZoneId.of("Z"))
            }
        }
        datetimeObj.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `plus(string datetime, string period)`
       * Adds a `period` type to the given datetime.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.plus("2019-09-18T18:53:41Z", "P2D")
       * ------------------------
       * .Result
       * ------------------------
       * "2019-09-20T18:53:41Z"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("plus", "datetime", "period") { (_, _, date: String, period: String) =>
        val datetime = java.time.ZonedDateTime.parse(date, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        if (period.contains("T")) {
          datetime.plus(Duration.parse(period)).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        } else {
          datetime.plus(Period.parse(period)).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        }
      },

      /** Documentation
       * ### `toLocalDate(string datetime)`
       * Converts a zone datetime to a local date
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.toLocalDate("2019-07-04T18:53:41Z")
       * ------------------------
       * .Result:
       * ------------------------
       * 2019-07-04
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toLocalDate", "datetime") { (_, _, datetime: String) =>
        val datetimeObj = java.time.ZonedDateTime.parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        datetimeObj.toLocalDate.format(DateTimeFormatter.ISO_LOCAL_DATE)
      },

      /** Documentation
       * ### `toLocalDateTime(string datetime)`
       * Converts a zone datetime to a local datetime
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.toLocalDateTime("2019-07-04T21:00:00Z")
       * ------------------------
       * .Result:
       * ------------------------
       * 2019-07-04T21:00:00
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toLocalDateTime", "datetime") { (_, _, datetime: String) =>
        val datetimeObj = java.time.ZonedDateTime.parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        datetimeObj.toLocalDateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
      },

      /** Documentation
       * ### `toLocalTime(string datetime, string format)`
       * Converts a zone datetime to a local time.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.toLocalTime("2019-07-04T21:00:00Z")
       * ------------------------
       * .Result:
       * ------------------------
       * 21:00:00
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toLocalTime", "datetime") { (_, _, datetime: String) =>
        val datetimeObj = java.time.ZonedDateTime.parse(datetime, DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        datetimeObj.toLocalTime.format(DateTimeFormatter.ISO_LOCAL_TIME)
      },

      /** Documentation
       * ### `today()`
       * Returns the datetime of the current day at midnight.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.today
       * ------------------------
       * .Result:
       * ------------------------
       * "2021-01-05T00:00:00-05:00"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("today") {
        (_,_,_) =>
          val date = java.time.ZonedDateTime.now()
          date.minusHours(date.getHour)
            .minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `tomorrow()`
       * Returns the datetime of the next day at midnight.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.tomorrow
       * ------------------------
       * .Result:
       * ------------------------
       * "2021-01-06T00:00:00-05:00"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("tomorrow") {
        (_,_,_) =>
          val date = java.time.ZonedDateTime.now()
          date.plusDays(1)
            .minusHours(date.getHour)
            .minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },

      /** Documentation
       * ### `yesterday()`
       * Returns the datetime of the previous day at midnight.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.datetime.yesterday
       * ------------------------
       * .Result:
       * ------------------------
       * "2021-01-04T00:00:00-05:00"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("yesterday") {
        (_,_,_) =>
          val date = java.time.ZonedDateTime.now()
          date.minusDays(1)
            .minusHours(date.getHour)
            .minusMinutes(date.getMinute)
            .minusSeconds(date.getSecond)
            .minusNanos(date.getNano).format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
      },
    ),

    /** Period */
    "period" -> moduleFrom(

      /** Documentation
       * ### `between(string datetime1, string datetime2)`
       * Returns the period between two datetime objects.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.between(
       *     ds.datetime.date({year:2020}),
       *     ds.datetime.date({year:2019, month: 3})
       * )
       * ------------------------
       * .Result:
       * ------------------------
       * "P-10M"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("between", "datetimeone", "datetimetwo") {
        (_,_, datetimeone: String , datetimetwo: String) =>
          Period.between(
            java.time.ZonedDateTime.parse(datetimeone, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toLocalDate,
            java.time.ZonedDateTime.parse(datetimetwo, DateTimeFormatter.ISO_OFFSET_DATE_TIME).toLocalDate
          ).toString
      },

      /** Documentation
       * ### `days(number num)`
       * Returns the number of given days in period format.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.days(5)
       * ------------------------
       * .Result:
       * ------------------------
       * "P5D"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("days", "num") {
        (_,_, num: Int ) =>
          Period.ofDays(num).toString
      },

      /** Documentation
       * ### `duration(object time)`
       * Returns the given time object in a Period of Time format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.duration({days: 1, hours: 1, minutes: 1, seconds: 1})
       * ------------------------
       * .Result:
       * ------------------------
       * "PT25H1M1S"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("duration", "obj") {
        (ev,fs, obj: Val.Obj ) =>
          val out = mutable.Map[String, Val]()
          obj.foreachVisibleKey( (key,_) => out.addOne(key, obj.value(key,-1)(fs,ev)))
          Duration.ZERO
            .plusDays(out.getOrElse("days",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .plusHours(out.getOrElse("hours",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .plusMinutes(out.getOrElse("minutes",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .plusSeconds(out.getOrElse("seconds",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .toString
      },

      /** Documentation
       * ### `hours(number num)`
       * Returns the number of given hours in a Period of Time format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.hours(1)
       * ------------------------
       * .Result:
       * ------------------------
       * "PT1H"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("hours", "num") {
        (_,_, num: Int ) =>
          Duration.ofHours(num).toString
      },

      /** Documentation
       * ### `minutes(number num)`
       * Returns the number of given minutes in a Period of Time format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.hours(1)
       * ------------------------
       * .Result:
       * ------------------------
       * "PT1M"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("minutes", "num") {
        (_,_, num: Int ) =>
          Duration.ofMinutes(num).toString
      },

      /** Documentation
       * ### `months(number num)`
       * Returns the number of given months in a Period format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.months(1)
       * ------------------------
       * .Result:
       * ------------------------
       * "P1M"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("months", "num") {
        (_,_, num: Int ) =>
          Period.ofMonths(num).toString
      },

      /** Documentation
       * ### `period(object time)`
       * Returns the given time object in a Period format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.period({years: 1, months: 1, days: 1})
       * ------------------------
       * .Result:
       * ------------------------
       * "P1Y1M1D"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("period", "obj") {
        (ev,fs, obj: Val.Obj ) =>
          val out = mutable.Map[String, Val]()
          obj.foreachVisibleKey( (key,_) => out.addOne(key, obj.value(key,-1)(fs,ev)))
          Period.ZERO
            .plusYears(out.getOrElse("years",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .plusMonths(out.getOrElse("months",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .plusDays(out.getOrElse("days",Val.Lazy(Val.Num(0)).force).cast[Val.Num].value.toLong)
            .toString
      },

      /** Documentation
       * ### `seconds(number num)`
       * Returns the number of given seconds in a Period of Time format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.seconds(1)
       * ------------------------
       * .Result:
       * ------------------------
       * "PT1S"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("seconds", "num") {
        (_,_, num: Int ) =>
          Duration.ofSeconds(num).toString
      },

      /** Documentation
       * ### `years(number num)`
       * Returns the number of given years in a Period format
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.period.years(1)
       * ------------------------
       * .Result:
       * ------------------------
       * "P1Y"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("years", "num") {
        (_,_, num: Int ) =>
          Period.ofYears(num).toString
      },
    ),

    /** Crypto */
    "crypto" -> moduleFrom(

      /** Documentation
       * ### `decrypt(string value, string secret, string algorithm, string mode, string padding)`
       * Decrypts the Base64 value with specified JDK Cipher Transformation string and the provided secret.
       * 
       * The transformation string describes the operation (or set of operations) to be performed on the given input, to produce some output. A transformation always includes the name of a cryptographic algorithm (e.g., AES), and may be followed by a feedback mode and padding scheme. A transformation is of the form: "algorithm/mode/padding" or "algorithm". See https://docs.oracle.com/en/java/javase/11/docs/api/java.base/javax/crypto/Cipher.html[Java Cipher] for more information.
       * 
       * *Example:*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.crypto.decrypt("Hello World", "DataSonnet123456", "AES/ECB/PKCS5Padding")
       * ------------------------
       * .Result
       * ------------------------
       * "HrkF1grBXCtATMLxh1gZVA=="
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0[Val]("decrypt", "value", "secret", "transformation") {
        (vals, ev,fs) =>
          val valSeq = validate(vals, ev, fs, Array(StringRead, StringRead, StringRead))
          val value = valSeq(0).asInstanceOf[String]
          val secret = valSeq(1).asInstanceOf[String]
          val transformation = valSeq(2).asInstanceOf[String]

          val cipher = Cipher.getInstance(transformation)
          val transformTokens = transformation.split("/")

          // special case for ECB because of java.security.InvalidAlgorithmParameterException: ECB mode cannot use IV
          if (transformTokens.length >= 2 && "ECB".equals(transformTokens(1))) {
            cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(secret.getBytes, transformTokens(0).toUpperCase))
            Val.Str(new String(cipher.doFinal(Base64.getDecoder.decode(value))))

          } else {
            // https://stackoverflow.com/a/52571774/4814697
            // separate prefix with IV from the rest of encrypted data//separate prefix with IV from the rest of encrypted data
            val encryptedPayload = Base64.getDecoder.decode(value)
            val iv = new Array[Byte](cipher.getBlockSize)
            val encryptedBytes = new Array[Byte](encryptedPayload.length - iv.length)
            val rand: SecureRandom = new SecureRandom()

            // populate iv with bytes:
            System.arraycopy(encryptedPayload, 0, iv, 0, iv.length)

            // populate encryptedBytes with bytes:
            System.arraycopy(encryptedPayload, iv.length, encryptedBytes, 0, encryptedBytes.length)

            cipher.init(Cipher.DECRYPT_MODE,
              new SecretKeySpec(secret.getBytes, transformTokens(0).toUpperCase),
              new IvParameterSpec(iv),
              rand)

            Val.Str(new String(cipher.doFinal(encryptedBytes)))
          }
      },

      /** Documentation
       * ### `encrypt(string value, string secret, string transformation)`
       * Encrypts the value with specified JDK Cipher Transformation and the provided secret. Converts the encryption to a readable format with Base64.
       * 
       * The transformation string describes the operation (or set of operations) to be performed on the given input, to produce some output. A transformation always includes the name of a cryptographic algorithm (e.g., AES), and may be followed by a feedback mode and padding scheme. A transformation is of the form: "algorithm/mode/padding" or "algorithm". See https://docs.oracle.com/en/java/javase/11/docs/api/java.base/javax/crypto/Cipher.html[Java Cipher] for more information.
       * 
       * *Example:*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.crypto.decrypt("HrkF1grBXCtATMLxh1gZVA==", "DataSonnet123456", "AES/ECB/PKCS5Padding")
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0[Val]("encrypt", "value", "secret", "transformation") {
        (vals, ev, fs) =>
          val valSeq = validate(vals, ev, fs, Array(StringRead, StringRead, StringRead))
          val value = valSeq(0).asInstanceOf[String]
          val secret = valSeq(1).asInstanceOf[String]
          val transformation = valSeq(2).asInstanceOf[String]

          val cipher = Cipher.getInstance(transformation)
          val transformTokens = transformation.split("/")

          // special case for ECB because of java.security.InvalidAlgorithmParameterException: ECB mode cannot use IV
          if (transformTokens.length >= 2 && "ECB".equals(transformTokens(1))) {
            cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(secret.getBytes, transformTokens(0).toUpperCase))
            Val.Str(Base64.getEncoder.encodeToString(cipher.doFinal(value.getBytes)))

          } else {
            // https://stackoverflow.com/a/52571774/4814697
            val rand: SecureRandom = new SecureRandom()
            val iv = new Array[Byte](cipher.getBlockSize)
            rand.nextBytes(iv)

            cipher.init(Cipher.ENCRYPT_MODE,
              new SecretKeySpec(secret.getBytes, transformTokens(0).toUpperCase),
              new IvParameterSpec(iv),
              rand)

            // encrypted data:
            val encryptedBytes = cipher.doFinal(value.getBytes)

            // append Initiation Vector as a prefix to use it during decryption:
            val combinedPayload = new Array[Byte](iv.length + encryptedBytes.length)

            // populate payload with prefix IV and encrypted data
            System.arraycopy(iv, 0, combinedPayload, 0, iv.length)
            System.arraycopy(encryptedBytes, 0, combinedPayload, iv.length, encryptedBytes.length)

            Val.Str(Base64.getEncoder.encodeToString(combinedPayload))
          }
      },

      /** Documentation
       * ### `hash(string value, string algorithm)`
       * Calculates hash of `value` using one of the supported algorithms. The `algorithm` must be one of `MD2`, `MD5`, `SHA-1`, `SHA-256`, `SHA-384`, `SHA-512`
       * 
       * The response is a string containing the hash bytes.
       * 
       * *Example:*
       * 
       * .DataSonnet map:
       * ------------------------
       * {
       *     hashValue: ds.crypto.hash("HelloWorld", "MD5")
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *     "hashValue": "68e109f0f40ca72a15e05cc22786f8e6"
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("hash", "value", "algorithm") {
        (_, _, value: String, algorithm: String) =>
          Crypto.hash(value, algorithm)
      },

      /** Documentation
       * ### `hmac(string value, string secret, string algorithm)`
       * Generates hash-based message authentication code using provided `value`, `secret`, and a hash function `algorithm`. The `algorithm` must be one of `HmacSHA1`, `HmacSHA256` or `HmacSHA512`.
       * 
       * *Example:*
       * 
       * .DataSonnet map:
       * ------------------------
       * {
       *     hmacValue: ds.crypto.hmac("HelloWorld", "DataSonnet rules!", "HmacSHA256")
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *     "hmacValue": "7854220ef827b07529509f68f391a80bf87fff328dbda140ed582520a1372dc1"
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("hmac", "value", "secret", "algorithm") {
        (_, _, value: String, secret: String, algorithm: String) =>
          Crypto.hmac(value, secret, algorithm)
      }
    ),

    /** Jsonpath */
    "jsonpath" -> moduleFrom(

      /** Documentation
       * ### `select(object json, string path)`
       * Evaluates JsonPath expression and returns the resulting JSON object.
       * It uses the https://github.com/json-path/JsonPath[Jayway JsonPath implementation] and fully supports https://goessner.net/articles/JsonPath/[JsonPath specification].
       * 
       * *Example*
       * 
       * .Payload
       * ------------------------
       * {
       *   "store": {
       *     "book": [
       *       {
       *         "category": "reference",
       *         "author": "Nigel Rees",
       *         "title": "Sayings of the Century",
       *         "price": 8.95
       *       },
       *       {
       *         "category": "fiction",
       *         "author": "Evelyn Waugh",
       *         "title": "Sword of Honour",
       *         "price": 12.99
       *       },
       *       {
       *         "category": "fiction",
       *         "author": "Herman Melville",
       *         "title": "Moby Dick",
       *         "isbn": "0-553-21311-3",
       *         "price": 8.99
       *       },
       *       {
       *         "category": "fiction",
       *         "author": "J. R. R. Tolkien",
       *         "title": "The Lord of the Rings",
       *         "isbn": "0-395-19395-8",
       *         "price": 22.99
       *       }
       *     ]
       *   }
       * }
       * ------------------------
       * .DataSonnet map:
       * ------------------------
       * {
       *     author: ds.jsonpath.select(payload, "$..book[-2:]..author")[0]
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *     "author": "Herman Melville"
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("select", "json", "path") {
        (ev, _, json: Val, path: String) =>
          Materializer.reverse(ujson.read(JsonPath.select(ujson.write(Materializer.apply(json)(ev)), path)))
      }
    ),

    /** TODO: Remove */
    "regex" -> moduleFrom(
      builtin("regexFullMatch", "expr", "str") {
        (_, _, expr: String, str: String) =>
          Materializer.reverse(Regex.regexFullMatch(expr, str))
      },

      builtin("regexPartialMatch", "expr", "str") {
        (_, _, expr: String, str: String) =>
          Materializer.reverse(Regex.regexPartialMatch(expr, str))
      },

      builtin("regexScan", "expr", "str") {
        (_, _, expr: String, str: String) =>
          Materializer.reverse(Regex.regexScan(expr, str))
      },

      builtin("regexQuoteMeta", "str") {
        (_, _, str: String) =>
          Regex.regexQuoteMeta(str)
      },

      builtin("regexReplace", "str", "pattern", "replace") {
        (_, _, str: String, pattern: String, replace: String) =>
          Regex.regexReplace(str, pattern, replace)
      },

      builtinWithDefaults("regexGlobalReplace", "str" -> None, "pattern" -> None, "replace" -> None) { (args, ev) =>
        val replace = args("replace")
        val str = args("str").asInstanceOf[Val.Str].value
        val pattern = args("pattern").asInstanceOf[Val.Str].value

        replace match {
          case replaceStr: Val.Str => Regex.regexGlobalReplace(str, pattern, replaceStr.value)
          case replaceF: Val.Func =>
            val func = new Function[Value, String] {
              override def apply(t: Value): String = {
                val v = Materializer.reverse(t)
                Applyer(replaceF, ev, null).apply(Val.Lazy(v)) match {
                  case resultStr: Val.Str => resultStr.value
                  case _ => throw Error.Delegate("The result of the replacement function must be a String")
                }
              }
            }
            Regex.regexGlobalReplace(str, pattern, func)

          case _ => throw Error.Delegate("'replace' parameter must be either String or Function")
        }
      }
    ),

    /** URL */
    "url" -> moduleFrom(

      /** Documentation
       * ### `decode(string data, string encoding="UTF-8")`
       * Decodes a application/x-www-form-urlencoded string using a specific encoding scheme. The supplied encoding is used to determine what characters are represented by any consecutive sequences of the form "%xy".
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.url.decode('Hello+World')
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtinWithDefaults("decode",
        "data" -> None,
        "encoding" -> Some(Expr.Str(0, "UTF-8"))) { (args, ev) =>
        val data = args("data").cast[Val.Str].value
        val encoding = args("encoding").cast[Val.Str].value

        java.net.URLDecoder.decode(data, encoding)
      },

      /** Documentation
       * ### `encode(string data, string encoding="UTF-8")`
       * Translates a string into `application/x-www-form-urlencoded` format using the supplied encoding scheme to obtain the bytes for unsafe characters. The default encoding is `UTF-8`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.url.encode('Hello World')
       * ------------------------
       * .Result
       * ------------------------
       * "Hello+World"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtinWithDefaults("encode",
        "data" -> None,
        "encoding" -> Some(Expr.Str(0, "UTF-8"))) { (args, ev) =>
        val data = args("data").cast[Val.Str].value
        val encoding = args("encoding").cast[Val.Str].value

        java.net.URLEncoder.encode(data, encoding)
      },
    ),

    /** Math */
    "math" -> moduleFrom(

      /** Documentation
       * ### `abs(number num)`
       * Returns the absolute value of `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.abs(-1)
       * ------------------------
       * .Result
       * ------------------------
       * 1
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("abs", "num") {
        (_, _, num: Double) =>
          Math.abs(num);
      },

      /** Documentation
       * ### `acos(number num)`
       * Performs math acos operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.acos(1)
       * ------------------------
       * .Result
       * ------------------------
       * 0
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("acos", "x") { (_, _, x: Double) =>
        Math.acos(x)
      },

      /** Documentation
       * ### `asin(number num)`
       * Performs math asin operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.asin(1)
       * ------------------------
       * .Result
       * ------------------------
       * 1.5707963267948966
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("asin", "x") { (_, _, x: Double) =>
        Math.asin(x)
      },

      /** Documentation
       * ### `atan(number num)`
       * Performs math atan operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.atan(1)
       * ------------------------
       * .Result
       * ------------------------
       * 0.7853981633974483
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("atan", "x") { (_, _, x: Double) =>
        Math.atan(x)
      },

      /** Documentation
       * ### `avg(array arr)`
       * Returns the average value of `arr`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.avg([1,2,3])
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      // See: https://damieng.com/blog/2014/12/11/sequence-averages-in-scala
      // See: https://gist.github.com/gclaramunt/5710280
      builtin("avg", "array") {
        (_, _, array: Val.Arr) =>
          val (sum, length) = array.value.foldLeft((0.0, 0))({
            case ((sum, length), num) =>
              (num.force match {
                case Val.Num(x) => sum + x
                case i => throw Error.Delegate("Expected Array pf Numbers, got: Array of " + i.prettyName)
              }, 1 + length)
          })
          sum / length
      },

      /** Documentation
       * ### `ceil(number num)`
       * Rounds `num` up.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.ceil(1.01)
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("ceil", "num") {
        (_, _, num: Double) =>
          Math.ceil(num);
      },

      /** Documentation
       * ### `clamp(number value, number minVal, number maxVal)`
       * Limits `value` to the range of `minVal` and `maxVal`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.clamp(100, 0, 10)
       * ------------------------
       * .Result
       * ------------------------
       * 10
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("clamp", "x", "minVal", "maxVal") { (_, _, x: Double, minVal: Double, maxVal: Double) =>
        Math.max(minVal, Math.min(x, maxVal))
      },

      /** Documentation
       * ### `cos(number num)`
       * Performs math cos operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.cos(0)
       * ------------------------
       * .Result
       * ------------------------
       * 1
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("cos", "x") { (_, _, x: Double) =>
        Math.cos(x)
      },

      /** Documentation
       * ### `exp(number num)`
       * Returns the result of e to the power of `num`, in other words e^`num`^.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.exp(2)
       * ------------------------
       * .Result
       * ------------------------
       * 7.38905609893065
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("exp", "x") { (_, _, x: Double) =>
        Math.exp(x)
      },

      /** Documentation
       * ### `exponent(number num)`
       * Returns the non-decimal portion of a logarithmic operation.
       * 
       * exponent = (log(`num`)/log(2)) + 1
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.exponent(2)
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("exponent", "x") { (_, _, x: Double) =>
        (Math.log(x) / Math.log(2)).toInt + 1
      },

      /** Documentation
       * ### `floor(number num)`
       * Rounds `num` down.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.floor(4.99)
       * ------------------------
       * .Result
       * ------------------------
       * 4
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("floor", "num") {
        (_, _, num: Double) =>
          Math.floor(num);
      },

      /** Documentation
       * ### `log(number num)`
       * Performs math log operation. on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.log(2)
       * ------------------------
       * .Result
       * ------------------------
       * 0.6931471805599453
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("log", "x") { (_, _, x: Double) =>
        Math.log(x)
      },

      /** Documentation
       * ### `mantissa(number num)`
       * Returns the decimal portion of a logarithmic operation.
       * 
       * exponent = (log(`num`)/log(2)) + 1
       * 
       * mantissa = `num` * pow(2, -exponent)
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.mantissa(2)
       * ------------------------
       * .Result
       * ------------------------
       * 0.5
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("mantissa", "x") { (_, _, x: Double) =>
        x * Math.pow(2.0, -((Math.log(x) / Math.log(2)).toInt + 1))
      },

      /** Documentation
       * ### `mod(number num1, number num2)`
       * Performs modulo operation, returns how many times `num1` can go into `num2`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.mod(2,4)
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("mod", "num1", "num2") {
        (_, _, num1: Double, num2: Double) =>
          num1 % num2;
      },

      /** Documentation
       * ### `pow(number num1, number num2)`
       * Returns the value of `num1` to the power of `num2`, in other words `num1`^`num2`^.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.pow(2,2)
       * ------------------------
       * .Result
       * ------------------------
       * 4
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("pow", "num1", "num2") {
        (_, _, num1: Double, num2: Double) =>
          Math.pow(num1, num2)
      },

      /** Documentation
       * ### `random`
       * Returns a random float value between 0 and 1.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.random
       * ------------------------
       * .Result
       * ------------------------
       * 0.5963038027787421
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("random") {
        (_, _, _) =>
          (0.0 + (1.0 - 0.0) * Random.nextDouble()).doubleValue()
      },

      /** Documentation
       * ### `randomInt(number num)`
       * Returns a random integer between 0 and the provided number inclusive.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.randomInt(500)
       * ------------------------
       * .Result
       * ------------------------
       * 485
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("randomInt", "num") {
        (_, _, num: Int) =>
          (Random.nextInt((num - 0) + 1) + 0).intValue()
      },

      /** Documentation
       * ### `round(number num)`
       * Rounds `num` to the nearest whole number.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.round(2.5)
       * ------------------------
       * .Result
       * ------------------------
       * 3
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtinWithDefaults("round",
        "num" -> None,
        "precision" -> Some(Expr.Num(0, 0))) { (args, ev) =>
        val num = args("num").cast[Val.Num].value
        val prec = args("precision").cast[Val.Num].value.toInt

        if (prec == 0) {
          Math.round(num).intValue()
        } else {
          BigDecimal.valueOf(num).setScale(prec, RoundingMode.HALF_UP).doubleValue()
        }
      },

      /** Documentation
       * ### `sin(number num)`
       * Performs math sin operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.sin(1)
       * ------------------------
       * .Result
       * ------------------------
       * 0.8414709848078965
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("sin", "x") { (_, _, x: Double) =>
        Math.sin(x)
      },

      /** Documentation
       * ### `sqrt(number num)`
       * Performs math square root operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.sqrt(4)
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("sqrt", "num") {
        (_, _, num: Double) =>
          Math.sqrt(num)
      },

      /** Documentation
       * ### `sum(array arr)`
       * Returns sum of all elements in `arr`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [ 10, 20, 30 ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.math.sum(payload)
       * ------------------------
       * .Result
       * ------------------------
       * 60
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("sum", "array") {
        (_, _, array: Val.Arr) =>
          array.value.foldLeft(0.0)((sum, value) =>
            value.force match {
              case Val.Num(x) => sum + x
              case i => throw Error.Delegate("Expected Array of Numbers, got: Array of " + i.prettyName)
            }
          )
      },

      /** Documentation
       * ### `tan(number num)`
       * Performs math tan operation on `num`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.math.tan(1)
       * ------------------------
       * .Result
       * ------------------------
       * 1.5574077246549023
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("tan", "x") { (_, _, x: Double) =>
        Math.tan(x)
      },
    ),

    /** Arrays */
    "arrays" -> moduleFrom(

      /** Documentation
       * ### `countBy(array arr, function func)`
       * Returns the number of items in `arr` that passes the condition of `func`.
       * 
       * The function `func` is expected to take the value as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *     1,
       *     2,
       *     3,
       *     4,
       *     5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.countBy(payload, function(item) item > 2)
       * ------------------------
       * .Result
       * ------------------------
       * 3
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("countBy", "arr", "funct") {
        (_, _, arr: Val.Arr, funct: Applyer) =>
          var total = 0
          for (x <- arr.value) {
            if (funct.apply(x) == Val.True) {
              total += 1
            }
          }
          total
      },

      /** Documentation
       * ### `deepFlatten(array arr)`
       * Given `arr`, which contains one level and multi level arrays, returns a flat array.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   [
       *     1,
       *     2
       *   ],
       *   [
       *     3,
       *     4,
       *     [
       *       5,
       *       6
       *     ]
       *   ]
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.deepFlatten(payload)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5,
       *   6
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("deepFlatten", "arr") {
        (_, _, arr: Val.Arr) =>
          Val.Arr(deepFlatten(arr.value))
      },

      /** Documentation
       * ### `divideBy(array arr, number size)`
       * Divides a single array `arr` into multiple arrays, limiting each one to `size`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.divideBy(payload, 2)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   [
       *     1,
       *     2
       *   ],
       *   [
       *     3,
       *     4
       *   ],
       *   [
       *     5
       *   ]
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("divideBy", "array", "size") {
        (_, _, array: Val.Arr, size: Int) =>
          Val.Arr(array.value.sliding(size, size).map(item => Val.Lazy(Val.Arr(item))).toSeq)
      },

      /** Documentation
       * ### `drop(array arr, number index)`
       * Removes every item in `arr` until the specified `index` is reached.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.drop(payload, 3)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   4,
       *   5
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("drop", "arr", "num") {
        (_, _, arr: Val.Arr, num: Int) =>
          Val.Arr(arr.value.drop(num))
      },

      /** Documentation
       * ### `dropWhile(array arr, function func)`
       * Removes every item in `arr` until `func` returns a false result, then stops.
       * 
       * The function `func` is expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.dropWhile(payload, function(item) item < 3)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   3,
       *   4,
       *   5
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("dropWhile", "arr", "funct") {
        (_, _, arr: Val.Arr, funct: Applyer) =>
          Val.Arr(arr.value.dropWhile(funct.apply(_) == Val.True))
      },

      /** Documentation
       * ### `duplicates(array arr)`
       * Returns the values that exist more than once in `arr`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   1,
       *   2
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.duplicates(payload)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   1
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("duplicates", "array") {
        (_, _, array: Val.Arr) =>
          val out = mutable.Buffer.empty[Val.Lazy]
          array.value.collect({
            case item if array.value.count(_.force == item.force)>=2 &&
                           !out.exists(_.force == item.force) => out.append(item)
          })
          Val.Arr(out.toSeq)
      },

      /** Documentation
       * ### `every(array arr, function func)`
       * Returns true if every value `arr` returns true in `func`.
       * 
       * The function `func` is expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.every(payload, function(item) item > 0)
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("every", "value", "funct") {
        (_, _, value: Val, funct: Applyer) =>
          value match {
            case Val.Arr(arr) => Val.bool(arr.forall(funct.apply(_) == Val.True))
            case Val.Null => Val.Lazy(Val.True).force
            case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `firstWith(array arr, function func)`
       * Returns the first value that passes the condition of `func` then stops.
       * 
       * The function `func` is expected to take the value as the first parameter (required) and the index as the second (optional).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.firstWith(payload, function(item,index) item == index + 1)
       * ------------------------
       * .Result
       * ------------------------
       * 1
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("firstWith", "arr", "funct") {
        (_, _, arr: Val.Arr, funct: Applyer) =>
          val args = funct.f.params.allIndices.size
          if (args == 2)
            arr.value.zipWithIndex.find(item => funct.apply(item._1, Val.Lazy(Val.Num(item._2))) == Val.True).map(_._1).getOrElse(Val.Lazy(Val.Null)).force
          else if (args == 1)
            arr.value.find(funct.apply(_) == Val.True).getOrElse(Val.Lazy(Val.Null)).force
          else {
            throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
          }
      },

      /** Documentation
       * ### `indexOf(array arr, any value)`
       * Returns the current index of the matching `value` in `arr`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.indexOf(payload, 3)
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("indexOf", "container", "value") {
        (_, _, container: Val, value: Val) =>
          container match {
            case Val.Str(str) => Val.Lazy(Val.Num(str.indexOf(value.cast[Val.Str].value))).force
            case Val.Arr(array) => Val.Lazy(Val.Num(array.indexWhere(_.force == value))).force
            case Val.Null => Val.Lazy(Val.Num(-1)).force
            case i => throw Error.Delegate("Expected String or Array, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `indexWhere(array arr, function func)`
       * Returns the first index where the condition of `func` passes.
       * 
       * The function `func` is expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.indexWhere(payload, function(item) item == 3)
       * ------------------------
       * .Result
       * ------------------------
       * 2
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("indexWhere", "arr", "funct") {
        (_, _, array: Val.Arr, funct: Applyer) =>
          array.value.indexWhere(funct.apply(_) == Val.Lazy(Val.True).force)
      },

      /** Documentation
       * ### `join(array arrL, array arrR, function funcL, function funcR)`
       * Joins two arrays together, returns the items of `arrL` with the items that match from `arrR`.
       * 
       * Both functions `funcL` and `funcR` are expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *     "countries": [
       *       {
       *         "id": 1,
       *         "name":"Spain"
       *       },
       *       {
       *         "id": 2,
       *         "name":"France"
       *       },
       *       {
       *         "id": 3,
       *         "name":"Germany"
       *       }
       *     ],
       *     "languages": [
       *       {
       *         "countryId": 1,
       *         "name":"Spanish"
       *       },
       *       {
       *         "countryId": 2,
       *         "name":"French"
       *       },
       *       {
       *         "countryId": 4,
       *         "name":"Danish"
       *       }
       *     ]
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.join(
       *     payload.countries,
       *     payload.languages,
       *     function(item) item.id,
       *     function(item) item.countryId
       * )
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   {
       *     "r": {
       *       "countryId": 1,
       *       "name": "Spanish"
       *     },
       *     "l": {
       *       "id": 1,
       *       "name": "Spain"
       *     }
       *   },
       *   {
       *     "r": {
       *       "countryId": 2,
       *       "name": "French"
       *     },
       *     "l": {
       *       "id": 2,
       *       "name": "France"
       *     }
       *   }
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("join", "arrL", "arryR", "functL", "functR") {
        (vals, ev, fs) =>
          //map the input values
          val valSeq = validate(vals, ev, fs, Array(ArrRead, ArrRead, ApplyerRead, ApplyerRead))
          val arrL = valSeq(0).asInstanceOf[Val.Arr]
          val arrR = valSeq(1).asInstanceOf[Val.Arr]
          val functL = valSeq(2).asInstanceOf[Applyer]
          val functR = valSeq(3).asInstanceOf[Applyer]

          val out = collection.mutable.Buffer.empty[Val.Lazy]

          arrL.value.foreach({
            valueL =>
              val compareL = functL.apply(valueL)
              //append all that match the condition
              out.appendAll(arrR.value.collect({
                case valueR if compareL.equals(functR.apply(valueR)) =>
                  val temp = scala.collection.mutable.Map[String, Val.Obj.Member]()
                  temp += ("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => valueL.force))
                  temp += ("r" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => valueR.force))
                  Val.Lazy(new Val.Obj(temp, _ => (), None))
              }))
          })
          Val.Arr(out.toSeq)
      },

      /** Documentation
       * ### `lastIndexOf(array arr, any value)`
       * Returns the current index of the final matching `value` in `arr`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5,
       *   3
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.lastIndexOf(payload, 3)
       * ------------------------
       * .Result
       * ------------------------
       * 5
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("lastIndexOf", "container", "value") {
        (_, _, container: Val, value: Val) =>
          container match {
            case Val.Str(str) => Val.Lazy(Val.Num(str.lastIndexOf(value.cast[Val.Str].value))).force
            case Val.Arr(array) => Val.Lazy(Val.Num(array.lastIndexWhere(_.force == value))).force
            case Val.Null => Val.Lazy(Val.Num(-1)).force
            case i => throw Error.Delegate("Expected String or Array, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `leftJoin(array arrL, array arrR, function funcL, function funcR)`
       * Joins two arrays together, returns all of the items of `arrL`, with the items that match from `arrR`.
       * 
       * Both functions `funcL` and `funcR` are expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *     "countries": [
       *       {
       *         "id": 1,
       *         "name":"Spain"
       *       },
       *       {
       *         "id": 2,
       *         "name":"France"
       *       },
       *       {
       *         "id": 3,
       *         "name":"Germany"
       *       }
       *     ],
       *     "languages": [
       *       {
       *         "countryId": 1,
       *         "name":"Spanish"
       *       },
       *       {
       *         "countryId": 2,
       *         "name":"French"
       *       },
       *       {
       *         "countryId": 4,
       *         "name":"Danish"
       *       }
       *     ]
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.leftJoin(
       *     payload.countries,
       *     payload.languages,
       *     function(item) item.id,
       *     function(item) item.countryId
       * )
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   {
       *     "r": {
       *       "countryId": 1,
       *       "name": "Spanish"
       *     },
       *     "l": {
       *       "id": 1,
       *       "name": "Spain"
       *     }
       *   },
       *   {
       *     "r": {
       *       "countryId": 2,
       *       "name": "French"
       *     },
       *     "l": {
       *       "id": 2,
       *       "name": "France"
       *     }
       *   },
       *   {
       *     "l": {
       *       "id": 3,
       *       "name": "Germany"
       *     }
       *   }
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("leftJoin", "arrL", "arryR", "functL", "functR") {
        (vals, ev, fs) =>
          //map the input values
          val valSeq = validate(vals, ev, fs, Array(ArrRead, ArrRead, ApplyerRead, ApplyerRead))
          val arrL = valSeq(0).asInstanceOf[Val.Arr]
          val arrR = valSeq(1).asInstanceOf[Val.Arr]
          val functL = valSeq(2).asInstanceOf[Applyer]
          val functR = valSeq(3).asInstanceOf[Applyer]

          //make backup array for leftovers
          var leftoversL = arrL.value

          val out = collection.mutable.Buffer.empty[Val.Lazy]

          arrL.value.foreach({
            valueL =>
              val compareL = functL.apply(valueL)
              //append all that match the condition
              out.appendAll(arrR.value.collect({
                case valueR if compareL.equals(functR.apply(valueR)) =>
                  val temp = scala.collection.mutable.Map[String, Val.Obj.Member]()
                  //remove matching values from the leftOvers arrays
                  leftoversL = leftoversL.filter(item => !item.force.equals(valueL.force))

                  temp += ("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => valueL.force))
                  temp += ("r" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => valueR.force))
                  Val.Lazy(new Val.Obj(temp, _ => (), None))
              }))
          })

          out.appendAll(leftoversL.map(
            leftOver =>
              Val.Lazy(new Val.Obj(
                scala.collection.mutable.Map("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => leftOver.force)),
                _ => (), None))
          ))
          Val.Arr(out.toSeq)
      },

      /** Documentation
       * ### `outerJoin(array arrL, array arrR, function funcL, function funcR)`
       * Joins two arrays together, returns the items of `arrL` with the items that match from `arrR`, the items from `arrL` that don't have matches, and items from `arrR` that don't have matches.
       * 
       * Both functions `funcL` and `funcR` are expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *     "countries": [
       *       {
       *         "id": 1,
       *         "name":"Spain"
       *       },
       *       {
       *         "id": 2,
       *         "name":"France"
       *       },
       *       {
       *         "id": 3,
       *         "name":"Germany"
       *       }
       *     ],
       *     "languages": [
       *       {
       *         "countryId": 1,
       *         "name":"Spanish"
       *       },
       *       {
       *         "countryId": 2,
       *         "name":"French"
       *       },
       *       {
       *         "countryId": 4,
       *         "name":"Danish"
       *       }
       *     ]
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.outerJoin(
       *     payload.countries,
       *     payload.languages,
       *     function(item) item.id,
       *     function(item) item.countryId
       * )
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   {
       *     "r": {
       *       "countryId": 1,
       *       "name": "Spanish"
       *     },
       *     "l": {
       *       "id": 1,
       *       "name": "Spain"
       *     }
       *   },
       *   {
       *     "r": {
       *       "countryId": 2,
       *       "name": "French"
       *     },
       *     "l": {
       *       "id": 2,
       *       "name": "France"
       *     }
       *   },
       *   {
       *     "l": {
       *       "id": 3,
       *       "name": "Germany"
       *     }
       *   },
       *   {
       *     "r": {
       *       "countryId": 4,
       *       "name": "Danish"
       *     }
       *   }
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin0("outerJoin", "arrL", "arryR", "functL", "functR") {
        (vals, ev, fs) =>
          //map the input values
          val valSeq = validate(vals, ev, fs, Array(ArrRead, ArrRead, ApplyerRead, ApplyerRead))
          val arrL = valSeq(0).asInstanceOf[Val.Arr]
          val arrR = valSeq(1).asInstanceOf[Val.Arr]
          val functL = valSeq(2).asInstanceOf[Applyer]
          val functR = valSeq(3).asInstanceOf[Applyer]

          //make backup array for leftovers
          var leftoversL = arrL.value
          var leftoversR = arrR.value

          val out = collection.mutable.Buffer.empty[Val.Lazy]

          arrL.value.foreach({
            valueL =>
              val compareL = functL.apply(valueL)
              //append all that match the condition
              out.appendAll(arrR.value.collect({
                case valueR if compareL.equals(functR.apply(valueR)) =>
                  val temp = scala.collection.mutable.Map[String, Val.Obj.Member]()
                  //remove matching values from the leftOvers arrays
                  leftoversL = leftoversL.filter(item => !item.force.equals(valueL.force))
                  leftoversR = leftoversR.filter(item => !item.force.equals(valueR.force))

                  temp += ("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => valueL.force))
                  temp += ("r" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => valueR.force))
                  Val.Lazy(new Val.Obj(temp, _ => (), None))
              }))
          })

          out.appendAll(leftoversL.map(
            leftOver =>
              Val.Lazy(new Val.Obj(
                scala.collection.mutable.Map("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => leftOver.force)),
                _ => (), None))
          ).appendedAll(leftoversR.map(
            leftOver =>
              Val.Lazy(new Val.Obj(
                scala.collection.mutable.Map("r" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => leftOver.force)),
                _ => (), None)))
          ))
          Val.Arr(out.toSeq)
      },

      /** Documentation
       * ### `occurrences(array arr, function func)`
       * Returns an object where the keys of the object are the result of `func` and the values of the object indicate how many times the key occurs in `arr`.
       * 
       * The function `func` is expected to take the value as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   "a",
       *   "a",
       *   "b",
       *   "b",
       *   "b",
       *   "c"
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.occurrences(payload, function(item) item)
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "a": 2,
       *   "b": 3,
       *   "c": 1
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("occurrences", "arr", "funct") {
        (_, _, array: Val.Arr, funct: Applyer) =>
          new Val.Obj(
            scala.collection.mutable.Map(
              array.value
                .groupBy(item => convertToString(funct.apply(item)))
                .map(item => item._1 -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Lazy(Val.Num(item._2.size)).force)).toSeq:
              _*),_ => (), None)
      },

      /** Documentation
       * ### `partition(array arr, function func)`
       * Splits `arr` into two arrays of successes and failures from the results of `func`.
       * 
       * The function `func` is expected to take the value as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.partition(payload, function(item) item > 3)
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "success": [
       *     4,
       *     5
       *   ],
       *   "failure": [
       *     1,
       *     2,
       *     3
       *   ]
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("partition", "arr", "funct") {
        (_, _, array: Val.Arr, funct: Applyer) =>
          val out = scala.collection.mutable.Map[String, Val.Obj.Member]()
          val part = array.value.partition(funct.apply(_) == Val.True)
          out += ("success" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Arr(part._1)))
          out += ("failure" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Arr(part._2)))
          new Val.Obj(out, _ => (), None)
      },

      /** Documentation
       * ### `slice(array arr, number start, number end)`
       * Returns a subset of `arr` between the indexes of `start` and `end`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.slice(payload, 2, 4)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   3,
       *   4
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("slice", "arr", "start", "end") {
        (_, _, array: Val.Arr, start: Int, end: Int) =>
          //version commented below is slightly slower
          //Val.Arr(array.value.splitAt(start)._2.splitAt(end-1)._1)
          Val.Arr(
            array.value.zipWithIndex.filter({
              case (_, index) => (index >= start) && (index < end)
            }).map(_._1)
          )
      },

      /** Documentation
       * ### `some(array arr, function func)`
       * Returns true if at least one item in `arr` passes the condition in `func`.
       * 
       * The function `func` is expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.some(payload, function(item) item > 2)
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("some", "value", "funct") {
        (_, _, value: Val, funct: Applyer) =>
          value match {
            case Val.Arr(array) =>
              Val.bool(array.exists(item => funct.apply(item) == Val.True))
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `splitAt(array arr, number index)`
       * Splits `arr` into a left and right array based on the `index`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.splitAt(payload, 3)
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "r": [
       *     4,
       *     5
       *   ],
       *   "l": [
       *     1,
       *     2,
       *     3
       *   ]
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("splitAt", "array", "index") {
        (_, _, array: Val.Arr, index: Int) =>
          val split = array.value.splitAt(index)
          val out = scala.collection.mutable.Map[String, Val.Obj.Member]()

          out += ("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Arr(split._1)))
          out += ("r" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Arr(split._2)))
          new Val.Obj(out, _ => (), None)
      },

      /** Documentation
       * ### `splitWhere(array arr, function func)`
       * Splits `arr` into a left and right array based on the first index that returns true for `func`.
       * 
       * The function `func` is expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.splitWhere(payload, function(item) item > 3)
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "r": [
       *     4,
       *     5
       *   ],
       *   "l": [
       *     1,
       *     2,
       *     3
       *   ]
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("splitWhere", "arr", "funct") {
        (_, _, arr: Val.Arr, funct: Applyer) =>
          val split = arr.value.splitAt(arr.value.indexWhere(funct.apply(_) == Val.True))
          val out = scala.collection.mutable.Map[String, Val.Obj.Member]()

          out += ("l" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Arr(split._1)))
          out += ("r" -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => Val.Arr(split._2)))
          new Val.Obj(out, _ => (), None)
      },

      /** Documentation
       * ### `sumBy(array arr, function func)`
       * Calculates the sum of `arr` by the function provided value.
       * 
       * The function `func` is expected to take the item as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.sumBy(payload, function(item) item)
       * ------------------------
       * .Result
       * ------------------------
       * 15
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("sumBy", "array", "funct") {
        (_, _, array: Val.Arr, funct: Applyer) =>
          array.value.foldLeft(0.0)((sum, num) => sum + funct.apply(num).asInstanceOf[Val.Num].value)
      },

      /** Documentation
       * ### `take(array arr, number index)`
       * Returns all values from `arr` up to the `index`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.take(payload, 3)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   1,
       *   2,
       *   3
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("take", "array", "index") {
        (_, _, array: Val.Arr, index: Int) =>
          Val.Arr(array.value.splitAt(index)._1)
      },

      /** Documentation
       * ### `takeWhile(array arr, function func)`
       * Takes all items from the array while `func` is true. Stops at the first false value.
       * 
       * The function `func` is expected to take the value as the first parameter (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * [
       *   1,
       *   2,
       *   3,
       *   4,
       *   5
       * ]
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.arrays.takeWhile(payload, function(item) item < 3)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   1,
       *   2
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("takeWhile", "array", "funct") {
        (_, _, array: Val.Arr, funct: Applyer) =>
          Val.Arr(array.value.takeWhile(item => funct.apply(item) == Val.True))
      }
    ),

    /** Binaries */
    /** TODO: Possibly remove? */
    "binaries" -> moduleFrom(

      /** Documentation
       * ### `fromBase64(string value)`
       * Converts `value` from base64.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.binaries.fromBase64("SGVsbG8gV29ybGQ=")
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("fromBase64", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) => Val.Lazy(Val.Str(new String(Base64.getDecoder.decode(x.toString)))).force
            case Val.Str(x) => Val.Lazy(Val.Str(new String(Base64.getDecoder.decode(x)))).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `fromHex(string value)`
       * Converts `value` from hexadecimal.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.binaries.fromHex("48656C6C6F20576F726C64")
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("fromHex", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Str(x) => Val.Lazy(Val.Str(
              x.toSeq.sliding(2, 2).map(byte => Integer.parseInt(byte.unwrap, 16).toChar).mkString
            )).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `readLinesWith(string value, string encoding)`
       * Reads `value` with the specified encoding `encoding`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.binaries.readLinesWith("Hello World", "UTF-8")
       * ------------------------
       * .Result
       * ------------------------
       * ["Hello World"]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("readLinesWith", "value", "encoding") {
        (_, _, value: String, enc: String) =>
          Val.Arr(
            new String(value.getBytes(), enc).split('\n').toIndexedSeq.collect({
              case str => Val.Lazy(Val.Str(str))
            })
          )
      },

      /** Documentation
       * ### `toBase64(any value)`
       * Converts `value` to base 64.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.binaries.toBase64("Hello World")
       * ------------------------
       * .Result
       * ------------------------
       * "SGVsbG8gV29ybGQ="
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toBase64", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) =>
              if (x % 1 == 0) Val.Lazy(Val.Str(new String(Base64.getEncoder.encode(x.toInt.toString.getBytes())))).force
              else Val.Lazy(Val.Str(new String(Base64.getEncoder.encode(x.toString.getBytes())))).force
            case Val.Str(x) => Val.Lazy(Val.Str(new String(Base64.getEncoder.encode(x.getBytes())))).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `toHex(any value)`
       * Converts `value` to hexadecimal.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.binaries.toHex("Hello World")
       * ------------------------
       * .Result
       * ------------------------
       * "48656C6C6F20576F726C64"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toHex", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) => Val.Lazy(Val.Str(Integer.toString(x.toInt, 16).toUpperCase())).force
            case Val.Str(x) => Val.Lazy(Val.Str(x.getBytes().map(_.toHexString).mkString.toUpperCase())).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `writeLinesWith(string value, string encoding)`
       * Writes `value` with the specified encoding `encoding`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.binaries.writeLinesWith(["Hello World"], "UTF-8")
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World\n"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("writeLinesWith", "value", "encoding") {
        (_, _, value: Val.Arr, enc: String) =>
          val str = value.value.map(item => item.force.asInstanceOf[Val.Str].value).mkString("\n") + "\n"
          Val.Lazy(Val.Str(new String(str.getBytes, enc))).force
      }
    ),

    /** Numbers */
    "numbers" -> moduleFrom(

      /** Documentation
       * ### `fromBinary(number value)`
       * Converts `value` from binary to decimal.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.numbers.fromBinary(1100100)
       * ------------------------
       * .Result
       * ------------------------
       * 100
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("fromBinary", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) =>
              if ("[^2-9]".r.matches(x.toString)) {
                throw Error.Delegate("Expected Binary, got: Number")
              }
              else Val.Lazy(Val.Num(BigInt.apply(x.toLong.toString,2).bigInteger.longValue())).force
            case Val.Str(x) => Val.Lazy(Val.Num(BigInt.apply(x, 2).bigInteger.longValue())).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected Binary, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `fromHex(number value)`
       * Converts `value` from hex to decimal.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.numbers.fromHex(64)
       * ------------------------
       * .Result
       * ------------------------
       * 100
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("fromHex", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) =>
              if ("[^0-9a-f]".r.matches(x.toString.toLowerCase())) {
                throw Error.Delegate("Expected Binary, got: Number")
              }
              else Val.Lazy(Val.Num(BigInt.apply(x.toLong.toString, 16).bigInteger.longValue())).force;
            case Val.Str(x) => Val.Lazy(Val.Num(BigInt.apply(x, 16).bigInteger.longValue())).force;
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected Binary, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `fromRadixNumber(number value, number baseAsBinary)`
       * Converts `value` to a decimal with the base `baseAsBinary`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.numbers.fromRadixNumber(1101000, 2)
       * ------------------------
       * .Result
       * ------------------------
       * 104
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("fromRadixNumber", "value", "num") {
        (_, _, value: Val, num: Int) =>
          value match {
            case Val.Num(x) => Val.Lazy(Val.Num(BigInt.apply(x.toLong.toString, num).bigInteger.longValue() )).force
            case Val.Str(x) => Val.Lazy(Val.Num(BigInt.apply(x, num).bigInteger.longValue() )).force
            case i => throw Error.Delegate("Expected Binary, got: " + i.prettyName)
            //null not supported in DW function
          }
      },

      /** Documentation
       * ### `toBinary(number value)`
       * Converts `value` from decimal to binary.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.numbers.toBinary(100)
       * ------------------------
       * .Result
       * ------------------------
       * "1100100"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toBinary", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) =>
              if (x < 0) Val.Lazy(Val.Str("-" + x.toLong.abs.toBinaryString)).force
              else Val.Lazy(Val.Str(x.toLong.toBinaryString)).force
            case Val.Str(x) =>
              if (x.startsWith("-")) Val.Lazy(Val.Str(x.toLong.abs.toBinaryString)).force
              else Val.Lazy(Val.Str(x.toLong.toBinaryString)).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected Binary, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `toHex(number value)`
       * Converts `value` from decimal to hex.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.numbers.toHex(100)
       * ------------------------
       * .Result
       * ------------------------
       * "64"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toHex", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Num(x) =>
              if (x < 0) Val.Lazy(Val.Str("-" + x.toLong.abs.toHexString)).force
              else Val.Lazy(Val.Str(x.toLong.toHexString)).force
            case Val.Str(x) =>
              if (x.startsWith("-")) Val.Lazy(Val.Str(x.toLong.abs.toHexString)).force
              else Val.Lazy(Val.Str(x.toLong.toHexString)).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected Binary, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `toRadixNumber(number value, number baseAsDecimal)`
       * Converts `value` to a number with the base `baseAsDecimal`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.numbers.toRadixNumber(104, 2)
       * ------------------------
       * .Result
       * ------------------------
       * "1101000"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("toRadixNumber", "value", "num") {
        (_, _, value: Val, num: Int) =>
          value match {
            case Val.Num(x) =>
              if (x < 0) Val.Lazy(Val.Str("-" + BigInt.apply(x.toLong).toString(num))).force
              else Val.Lazy(Val.Str(BigInt.apply(x.toLong).toString(num))).force
            // Val.Lazy(Val.Str(Integer.toString(x.toInt, num))).force
            case Val.Str(x) =>
              if (x.startsWith("-")) Val.Lazy(Val.Str("-" + BigInt.apply(x.toLong).toString(num))).force
              else Val.Lazy(Val.Str(BigInt.apply(x.toLong).toString(num))).force
            case i => throw Error.Delegate("Expected Binary, got: " + i.prettyName)
          }
      }
    ),

    /** Objects */
    "objects" -> moduleFrom(

      /** Documentation
       * ### `divideBy(object obj, number num)`
       * Creates an array of objects where each nested object has the specified number `num` of key-value pairs.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *   "a":1,
       *   "b":2,
       *   "c":3
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.objects.divideBy(payload, 2)
       * ------------------------
       * .Result
       * ------------------------
       * [
       *   {
       *     "a": 1,
       *     "b": 2
       *   },
       *   {
       *     "c": 3
       *   }
       * ]
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("divideBy", "obj", "num") {
        (ev, fs, obj: Val.Obj, num: Int) =>
          val out = collection.mutable.Buffer.empty[Val.Lazy]

          obj.getVisibleKeys().sliding(num, num).foreach({
            map =>
              val currentObject = collection.mutable.Map[String, Val.Obj.Member]()
              map.foreachEntry((key, _) => currentObject += (key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))))
              out.append(Val.Lazy(new Val.Obj(currentObject, _ => (), None)))
          })
          Val.Arr(out.toSeq)
      },

      /** Documentation
       * ### `everyEntry(object obj, function func)`
       * Returns a boolean depending on if all key-value pairs of `obj` pass the `func`.
       * 
       * The function `func` is expected to take the value as the first parameter (required) and the key as the second parameter (optional).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *   "a":1,
       *   "b":2,
       *   "c":1
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.objects.everyEntry(payload,function(value,key) value < 2)
       * ------------------------
       * .Result
       * ------------------------
       * false
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("everyEntry", "value", "funct") {
        (ev, fs, value: Val, funct: Applyer) =>
          value match {
            case obj: Val.Obj =>
              val args = funct.f.params.allIndices.size
              if (args == 2)
                Val.bool(obj.getVisibleKeys().toSeq.forall(key => funct.apply(Val.Lazy(obj.value(key._1, -1)(fs, ev)), Val.Lazy(Val.Str(key._1))) == Val.True))
              else if (args == 1)
                Val.bool(obj.getVisibleKeys().toSeq.forall(key => funct.apply(Val.Lazy(obj.value(key._1, -1)(fs, ev))) == Val.True))
              else {
                throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
              }
            case Val.Null => Val.Lazy(Val.True).force
            case i => throw Error.Delegate("Expected Array, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `mergeWith(object obj1, object obj2)`
       * Combines `obj1` and `obj2`.
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *   "obj1": {
       *     "a":1
       *   },
       *   "obj2":{
       *     "b":2
       *   }
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.objects.mergeWith(payload.obj1,payload.obj2)
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "a": 1,
       *   "b": 2
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("mergeWith", "valueOne", "valueTwo") {
        (ev, fs, valueOne: Val, valueTwo: Val) =>
          val out = scala.collection.mutable.Map[String, Val.Obj.Member]()
          valueOne match {
            case obj: Val.Obj =>
              valueTwo match {
                case obj2: Val.Obj =>
                  obj2.foreachVisibleKey(
                    (key, _) => out += (key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj2.value(key, -1)(fs, ev)))
                  )
                  val keySet = obj2.getVisibleKeys().keySet
                  obj.foreachVisibleKey(
                    (key, _) => if (!keySet.contains(key)) out += (key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev)))
                  )
                  new Val.Obj(out, _ => (), None)
                case Val.Null => valueOne
                case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
              }
            case Val.Null =>
              valueTwo match {
                case _: Val.Obj => valueTwo
                case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
              }
            case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `someEntry(object obj, function func)`
       * Returns a boolean depending on if at least one key-value pair passes the function `func`.
       * 
       * The function `func` is expected to take the property value as the first parameter (required) and  the property key as the second (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *   "a":1,
       *   "b":2,
       *   "c":1
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.objects.someEntry(payload, function(value, key) value < 2)
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("someEntry", "value", "funct") {
        (ev, fs, value: Val, funct: Applyer) =>
          value match {
            case obj: Val.Obj =>
              Val.bool(obj.getVisibleKeys().exists(
                item => funct.apply(Val.Lazy(obj.value(item._1, -1)(fs, ev)), Val.Lazy(Val.Str(item._1))) == Val.True
              ))
            case Val.Null => Val.Lazy(Val.False).force
            case i => throw Error.Delegate("Expected Object, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `takeWhile(object obj, function func)`
       * Takes all key value pairs that result in true from the function. Stops on the first value that fails.
       * 
       * The function `func` is expected to take the property value as the first parameter (required) and  the property key as the second (required).
       * 
       * *Example*
       * 
       * .Payload
       * ----------
       * {
       *   "a":1,
       *   "b":2,
       *   "c":1
       * }
       * ----------
       * .DataSonnet map:
       * ------------------------
       * ds.objects.takeWhile(payload, function(value,key) value < 2)
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "a": 1
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("takeWhile", "obj", "funct") {
        (ev, fs, obj: Val.Obj, funct: Applyer) =>
          val out = scala.collection.mutable.Map[String, Val.Obj.Member]()
          obj.getVisibleKeys().takeWhile(
            item => funct.apply(Val.Lazy(obj.value(item._1, -1)(fs, ev)), Val.Lazy(Val.Str(item._1))) == Val.True
          ).foreachEntry((key, _) => out += (key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))))

          new Val.Obj(out, _ => (), None)
      },
    ),

    /** Strings */
    "strings" -> moduleFrom(

      /** Documentation
       * ### `appendIfMissing(string str, string value)`
       * Appends `str` with `value` if `str` does not already end with `value`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * {
       *     existing: ds.strings.appendIfMissing("Hello World","World"),
       *     missing: ds.strings.appendIfMissing("Hello ","World")
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "existing": "Hello World",
       *   "missing": "Hello World"
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("appendIfMissing", "str1", "str2") {
        (_, _, value: Val, append: String) =>
          value match {
            case Val.Str(str) =>
              var ret = str
              if (!str.endsWith(append)) {
                ret = str + append
              }
              Val.Lazy(Val.Str(ret)).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `camelize(string str)`
       * Converts words in `str` using camel case, which removes all spaces and converts the first letter of each word except the first word to upper case.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.camelize("Hello_world")
       * ------------------------
       * .Result
       * ------------------------
       * "helloWorld"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("camelize", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              //regex fo _CHAR
              val regex = "(_+)([0-9A-Za-z])".r("underscore", "letter")

              //Start string at first non underscore, lower case it
              var temp = value.substring("[^_]".r.findFirstMatchIn(value).map(_.start).toList.head)
              temp = temp.replaceFirst(temp.charAt(0).toString, temp.charAt(0).toLower.toString)

              //replace and uppercase
              temp = regex.replaceAllIn(temp, m => s"${(m group "letter").toUpperCase()}")
              Val.Lazy(Val.Str(temp)).force;

            case Val.Null =>
              Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `capitalize(string str)`
       * Converts words in `str` using capitalized case, which changes the first letter of each word to uppercase, with the rest of the letters in lowercase.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.capitalize("hello world")
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("capitalize", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              //regex fo _CHAR
              val regex = "([_\\s-]+)([0-9A-Za-z])([A-Z]+|)".r("one", "two", "three")
              val middleRegex = "([a-z])([A-Z])".r("end", "start")

              //Start string at first non underscore, lower case it
              var temp = value.substring("[0-9A-Za-z]".r.findFirstMatchIn(value).map(_.start).toList.head)
              temp = temp.replaceFirst(temp.charAt(0).toString, temp.charAt(0).toUpper.toString)

              //replace and uppercase
              temp = regex.replaceAllIn(temp, m => s" ${(m group "two").toUpperCase() + (m group "three").toLowerCase()}")
              temp = middleRegex.replaceAllIn(temp, m => s"${m group "end"} ${(m group "start").toUpperCase()}")

              Val.Lazy(Val.Str(temp)).force;

            case Val.Null =>
              Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `charCode(string char)`
       * Converts `char` to its char code.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.charCode("*")
       * ------------------------
       * .Result
       * ------------------------
       * 42
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("charCode", "str") {
        (_, _, str: String) =>
          str.codePointAt(0)
      },

      /** Documentation
       * ### `charCodeAt(string str, number index)`
       * Returns the char code at `index` in `str`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.charCodeAt("*", 0)
       * ------------------------
       * .Result
       * ------------------------
       * 42
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("charCodeAt", "str", "num") {
        (_, _, str: String, num: Int) =>
          str.codePointAt(num)
      },

      /** Documentation
       * ### `dasherize(string str)`
       * Converts words in `str` using kebab-case, which converts all letters in `str` to lowercase and all spaces into dashes (-).
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.dasherize("Hello WorldX")
       * ------------------------
       * .Result
       * ------------------------
       * "hello-world-x"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("dasherize", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              //regex fo _CHAR
              val regex = "([_\\s-]+)([0-9A-Za-z])([A-Z]+|)".r("one", "two", "three")
              val middleRegex = "([a-z])([A-Z])".r("end", "start")

              //Start string at first non underscore, lower case it
              var temp = value

              //replace and uppercase
              temp = regex.replaceAllIn(temp, m => s"-${(m group "two") + (m group "three").toLowerCase()}")
              temp = middleRegex.replaceAllIn(temp, m => s"${m group "end"}-${m group "start"}")

              temp = temp.toLowerCase()

              Val.Lazy(Val.Str(temp)).force;

            case Val.Null =>
              Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `fromCharCode(number charCode)`
       * Converts `charCode` to its string value.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.fromCharCode(42)
       * ------------------------
       * .Result
       * ------------------------
       * "*"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("fromCharCode", "num") {
        (_, _, num: Int) =>
          String.valueOf(num.asInstanceOf[Char])
      },

      /** Documentation
       * ### `isAlpha(string str)`
       * Returns a boolean which determines if the provided string only contains alpha characters.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.isAlpha("abcde")
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isAlpha", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              if ("^[A-Za-z]+$".r.matches(value)) {
                true
              }
              else {
                false
              }
            case Val.Null => false
            case Val.Num(_) => false
            case Val.True | Val.False => true
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `isAlphanumeric(string str)`
       * Returns a boolean which determines if `str` only contains alpha numeric values.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.isAlphanumeric("a1b2cd3e4")
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isAlphanumeric", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              if ("^[A-Za-z0-9]+$".r.matches(value)) {
                true
              }
              else {
                false
              }
            case Val.Null => false
            case Val.Num(_) => true
            case Val.True | Val.False => true
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `isLowerCase(string str)`
       * Returns a boolean which determines if `str` is all lowercase.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.isLowerCase("hello")
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isLowerCase", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              if ("^[a-z]+$".r.matches(value)) {
                true
              }
              else {
                false
              }
            case Val.Null => false
            case Val.Num(_) => false
            case Val.True | Val.False => true
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `isNumeric(string str)`
       * Returns a boolean which determines if `str` contains only numbers.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.isNumeric("34634")
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isNumeric", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              if ("^[0-9]+$".r.matches(value)) {
                true
              }
              else {
                false
              }
            case Val.Num(_) => true
            case Val.True | Val.False | Val.Null => false
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `isUpperCase(string str)`
       * Returns a boolean which determines if `str` is all uppercase.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.isUpperCase("HELLO")
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isUpperCase", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              if ("^[A-Z]+$".r.matches(value)) {
                true
              }
              else {
                false
              }
            case Val.Num(_) => false
            case Val.True | Val.False | Val.Null => false
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `isWhitespace(string str)`
       * Returns a boolean which determines if `str` only contains spaces.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.isWhitespace("      ")
       * ------------------------
       * .Result
       * ------------------------
       * true
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("isWhitespace", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) => value.trim().isEmpty
            case Val.Num(_) => false
            case Val.True | Val.False | Val.Null => false
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `leftPad(string str, number offset)`
       * Pads the left side of `str` with spaces if the string is below the `offset` length.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.leftPad("Hello",10)
       * ------------------------
       * .Result
       * ------------------------
       * "     Hello"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("leftPad", "str", "offset") {
        (_, _, str: Val, offset: Int) =>
          str match {
            case Val.Str(value) =>
              Val.Lazy(Val.Str(("%" + offset + "s").format(value))).force
            case Val.True =>
              Val.Lazy(Val.Str(("%" + offset + "s").format("true"))).force
            case Val.False =>
              Val.Lazy(Val.Str(("%" + offset + "s").format("false"))).force
            case Val.Num(x) =>
              //TODO change to use sjsonnet's Format and DecimalFormat
              Val.Lazy(Val.Str(("%" + offset + "s").format(new DecimalFormat("0.#").format(x)))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `ordinalize(number num)`
       * Converts `num` to its ordinal string format, e.g. 1st, 2nd, 3rd, etc.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.ordinalize(1)
       * ------------------------
       * .Result
       * ------------------------
       * "1st"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("ordinalize", "num") {
        (_, _, num: Val) =>
          (num match { //convert number value to string
            case Val.Null => "null"
            case Val.Str(value) =>
              if ("^[0-9]+$".r.matches(value)) {
                value
              }
              else {
                "X"
              }
            case Val.Num(value) => value.toInt.toString
            case _ => throw Error.Delegate("Expected Number, got: " + num.prettyName)
          }) match { //convert string number to ordinalized string number
            case "null" => Val.Lazy(Val.Null).force
            case "X" => throw Error.Delegate("Expected Number, got: " + num.prettyName)
            case str =>
              if (str.endsWith("11") || str.endsWith("12") || str.endsWith("13")) {
                Val.Lazy(Val.Str(str + "th")).force
              }
              else {
                if (str.endsWith("1")) {
                  Val.Lazy(Val.Str(str + "st")).force
                }
                else if (str.endsWith("2")) {
                  Val.Lazy(Val.Str(str + "nd")).force
                }
                else if (str.endsWith("3")) {
                  Val.Lazy(Val.Str(str + "rd")).force
                }
                else {
                  Val.Lazy(Val.Str(str + "th")).force
                }
              }
          }
      },

      /** Documentation
       * ### `pluralize(string singularWord)`
       * Converts `singularWord` to its plural counterpart. May not work with all edge cases.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.pluralize("car")
       * ------------------------
       * .Result
       * ------------------------
       * "cars"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("pluralize", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Str(str) =>
              val comparator = str.toLowerCase()
              val specialSList = List("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
              if (specialSList.contains(comparator)) {
                Val.Lazy(Val.Str(str + "s")).force
              }
              else if (comparator.isEmpty) Val.Lazy(Val.Str("")).force
              else {
                if (comparator.endsWith("y")) {
                  Val.Lazy(Val.Str(str.substring(0, str.length - 1) + "ies")).force
                }
                else if (comparator.endsWith("x")) {
                  Val.Lazy(Val.Str(str + "es")).force
                }
                else {
                  Val.Lazy(Val.Str(str + "s")).force
                }
              }
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected Number, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `prependIfMissing(string str, string value)`
       * Prepends `str` with `value` if `str` does not already begin with `value`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * {
       *     existing: ds.strings.prependIfMissing("Hello World","Hello"),
       *     missing: ds.strings.prependIfMissing(" World","Hello")
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "existing": "Hello World",
       *   "missing": "Hello World"
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("prependIfMissing", "str1", "str2") {
        (_, _, value: Val, append: String) =>
          value match {
            case Val.Str(str) =>
              var ret = str
              if (!str.startsWith(append)) {
                ret = append + str
              }
              Val.Lazy(Val.Str(ret)).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `repeat(string str, number times)`
       * Repeats `str` the given amount of `times`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.repeat("Hello ", 2)
       * ------------------------
       * .Result
       * ------------------------
       * "Hello Hello "
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("repeat", "str", "num") {
        (_, _, str: String, num: Int) =>
          var ret = ""
          for (_ <- 0 until num) {
            ret += str
          }
          Val.Lazy(Val.Str(ret)).force
      },

      /** Documentation
       * ### `rightPad(string str, number offset)`
       * Pads the right side `str` with spaces if the string is below the `offset` length.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.rightPad("Hello",10)
       * ------------------------
       * .Result
       * ------------------------
       * "Hello     "
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("rightPad", "str", "offset") {
        (_, _, value: Val, offset: Int) =>
          value match {
            case Val.Str(str) =>
              Val.Lazy(Val.Str(str.padTo(offset, ' '))).force
            case Val.Num(x) =>
              //TODO change to use sjsonnet's Format and DecimalFormat
              Val.Lazy(Val.Str(new DecimalFormat("0.#").format(x).padTo(offset, ' '))).force
            case Val.True =>
              Val.Lazy(Val.Str("true".padTo(offset, ' '))).force
            case Val.False =>
              Val.Lazy(Val.Str("false".padTo(offset, ' '))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `singularize(string pluralWord)`
       * Converts `pluralWord` to a singular word. May not work with all edge cases.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.singularize("cars")
       * ------------------------
       * .Result
       * ------------------------
       * "car"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("singularize", "value") {
        (_, _, value: Val) =>
          value match {
            case Val.Str(s) =>
              if (s.endsWith("ies"))
                Val.Lazy(Val.Str(s.substring(0, s.length - 3) + "y")).force
              else if (s.endsWith("es"))
                Val.Lazy(Val.Str(s.substring(0, s.length - 2))).force
              else
                Val.Lazy(Val.Str(s.substring(0, s.length - 1))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `substringAfter(string str, string separator)`
       * Gets the substring of `str` after the first occurrence of the `separator`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.substringAfter("!XHelloXWorldXAfter", "X")
       * ------------------------
       * .Result
       * ------------------------
       * "HelloXWorldXAfter"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("substringAfter", "value", "sep") {
        (_, _, value: Val, sep: String) =>
          value match {
            case Val.Str(s) =>
              Val.Lazy(Val.Str(s.substring(
                s.indexOf(sep) match {
                  case -1 => s.length
                  case i => if (sep.equals("")) i else i + 1
                }
              ))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `substringAfterLast(string str, string separator)`
       * Gets the substring in `str` after the final occurrence of the `separator`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.substringAfterLast("!XHelloXWorldXAfter", "X")
       * ------------------------
       * .Result
       * ------------------------
       * "After"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("substringAfterLast", "value", "sep") {
        (_, _, value: Val, sep: String) =>
          value match {
            case Val.Str(s) =>
              val split = s.split(sep)
              if (sep.equals("")) Val.Lazy(Val.Str("")).force
              else if (split.length == 1) Val.Lazy(Val.Str("")).force
              else Val.Lazy(Val.Str(split(split.length - 1))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `substringBefore(string str, string separator)`
       * Gets the substring in `str` before the first occurrence of the `separator`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.substringBefore("!XHelloXWorldXAfter", "X")
       * ------------------------
       * .Result
       * ------------------------
       * "!"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("substringBefore", "value", "sep") {
        (_, _, value: Val, sep: String) =>
          value match {
            case Val.Str(s) =>
              Val.Lazy(Val.Str(s.substring(0,
                s.indexOf(sep) match {
                  case -1 => 0
                  case i => i
                }
              ))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `substringBeforeLast(string str, string separator)`
       * Gets the substring in `str` before the final occurrence of the `separator`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.substringBeforeLast("!XHelloXWorldXAfter", "X")
       * ------------------------
       * .Result
       * ------------------------
       * "!XHelloXWorld"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("substringBeforeLast", "value", "sep") {
        (_, _, value: Val, sep: String) =>
          value match {
            case Val.Str(s) =>
              Val.Lazy(Val.Str(s.substring(0,
                s.lastIndexOf(sep) match {
                  case -1 => 0
                  case i => i
                }
              ))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `underscore(string str)`
       * Converts words in `str` using snake case, which converts all letters in `str` to lowercase and all spaces into underscores (_).
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.underscore("Hello WorldX")
       * ------------------------
       * .Result
       * ------------------------
       * "hello_world_x"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("underscore", "str") {
        (_, _, str: Val) =>
          str match {
            case Val.Str(value) =>
              //regex fo _CHAR
              val regex = "([_\\s-]+)([0-9A-Za-z])([A-Z]+|)".r("one", "two", "three")
              val middleRegex = "([a-z])([A-Z])".r("end", "start")

              //Start string at first non underscore, lower case it
              var temp = value.substring("[0-9A-Za-z]".r.findFirstMatchIn(value).map(_.start).toList.head)
              temp = temp.replaceFirst(temp.charAt(0).toString, temp.charAt(0).toLower.toString)

              //replace and uppercase
              temp = regex.replaceAllIn(temp, m => s"_${(m group "two") + (m group "three")}")
              temp = middleRegex.replaceAllIn(temp, m => s"${m group "end"}_${m group "start"}")

              Val.Lazy(Val.Str(temp.toLowerCase)).force;

            case Val.Null =>
              Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `unwrap(string str, string wrapper)`
       * Returns the `str` without the `wrapper` text.
       * Returns the `str` without the `wrapper` text.
       * The `wrapper` text is the prepended and/or appended values to the `str`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * {
       *     exists: ds.strings.unwrap("Hello World Hello","Hello"),
       *     partial: ds.strings.unwrap("Hello World ","Hello"),
       *     missing: ds.strings.unwrap(" World ","Hello")
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "exists": " World ",
       *   "partial": " World Hello",
       *   "missing": " World "
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("unwrap", "value", "wrapper") {
        (_, _, value: Val, wrapper: String) =>
          value match {
            case Val.Str(str) =>
              val starts = str.startsWith(wrapper)
              val ends = str.endsWith(wrapper)
              if (starts && ends) Val.Lazy(Val.Str(str.substring(0 + wrapper.length, str.length - wrapper.length))).force
              else if (starts) Val.Lazy(Val.Str(str.substring(0 + wrapper.length, str.length) + wrapper)).force
              else if (ends) Val.Lazy(Val.Str(wrapper + str.substring(0, str.length - wrapper.length))).force
              else Val.Lazy(Val.Str(str)).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `withMaxSize(string str, number size)`
       * Limits the `size` of `str`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.withMaxSize("Hello World", 5)
       * ------------------------
       * .Result
       * ------------------------
       * "Hello"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("withMaxSize", "value", "num") {
        (_, _, value: Val, num: Int) =>
          value match {
            case Val.Str(str) =>
              if (str.length <= num) Val.Lazy(Val.Str(str)).force
              else Val.Lazy(Val.Str(str.substring(0, num))).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `wrapIfMissing(string str, string wrapper)`
       * Prepends and appends the `wrapper` to `str` if `str` is not already wrapped. Will update only missing side if `wrapper` already exists at the beginning or end.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * {
       *     exists: ds.strings.wrapIfMissing("Hello World Hello","Hello"),
       *     partialBeg: ds.strings.wrapIfMissing("Hello World ","Hello"),
       *     partialEnd: ds.strings.wrapIfMissing(" World Hello","Hello"),
       *     missing: ds.strings.wrapIfMissing(" World ","Hello")
       * }
       * ------------------------
       * .Result
       * ------------------------
       * {
       *   "exists": "Hello World Hello",
       *   "partialBeg": "Hello World Hello",
       *   "partialEnd": "Hello World Hello",
       *   "missing": "Hello World Hello"
       * }
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("wrapIfMissing", "value", "wrapper") {
        (_, _, value: Val, wrapper: String) =>
          value match {
            case Val.Str(str) =>
              val ret = new StringBuilder(str)
              if (!str.startsWith(wrapper)) ret.insert(0, wrapper)
              if (!str.endsWith(wrapper)) ret.append(wrapper)
              Val.Lazy(Val.Str(ret.toString())).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      },

      /** Documentation
       * ### `wrapWith(string str, string wrapper)`
       * Prepends and appends the `wrapper` to `str`.
       * 
       * *Example*
       * 
       * .DataSonnet map:
       * ------------------------
       * ds.strings.wrapWith(" World ","Hello")
       * ------------------------
       * .Result
       * ------------------------
       * "Hello World Hello"
       * ------------------------
       *
       * *Version*:
       *
       * Created: 1.0.0
       */
      builtin("wrapWith", "value", "wrapper") {
        (_, _, value: Val, wrapper: String) =>
          value match {
            case Val.Str(str) => Val.Lazy(Val.Str(wrapper + str + wrapper)).force
            case Val.Null => Val.Lazy(Val.Null).force
            case i => throw Error.Delegate("Expected String, got: " + i.prettyName)
          }
      }
    )
  ).asJava

  def read(dataFormats: DataFormatService, data: String, mimeType: String, params: Val.Obj, ev: EvalScope): Val = {
    val Array(supert, subt) = mimeType.split("/", 2)
    val paramsAsJava = ujsonUtils.javaObjectFrom(ujson.read(Materializer.apply(params)(ev)).obj).asInstanceOf[java.util.Map[String, String]]
    val doc = new DefaultDocument(data, new MediaType(supert, subt, paramsAsJava))

    val plugin = dataFormats.thatCanRead(doc)
      .orElseThrow(() => Error.Delegate("No suitable plugin found for mime type: " + mimeType))

    Materializer.reverse(plugin.read(doc))
  }

  def write(dataFormats: DataFormatService, json: Val, mimeType: String, params: Val.Obj, ev: EvalScope): String = {
    val Array(supert, subt) = mimeType.split("/", 2)
    val paramsAsJava = ujsonUtils.javaObjectFrom(ujson.read(Materializer.apply(params)(ev)).obj).asInstanceOf[java.util.Map[String, String]]
    val mediaType = new MediaType(supert, subt, paramsAsJava)

    val plugin = dataFormats.thatCanWrite(mediaType, classOf[String])
      .orElseThrow(() => Error.Delegate("No suitable plugin found for mime type: " + mimeType))

    plugin.write(Materializer.apply(json)(ev), mediaType, classOf[String]).getContent
  }

  private def distinctBy(array: Seq[Val.Lazy], funct: Applyer): Val = {
    val args = funct.f.params.allIndices.size

    Val.Arr(
      if (args == 2) { // 2 args
        array.zipWithIndex.distinctBy(item => funct.apply(item._1, Val.Lazy(Val.Num(item._2)))).map(_._1)
      }
      else if (args == 1) { // 1 arg
        array.distinctBy(item => funct.apply(item))
      }
      else {
        throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
      }
    )
  }

  private def distinctBy(obj: Val.Obj, funct: Applyer, ev: EvalScope, fs: FileScope): Val = {
    val args = funct.f.params.allIndices.size

    new Val.Obj(
      if (args == 2) { // 2 args
        scala.collection.mutable.Map(
          obj.getVisibleKeys().keySet.toSeq.distinctBy(outKey =>
            funct.apply(
              Val.Lazy(obj.value(outKey, -1)(fs, ev)),
              Val.Lazy(Val.Str(outKey))
            )).collect(key => key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))
          ): _*)
      }
      else if (args == 1) { //1 arg
        scala.collection.mutable.Map(
          obj.getVisibleKeys().keySet.toSeq.distinctBy(outKey =>
            funct.apply(Val.Lazy(obj.value(outKey, -1)(fs, ev)))
          ).collect(key => key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))
        ): _*)
      }
      else {
        throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
      }
    ,_ => (), None)
  }

  private def filter(array: Seq[Val.Lazy], funct: Applyer): Val = {
    val args = funct.f.params.allIndices.size
    Val.Arr(
      if (args == 2) {
        //The three options are below, classic index for loop seems to be the fastest
        /*array.view.zipWithIndex.filter({
          case (item, index) => funct.apply(item, Val.Lazy(Val.Num(index))) == Val.True
        }).map(_._1).toSeq*/
        val out = collection.mutable.Buffer.empty[Val.Lazy]
        for(index <- array.indices){
          val item = array(index)
          if (funct.apply(array(index), Val.Lazy(Val.Num(index))) == Val.True){
            out.append(item)
          }
        }
        out.toSeq
        /*array.indices.collect({
          case index if funct.apply(array(index), Val.Lazy(Val.Num(index))) == Val.True => array(index)
        })*/
      } else if (args == 1)
        array.filter(lazyItem => funct.apply(lazyItem).equals(Val.True))
      else {
        throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
      }
    )
  }

  private def filterObject(obj: Val.Obj, func: Applyer, ev: EvalScope, fs: FileScope): Val = {
    val args = func.f.params.allIndices.size
    new Val.Obj(
      if (args == 3) {
        scala.collection.mutable.Map(
          obj.getVisibleKeys().keySet.zipWithIndex.filter({
            case (key,index) => func.apply(Val.Lazy(obj.value(key, -1)(fs, ev)), Val.Lazy(Val.Str(key)), Val.Lazy(Val.Num(index))) == Val.True
          }).map(_._1).collect(key => key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))
          ).toSeq: _*)
      }
      else if (args == 2) {
        scala.collection.mutable.Map(
          obj.getVisibleKeys().view.keySet
            .filter(key => func.apply(Val.Lazy(obj.value(key, -1)(fs, ev)), Val.Lazy(Val.Str(key))) == Val.True)
            .collect(key => key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))).toSeq: _*)
      }
      else if (args == 1) {
        scala.collection.mutable.Map(
          obj.getVisibleKeys().view.keySet
            .filter(key => func.apply(Val.Lazy(obj.value(key, -1)(fs, ev))) == Val.True)
            .collect(key => key -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(key, -1)(fs, ev))).toSeq: _*)
      }
      else {
        throw Error.Delegate("Expected embedded function to have between 1 and 3 parameters, received: " + args)
      }, _ => (), None) // end of new object to return
  }

  private def flatMap(array: Seq[Val.Lazy], funct: Applyer): Val = {
    val args = funct.f.params.allIndices.size
    val out = collection.mutable.Buffer.empty[Val.Lazy]
    if (args == 2) { // 2 args
      array.foreach(
        _.force match {
          case Val.Arr(inner) =>
            for(ind <- inner.indices){
              out.append(Val.Lazy(funct.apply(inner(ind), Val.Lazy(Val.Num(ind)))))
            }
          case i => throw Error.Delegate("Expected Array of Arrays, got: Array of " + i.prettyName)
        }
      )
    }
    else if (args == 1) { //  1 arg
      array.foreach(
        _.force match {
          case Val.Arr(inner) => out.appendAll(inner.map(it => Val.Lazy(funct.apply(it))))
          case i => throw Error.Delegate("Expected Array of Arrays, got: Array of " + i.prettyName)
        }
      )
    }
    else {
      throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
    }
    Val.Arr(out.toSeq)
  }

  private def groupBy(s: Seq[Val.Lazy], funct: Applyer): Val = {
    val args = funct.f.params.allIndices.size
    val out = mutable.Map[String, mutable.IndexedBuffer[Val.Lazy]]()
    if (args == 2) {
      for( index <- s.indices){
        val item = s(index)
        val key = convertToString(funct.apply(item, Val.Lazy(Val.Num(index))))
        out.getOrElseUpdate(key, mutable.IndexedBuffer[Val.Lazy]()).addOne(item)
      }
    } else if (args == 1) {
      s.foreach({ item =>
        val key = convertToString(funct.apply(item))
        out.getOrElseUpdate(key, mutable.IndexedBuffer[Val.Lazy]()).addOne(item)
      })
    }
    else {
      throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
    }
    new Val.Obj(out.map(keyVal => (keyVal._1, Library.memberOf(Val.Arr(keyVal._2.toIndexedSeq)))), _ => (), None)
  }

  private def groupBy(obj: Val.Obj, funct: Applyer, ev: EvalScope, fs: FileScope): Val = {
    val args = funct.f.params.allIndices.size
    val out = mutable.Map[String, mutable.LinkedHashMap[String, Val.Obj.Member]]()
    if (args == 2) {
      obj.foreachVisibleKey((key,_) =>{
        val item = obj.value(key, -1)(fs, ev)
        val functKey = convertToString(funct.apply(Val.Lazy(item), Val.Lazy(Val.Str(key))))
        out.getOrElseUpdate(functKey, mutable.LinkedHashMap[String, Val.Obj.Member]()).addOne(key, Library.memberOf(item))
      })
    }
    else if (args == 1) {
      obj.foreachVisibleKey((key,_)=>{
        val item = obj.value(key, -1)(fs, ev)
        val functKey = convertToString(funct.apply(Val.Lazy(item)))
        out.getOrElseUpdate(functKey, mutable.LinkedHashMap[String, Val.Obj.Member]()).addOne(key, Library.memberOf(item))
      })
    }
    else {
      throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
    }

    new Val.Obj(out.map(keyVal => (keyVal._1, Library.memberOf(new Val.Obj(keyVal._2, _ => (), None)))), _ => (), None)

  }

  private def map(array: Seq[Val.Lazy], funct: Applyer): Val = {
    val args = funct.f.params.allIndices.size
    Val.Arr(
      if (args == 2) { //2 args
        array.zipWithIndex.map {
          case (item, index) => Val.Lazy(funct.apply(item, Val.Lazy(Val.Num(index))))
        }
      } else if (args == 1) { // 1 arg
        array.map(item => Val.Lazy(funct.apply(item)))
      }
      else {
        throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
      }
    )
  }

  private def mapObject(obj: Val.Obj, funct: Applyer, ev: EvalScope, fs: FileScope): Val = {
    val args = funct.f.params.allIndices.size
    val out = scala.collection.mutable.Map[String, Val.Obj.Member]()
    if (args.equals(3)) {
      for (((key, _), index) <- obj.getVisibleKeys().zipWithIndex) {
        funct.apply(Val.Lazy(obj.value(key, -1)(fs, ev)), Val.Lazy(Val.Str(key)), Val.Lazy(Val.Num(index))) match {
          case s: Val.Obj =>
            out.addAll(s.getVisibleKeys().map {
              case (sKey, _) => sKey -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => s.value(sKey, -1)(fs, ev))
            })
          case i => Error.Delegate("Function must return an Object, got: " + i.prettyName)
        }
      }
      new Val.Obj(out, _ => (), None)
    }
    else if (args.equals(2)) {
      for ((key, _) <- obj.getVisibleKeys()) {
        funct.apply(Val.Lazy(obj.value(key, -1)(fs, ev)), Val.Lazy(Val.Str(key))) match {
          case s: Val.Obj =>
            out.addAll(s.getVisibleKeys().map {
              case (sKey, _) => sKey -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => s.value(sKey, -1)(fs, ev))
            })
          case i => Error.Delegate("Function must return an Object, got: " + i.prettyName)
        }
      }
      new Val.Obj(out, _ => (), None)
    }
    else if (args.equals(1)) {
      for ((key, _) <- obj.getVisibleKeys()) {
        funct.apply(Val.Lazy(obj.value(key, -1)(fs, ev))) match {
          case s: Val.Obj =>
            out.addAll(s.getVisibleKeys().map {
              case (sKey, _) => sKey -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => s.value(sKey, -1)(fs, ev))
            })
          case i => Error.Delegate("Function must return an Object, got: " + i.prettyName)
        }
      }
      new Val.Obj(out, _ => (), None)
    }
    else {
      throw Error.Delegate("Expected embedded function to have between 1 and 3 parameters, received: " + args)
    }
  }

  // TODO: change zipWithIndex to indexed for loop
  private def orderBy(array: Seq[Val.Lazy], funct: Applyer): Val = {
    val args = funct.f.params.allIndices.size
    if (args == 2) {
      Val.Arr(
        array.zipWithIndex.sortBy(
          it => funct.apply(it._1, Val.Lazy(Val.Num(it._2)))
        )(ord = ValOrdering).map(_._1))
    }
    else if (args == 1) {
      Val.Arr(array.sortBy(it => funct.apply(it))(ord = ValOrdering))
    }
    else {
      throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
    }
  }

  // TODO: we're traversing the object twice, needed?
  private def orderBy(obj: Val.Obj, funct: Applyer, ev: EvalScope, fs: FileScope): Val = {
    val args = funct.f.params.allIndices.size
    var out = scala.collection.mutable.LinkedHashMap.empty[String, Val.Obj.Member]
    for ((item, _) <- obj.getVisibleKeys()) {
      out += (item -> Val.Obj.Member(add = false, Visibility.Normal, (_, _, _, _) => obj.value(item, -1)(fs, ev)))
    }
    if (args == 2) {
      new Val.Obj(
        scala.collection.mutable.LinkedHashMap(
          out.toSeq.sortBy(
            item => funct.apply(Val.Lazy(obj.value(item._1, -1)(fs, ev)), Val.Lazy(Val.Str(item._1)))
          )(ord = ValOrdering): _*), _ => (), None)
    }
    else if (args == 1) {
      new Val.Obj(
        scala.collection.mutable.LinkedHashMap(
          out.toSeq.sortBy(
            item => funct.apply(Val.Lazy(obj.value(item._1, -1)(fs, ev)))
          )(ord = ValOrdering): _*), _ => (), None)
    }
    else {
      throw Error.Delegate("Expected embedded function to have 1 or 2 parameters, received: " + args)
    }
  }

  private def mapEntries(obj: Val.Obj, funct: Applyer, ev: EvalScope, fs: FileScope): Val = {
    val args = funct.f.params.allIndices.size
    val out = collection.mutable.Buffer.empty[Val.Lazy]
    if (args.equals(3)) {
      out.appendAll(obj.getVisibleKeys().keySet.zipWithIndex.map(
        item => Val.Lazy(funct.apply(Val.Lazy(obj.value(item._1, -1)(fs, ev)), Val.Lazy(Val.Str(item._1)), Val.Lazy(Val.Num(item._2))))
      ))
    }
    else if (args.equals(2)) {
      out.appendAll(obj.getVisibleKeys().keySet.map(
        item => Val.Lazy(funct.apply(Val.Lazy(obj.value(item, -1)(fs, ev)), Val.Lazy(Val.Str(item))))
      ))
    }
    else if (args.equals(1)) {
      out.appendAll(obj.getVisibleKeys().keySet.map(
        item => Val.Lazy(funct.apply(Val.Lazy(obj.value(item, -1)(fs, ev))))
      ))
    }
    else {
      throw Error.Delegate("Expected embedded function to have between 1 and 3 parameters, received: " + args)
    }

    Val.Arr(out.toSeq)
  }

  private def deepFlatten(array: Seq[Val.Lazy]): Seq[Val.Lazy] = {
    array.foldLeft(mutable.Buffer.empty[Val.Lazy])((agg, curr) =>{
      curr.force match {
        case Val.Arr(inner) => agg.appendAll(deepFlatten(inner))
        case _ => agg.append(curr)
      }
    }).toSeq
  }

  private def select(obj: Val.Obj, path: String, ev: EvalScope, fs: FileScope): Val = {
    val arr = path.split("\\.", 2)
    try {
      val objVal = obj.value(arr(0), -1)(fs, ev)
      if (arr.length > 1) {
        objVal match {
          case x: Val.Obj => select(x, arr(1), ev, fs)
          case _ =>  Val.Lazy(Val.Null).force
        }
      }
      else {
        objVal
      }
    } catch {
      case _: Error =>
        Val.Lazy(Val.Null).force
    }
  }

  private def convertToString(value: Val): String = {
    value match {
      case x: Val.Num =>
        val tmp = x.value
        if(tmp.ceil == tmp.floor) tmp.longValue.toString
        else tmp.toString
      case x: Val.Str => x.value
      case Val.Null => "null"
      case Val.True => "true"
      case Val.False => "false"
    }
  }
}

// this assumes that we're comparing same Vals of the same type
object ValOrdering extends Ordering[Val] {
  def compare(x: Val, y: Val): Int =
    x match {
      case Val.Num(value) => Ordering.Double.TotalOrdering.compare(value, y.asInstanceOf[Val.Num].value)
      case Val.Str(value) => Ordering.String.compare(value, y.asInstanceOf[Val.Str].value)
      // TODO: need to convert the Val.Bool to an actual boolean
      case bool: Val.Bool => Ordering.Boolean.compare(x.asInstanceOf, y.asInstanceOf)
      case unsupported: Val => throw Error.Delegate("Expected embedded function to return a String, Number, or Boolean, received: " + unsupported.prettyName)
  }
}
