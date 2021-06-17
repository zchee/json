// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package json

import (
	"fmt"
	"reflect"
	"errors"
	"sync"
)

var (
	encoderPointerType   = reflect.TypeOf((*Encoder)(nil))
	decoderPointerType   = reflect.TypeOf((*Decoder)(nil))
	marshalOptionsType   = reflect.TypeOf((*MarshalOptions)(nil)).Elem()
	unmarshalOptionsType = reflect.TypeOf((*UnmarshalOptions)(nil)).Elem()
	bytesType            = reflect.TypeOf((*[]byte)(nil)).Elem()
	errorType            = reflect.TypeOf((*error)(nil)).Elem()
)

// SkipFunc may be returned by custom marshal and unmarshal functions
// that operate on an Encoder or Decoder.
//
// Any function that returns SkipFunc must not cause observable side effects
// on the provided Encoder or Decoder. For example, it is permissible to call
// Decoder.PeekKind, but not permissible to call Decoder.ReadToken or
// Encoder.WriteToken since such methods mutate the state.
const SkipFunc = jsonError("skip function")

// Marshalers is a list of functions that may override the marshal behavior
// of specific types. Populate MarshalOptions.Marshalers to use it.
// A nil *Marshalers is equivalent to an empty list.
type Marshalers = arshalers[MarshalOptions, Encoder]

// NewMarshalers constructs a list of marshal functions to override
// the marshal behavior for specific types.
//
// Each input must be a function with one the following signatures:
//
//	func(T) ([]byte, error)
//	func(MarshalOptions, *Encoder, T) error
//
// A marshal function operating on an Encoder may return SkipFunc to signal
// that the function is to be skipped and that the next function be used.
// When skipping, the function must not write any tokens to Encoder.
//
//
// The input may also include *Marshalers values, which is equivalent to
// inlining the list of marshal functions used to construct it.
func NewMarshalers(ms ...*Marshalers) *Marshalers {
	// TODO: Document what T may be and the guarantees
	// for the values passed to custom marshalers.
	return newArshalers(ms...)
}

// Unmarshalers is a list of functions that may override the unmarshal behavior
// of specific types. Populate UnmarshalOptions.Unmarshalers to use it.
// A nil *Unmarshalers is equivalent to an empty list.
type Unmarshalers = arshalers[UnmarshalOptions, Decoder]

// NewUnmarshalers constructs a list of unmarshal functions to override
// the unmarshal behavior for specific types.
//
// Each input must be a function with one the following signatures:
//
//	func([]byte, T) error
//	func(UnmarshalOptions, *Decoder, T) error
//
// An unmarshal function operating on a Decoder may return SkipFunc to signal
// that the function is to be skipped and that the next function be used.
// When skipping, the function must not read any tokens from Decoder.
//
// The input may also include *Unmarshalers values, which is equivalent to
// inlining the list of unmarshal functions used to construct it.
func NewUnmarshalers(us ...*Unmarshalers) *Unmarshalers {
	// TODO: Document what T may be and the guarantees
	// for the values passed to custom unmarshalers.
	return newArshalers(us...)
}

type arshalers[Options, Coder any] struct {
	nonComparable
	fncVals  []typedArshaler[Options, Coder]
	fncCache sync.Map // map[reflect.Type]unmarshaler
}
type typedArshaler[Options, Coder any] struct {
	typ     reflect.Type
	fnc     func(Options, *Coder, addressableValue) error
	maySkip bool
}

func newArshalers[Options, Coder any](as ...*arshalers[Options, Coder]) *arshalers[Options, Coder] {
	var a arshalers[Options, Coder]
	for _, a2 := range as {
		if a2 != nil {
			a.fncVals = append(a.fncVals, a2.fncVals...)
		}
	}
	if len(a.fncVals) == 0 {
		return nil
	}
	return &a
}

func (a *arshalers[Options, Coder]) lookup(fnc func(Options, *Coder, addressableValue) error, t reflect.Type) func(Options, *Coder, addressableValue) error {
	if a == nil {
		return fnc
	}
	if v, ok := a.fncCache.Load(t); ok {
		return v.(func(Options, *Coder, addressableValue) error)
	}

	// Collect a list of arshalers that can be called for this type.
	// This list may be longer than 1 since some arshalers can be skipped.
	var fncs []func(Options, *Coder, addressableValue) error
	for _, fncVal := range a.fncVals {
		if !castTo(t, fncVal.typ) {
			continue
		}
		fncs = append(fncs, fncVal.fnc)
		if !fncVal.maySkip {
			break // subsequent arshalers will never be called
		}
	}

	// Construct an arshaler that may call every applicable arshaler.
	if len(fncs) > 0 {
		fncDefault := fnc
		fnc = func(o Options, c *Coder, v addressableValue) error {
			for _, fnc := range fncs {
				if err := fnc(o, c, v); err != SkipFunc {
					return err // either nil or non-nil
				}
			}
			return fncDefault(o, c, v)
		}
	}

	// Use the last stored so duplicate arshalers can be garbage collected.
	v, _ := a.fncCache.LoadOrStore(t, fnc)
	return v.(func(Options, *Coder, addressableValue) error)
}

func MarshalFuncV1[T any](fn func(T) ([]byte, error)) *Marshalers {
	t := reflect.TypeOf((*T)(nil)).Elem()
	checkCastTo(t, true)
	typFnc :=  typedArshaler[MarshalOptions, Encoder]{
		typ: t,
		fnc: func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
			val, err := fn(va.castTo(t).Interface().(T))
			if err != nil {
				if err == SkipFunc {
					err = errors.New("marshal function of type func(T) ([]byte, error) cannot be skipped")
				}
				// TODO: Avoid wrapping semantic errors.
				return &SemanticError{action: "marshal", GoType: t, Err: err}
			}
			if err := enc.WriteValue(val); err != nil {
				// TODO: Avoid wrapping semantic or I/O errors.
				return &SemanticError{action: "marshal", JSONKind: RawValue(val).Kind(), GoType: t, Err: err}
			}
			return nil
		},
	}
	return &Marshalers{fncVals: [] typedArshaler[MarshalOptions, Encoder]{typFnc}}
}

func MarshalFuncV2[T any](fn func(MarshalOptions, *Encoder, T) error) *Marshalers {
	t := reflect.TypeOf((*T)(nil)).Elem()
	checkCastTo(t, true)
	typFnc :=  typedArshaler[MarshalOptions, Encoder]{
		typ: t,
		fnc: func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
			prevDepth, prevLength := enc.tokens.depthLength()
			err := fn(mo, enc, va.castTo(t).Interface().(T))
			currDepth, currLength := enc.tokens.depthLength()
			if (prevDepth != currDepth || prevLength+1 != currLength) && err == nil {
				err = errors.New("must write exactly one JSON value")
			}
			if err != nil {
				if err == SkipFunc {
					if prevDepth == currDepth && prevLength == currLength {
						return SkipFunc
					}
					err = errors.New("must not write any JSON tokens when skipping")
				}
				// TODO: Avoid wrapping semantic or I/O errors.
				return &SemanticError{action: "marshal", GoType: t, Err: err}
			}
			return nil
		},
		maySkip: true,
	}
	return &Marshalers{fncVals: [] typedArshaler[MarshalOptions, Encoder]{typFnc}}
}

func UnmarshalFuncV1[T any](fn func([]byte, T) error) *Unmarshalers {
	t := reflect.TypeOf((*T)(nil)).Elem()
	checkCastTo(t, false)
	typFnc := typedArshaler[UnmarshalOptions, Decoder]{
		typ: t,
		fnc: func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
			val, err := dec.ReadValue()
			if err != nil {
				return err // must be a syntactic or I/O error
			}
			err = fn(val, va.castTo(t).Interface().(T))
			if err != nil {
				if err == SkipFunc {
					err = errors.New("unmarshal function of type func([]byte, T) error cannot be skipped")
				}
				// TODO: Avoid wrapping semantic, syntactic, or I/O errors.
				return &SemanticError{action: "unmarshal", JSONKind: val.Kind(), GoType: t, Err: err}
			}
			return nil
		},
	}
	return &Unmarshalers{fncVals: []typedArshaler[UnmarshalOptions, Decoder]{typFnc}}
}

func UnmarshalFuncV2[T any](fn func(UnmarshalOptions, *Decoder, T) error) *Unmarshalers {
	t := reflect.TypeOf((*T)(nil)).Elem()
	checkCastTo(t, false)
	typFnc := typedArshaler[UnmarshalOptions, Decoder]{
		typ: t,
		fnc: func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
			prevDepth, prevLength := dec.tokens.depthLength()
			err := fn(uo, dec, va.castTo(t).Interface().(T))
			currDepth, currLength := dec.tokens.depthLength()
			if (prevDepth != currDepth || prevLength+1 != currLength) && err == nil {
				err = errors.New("must read exactly one JSON value")
			}
			if err != nil {
				if err == SkipFunc {
					if prevDepth == currDepth && prevLength == currLength {
						return SkipFunc
					}
					err = errors.New("must not read any JSON tokens when skipping")
				}
				// TODO: Avoid wrapping semantic, syntactic, or I/O errors.
				return &SemanticError{action: "unmarshal", GoType: t, Err: err}
			}
			return nil
		},
		maySkip: true,
	}
	return &Unmarshalers{fncVals: []typedArshaler[UnmarshalOptions, Decoder]{typFnc}}
}


// checkCastTo checks whether t is valid for castTo to cast to.
func checkCastTo(t reflect.Type, marshal bool) {
	switch k := t.Kind(); {
	case k == reflect.Interface:
		return
	case k == reflect.Ptr && t.Name() == "":
		// Only allow unnamed pointers to provide symmetry with the fact that
		// methods can only be declared on unnamed pointers.
		return
	case k != reflect.Ptr && marshal:
		// Technically, non-pointer types are permissible for unmarshal.
		// However, they are often a bug since the receiver would be immutable.
		// Thus, only allow them for marshaling.
		return
	default:
		if marshal {
			panic(fmt.Sprintf("input type %v must be an interface type, an unnamed pointer type, or a non-pointer type", t))
		} else {
			panic(fmt.Sprintf("input type %v must be an interface type or an unnamed pointer type", t))
		}
	}
}

// castTo checks whether values of type from can be casted to type to that
// avoids ever allowing a nil pointer or interface to be considered castable.
//
// This function must be kept in sync with addressableValue.castTo.
func castTo(from, to reflect.Type) bool {
	switch to.Kind() {
	case reflect.Interface:
		// As a special-case, every value is castable to an empty interface.
		// This allows a custom marshaler to intercept every arshal call.
		if to.NumMethod() == 0 {
			return true
		}

		// Ignore interface and pointer types since we will check later
		// whether the underlying type implements the interface.
		switch from.Kind() {
		case reflect.Interface, reflect.Ptr:
			return false
		default:
			return from.Implements(to) || reflect.PtrTo(from).Implements(to)
		}
	case reflect.Ptr:
		return from == to.Elem() // common case for unmarshaling
	default:
		return from == to // common case for marshaling
	}
}

// castTo casts va to the specified type.
// If the type is an interface, then the underlying type will always
// be a non-nil pointer to a concrete type.
//
// Requirement: castTo(va.Type(), t) must hold.
func (va addressableValue) castTo(t reflect.Type) reflect.Value {
	switch t.Kind() {
	case reflect.Interface, reflect.Ptr:
		return va.Addr().Convert(t) // ensures returned value is non-nil pointer
	default:
		return va.Convert(t)
	}
}
