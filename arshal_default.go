// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package json

import (
	"encoding/base32"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"math"
	"reflect"
	"strconv"
	"sync"
)

// Most natural Go type that correspond with each JSON type.
var (
	boolType           = reflect.TypeOf((*bool)(nil)).Elem()                   // JSON bool
	stringType         = reflect.TypeOf((*string)(nil)).Elem()                 // JSON string
	float64Type        = reflect.TypeOf((*float64)(nil)).Elem()                // JSON number
	mapStringIfaceType = reflect.TypeOf((*map[string]interface{})(nil)).Elem() // JSON object
	sliceIfaceType     = reflect.TypeOf((*[]interface{})(nil)).Elem()          // JSON array
)

const startDetectingCyclesAfter = 1000

type seenPointers map[typedPointer]struct{}

type typedPointer struct {
	typ reflect.Type
	// TODO: This breaks if Go ever switches to a moving garbage collector.
	// This should use unsafe.Pointer, but that requires importing unsafe.
	// We only use pointers for comparisons, and never for unsafe type casts.
	// See https://golang.org/cl/14137 and https://golang.org/issue/40592.
	ptr uintptr
}

// visit visits pointer p of type t, reporting an error if seen before.
// If successfully visited, then the caller must eventually call leave.
func (m *seenPointers) visit(v reflect.Value) error {
	p := typedPointer{v.Type(), v.Pointer()}
	if _, ok := (*m)[p]; ok {
		return &SemanticError{action: "marshal", GoType: p.typ, Err: errors.New("encountered a cycle")}
	}
	if *m == nil {
		*m = make(map[typedPointer]struct{})
	}
	(*m)[p] = struct{}{}
	return nil
}
func (m *seenPointers) leave(v reflect.Value) {
	p := typedPointer{v.Type(), v.Pointer()}
	delete(*m, p)
}

func makeDefaultArshaler(t reflect.Type) *arshaler {
	switch t.Kind() {
	case reflect.Bool:
		return makeBoolArshaler(t)
	case reflect.String:
		return makeStringArshaler(t)
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return makeIntArshaler(t)
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		return makeUintArshaler(t)
	case reflect.Float32, reflect.Float64:
		return makeFloatArshaler(t)
	case reflect.Map:
		return makeMapArshaler(t)
	case reflect.Struct:
		return makeStructArshaler(t)
	case reflect.Slice:
		fncs := makeSliceArshaler(t)
		if t.AssignableTo(bytesType) {
			return makeBytesArshaler(t, fncs)
		}
		return fncs
	case reflect.Array:
		fncs := makeArrayArshaler(t)
		if reflect.SliceOf(t.Elem()).AssignableTo(bytesType) {
			return makeBytesArshaler(t, fncs)
		}
		return fncs
	case reflect.Ptr:
		return makePtrArshaler(t)
	case reflect.Interface:
		return makeInterfaceArshaler(t)
	default:
		return makeInvalidArshaler(t)
	}
}

func makeBoolArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		return enc.WriteToken(Bool(va.Bool()))
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		tok, err := dec.ReadToken()
		if err != nil {
			return err
		}
		k := tok.Kind()
		switch k {
		case 'n':
			va.SetBool(false)
			return nil
		case 't', 'f':
			va.SetBool(tok.Bool())
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makeStringArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		return enc.WriteToken(String(va.String()))
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		tok, err := dec.ReadToken()
		if err != nil {
			return err
		}
		k := tok.Kind()
		switch k {
		case 'n':
			va.SetString("")
			return nil
		case '"':
			va.SetString(tok.String())
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

var (
	encodeBase16        = func(dst, src []byte) { hex.Encode(dst, src) }
	encodeBase32        = base32.StdEncoding.Encode
	encodeBase32Hex     = base32.HexEncoding.Encode
	encodeBase64        = base64.StdEncoding.Encode
	encodeBase64URL     = base64.URLEncoding.Encode
	encodedLenBase16    = hex.EncodedLen
	encodedLenBase32    = base32.StdEncoding.EncodedLen
	encodedLenBase32Hex = base32.HexEncoding.EncodedLen
	encodedLenBase64    = base64.StdEncoding.EncodedLen
	encodedLenBase64URL = base64.URLEncoding.EncodedLen
	decodeBase16        = hex.Decode
	decodeBase32        = base32.StdEncoding.Decode
	decodeBase32Hex     = base32.HexEncoding.Decode
	decodeBase64        = base64.StdEncoding.Decode
	decodeBase64URL     = base64.URLEncoding.Decode
	decodedLenBase16    = hex.DecodedLen
	decodedLenBase32    = base32.StdEncoding.WithPadding(base32.NoPadding).DecodedLen
	decodedLenBase32Hex = base32.HexEncoding.WithPadding(base32.NoPadding).DecodedLen
	decodedLenBase64    = base64.StdEncoding.WithPadding(base64.NoPadding).DecodedLen
	decodedLenBase64URL = base64.URLEncoding.WithPadding(base64.NoPadding).DecodedLen
)

func makeBytesArshaler(t reflect.Type, fncs *arshaler) *arshaler {
	// NOTE: This handles both []byte and [N]byte.
	marshalDefault := fncs.marshal
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		var encode func(dst, src []byte)
		var encodedLen func(int) int
		switch mo.format {
		case "base64", "":
			encode, encodedLen = encodeBase64, encodedLenBase64
		case "base64url":
			encode, encodedLen = encodeBase64URL, encodedLenBase64URL
		case "base32":
			encode, encodedLen = encodeBase32, encodedLenBase32
		case "base32hex":
			encode, encodedLen = encodeBase32Hex, encodedLenBase32Hex
		case "base16", "hex":
			encode, encodedLen = encodeBase16, encodedLenBase16
		case "uintarray":
			mo.format = ""
			return marshalDefault(mo, enc, va)
		default:
			return newInvalidFormatError("marshal", t, mo.format)
		}
		val := enc.UnusedBuffer()
		var b []byte
		if t.Kind() == reflect.Array {
			// TODO(https://golang.org/issue/47066): Avoid reflect.Value.Slice.
			b = va.Slice(0, t.Len()).Bytes()
		} else {
			b = va.Bytes()
		}
		n := len(`"`) + encodedLen(len(b)) + len(`"`)
		if cap(val) < n {
			val = make([]byte, n)
		} else {
			val = val[:n]
		}
		val[0] = '"'
		encode(val[len(`"`):len(val)-len(`"`)], b)
		val[len(val)-1] = '"'
		return enc.WriteValue(val)
	}
	unmarshalDefault := fncs.unmarshal
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		var decode func(dst, src []byte) (int, error)
		var decodedLen func(int) int
		switch uo.format {
		case "base64", "":
			decode, decodedLen = decodeBase64, decodedLenBase64
		case "base64url":
			decode, decodedLen = decodeBase64URL, decodedLenBase64URL
		case "base32":
			decode, decodedLen = decodeBase32, decodedLenBase32
		case "base32hex":
			decode, decodedLen = decodeBase32Hex, decodedLenBase32Hex
		case "base16", "hex":
			decode, decodedLen = decodeBase16, decodedLenBase16
		case "uintarray":
			uo.format = ""
			return unmarshalDefault(uo, dec, va)
		default:
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		val, err := dec.ReadValue()
		if err != nil {
			return err
		}
		k := val.Kind()
		switch k {
		case 'n':
			va.Set(reflect.Zero(t))
			return nil
		case '"':
			val = unescapeSimpleString(val)

			// For base64 and base32, decodedLen computes the maximum output size
			// when given the original input size. To compute the exact size,
			// adjust the input size by excluding trailing padding characters.
			// This is unnecessary for base16, but also harmless.
			n := len(val)
			for n > 0 && val[n-1] == '=' {
				n--
			}
			n = decodedLen(n)
			var b []byte
			if t.Kind() == reflect.Array {
				// TODO(https://golang.org/issue/47066): Avoid reflect.Value.Slice.
				b = va.Slice(0, t.Len()).Bytes()
				if n != len(b) {
					err := fmt.Errorf("decoded base64 length of %d mismatches array length of %d", n, t.Len())
					return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
				}
			} else {
				b = va.Bytes()
				if b == nil || cap(b) < n {
					b = make([]byte, n)
				} else {
					b = b[:n]
				}
			}
			if _, err := decode(b, val); err != nil {
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
			}
			if t.Kind() == reflect.Slice {
				va.SetBytes(b)
			}
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return fncs
}

func makeIntArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		val := enc.UnusedBuffer()
		if mo.StringifyNumbers {
			val = append(val, '"')
		}
		val = strconv.AppendInt(val, va.Int(), 10)
		if mo.StringifyNumbers {
			val = append(val, '"')
		}
		return enc.WriteValue(val)
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		val, err := dec.ReadValue()
		if err != nil {
			return err
		}
		k := val.Kind()
		switch k {
		case 'n':
			va.SetInt(0)
			return nil
		case '"':
			if !uo.StringifyNumbers {
				break
			}
			val = unescapeSimpleString(val)
			fallthrough
		case '0':
			var negOffset int
			neg := val[0] == '-'
			if neg {
				negOffset = 1
			}
			n, ok := parseDecUint(val[negOffset:])
			maxInt := uint64(1) << (t.Bits() - 1)
			overflow := (neg && n > maxInt) || (!neg && n > maxInt-1)
			if !ok {
				if n != math.MaxUint64 {
					err := fmt.Errorf("cannot parse %q as signed integer: %w", val, strconv.ErrSyntax)
					return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
				}
				overflow = true
			}
			if overflow {
				err := fmt.Errorf("cannot parse %q as signed integer: %w", val, strconv.ErrRange)
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
			}
			if neg {
				va.SetInt(int64(-n))
			} else {
				va.SetInt(int64(+n))
			}
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makeUintArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		val := enc.UnusedBuffer()
		if mo.StringifyNumbers {
			val = append(val, '"')
		}
		val = strconv.AppendUint(val, va.Uint(), 10)
		if mo.StringifyNumbers {
			val = append(val, '"')
		}
		return enc.WriteValue(val)
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		val, err := dec.ReadValue()
		if err != nil {
			return err
		}
		k := val.Kind()
		switch k {
		case 'n':
			va.SetUint(0)
			return nil
		case '"':
			if !uo.StringifyNumbers {
				break
			}
			val = unescapeSimpleString(val)
			fallthrough
		case '0':
			n, ok := parseDecUint(val)
			maxUint := uint64(1) << t.Bits()
			overflow := n > maxUint-1
			if !ok {
				if n != math.MaxUint64 {
					err := fmt.Errorf("cannot parse %q as unsigned integer: %w", val, strconv.ErrSyntax)
					return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
				}
				overflow = true
			}
			if overflow {
				err := fmt.Errorf("cannot parse %q as unsigned integer: %w", val, strconv.ErrRange)
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
			}
			va.SetUint(uint64(n))
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makeFloatArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		var allowNonFinite bool
		switch mo.format {
		case "":
			break
		case "nonfinite":
			allowNonFinite = true
		default:
			return newInvalidFormatError("marshal", t, mo.format)
		}
		fv := va.Float()
		val := enc.UnusedBuffer()
		switch {
		case !allowNonFinite && (math.IsNaN(fv) || math.IsInf(fv, 0)):
			err := fmt.Errorf("invalid value: %v", fv)
			return &SemanticError{action: "marshal", GoType: t, Err: err}
		case math.IsNaN(fv):
			val = append(val, `"NaN"`...)
		case math.IsInf(fv, +1):
			val = append(val, `"Infinity"`...)
		case math.IsInf(fv, -1):
			val = append(val, `"-Infinity"`...)
		default:
			if mo.StringifyNumbers {
				val = append(val, '"')
			}
			val = appendNumber(val, fv, t.Bits())
			if mo.StringifyNumbers {
				val = append(val, '"')
			}
		}
		return enc.WriteValue(val)
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		var allowNonFinite bool
		switch uo.format {
		case "":
			break
		case "nonfinite":
			allowNonFinite = true
		default:
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		val, err := dec.ReadValue()
		if err != nil {
			return err
		}
		k := val.Kind()
		switch k {
		case 'n':
			va.SetFloat(0)
			return nil
		case '"':
			val = unescapeSimpleString(val)
			if allowNonFinite {
				switch string(val) {
				case "NaN":
					va.SetFloat(math.NaN())
					return nil
				case "Infinity":
					va.SetFloat(math.Inf(+1))
					return nil
				case "-Infinity":
					va.SetFloat(math.Inf(-1))
					return nil
				}
			}
			if !uo.StringifyNumbers {
				break
			}
			if n, err := consumeNumber(val); n != len(val) || err != nil {
				err := fmt.Errorf("cannot parse %q as JSON number: %w", val, strconv.ErrSyntax)
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
			}
			fallthrough
		case '0':
			// NOTE: Floating-point parsing is by nature a lossy operation.
			// We never report an overflow condition since we can always
			// round the input to the closest representable finite value.
			// For extremely large numbers, the closest value is ±MaxFloat.
			fv, _ := parseFloat(val, t.Bits())
			va.SetFloat(fv)
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makeMapArshaler(t reflect.Type) *arshaler {
	// NOTE: Values retrieved from a map are not addressable,
	// so we shallow copy the values to make them addressable and
	// store them back into the map afterwards.
	var fncs arshaler
	var (
		once    sync.Once
		keyFncs *arshaler
		valFncs *arshaler
	)
	init := func() {
		keyFncs = lookupArshaler(t.Key())
		valFncs = lookupArshaler(t.Elem())
	}
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		// Check for cycles.
		if enc.tokens.depth() > startDetectingCyclesAfter {
			if err := enc.seenPointers.visit(va.Value); err != nil {
				return err
			}
			defer enc.seenPointers.leave(va.Value)
		}

		switch mo.format {
		case "":
			break
		case "emitnull":
			if va.IsNil() {
				return enc.WriteToken(Null)
			}
			mo.format = ""
		default:
			return newInvalidFormatError("marshal", t, mo.format)
		}
		if err := enc.WriteToken(ObjectStart); err != nil {
			return err
		}
		if va.Len() > 0 {
			// Handle maps with numeric key types by stringifying them.
			mko := mo
			mko.StringifyNumbers = true

			once.Do(init)
			// TODO: Handle custom arshalers.
			marshalKey := keyFncs.marshal
			marshalVal := valFncs.marshal
			k := newAddressableValue(t.Key())
			v := newAddressableValue(t.Elem())
			// NOTE: Map entries are serialized in a non-deterministic order.
			// Users that need stable output should call RawValue.Canonicalize.
			for iter := va.MapRange(); iter.Next(); {
				k.Set(iter.Key())
				if err := marshalKey(mko, enc, k); err != nil {
					// TODO: If err is errMissingName, then wrap it with as a
					// SemanticError since this key type cannot be serialized
					// as a JSON string.
					return err
				}
				v.Set(iter.Value())
				if err := marshalVal(mo, enc, v); err != nil {
					return err
				}
			}
		}
		if err := enc.WriteToken(ObjectEnd); err != nil {
			return err
		}
		return nil
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		switch uo.format {
		case "":
			break
		case "emitnull":
			uo.format = ""
		default:
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		tok, err := dec.ReadToken()
		if err != nil {
			return err
		}
		k := tok.Kind()
		switch k {
		case 'n':
			va.Set(reflect.Zero(t))
			return nil
		case '{':
			if va.IsNil() {
				va.Set(reflect.MakeMap(t))
			}

			// Handle maps with numeric key types by stringifying them.
			uko := uo
			uko.StringifyNumbers = true

			once.Do(init)
			// TODO: Handle custom arshalers.
			unmarshalKey := keyFncs.unmarshal
			unmarshalVal := valFncs.unmarshal
			k := newAddressableValue(t.Key())
			v := newAddressableValue(t.Elem())
			for dec.PeekKind() != '}' {
				k.Set(reflect.Zero(t.Key()))
				if err := unmarshalKey(uko, dec, k); err != nil {
					return err
				}
				if k.Kind() == reflect.Interface && !k.IsNil() && !k.Elem().Type().Comparable() {
					err := fmt.Errorf("invalid incomparable key type %v", k.Elem().Type())
					return &SemanticError{action: "unmarshal", GoType: t, Err: err}
				}

				if v2 := va.MapIndex(k.Value); v2.IsValid() {
					v.Set(v2)
				} else {
					v.Set(reflect.Zero(v.Type()))
				}
				err := unmarshalVal(uo, dec, v)
				va.SetMapIndex(k.Value, v.Value)
				if err != nil {
					return err
				}
			}
			if _, err := dec.ReadToken(); err != nil {
				return err
			}
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makeStructArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	var (
		once    sync.Once
		fields  structFields
		errInit *SemanticError
	)
	init := func() {
		fields, errInit = makeStructFields(t)
	}
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		if err := enc.WriteToken(ObjectStart); err != nil {
			return err
		}
		once.Do(init)
		if errInit != nil {
			err := *errInit // shallow copy SemanticError
			err.action = "marshal"
			return &err
		}
		for _, f := range fields.flattened {
			v := addressableValue{va.Field(f.index[0])} // addressable if struct value is addressable
			if len(f.index) > 1 {
				v = v.fieldByIndex(f.index[1:], false)
				if !v.IsValid() {
					continue // implies a nil inlined field
				}
			}

			// OmitZero skips the field if the Go value is zero,
			// which we can determine up front without calling the marshaler.
			if f.omitzero && (v.IsZero() || (f.isZero != nil && f.isZero(v))) {
				continue
			}

			marshal := f.fncs.marshal // TODO: Handle custom arshalers.
			nonDefault := f.fncs.nonDefault

			// OmitEmpty skips the field if the marshaled JSON value is empty,
			// which we can know up front if there are no custom marshalers,
			// otherwise we must marshal the value and unwrite it if empty.
			if f.omitempty && !nonDefault && f.isEmpty != nil && f.isEmpty(v) {
				continue // fast path for omitempty
			}

			// Write the object member name and value.
			if err := enc.WriteToken(String(f.name)); err != nil {
				return err
			}
			mo2 := mo
			mo2.StringifyNumbers = mo2.StringifyNumbers || f.string
			mo2.format = f.format // TODO: Make this not deeply recursively.
			if err := marshal(mo2, enc, v); err != nil {
				return err
			}

			// Try unwriting the member if empty (slow path for omitempty).
			if f.omitempty {
				enc.unwriteEmptyObjectMember()
			}
		}
		if fields.inlinedFallback != nil && !(mo.DiscardUnknownMembers && fields.inlinedFallback.unknown) {
			if err := marshalInlinedFallbackAll(mo, enc, va, fields.inlinedFallback); err != nil {
				return err
			}
		}
		if err := enc.WriteToken(ObjectEnd); err != nil {
			return err
		}
		return nil
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		tok, err := dec.ReadToken()
		if err != nil {
			return err
		}
		k := tok.Kind()
		switch k {
		case 'n':
			va.Set(reflect.Zero(t))
			return nil
		case '{':
			once.Do(init)
			if errInit != nil {
				err := *errInit // shallow copy SemanticError
				err.action = "unmarshal"
				return &err
			}
			for dec.PeekKind() != '}' {
				// Process the object member name.
				val, err := dec.ReadValue()
				if err != nil {
					return err
				}
				name := unescapeSimpleString(val)
				f := fields.byActualName[string(name)]
				if f == nil {
					for _, f2 := range fields.byFoldedName[string(foldName(name))] {
						if f2.nocase || uo.MatchCaseInsensitiveNames {
							f = f2
							break
						}
					}
					if f == nil {
						if uo.RejectUnknownNames && (fields.inlinedFallback == nil || fields.inlinedFallback.unknown) {
							return &SemanticError{action: "unmarshal", GoType: t, Err: ErrUnknownName}
						}

						if fields.inlinedFallback == nil {
							// Skip unknown value since we have no place to store it.
							if err := dec.skipValue(); err != nil {
								return err
							}
						} else {
							// Marshal into value capable of storing arbitrary object members.
							if err := unmarshalInlinedFallbackNext(uo, dec, va, fields.inlinedFallback, val); err != nil {
								return err
							}
						}
						continue
					}
				}

				// Process the object member value.
				unmarshal := f.fncs.unmarshal // TODO: Handle custom arshalers.
				uo2 := uo
				uo2.StringifyNumbers = uo2.StringifyNumbers || f.string
				uo2.format = f.format                       // TODO: Make this not deeply recursively.
				v := addressableValue{va.Field(f.index[0])} // addressable if struct value is addressable
				if len(f.index) > 1 {
					v = v.fieldByIndex(f.index[1:], true)
				}
				if err := unmarshal(uo2, dec, v); err != nil {
					return err
				}
			}
			if _, err := dec.ReadToken(); err != nil {
				return err
			}
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func (va addressableValue) fieldByIndex(index []int, mayAlloc bool) addressableValue {
	for _, i := range index {
		va = va.indirect(mayAlloc)
		if !va.IsValid() {
			return va
		}
		va = addressableValue{va.Field(i)} // addressable if struct value is addressable
	}
	return va
}

func (va addressableValue) indirect(mayAlloc bool) addressableValue {
	if va.Kind() == reflect.Ptr {
		if va.IsNil() {
			if !mayAlloc {
				return addressableValue{}
			}
			va.Set(reflect.New(va.Type().Elem()))
		}
		va = addressableValue{va.Elem()} // dereferenced pointer is always addressable
	}
	return va
}

func makeSliceArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	var (
		once    sync.Once
		valFncs *arshaler
	)
	init := func() {
		valFncs = lookupArshaler(t.Elem())
	}
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		// Check for cycles.
		if enc.tokens.depth() > startDetectingCyclesAfter {
			if err := enc.seenPointers.visit(va.Value); err != nil {
				return err
			}
			defer enc.seenPointers.leave(va.Value)
		}

		switch mo.format {
		case "":
			break
		case "emitnull":
			if va.IsNil() {
				return enc.WriteToken(Null)
			}
			mo.format = ""
		default:
			return newInvalidFormatError("marshal", t, mo.format)
		}
		if err := enc.WriteToken(ArrayStart); err != nil {
			return err
		}
		once.Do(init)
		marshal := valFncs.marshal // TODO: Handle custom arshalers.
		for i := 0; i < va.Len(); i++ {
			v := addressableValue{va.Index(i)} // indexed slice element is always addressable
			if err := marshal(mo, enc, v); err != nil {
				return err
			}
		}
		if err := enc.WriteToken(ArrayEnd); err != nil {
			return err
		}
		return nil
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		switch uo.format {
		case "":
			break
		case "emitnull":
			uo.format = ""
		default:
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		tok, err := dec.ReadToken()
		if err != nil {
			return err
		}
		k := tok.Kind()
		switch k {
		case 'n':
			va.Set(reflect.Zero(t))
			return nil
		case '[':
			once.Do(init)
			unmarshal := valFncs.unmarshal // TODO: Handle custom arshalers.
			va.SetLen(0)
			mustZero := true // we do not know the cleanliness of unused capacity
			for i := 0; dec.PeekKind() != ']'; i++ {
				if i+1 < va.Cap() {
					va.SetLen(i + 1)
				} else {
					// TODO(https://golang.org/issue/48000): Use reflect.Value.Append.
					va.Set(reflect.Append(va.Value, reflect.Zero(t.Elem())))
					mustZero = false // append guarantees that unused capacity is zero-initialized
				}
				v := addressableValue{va.Index(i)} // indexed slice element is always addressable
				if mustZero {
					v.Set(reflect.Zero(t.Elem()))
				}
				if err := unmarshal(uo, dec, v); err != nil {
					return err
				}
			}
			if va.IsNil() {
				va.Set(reflect.MakeSlice(va.Type(), 0, 0))
			}
			if _, err := dec.ReadToken(); err != nil {
				return err
			}
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makeArrayArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	var (
		once    sync.Once
		valFncs *arshaler
	)
	init := func() {
		valFncs = lookupArshaler(t.Elem())
	}
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		if err := enc.WriteToken(ArrayStart); err != nil {
			return err
		}
		once.Do(init)
		marshal := valFncs.marshal // TODO: Handle custom arshalers.
		for i := 0; i < t.Len(); i++ {
			v := addressableValue{va.Index(i)} // indexed array element is addressable if array is addressable
			if err := marshal(mo, enc, v); err != nil {
				return err
			}
		}
		if err := enc.WriteToken(ArrayEnd); err != nil {
			return err
		}
		return nil
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		tok, err := dec.ReadToken()
		if err != nil {
			return err
		}
		k := tok.Kind()
		switch k {
		case 'n':
			va.Set(reflect.Zero(t))
			return nil
		case '[':
			once.Do(init)
			unmarshal := valFncs.unmarshal // TODO: Handle custom arshalers.
			var i int
			for dec.PeekKind() != ']' {
				if i >= t.Len() {
					err := errors.New("too many array elements")
					return &SemanticError{action: "unmarshal", GoType: t, Err: err}
				}
				v := addressableValue{va.Index(i)} // indexed array element is addressable if array is addressable
				v.Set(reflect.Zero(v.Type()))
				if err := unmarshal(uo, dec, v); err != nil {
					return err
				}
				i++
			}
			if _, err := dec.ReadToken(); err != nil {
				return err
			}
			if i < t.Len() {
				err := errors.New("too few array elements")
				return &SemanticError{action: "unmarshal", GoType: t, Err: err}
			}
			return nil
		}
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
	}
	return &fncs
}

func makePtrArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	var (
		once    sync.Once
		valFncs *arshaler
	)
	init := func() {
		valFncs = lookupArshaler(t.Elem())
	}
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		// Check for cycles.
		if enc.tokens.depth() > startDetectingCyclesAfter {
			if err := enc.seenPointers.visit(va.Value); err != nil {
				return err
			}
			defer enc.seenPointers.leave(va.Value)
		}

		// NOTE: MarshalOptions.format is forwarded to underlying marshal.
		if va.IsNil() {
			return enc.WriteToken(Null)
		}
		once.Do(init)
		marshal := valFncs.marshal       // TODO: Handle custom arshalers. Should this occur before the nil check?
		v := addressableValue{va.Elem()} // dereferenced pointer is always addressable
		return marshal(mo, enc, v)
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		// NOTE: UnmarshalOptions.format is forwarded to underlying unmarshal.
		if dec.PeekKind() == 'n' {
			if _, err := dec.ReadToken(); err != nil {
				return err
			}
			va.Set(reflect.Zero(t))
			return nil
		}
		once.Do(init)
		unmarshal := valFncs.unmarshal // TODO: Handle custom arshalers. Should this occur before the nil check?
		if va.IsNil() {
			va.Set(reflect.New(t.Elem()))
		}
		v := addressableValue{va.Elem()} // dereferenced pointer is always addressable
		return unmarshal(uo, dec, v)
	}
	return &fncs
}

func makeInterfaceArshaler(t reflect.Type) *arshaler {
	// NOTE: Values retrieved from an interface are not addressable,
	// so we shallow copy the values to make them addressable and
	// store them back into the interface afterwards.
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		if mo.format != "" {
			return newInvalidFormatError("marshal", t, mo.format)
		}
		if va.IsNil() {
			return enc.WriteToken(Null)
		}
		v := newAddressableValue(va.Elem().Type())
		v.Set(va.Elem())
		marshal := lookupArshaler(v.Type()).marshal // TODO: Handle custom arshalers. Should this occur before the nil check?
		return marshal(mo, enc, v)
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		if uo.format != "" {
			return newInvalidFormatError("unmarshal", t, uo.format)
		}
		if dec.PeekKind() == 'n' {
			if _, err := dec.ReadToken(); err != nil {
				return err
			}
			va.Set(reflect.Zero(t))
			return nil
		}
		var v addressableValue
		if va.IsNil() {
			k := dec.PeekKind()
			if t.NumMethod() > 0 {
				// TODO: If types sets are allowed in ordinary interface types,
				// then the concrete type to use can be known in the case where
				// the type set contains exactly one Go type.
				// See https://golang.org/issue/45346.
				err := errors.New("cannot derive concrete type for non-empty interface")
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
			}
			switch k {
			case 'f', 't':
				v = newAddressableValue(boolType)
			case '"':
				v = newAddressableValue(stringType)
			case '0':
				v = newAddressableValue(float64Type)
			case '{':
				v = newAddressableValue(mapStringIfaceType)
			case '[':
				v = newAddressableValue(sliceIfaceType)
			default:
				// TODO: This could also be due to an I/O error.
				return &SyntacticError{ByteOffset: dec.InputOffset(), str: "invalid JSON token"}
			}
		} else {
			// Shallow copy the existing value to keep it addressable.
			// Any mutations at the top-level of the value will be observable
			// since we always store this value back into the interface value.
			v = newAddressableValue(va.Elem().Type())
			v.Set(va.Elem())
		}
		unmarshal := lookupArshaler(v.Type()).unmarshal // TODO: Handle custom arshalers. Should this occur before the nil check?
		err := unmarshal(uo, dec, v)
		va.Set(v.Value)
		return err
	}
	return &fncs
}

func makeInvalidArshaler(t reflect.Type) *arshaler {
	var fncs arshaler
	fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
		return &SemanticError{action: "marshal", GoType: t}
	}
	fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
		return &SemanticError{action: "unmarshal", GoType: t}
	}
	return &fncs
}

func newInvalidFormatError(action string, t reflect.Type, format string) error {
	err := fmt.Errorf("invalid format flag: %q", format)
	return &SemanticError{action: action, GoType: t, Err: err}
}
