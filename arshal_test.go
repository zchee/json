// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package json

import (
	"bytes"
	"encoding/base32"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"math"
	"net"
	"reflect"
	"strconv"
	"strings"
	"testing"
	"time"
)

type (
	jsonObject = map[string]interface{}
	jsonArray  = []interface{}

	namedBool    bool
	namedString  string
	namedBytes   []byte
	namedInt64   int64
	namedUint64  uint64
	namedFloat64 float64
	namedByte    byte

	recursiveMap   map[string]recursiveMap
	recursiveSlice []recursiveSlice
	recursivePtr   struct{ P *recursivePtr }

	structEmpty       struct{}
	structConflicting struct {
		A string `json:"conflict"`
		B string `json:"conflict"`
	}
	structNoneExported struct {
		unexported string
	}
	structUnexportedIgnored struct {
		ignored string `json:"-"`
	}
	structMalformedTag struct {
		Malformed string `json:"\""`
	}
	structUnexportedTag struct {
		unexported string `json:"name"`
	}
	structUnexportedEmbedded struct {
		namedString
	}
	structIgnoredUnexportedEmbedded struct {
		namedString `json:"-"`
	}
	structWeirdNames struct {
		Empty string `json:"''"`
		Comma string `json:"','"`
		Quote string `json:"'\"'"`
	}
	structNoCase struct {
		AaA string `json:",nocase"`
		AAa string `json:",nocase"`
		AAA string
	}
	structScalars struct {
		unexported bool
		Ignored    bool `json:"-"`

		Bool   bool
		String string
		Bytes  []byte
		Int    int64
		Uint   uint64
		Float  float64
	}
	structSlices struct {
		unexported bool
		Ignored    bool `json:"-"`

		SliceBool   []bool
		SliceString []string
		SliceBytes  [][]byte
		SliceInt    []int64
		SliceUint   []uint64
		SliceFloat  []float64
	}
	structMaps struct {
		unexported bool
		Ignored    bool `json:"-"`

		MapBool   map[string]bool
		MapString map[string]string
		MapBytes  map[string][]byte
		MapInt    map[string]int64
		MapUint   map[string]uint64
		MapFloat  map[string]float64
	}
	structAll struct {
		Bool          bool
		String        string
		Bytes         []byte
		Int           int64
		Uint          uint64
		Float         float64
		Map           map[string]string
		StructScalars structScalars
		StructMaps    structMaps
		StructSlices  structSlices
		Slice         []string
		Array         [1]string
		Ptr           *structAll
		Interface     interface{}
	}
	structStringifiedAll struct {
		Bool          bool                  `json:",string"`
		String        string                `json:",string"`
		Bytes         []byte                `json:",string"`
		Int           int64                 `json:",string"`
		Uint          uint64                `json:",string"`
		Float         float64               `json:",string"`
		Map           map[string]string     `json:",string"`
		StructScalars structScalars         `json:",string"`
		StructMaps    structMaps            `json:",string"`
		StructSlices  structSlices          `json:",string"`
		Slice         []string              `json:",string"`
		Array         [1]string             `json:",string"`
		Ptr           *structStringifiedAll `json:",string"`
		Interface     interface{}           `json:",string"`
	}
	structOmitZeroAll struct {
		Bool          bool               `json:",omitzero"`
		String        string             `json:",omitzero"`
		Bytes         []byte             `json:",omitzero"`
		Int           int64              `json:",omitzero"`
		Uint          uint64             `json:",omitzero"`
		Float         float64            `json:",omitzero"`
		Map           map[string]string  `json:",omitzero"`
		StructScalars structScalars      `json:",omitzero"`
		StructMaps    structMaps         `json:",omitzero"`
		StructSlices  structSlices       `json:",omitzero"`
		Slice         []string           `json:",omitzero"`
		Array         [1]string          `json:",omitzero"`
		Ptr           *structOmitZeroAll `json:",omitzero"`
		Interface     interface{}        `json:",omitzero"`
	}
	structOmitZeroMethodAll struct {
		ValAlwaysZero       valueAlwaysZero     `json:",omitzero"`
		ValNeverZero        valueNeverZero      `json:",omitzero"`
		PtrAlwaysZero       pointerAlwaysZero   `json:",omitzero"`
		PtrNeverZero        pointerNeverZero    `json:",omitzero"`
		PtrValAlwaysZero    *valueAlwaysZero    `json:",omitzero"`
		PtrValNeverZero     *valueNeverZero     `json:",omitzero"`
		PtrPtrAlwaysZero    *pointerAlwaysZero  `json:",omitzero"`
		PtrPtrNeverZero     *pointerNeverZero   `json:",omitzero"`
		PtrPtrValAlwaysZero **valueAlwaysZero   `json:",omitzero"`
		PtrPtrValNeverZero  **valueNeverZero    `json:",omitzero"`
		PtrPtrPtrAlwaysZero **pointerAlwaysZero `json:",omitzero"`
		PtrPtrPtrNeverZero  **pointerNeverZero  `json:",omitzero"`
	}
	structOmitEmptyAll struct {
		Bool              bool                    `json:",omitempty"`
		PtrBool           *bool                   `json:",omitempty"`
		String            string                  `json:",omitempty"`
		StringEmpty       stringMarshalEmpty      `json:",omitempty"`
		StringNonEmpty    stringMarshalNonEmpty   `json:",omitempty"`
		PtrString         *string                 `json:",omitempty"`
		PtrStringEmpty    *stringMarshalEmpty     `json:",omitempty"`
		PtrStringNonEmpty *stringMarshalNonEmpty  `json:",omitempty"`
		Bytes             []byte                  `json:",omitempty"`
		BytesEmpty        bytesMarshalEmpty       `json:",omitempty"`
		BytesNonEmpty     bytesMarshalNonEmpty    `json:",omitempty"`
		PtrBytes          *[]byte                 `json:",omitempty"`
		PtrBytesEmpty     *bytesMarshalEmpty      `json:",omitempty"`
		PtrBytesNonEmpty  *bytesMarshalNonEmpty   `json:",omitempty"`
		Float             float64                 `json:",omitempty"`
		PtrFloat          *float64                `json:",omitempty"`
		Map               map[string]string       `json:",omitempty"`
		MapEmpty          mapMarshalEmpty         `json:",omitempty"`
		MapNonEmpty       mapMarshalNonEmpty      `json:",omitempty"`
		PtrMap            *map[string]string      `json:",omitempty"`
		PtrMapEmpty       *mapMarshalEmpty        `json:",omitempty"`
		PtrMapNonEmpty    *mapMarshalNonEmpty     `json:",omitempty"`
		Slice             []string                `json:",omitempty"`
		SliceEmpty        sliceMarshalEmpty       `json:",omitempty"`
		SliceNonEmpty     sliceMarshalNonEmpty    `json:",omitempty"`
		PtrSlice          *[]string               `json:",omitempty"`
		PtrSliceEmpty     *sliceMarshalEmpty      `json:",omitempty"`
		PtrSliceNonEmpty  *sliceMarshalNonEmpty   `json:",omitempty"`
		Ptr               *structOmitZeroEmptyAll `json:",omitempty"`
		Interface         interface{}             `json:",omitempty"`
	}
	structOmitZeroEmptyAll struct {
		Bool      bool                    `json:",omitzero,omitempty"`
		String    string                  `json:",omitzero,omitempty"`
		Bytes     []byte                  `json:",omitzero,omitempty"`
		Int       int64                   `json:",omitzero,omitempty"`
		Uint      uint64                  `json:",omitzero,omitempty"`
		Float     float64                 `json:",omitzero,omitempty"`
		Map       map[string]string       `json:",omitzero,omitempty"`
		Slice     []string                `json:",omitzero,omitempty"`
		Array     [1]string               `json:",omitzero,omitempty"`
		Ptr       *structOmitZeroEmptyAll `json:",omitzero,omitempty"`
		Interface interface{}             `json:",omitzero,omitempty"`
	}
	structFormatBytes struct {
		Base16    []byte `json:",format:base16"`
		Base32    []byte `json:",format:base32"`
		Base32Hex []byte `json:",format:base32hex"`
		Base64    []byte `json:",format:base64"`
		Base64URL []byte `json:",format:base64url"`
		UintArray []byte `json:",format:uintarray"`
	}
	structFormatFloats struct {
		NonFinite    float64  `json:",format:nonfinite"`
		PtrNonFinite *float64 `json:",format:nonfinite"`
	}
	structFormatMaps struct {
		EmitNull    map[string]string  `json:",format:emitnull"`
		PtrEmitNull *map[string]string `json:",format:emitnull"`
	}
	structFormatSlices struct {
		EmitNull    []string  `json:",format:emitnull"`
		PtrEmitNull *[]string `json:",format:emitnull"`
	}
	structFormatInvalid struct {
		Bool      bool              `json:",omitzero,format:invalid"`
		String    string            `json:",omitzero,format:invalid"`
		Bytes     []byte            `json:",omitzero,format:invalid"`
		Int       int64             `json:",omitzero,format:invalid"`
		Uint      uint64            `json:",omitzero,format:invalid"`
		Float     float64           `json:",omitzero,format:invalid"`
		Map       map[string]string `json:",omitzero,format:invalid"`
		Struct    structAll         `json:",omitzero,format:invalid"`
		Slice     []string          `json:",omitzero,format:invalid"`
		Array     [1]string         `json:",omitzero,format:invalid"`
		Interface interface{}       `json:",omitzero,format:invalid"`
	}
	structTimeFormat struct {
		T1  time.Time
		T2  time.Time `json:",format:ANSIC"`
		T3  time.Time `json:",format:UnixDate"`
		T4  time.Time `json:",format:RubyDate"`
		T5  time.Time `json:",format:RFC822"`
		T6  time.Time `json:",format:RFC822Z"`
		T7  time.Time `json:",format:RFC850"`
		T8  time.Time `json:",format:RFC1123"`
		T9  time.Time `json:",format:RFC1123Z"`
		T10 time.Time `json:",format:RFC3339"`
		T11 time.Time `json:",format:RFC3339Nano"`
		T12 time.Time `json:",format:Kitchen"`
		T13 time.Time `json:",format:Stamp"`
		T14 time.Time `json:",format:StampMilli"`
		T15 time.Time `json:",format:StampMicro"`
		T16 time.Time `json:",format:StampNano"`
		T17 time.Time `json:",format:'2006-01-02'"`
		T18 time.Time `json:",format:'\"weird\"2006'"`
	}
	structInlined struct {
		X             structInlinedL1 `json:",inline"`
		*StructEmbed2                 // implicit inline
	}
	structInlinedL1 struct {
		X            *structInlinedL2 `json:",inline"`
		StructEmbed1 `json:",inline"`
	}
	structInlinedL2       struct{ A, B, C string }
	StructEmbed1          struct{ C, D, E string }
	StructEmbed2          struct{ E, F, G string }
	structUnknownRawValue struct {
		A int      `json:",omitzero"`
		X RawValue `json:",unknown"`
		B int      `json:",omitzero"`
	}
	structInlineRawValue struct {
		A int      `json:",omitzero"`
		X RawValue `json:",inline"`
		B int      `json:",omitzero"`
	}
	structInlinePointerRawValue struct {
		A int       `json:",omitzero"`
		X *RawValue `json:",inline"`
		B int       `json:",omitzero"`
	}
	structInlinePointerInlineRawValue struct {
		X *struct {
			A int
			X RawValue `json:",inline"`
		} `json:",inline"`
	}
	structInlineInlinePointerRawValue struct {
		X struct {
			X *RawValue `json:",inline"`
		} `json:",inline"`
	}
	structInlineMapStringAny struct {
		A int        `json:",omitzero"`
		X jsonObject `json:",inline"`
		B int        `json:",omitzero"`
	}
	structInlinePointerMapStringAny struct {
		A int         `json:",omitzero"`
		X *jsonObject `json:",inline"`
		B int         `json:",omitzero"`
	}
	structInlinePointerInlineMapStringAny struct {
		X *struct {
			A int
			X jsonObject `json:",inline"`
		} `json:",inline"`
	}
	structInlineInlinePointerMapStringAny struct {
		X struct {
			X *jsonObject `json:",inline"`
		} `json:",inline"`
	}
	structInlineMapStringInt struct {
		X map[string]int `json:",inline"`
	}

	allMethods struct {
		method string // the method that was called
		value  []byte // the raw value to provide or store
	}
	allMethodsExceptJSONv2 struct {
		allMethods
		MarshalNextJSON   struct{} // cancel out MarshalNextJSON method with collision
		UnmarshalNextJSON struct{} // cancel out UnmarshalNextJSON method with collision
	}
	allMethodsExceptJSONv1 struct {
		allMethods
		MarshalJSON   struct{} // cancel out MarshalJSON method with collision
		UnmarshalJSON struct{} // cancel out UnmarshalJSON method with collision
	}
	allMethodsExceptText struct {
		allMethods
		MarshalText   struct{} // cancel out MarshalText method with collision
		UnmarshalText struct{} // cancel out UnmarshalText method with collision
	}
	onlyMethodJSONv2 struct {
		allMethods
		MarshalJSON   struct{} // cancel out MarshalJSON method with collision
		UnmarshalJSON struct{} // cancel out UnmarshalJSON method with collision
		MarshalText   struct{} // cancel out MarshalText method with collision
		UnmarshalText struct{} // cancel out UnmarshalText method with collision
	}
	onlyMethodJSONv1 struct {
		allMethods
		MarshalNextJSON   struct{} // cancel out MarshalNextJSON method with collision
		UnmarshalNextJSON struct{} // cancel out UnmarshalNextJSON method with collision
		MarshalText       struct{} // cancel out MarshalText method with collision
		UnmarshalText     struct{} // cancel out UnmarshalText method with collision
	}
	onlyMethodText struct {
		allMethods
		MarshalNextJSON   struct{} // cancel out MarshalNextJSON method with collision
		UnmarshalNextJSON struct{} // cancel out UnmarshalNextJSON method with collision
		MarshalJSON       struct{} // cancel out MarshalJSON method with collision
		UnmarshalJSON     struct{} // cancel out UnmarshalJSON method with collision
	}

	structMethodJSONv2 struct{ value string }
	structMethodJSONv1 struct{ value string }
	structMethodText   struct{ value string }

	marshalJSONv2Func   func(*Encoder, MarshalOptions) error
	marshalJSONv1Func   func() ([]byte, error)
	marshalTextFunc     func() ([]byte, error)
	unmarshalJSONv2Func func(*Decoder, UnmarshalOptions) error
	unmarshalJSONv1Func func([]byte) error
	unmarshalTextFunc   func([]byte) error

	stringMarshalEmpty    string
	stringMarshalNonEmpty string
	bytesMarshalEmpty     []byte
	bytesMarshalNonEmpty  []byte
	mapMarshalEmpty       map[string]string
	mapMarshalNonEmpty    map[string]string
	sliceMarshalEmpty     []string
	sliceMarshalNonEmpty  []string

	valueAlwaysZero   string
	valueNeverZero    string
	pointerAlwaysZero string
	pointerNeverZero  string
)

func (p *allMethods) MarshalNextJSON(enc *Encoder, mo MarshalOptions) error {
	if got, want := "MarshalNextJSON", p.method; got != want {
		return fmt.Errorf("called wrong method: got %v, want %v", got, want)
	}
	return enc.WriteValue(p.value)
}
func (p *allMethods) MarshalJSON() ([]byte, error) {
	if got, want := "MarshalJSON", p.method; got != want {
		return nil, fmt.Errorf("called wrong method: got %v, want %v", got, want)
	}
	return p.value, nil
}
func (p *allMethods) MarshalText() ([]byte, error) {
	if got, want := "MarshalText", p.method; got != want {
		return nil, fmt.Errorf("called wrong method: got %v, want %v", got, want)
	}
	return p.value, nil
}

func (p *allMethods) UnmarshalNextJSON(dec *Decoder, uo UnmarshalOptions) error {
	p.method = "UnmarshalNextJSON"
	val, err := dec.ReadValue()
	p.value = val
	return err
}
func (p *allMethods) UnmarshalJSON(val []byte) error {
	p.method = "UnmarshalJSON"
	p.value = val
	return nil
}
func (p *allMethods) UnmarshalText(val []byte) error {
	p.method = "UnmarshalText"
	p.value = val
	return nil
}

func (s structMethodJSONv2) MarshalNextJSON(enc *Encoder, mo MarshalOptions) error {
	return enc.WriteToken(String(s.value))
}
func (s *structMethodJSONv2) UnmarshalNextJSON(dec *Decoder, uo UnmarshalOptions) error {
	tok, err := dec.ReadToken()
	if err != nil {
		return err
	}
	if k := tok.Kind(); k != '"' {
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: structMethodJSONv2Type}
	}
	s.value = tok.String()
	return nil
}

func (s structMethodJSONv1) MarshalJSON() ([]byte, error) {
	return appendString(nil, s.value, false, nil)
}
func (s *structMethodJSONv1) UnmarshalJSON(b []byte) error {
	if k := RawValue(b).Kind(); k != '"' {
		return &SemanticError{action: "unmarshal", JSONKind: k, GoType: structMethodJSONv1Type}
	}
	b, _ = unescapeString(nil, b)
	s.value = string(b)
	return nil
}

func (s structMethodText) MarshalText() ([]byte, error) {
	return []byte(s.value), nil
}
func (s *structMethodText) UnmarshalText(b []byte) error {
	s.value = string(b)
	return nil
}

func (f marshalJSONv2Func) MarshalNextJSON(enc *Encoder, mo MarshalOptions) error {
	return f(enc, mo)
}
func (f marshalJSONv1Func) MarshalJSON() ([]byte, error) {
	return f()
}
func (f marshalTextFunc) MarshalText() ([]byte, error) {
	return f()
}
func (f unmarshalJSONv2Func) UnmarshalNextJSON(dec *Decoder, uo UnmarshalOptions) error {
	return f(dec, uo)
}
func (f unmarshalJSONv1Func) UnmarshalJSON(b []byte) error {
	return f(b)
}
func (f unmarshalTextFunc) UnmarshalText(b []byte) error {
	return f(b)
}

func (stringMarshalEmpty) MarshalJSON() ([]byte, error)    { return []byte(`""`), nil }
func (stringMarshalNonEmpty) MarshalJSON() ([]byte, error) { return []byte(`"value"`), nil }
func (bytesMarshalEmpty) MarshalJSON() ([]byte, error)     { return []byte(`[]`), nil }
func (bytesMarshalNonEmpty) MarshalJSON() ([]byte, error)  { return []byte(`["value"]`), nil }
func (mapMarshalEmpty) MarshalJSON() ([]byte, error)       { return []byte(`{}`), nil }
func (mapMarshalNonEmpty) MarshalJSON() ([]byte, error)    { return []byte(`{"key":"value"}`), nil }
func (sliceMarshalEmpty) MarshalJSON() ([]byte, error)     { return []byte(`[]`), nil }
func (sliceMarshalNonEmpty) MarshalJSON() ([]byte, error)  { return []byte(`["value"]`), nil }

func (valueAlwaysZero) IsZero() bool    { return true }
func (valueNeverZero) IsZero() bool     { return false }
func (*pointerAlwaysZero) IsZero() bool { return true }
func (*pointerNeverZero) IsZero() bool  { return false }

var (
	emptyInterfaceType           = reflect.TypeOf((*interface{})(nil)).Elem()
	namedBoolType                = reflect.TypeOf((*namedBool)(nil)).Elem()
	intType                      = reflect.TypeOf((*int)(nil)).Elem()
	int8Type                     = reflect.TypeOf((*int8)(nil)).Elem()
	int16Type                    = reflect.TypeOf((*int16)(nil)).Elem()
	int32Type                    = reflect.TypeOf((*int32)(nil)).Elem()
	int64Type                    = reflect.TypeOf((*int64)(nil)).Elem()
	uintType                     = reflect.TypeOf((*uint)(nil)).Elem()
	uint8Type                    = reflect.TypeOf((*uint8)(nil)).Elem()
	uint16Type                   = reflect.TypeOf((*uint16)(nil)).Elem()
	uint32Type                   = reflect.TypeOf((*uint32)(nil)).Elem()
	uint64Type                   = reflect.TypeOf((*uint64)(nil)).Elem()
	sliceStringType              = reflect.TypeOf((*[]string)(nil)).Elem()
	array1StringType             = reflect.TypeOf((*[1]string)(nil)).Elem()
	array0ByteType               = reflect.TypeOf((*[0]byte)(nil)).Elem()
	array1ByteType               = reflect.TypeOf((*[1]byte)(nil)).Elem()
	array2ByteType               = reflect.TypeOf((*[2]byte)(nil)).Elem()
	array3ByteType               = reflect.TypeOf((*[3]byte)(nil)).Elem()
	array4ByteType               = reflect.TypeOf((*[4]byte)(nil)).Elem()
	mapStringStringType          = reflect.TypeOf((*map[string]string)(nil)).Elem()
	structAllType                = reflect.TypeOf((*structAll)(nil)).Elem()
	structConflictingType        = reflect.TypeOf((*structConflicting)(nil)).Elem()
	structNoneExportedType       = reflect.TypeOf((*structNoneExported)(nil)).Elem()
	structMalformedTagType       = reflect.TypeOf((*structMalformedTag)(nil)).Elem()
	structUnexportedTagType      = reflect.TypeOf((*structUnexportedTag)(nil)).Elem()
	structUnexportedEmbeddedType = reflect.TypeOf((*structUnexportedEmbedded)(nil)).Elem()
	structUnknownRawValueType    = reflect.TypeOf((*structUnknownRawValue)(nil)).Elem()
	allMethodsType               = reflect.TypeOf((*allMethods)(nil)).Elem()
	allMethodsExceptJSONv2Type   = reflect.TypeOf((*allMethodsExceptJSONv2)(nil)).Elem()
	allMethodsExceptJSONv1Type   = reflect.TypeOf((*allMethodsExceptJSONv1)(nil)).Elem()
	allMethodsExceptTextType     = reflect.TypeOf((*allMethodsExceptText)(nil)).Elem()
	onlyMethodJSONv2Type         = reflect.TypeOf((*onlyMethodJSONv2)(nil)).Elem()
	onlyMethodJSONv1Type         = reflect.TypeOf((*onlyMethodJSONv1)(nil)).Elem()
	onlyMethodTextType           = reflect.TypeOf((*onlyMethodText)(nil)).Elem()
	structMethodJSONv2Type       = reflect.TypeOf((*structMethodJSONv2)(nil)).Elem()
	structMethodJSONv1Type       = reflect.TypeOf((*structMethodJSONv1)(nil)).Elem()
	structMethodTextType         = reflect.TypeOf((*structMethodText)(nil)).Elem()
	marshalJSONv2FuncType        = reflect.TypeOf((*marshalJSONv2Func)(nil)).Elem()
	marshalJSONv1FuncType        = reflect.TypeOf((*marshalJSONv1Func)(nil)).Elem()
	marshalTextFuncType          = reflect.TypeOf((*marshalTextFunc)(nil)).Elem()
	unmarshalJSONv2FuncType      = reflect.TypeOf((*unmarshalJSONv2Func)(nil)).Elem()
	unmarshalJSONv1FuncType      = reflect.TypeOf((*unmarshalJSONv1Func)(nil)).Elem()
	unmarshalTextFuncType        = reflect.TypeOf((*unmarshalTextFunc)(nil)).Elem()
	ioReaderType                 = reflect.TypeOf((*io.Reader)(nil)).Elem()
	chanStringType               = reflect.TypeOf((*chan string)(nil)).Elem()
)

func addr(v interface{}) interface{} {
	// TODO: Make this generic.
	v1 := reflect.ValueOf(v)
	v2 := reflect.New(v1.Type())
	v2.Elem().Set(v1)
	return v2.Interface()
}

func mustParseTime(layout, value string) time.Time {
	t, err := time.Parse(layout, value)
	if err != nil {
		panic(err)
	}
	return t
}

func TestMarshal(t *testing.T) {
	tests := []struct {
		name    string
		mopts   MarshalOptions
		eopts   EncodeOptions
		in      interface{}
		want    string
		wantErr error

		canonicalize bool // canonicalize the output before comparing?
		useWriter    bool // call MarshalFull instead
	}{{
		name: "Nil",
		in:   nil,
		want: `null`,
	}, {
		name: "Bools",
		in:   []bool{false, true},
		want: `[false,true]`,
	}, {
		name: "Bools/Named",
		in:   []namedBool{false, true},
		want: `[false,true]`,
	}, {
		name:  "Bools/NotStringified",
		mopts: MarshalOptions{StringifyNumbers: true},
		in:    []bool{false, true},
		want:  `[false,true]`,
	}, {
		name: "Strings",
		in:   []string{"", "hello", "世界"},
		want: `["","hello","世界"]`,
	}, {
		name: "Strings/Named",
		in:   []namedString{"", "hello", "世界"},
		want: `["","hello","世界"]`,
	}, {
		name: "Bytes",
		in:   [][]byte{nil, {}, {1}, {1, 2}, {1, 2, 3}},
		want: `["","","AQ==","AQI=","AQID"]`,
	}, {
		name: "Bytes/Large",
		in:   []byte("the quick brown fox jumped over the lazy dog and ate the homework that I spent so much time on."),
		want: `"dGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cgYW5kIGF0ZSB0aGUgaG9tZXdvcmsgdGhhdCBJIHNwZW50IHNvIG11Y2ggdGltZSBvbi4="`,
	}, {
		name: "Bytes/Named",
		in:   []namedBytes{nil, {}, {1}, {1, 2}, {1, 2, 3}},
		want: `["","","AQ==","AQI=","AQID"]`,
	}, {
		name:  "Bytes/NotStringified",
		mopts: MarshalOptions{StringifyNumbers: true},
		in:    [][]byte{nil, {}, {1}, {1, 2}, {1, 2, 3}},
		want:  `["","","AQ==","AQI=","AQID"]`,
	}, {
		// NOTE: []namedByte is not assignable to []byte,
		// so the following should be treated as a slice of uints.
		name: "Bytes/Invariant",
		in:   [][]namedByte{nil, {}, {1}, {1, 2}, {1, 2, 3}},
		want: `[[],[],[1],[1,2],[1,2,3]]`,
	}, {
		// NOTE: This differs in behavior from v1,
		// but keeps the representation of slices and arrays more consistent.
		name: "Bytes/ByteArray",
		in:   [5]byte{'h', 'e', 'l', 'l', 'o'},
		want: `"aGVsbG8="`,
	}, {
		// NOTE: []namedByte is not assignable to []byte,
		// so the following should be treated as an array of uints.
		name: "Bytes/NamedByteArray",
		in:   [5]namedByte{'h', 'e', 'l', 'l', 'o'},
		want: `[104,101,108,108,111]`,
	}, {
		name: "Ints",
		in: []interface{}{
			int(0), int8(math.MinInt8), int16(math.MinInt16), int32(math.MinInt32), int64(math.MinInt64), namedInt64(-6464),
		},
		want: `[0,-128,-32768,-2147483648,-9223372036854775808,-6464]`,
	}, {
		name:  "Ints/Stringified",
		mopts: MarshalOptions{StringifyNumbers: true},
		in: []interface{}{
			int(0), int8(math.MinInt8), int16(math.MinInt16), int32(math.MinInt32), int64(math.MinInt64), namedInt64(-6464),
		},
		want: `["0","-128","-32768","-2147483648","-9223372036854775808","-6464"]`,
	}, {
		name: "Uints",
		in: []interface{}{
			uint(0), uint8(math.MaxUint8), uint16(math.MaxUint16), uint32(math.MaxUint32), uint64(math.MaxUint64), namedUint64(6464),
		},
		want: `[0,255,65535,4294967295,18446744073709551615,6464]`,
	}, {
		name:  "Uints/Stringified",
		mopts: MarshalOptions{StringifyNumbers: true},
		in: []interface{}{
			uint(0), uint8(math.MaxUint8), uint16(math.MaxUint16), uint32(math.MaxUint32), uint64(math.MaxUint64), namedUint64(6464),
		},
		want: `["0","255","65535","4294967295","18446744073709551615","6464"]`,
	}, {
		name: "Floats",
		in: []interface{}{
			float32(math.MaxFloat32), float64(math.MaxFloat64), namedFloat64(64.64),
		},
		want: `[3.4028235e+38,1.7976931348623157e+308,64.64]`,
	}, {
		name:  "Floats/Stringified",
		mopts: MarshalOptions{StringifyNumbers: true},
		in: []interface{}{
			float32(math.MaxFloat32), float64(math.MaxFloat64), namedFloat64(64.64),
		},
		want: `["3.4028235e+38","1.7976931348623157e+308","64.64"]`,
	}, {
		name:    "Floats/Invalid/NaN",
		mopts:   MarshalOptions{StringifyNumbers: true},
		in:      math.NaN(),
		wantErr: &SemanticError{action: "marshal", GoType: float64Type, Err: fmt.Errorf("invalid value: %v", math.NaN())},
	}, {
		name:    "Floats/Invalid/PositiveInfinity",
		mopts:   MarshalOptions{StringifyNumbers: true},
		in:      math.Inf(+1),
		wantErr: &SemanticError{action: "marshal", GoType: float64Type, Err: fmt.Errorf("invalid value: %v", math.Inf(+1))},
	}, {
		name:    "Floats/Invalid/NegativeInfinity",
		mopts:   MarshalOptions{StringifyNumbers: true},
		in:      math.Inf(-1),
		wantErr: &SemanticError{action: "marshal", GoType: float64Type, Err: fmt.Errorf("invalid value: %v", math.Inf(-1))},
	}, {
		name:    "Maps/InvalidKey/Bool",
		in:      map[bool]string{false: "value"},
		want:    `{`,
		wantErr: errMissingName,
	}, {
		name:    "Maps/InvalidKey/NamedBool",
		in:      map[namedBool]string{false: "value"},
		want:    `{`,
		wantErr: errMissingName,
	}, {
		name:    "Maps/InvalidKey/Array",
		in:      map[[1]string]string{[1]string{"key"}: "value"},
		want:    `{`,
		wantErr: errMissingName,
	}, {
		name:    "Maps/InvalidKey/Channel",
		in:      map[chan string]string{make(chan string): "value"},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: chanStringType},
	}, {
		name:         "Maps/ValidKey/Int",
		in:           map[int64]string{math.MinInt64: "MinInt64", 0: "Zero", math.MaxInt64: "MaxInt64"},
		canonicalize: true,
		want:         `{"-9223372036854775808":"MinInt64","0":"Zero","9223372036854775807":"MaxInt64"}`,
	}, {
		name:         "Maps/ValidKey/NamedInt",
		in:           map[namedInt64]string{math.MinInt64: "MinInt64", 0: "Zero", math.MaxInt64: "MaxInt64"},
		canonicalize: true,
		want:         `{"-9223372036854775808":"MinInt64","0":"Zero","9223372036854775807":"MaxInt64"}`,
	}, {
		name:         "Maps/ValidKey/Uint",
		in:           map[uint64]string{0: "Zero", math.MaxUint64: "MaxUint64"},
		canonicalize: true,
		want:         `{"0":"Zero","18446744073709551615":"MaxUint64"}`,
	}, {
		name:         "Maps/ValidKey/NamedUint",
		in:           map[namedUint64]string{0: "Zero", math.MaxUint64: "MaxUint64"},
		canonicalize: true,
		want:         `{"0":"Zero","18446744073709551615":"MaxUint64"}`,
	}, {
		name: "Maps/ValidKey/Float",
		in:   map[float64]string{3.14159: "value"},
		want: `{"3.14159":"value"}`,
	}, {
		name: "Maps/ValidKey/Interface",
		in: map[interface{}]interface{}{
			"key":               "key",
			namedInt64(-64):     int32(-32),
			namedUint64(+64):    uint32(+32),
			namedFloat64(64.64): float32(32.32),
		},
		canonicalize: true,
		want:         `{"-64":-32,"64":32,"64.64":32.32,"key":"key"}`,
	}, {
		name: "Maps/InvalidValue/Channel",
		in: map[string]chan string{
			"key": nil,
		},
		want:    `{"key"`,
		wantErr: &SemanticError{action: "marshal", GoType: chanStringType},
	}, {
		name: "Maps/RecursiveMap",
		in: recursiveMap{
			"fizz": {
				"foo": {},
				"bar": nil,
			},
			"buzz": nil,
		},
		canonicalize: true,
		want:         `{"buzz":{},"fizz":{"bar":{},"foo":{}}}`,
	}, {
		name: "Maps/CyclicMap",
		in: func() recursiveMap {
			m := recursiveMap{"k": nil}
			m["k"] = m
			return m
		}(),
		want:    strings.Repeat(`{"k":`, startDetectingCyclesAfter) + `{"k"`,
		wantErr: &SemanticError{action: "marshal", GoType: reflect.TypeOf(recursiveMap{}), Err: errors.New("encountered a cycle")},
	}, {
		name: "Structs/Empty",
		in:   structEmpty{},
		want: `{}`,
	}, {
		name: "Structs/UnexportedIgnored",
		in:   structUnexportedIgnored{ignored: "ignored"},
		want: `{}`,
	}, {
		name: "Structs/IgnoredUnexportedEmbedded",
		in:   structIgnoredUnexportedEmbedded{namedString: "ignored"},
		want: `{}`,
	}, {
		name: "Structs/WeirdNames",
		in:   structWeirdNames{Empty: "empty", Comma: "comma", Quote: "quote"},
		want: `{"":"empty",",":"comma","\"":"quote"}`,
	}, {
		name: "Structs/NoCase",
		in:   structNoCase{AaA: "AaA", AAa: "AAa", AAA: "AAA"},
		want: `{"AaA":"AaA","AAa":"AAa","AAA":"AAA"}`,
	}, {
		name:  "Structs/Normal",
		eopts: EncodeOptions{Indent: "\t"},
		in: structAll{
			Bool:   true,
			String: "hello",
			Bytes:  []byte{1, 2, 3},
			Int:    -64,
			Uint:   +64,
			Float:  3.14159,
			Map:    map[string]string{"key": "value"},
			StructScalars: structScalars{
				Bool:   true,
				String: "hello",
				Bytes:  []byte{1, 2, 3},
				Int:    -64,
				Uint:   +64,
				Float:  3.14159,
			},
			StructMaps: structMaps{
				MapBool:   map[string]bool{"": true},
				MapString: map[string]string{"": "hello"},
				MapBytes:  map[string][]byte{"": []byte{1, 2, 3}},
				MapInt:    map[string]int64{"": -64},
				MapUint:   map[string]uint64{"": +64},
				MapFloat:  map[string]float64{"": 3.14159},
			},
			StructSlices: structSlices{
				SliceBool:   []bool{true},
				SliceString: []string{"hello"},
				SliceBytes:  [][]byte{[]byte{1, 2, 3}},
				SliceInt:    []int64{-64},
				SliceUint:   []uint64{+64},
				SliceFloat:  []float64{3.14159},
			},
			Slice:     []string{"fizz", "buzz"},
			Array:     [1]string{"goodbye"},
			Ptr:       new(structAll),
			Interface: (*structAll)(nil),
		},
		want: `{
	"Bool": true,
	"String": "hello",
	"Bytes": "AQID",
	"Int": -64,
	"Uint": 64,
	"Float": 3.14159,
	"Map": {
		"key": "value"
	},
	"StructScalars": {
		"Bool": true,
		"String": "hello",
		"Bytes": "AQID",
		"Int": -64,
		"Uint": 64,
		"Float": 3.14159
	},
	"StructMaps": {
		"MapBool": {
			"": true
		},
		"MapString": {
			"": "hello"
		},
		"MapBytes": {
			"": "AQID"
		},
		"MapInt": {
			"": -64
		},
		"MapUint": {
			"": 64
		},
		"MapFloat": {
			"": 3.14159
		}
	},
	"StructSlices": {
		"SliceBool": [
			true
		],
		"SliceString": [
			"hello"
		],
		"SliceBytes": [
			"AQID"
		],
		"SliceInt": [
			-64
		],
		"SliceUint": [
			64
		],
		"SliceFloat": [
			3.14159
		]
	},
	"Slice": [
		"fizz",
		"buzz"
	],
	"Array": [
		"goodbye"
	],
	"Ptr": {
		"Bool": false,
		"String": "",
		"Bytes": "",
		"Int": 0,
		"Uint": 0,
		"Float": 0,
		"Map": {},
		"StructScalars": {
			"Bool": false,
			"String": "",
			"Bytes": "",
			"Int": 0,
			"Uint": 0,
			"Float": 0
		},
		"StructMaps": {
			"MapBool": {},
			"MapString": {},
			"MapBytes": {},
			"MapInt": {},
			"MapUint": {},
			"MapFloat": {}
		},
		"StructSlices": {
			"SliceBool": [],
			"SliceString": [],
			"SliceBytes": [],
			"SliceInt": [],
			"SliceUint": [],
			"SliceFloat": []
		},
		"Slice": [],
		"Array": [
			""
		],
		"Ptr": null,
		"Interface": null
	},
	"Interface": null
}`,
	}, {
		name:  "Structs/Stringified",
		eopts: EncodeOptions{Indent: "\t"},
		in: structStringifiedAll{
			Bool:   true,
			String: "hello",
			Bytes:  []byte{1, 2, 3},
			Int:    -64,     // should be stringified
			Uint:   +64,     // should be stringified
			Float:  3.14159, // should be stringified
			Map:    map[string]string{"key": "value"},
			StructScalars: structScalars{
				Bool:   true,
				String: "hello",
				Bytes:  []byte{1, 2, 3},
				Int:    -64,     // should be stringified
				Uint:   +64,     // should be stringified
				Float:  3.14159, // should be stringified
			},
			StructMaps: structMaps{
				MapBool:   map[string]bool{"": true},
				MapString: map[string]string{"": "hello"},
				MapBytes:  map[string][]byte{"": []byte{1, 2, 3}},
				MapInt:    map[string]int64{"": -64},       // should be stringified
				MapUint:   map[string]uint64{"": +64},      // should be stringified
				MapFloat:  map[string]float64{"": 3.14159}, // should be stringified
			},
			StructSlices: structSlices{
				SliceBool:   []bool{true},
				SliceString: []string{"hello"},
				SliceBytes:  [][]byte{[]byte{1, 2, 3}},
				SliceInt:    []int64{-64},       // should be stringified
				SliceUint:   []uint64{+64},      // should be stringified
				SliceFloat:  []float64{3.14159}, // should be stringified
			},
			Slice:     []string{"fizz", "buzz"},
			Array:     [1]string{"goodbye"},
			Ptr:       new(structStringifiedAll), // should be stringified
			Interface: (*structStringifiedAll)(nil),
		},
		want: `{
	"Bool": true,
	"String": "hello",
	"Bytes": "AQID",
	"Int": "-64",
	"Uint": "64",
	"Float": "3.14159",
	"Map": {
		"key": "value"
	},
	"StructScalars": {
		"Bool": true,
		"String": "hello",
		"Bytes": "AQID",
		"Int": "-64",
		"Uint": "64",
		"Float": "3.14159"
	},
	"StructMaps": {
		"MapBool": {
			"": true
		},
		"MapString": {
			"": "hello"
		},
		"MapBytes": {
			"": "AQID"
		},
		"MapInt": {
			"": "-64"
		},
		"MapUint": {
			"": "64"
		},
		"MapFloat": {
			"": "3.14159"
		}
	},
	"StructSlices": {
		"SliceBool": [
			true
		],
		"SliceString": [
			"hello"
		],
		"SliceBytes": [
			"AQID"
		],
		"SliceInt": [
			"-64"
		],
		"SliceUint": [
			"64"
		],
		"SliceFloat": [
			"3.14159"
		]
	},
	"Slice": [
		"fizz",
		"buzz"
	],
	"Array": [
		"goodbye"
	],
	"Ptr": {
		"Bool": false,
		"String": "",
		"Bytes": "",
		"Int": "0",
		"Uint": "0",
		"Float": "0",
		"Map": {},
		"StructScalars": {
			"Bool": false,
			"String": "",
			"Bytes": "",
			"Int": "0",
			"Uint": "0",
			"Float": "0"
		},
		"StructMaps": {
			"MapBool": {},
			"MapString": {},
			"MapBytes": {},
			"MapInt": {},
			"MapUint": {},
			"MapFloat": {}
		},
		"StructSlices": {
			"SliceBool": [],
			"SliceString": [],
			"SliceBytes": [],
			"SliceInt": [],
			"SliceUint": [],
			"SliceFloat": []
		},
		"Slice": [],
		"Array": [
			""
		],
		"Ptr": null,
		"Interface": null
	},
	"Interface": null
}`,
	}, {
		name: "Structs/OmitZero/Zero",
		in:   structOmitZeroAll{},
		want: `{}`,
	}, {
		name:  "Structs/OmitZero/NonZero",
		eopts: EncodeOptions{Indent: "\t"},
		in: structOmitZeroAll{
			Bool:          true,                                   // not omitted since true is non-zero
			String:        " ",                                    // not omitted since non-empty string is non-zero
			Bytes:         []byte{},                               // not omitted since allocated slice is non-zero
			Int:           1,                                      // not omitted since 1 is non-zero
			Uint:          1,                                      // not omitted since 1 is non-zero
			Float:         math.Copysign(0, -1),                   // not omitted since -0 is technically non-zero
			Map:           map[string]string{},                    // not omitted since allocated map is non-zero
			StructScalars: structScalars{unexported: true},        // not omitted since unexported is non-zero
			StructSlices:  structSlices{Ignored: true},            // not omitted since Ignored is non-zero
			StructMaps:    structMaps{MapBool: map[string]bool{}}, // not omitted since MapBool is non-zero
			Slice:         []string{},                             // not omitted since allocated slice is non-zero
			Array:         [1]string{" "},                         // not omitted since single array element is non-zero
			Ptr:           new(structOmitZeroAll),                 // not omitted since pointer is non-zero (even if all fields of the struct value are zero)
			Interface:     (*structOmitZeroAll)(nil),              // not omitted since interface value is non-zero (even if interface value is a nil pointer)
		},
		want: `{
	"Bool": true,
	"String": " ",
	"Bytes": "",
	"Int": 1,
	"Uint": 1,
	"Float": -0,
	"Map": {},
	"StructScalars": {
		"Bool": false,
		"String": "",
		"Bytes": "",
		"Int": 0,
		"Uint": 0,
		"Float": 0
	},
	"StructMaps": {
		"MapBool": {},
		"MapString": {},
		"MapBytes": {},
		"MapInt": {},
		"MapUint": {},
		"MapFloat": {}
	},
	"StructSlices": {
		"SliceBool": [],
		"SliceString": [],
		"SliceBytes": [],
		"SliceInt": [],
		"SliceUint": [],
		"SliceFloat": []
	},
	"Slice": [],
	"Array": [
		" "
	],
	"Ptr": {},
	"Interface": null
}`,
	}, {
		name: "Structs/OmitZeroMethod/Zero",
		in:   structOmitZeroMethodAll{},
		want: `{}`,
	}, {
		name:  "Structs/OmitZeroMethod/NonZero",
		eopts: EncodeOptions{Indent: "\t"},
		in: structOmitZeroMethodAll{
			ValAlwaysZero:       valueAlwaysZero("nonzero"),
			ValNeverZero:        valueNeverZero("nonzero"),
			PtrAlwaysZero:       pointerAlwaysZero("nonzero"),
			PtrNeverZero:        pointerNeverZero("nonzero"),
			PtrValAlwaysZero:    addr(valueAlwaysZero("nonzero")).(*valueAlwaysZero),
			PtrValNeverZero:     addr(valueNeverZero("nonzero")).(*valueNeverZero),
			PtrPtrAlwaysZero:    addr(pointerAlwaysZero("nonzero")).(*pointerAlwaysZero),
			PtrPtrNeverZero:     addr(pointerNeverZero("nonzero")).(*pointerNeverZero),
			PtrPtrValAlwaysZero: addr(addr(valueAlwaysZero("nonzero"))).(**valueAlwaysZero), // marshaled since **valueAlwaysZero does not implement IsZero
			PtrPtrValNeverZero:  addr(addr(valueNeverZero("nonzero"))).(**valueNeverZero),
			PtrPtrPtrAlwaysZero: addr(addr(pointerAlwaysZero("nonzero"))).(**pointerAlwaysZero), // marshaled since **pointerAlwaysZero does not implement IsZero
			PtrPtrPtrNeverZero:  addr(addr(pointerNeverZero("nonzero"))).(**pointerNeverZero),
		},
		want: `{
	"ValNeverZero": "nonzero",
	"PtrNeverZero": "nonzero",
	"PtrValNeverZero": "nonzero",
	"PtrPtrNeverZero": "nonzero",
	"PtrPtrValAlwaysZero": "nonzero",
	"PtrPtrValNeverZero": "nonzero",
	"PtrPtrPtrAlwaysZero": "nonzero",
	"PtrPtrPtrNeverZero": "nonzero"
}`,
	}, {
		name:  "Structs/OmitEmpty/Zero",
		eopts: EncodeOptions{Indent: "\t"},
		in:    structOmitEmptyAll{},
		want: `{
	"Bool": false,
	"StringNonEmpty": "value",
	"BytesNonEmpty": [
		"value"
	],
	"Float": 0,
	"MapNonEmpty": {
		"key": "value"
	},
	"SliceNonEmpty": [
		"value"
	]
}`,
	}, {
		name:  "Structs/OmitEmpty/EmptyNonZero",
		eopts: EncodeOptions{Indent: "\t"},
		in: structOmitEmptyAll{
			String:            string(""),
			StringEmpty:       stringMarshalEmpty(""),
			StringNonEmpty:    stringMarshalNonEmpty(""),
			PtrString:         addr(string("")).(*string),
			PtrStringEmpty:    addr(stringMarshalEmpty("")).(*stringMarshalEmpty),
			PtrStringNonEmpty: addr(stringMarshalNonEmpty("")).(*stringMarshalNonEmpty),
			Bytes:             []byte(""),
			BytesEmpty:        bytesMarshalEmpty([]byte("")),
			BytesNonEmpty:     bytesMarshalNonEmpty([]byte("")),
			PtrBytes:          addr([]byte("")).(*[]byte),
			PtrBytesEmpty:     addr(bytesMarshalEmpty([]byte(""))).(*bytesMarshalEmpty),
			PtrBytesNonEmpty:  addr(bytesMarshalNonEmpty([]byte(""))).(*bytesMarshalNonEmpty),
			Map:               map[string]string{},
			MapEmpty:          mapMarshalEmpty{},
			MapNonEmpty:       mapMarshalNonEmpty{},
			PtrMap:            addr(map[string]string{}).(*map[string]string),
			PtrMapEmpty:       addr(mapMarshalEmpty{}).(*mapMarshalEmpty),
			PtrMapNonEmpty:    addr(mapMarshalNonEmpty{}).(*mapMarshalNonEmpty),
			Slice:             []string{},
			SliceEmpty:        sliceMarshalEmpty{},
			SliceNonEmpty:     sliceMarshalNonEmpty{},
			PtrSlice:          addr([]string{}).(*[]string),
			PtrSliceEmpty:     addr(sliceMarshalEmpty{}).(*sliceMarshalEmpty),
			PtrSliceNonEmpty:  addr(sliceMarshalNonEmpty{}).(*sliceMarshalNonEmpty),
			Ptr:               &structOmitZeroEmptyAll{},
			Interface:         []string{},
		},
		want: `{
	"Bool": false,
	"StringNonEmpty": "value",
	"PtrStringNonEmpty": "value",
	"BytesNonEmpty": [
		"value"
	],
	"PtrBytesNonEmpty": [
		"value"
	],
	"Float": 0,
	"MapNonEmpty": {
		"key": "value"
	},
	"PtrMapNonEmpty": {
		"key": "value"
	},
	"SliceNonEmpty": [
		"value"
	],
	"PtrSliceNonEmpty": [
		"value"
	]
}`,
	}, {
		name:  "Structs/OmitEmpty/NonEmpty",
		eopts: EncodeOptions{Indent: "\t"},
		in: structOmitEmptyAll{
			Bool:              true,
			PtrBool:           addr(true).(*bool),
			String:            string("value"),
			StringEmpty:       stringMarshalEmpty("value"),
			StringNonEmpty:    stringMarshalNonEmpty("value"),
			PtrString:         addr(string("value")).(*string),
			PtrStringEmpty:    addr(stringMarshalEmpty("value")).(*stringMarshalEmpty),
			PtrStringNonEmpty: addr(stringMarshalNonEmpty("value")).(*stringMarshalNonEmpty),
			Bytes:             []byte("value"),
			BytesEmpty:        bytesMarshalEmpty([]byte("value")),
			BytesNonEmpty:     bytesMarshalNonEmpty([]byte("value")),
			PtrBytes:          addr([]byte("value")).(*[]byte),
			PtrBytesEmpty:     addr(bytesMarshalEmpty([]byte("value"))).(*bytesMarshalEmpty),
			PtrBytesNonEmpty:  addr(bytesMarshalNonEmpty([]byte("value"))).(*bytesMarshalNonEmpty),
			Float:             math.Copysign(0, -1),
			PtrFloat:          addr(math.Copysign(0, -1)).(*float64),
			Map:               map[string]string{"": ""},
			MapEmpty:          mapMarshalEmpty{"key": "value"},
			MapNonEmpty:       mapMarshalNonEmpty{"key": "value"},
			PtrMap:            addr(map[string]string{"": ""}).(*map[string]string),
			PtrMapEmpty:       addr(mapMarshalEmpty{"key": "value"}).(*mapMarshalEmpty),
			PtrMapNonEmpty:    addr(mapMarshalNonEmpty{"key": "value"}).(*mapMarshalNonEmpty),
			Slice:             []string{""},
			SliceEmpty:        sliceMarshalEmpty{"value"},
			SliceNonEmpty:     sliceMarshalNonEmpty{"value"},
			PtrSlice:          addr([]string{""}).(*[]string),
			PtrSliceEmpty:     addr(sliceMarshalEmpty{"value"}).(*sliceMarshalEmpty),
			PtrSliceNonEmpty:  addr(sliceMarshalNonEmpty{"value"}).(*sliceMarshalNonEmpty),
			Ptr:               &structOmitZeroEmptyAll{Float: math.Copysign(0, -1)},
			Interface:         []string{""},
		},
		want: `{
	"Bool": true,
	"PtrBool": true,
	"String": "value",
	"StringNonEmpty": "value",
	"PtrString": "value",
	"PtrStringNonEmpty": "value",
	"Bytes": "dmFsdWU=",
	"BytesNonEmpty": [
		"value"
	],
	"PtrBytes": "dmFsdWU=",
	"PtrBytesNonEmpty": [
		"value"
	],
	"Float": -0,
	"PtrFloat": -0,
	"Map": {
		"": ""
	},
	"MapNonEmpty": {
		"key": "value"
	},
	"PtrMap": {
		"": ""
	},
	"PtrMapNonEmpty": {
		"key": "value"
	},
	"Slice": [
		""
	],
	"SliceNonEmpty": [
		"value"
	],
	"PtrSlice": [
		""
	],
	"PtrSliceNonEmpty": [
		"value"
	],
	"Ptr": {
		"Float": -0
	},
	"Interface": [
		""
	]
}`,
	}, {
		name: "Structs/OmitZeroEmpty/Zero",
		in:   structOmitZeroEmptyAll{},
		want: `{}`,
	}, {
		name: "Structs/OmitZeroEmpty/Empty",
		in: structOmitZeroEmptyAll{
			Bytes:     []byte{},
			Map:       map[string]string{},
			Slice:     []string{},
			Ptr:       &structOmitZeroEmptyAll{},
			Interface: []string{},
		},
		want: `{}`,
	}, {
		name: "Structs/OmitEmpty/PathologicalDepth",
		in: func() interface{} {
			type X struct {
				X *X `json:",omitempty"`
			}
			var make func(int) *X
			make = func(n int) *X {
				if n == 0 {
					return nil
				}
				return &X{make(n - 1)}
			}
			return make(100)
		}(),
		want:      `{}`,
		useWriter: true,
	}, {
		name: "Structs/OmitEmpty/PathologicalBreadth",
		in: func() interface{} {
			var fields []reflect.StructField
			for i := 0; i < 100; i++ {
				fields = append(fields, reflect.StructField{
					Name: fmt.Sprintf("X%d", i),
					Type: reflect.TypeOf(stringMarshalEmpty("")),
					Tag:  `json:",omitempty"`,
				})
			}
			return reflect.New(reflect.StructOf(fields)).Interface()
		}(),
		want:      `{}`,
		useWriter: true,
	}, {
		name: "Structs/OmitEmpty/PathologicalTree",
		in: func() interface{} {
			type X struct {
				XL, XR *X `json:",omitempty"`
			}
			var make func(int) *X
			make = func(n int) *X {
				if n == 0 {
					return nil
				}
				return &X{make(n - 1), make(n - 1)}
			}
			return make(8)
		}(),
		want:      `{}`,
		useWriter: true,
	}, {
		name: "Structs/OmitZeroEmpty/NonEmpty",
		in: structOmitZeroEmptyAll{
			Bytes:     []byte("value"),
			Map:       map[string]string{"": ""},
			Slice:     []string{""},
			Ptr:       &structOmitZeroEmptyAll{Bool: true},
			Interface: []string{""},
		},
		want: `{"Bytes":"dmFsdWU=","Map":{"":""},"Slice":[""],"Ptr":{"Bool":true},"Interface":[""]}`,
	}, {
		name:  "Structs/Format/Bytes",
		eopts: EncodeOptions{Indent: "\t"},
		in: structFormatBytes{
			Base16:    []byte("\x01\x23\x45\x67\x89\xab\xcd\xef"),
			Base32:    []byte("\x00D2\x14\xc7BT\xb65τe:V\xd7\xc6u\xbew\xdf"),
			Base32Hex: []byte("\x00D2\x14\xc7BT\xb65τe:V\xd7\xc6u\xbew\xdf"),
			Base64:    []byte("\x00\x10\x83\x10Q\x87 \x92\x8b0ӏA\x14\x93QU\x97a\x96\x9bqן\x82\x18\xa3\x92Y\xa7\xa2\x9a\xab\xb2ۯ\xc3\x1c\xb3\xd3]\xb7㞻\xf3߿"),
			Base64URL: []byte("\x00\x10\x83\x10Q\x87 \x92\x8b0ӏA\x14\x93QU\x97a\x96\x9bqן\x82\x18\xa3\x92Y\xa7\xa2\x9a\xab\xb2ۯ\xc3\x1c\xb3\xd3]\xb7㞻\xf3߿"),
			UintArray: []byte{1, 2, 3, 4},
		},
		want: `{
	"Base16": "0123456789abcdef",
	"Base32": "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567",
	"Base32Hex": "0123456789ABCDEFGHIJKLMNOPQRSTUV",
	"Base64": "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
	"Base64URL": "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",
	"UintArray": [
		1,
		2,
		3,
		4
	]
}`,
	}, {
		name:  "Structs/Format/Floats",
		eopts: EncodeOptions{Indent: "\t"},
		in: []structFormatFloats{
			{NonFinite: math.Pi, PtrNonFinite: addr(math.Pi).(*float64)},
			{NonFinite: math.NaN(), PtrNonFinite: addr(math.NaN()).(*float64)},
			{NonFinite: math.Inf(-1), PtrNonFinite: addr(math.Inf(-1)).(*float64)},
			{NonFinite: math.Inf(+1), PtrNonFinite: addr(math.Inf(+1)).(*float64)},
		},
		want: `[
	{
		"NonFinite": 3.141592653589793,
		"PtrNonFinite": 3.141592653589793
	},
	{
		"NonFinite": "NaN",
		"PtrNonFinite": "NaN"
	},
	{
		"NonFinite": "-Infinity",
		"PtrNonFinite": "-Infinity"
	},
	{
		"NonFinite": "Infinity",
		"PtrNonFinite": "Infinity"
	}
]`,
	}, {
		name:  "Structs/Format/Maps",
		eopts: EncodeOptions{Indent: "\t"},
		in: []structFormatMaps{
			{EmitNull: nil, PtrEmitNull: new(map[string]string)},
			{EmitNull: map[string]string{}, PtrEmitNull: addr(map[string]string{}).(*map[string]string)},
			{EmitNull: map[string]string{"k": "v"}, PtrEmitNull: addr(map[string]string{"k": "v"}).(*map[string]string)},
		},
		want: `[
	{
		"EmitNull": null,
		"PtrEmitNull": null
	},
	{
		"EmitNull": {},
		"PtrEmitNull": {}
	},
	{
		"EmitNull": {
			"k": "v"
		},
		"PtrEmitNull": {
			"k": "v"
		}
	}
]`,
	}, {
		name:  "Structs/Format/Slices",
		eopts: EncodeOptions{Indent: "\t"},
		in: []structFormatSlices{
			{EmitNull: nil, PtrEmitNull: new([]string)},
			{EmitNull: []string{}, PtrEmitNull: addr([]string{}).(*[]string)},
			{EmitNull: []string{"v"}, PtrEmitNull: addr([]string{"v"}).(*[]string)},
		},
		want: `[
	{
		"EmitNull": null,
		"PtrEmitNull": null
	},
	{
		"EmitNull": [],
		"PtrEmitNull": []
	},
	{
		"EmitNull": [
			"v"
		],
		"PtrEmitNull": [
			"v"
		]
	}
]`,
	}, {
		name:    "Structs/Format/Invalid/Bool",
		in:      structFormatInvalid{Bool: true},
		want:    `{"Bool"`,
		wantErr: &SemanticError{action: "marshal", GoType: boolType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/String",
		in:      structFormatInvalid{String: "string"},
		want:    `{"String"`,
		wantErr: &SemanticError{action: "marshal", GoType: stringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Bytes",
		in:      structFormatInvalid{Bytes: []byte("bytes")},
		want:    `{"Bytes"`,
		wantErr: &SemanticError{action: "marshal", GoType: bytesType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Int",
		in:      structFormatInvalid{Int: 1},
		want:    `{"Int"`,
		wantErr: &SemanticError{action: "marshal", GoType: int64Type, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Uint",
		in:      structFormatInvalid{Uint: 1},
		want:    `{"Uint"`,
		wantErr: &SemanticError{action: "marshal", GoType: uint64Type, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Float",
		in:      structFormatInvalid{Float: 1},
		want:    `{"Float"`,
		wantErr: &SemanticError{action: "marshal", GoType: float64Type, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Map",
		in:      structFormatInvalid{Map: map[string]string{}},
		want:    `{"Map"`,
		wantErr: &SemanticError{action: "marshal", GoType: mapStringStringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Struct",
		in:      structFormatInvalid{Struct: structAll{Bool: true}},
		want:    `{"Struct"`,
		wantErr: &SemanticError{action: "marshal", GoType: structAllType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Slice",
		in:      structFormatInvalid{Slice: []string{}},
		want:    `{"Slice"`,
		wantErr: &SemanticError{action: "marshal", GoType: sliceStringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Array",
		in:      structFormatInvalid{Array: [1]string{"string"}},
		want:    `{"Array"`,
		wantErr: &SemanticError{action: "marshal", GoType: array1StringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Interface",
		in:      structFormatInvalid{Interface: "anything"},
		want:    `{"Interface"`,
		wantErr: &SemanticError{action: "marshal", GoType: emptyInterfaceType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name: "Structs/Inline/Zero",
		in:   structInlined{},
		want: `{"D":""}`,
	}, {
		name: "Structs/Inline/Alloc",
		in: structInlined{
			X: structInlinedL1{
				X:            &structInlinedL2{},
				StructEmbed1: StructEmbed1{},
			},
			StructEmbed2: &StructEmbed2{},
		},
		want: `{"E":"","F":"","G":"","A":"","B":"","D":""}`,
	}, {
		name: "Structs/Inline/NonZero",
		in: structInlined{
			X: structInlinedL1{
				X:            &structInlinedL2{A: "A1", B: "B1", C: "C1"},
				StructEmbed1: StructEmbed1{C: "C2", D: "D2", E: "E2"},
			},
			StructEmbed2: &StructEmbed2{E: "E3", F: "F3", G: "G3"},
		},
		want: `{"E":"E3","F":"F3","G":"G3","A":"A1","B":"B1","D":"D2"}`,
	}, {
		name: "Structs/InlinedFallback/RawValue/Nil",
		in:   structInlineRawValue{X: RawValue(nil)},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/RawValue/Empty",
		in:   structInlineRawValue{X: RawValue("")},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/RawValue/NonEmptyN1",
		in:   structInlineRawValue{X: RawValue(` { "fizz" : "buzz" } `)},
		want: `{"fizz":"buzz"}`,
	}, {
		name: "Structs/InlinedFallback/RawValue/NonEmptyN2",
		in:   structInlineRawValue{X: RawValue(` { "fizz" : "buzz" , "foo" : "bar" } `)},
		want: `{"fizz":"buzz","foo":"bar"}`,
	}, {
		name: "Structs/InlinedFallback/RawValue/NonEmptyWithOthers",
		in: structInlineRawValue{
			A: 1,
			X: RawValue(` { "fizz" : "buzz" , "foo" : "bar" } `),
			B: 2,
		},
		// NOTE: Inlined fallback fields are always serialized last.
		want: `{"A":1,"B":2,"fizz":"buzz","foo":"bar"}`,
	}, {
		name:    "Structs/InlinedFallback/RawValue/RejectDuplicateNames",
		eopts:   EncodeOptions{AllowDuplicateNames: false},
		in:      structInlineRawValue{X: RawValue(` { "fizz" : "buzz" , "fizz" : "buzz" } `)},
		want:    `{"fizz":"buzz"`,
		wantErr: &SyntacticError{str: `duplicate name "fizz" in object`},
	}, {
		name:  "Structs/InlinedFallback/RawValue/AllowDuplicateNames",
		eopts: EncodeOptions{AllowDuplicateNames: true},
		in:    structInlineRawValue{X: RawValue(` { "fizz" : "buzz" , "fizz" : "buzz" } `)},
		want:  `{"fizz":"buzz","fizz":"buzz"}`,
	}, {
		name:    "Structs/InlinedFallback/RawValue/RejectInvalidUTF8",
		eopts:   EncodeOptions{AllowInvalidUTF8: false},
		in:      structInlineRawValue{X: RawValue(`{"` + "\xde\xad\xbe\xef" + `":"value"}`)},
		want:    `{`,
		wantErr: &SyntacticError{str: "invalid UTF-8 within string"},
	}, {
		name:  "Structs/InlinedFallback/RawValue/AllowInvalidUTF8",
		eopts: EncodeOptions{AllowInvalidUTF8: true},
		in:    structInlineRawValue{X: RawValue(`{"` + "\xde\xad\xbe\xef" + `":"value"}`)},
		want:  `{"ޭ��":"value"}`,
	}, {
		name:    "Structs/InlinedFallback/RawValue/InvalidWhitespace",
		in:      structInlineRawValue{X: RawValue("\n\r\t ")},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: rawValueType, Err: io.EOF},
	}, {
		name:    "Structs/InlinedFallback/RawValue/InvalidObject",
		in:      structInlineRawValue{X: RawValue(` true `)},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", JSONKind: 't', GoType: rawValueType, Err: errors.New("inlined raw value must be a JSON object")},
	}, {
		name:    "Structs/InlinedFallback/RawValue/InvalidObjectName",
		in:      structInlineRawValue{X: RawValue(` { true : false } `)},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: rawValueType, Err: errMissingName.withOffset(int64(len(" { ")))},
	}, {
		name:    "Structs/InlinedFallback/RawValue/InvalidObjectEnd",
		in:      structInlineRawValue{X: RawValue(` { "name" : false , } `)},
		want:    `{"name":false`,
		wantErr: &SemanticError{action: "marshal", GoType: rawValueType, Err: newInvalidCharacterError(',', "before next token").withOffset(int64(len(` { "name" : false `)))},
	}, {
		name:    "Structs/InlinedFallback/RawValue/InvalidDualObject",
		in:      structInlineRawValue{X: RawValue(`{}{}`)},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: rawValueType, Err: newInvalidCharacterError('{', "after top-level value")},
	}, {
		name: "Structs/InlinedFallback/RawValue/Nested/Nil",
		in:   structInlinePointerInlineRawValue{},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/PointerRawValue/Nil",
		in:   structInlinePointerRawValue{},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/PointerRawValue/NonEmpty",
		in:   structInlinePointerRawValue{X: addr(RawValue(` { "fizz" : "buzz" } `)).(*RawValue)},
		want: `{"fizz":"buzz"}`,
	}, {
		name: "Structs/InlinedFallback/PointerRawValue/Nested/Nil",
		in:   structInlineInlinePointerRawValue{},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/MapStringAny/Nil",
		in:   structInlineMapStringAny{X: nil},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/MapStringAny/Empty",
		in:   structInlineMapStringAny{X: make(jsonObject)},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/MapStringAny/NonEmptyN1",
		in:   structInlineMapStringAny{X: jsonObject{"fizz": nil}},
		want: `{"fizz":null}`,
	}, {
		name:         "Structs/InlinedFallback/MapStringAny/NonEmptyN2",
		in:           structInlineMapStringAny{X: jsonObject{"fizz": time.Time{}, "buzz": math.Pi}},
		want:         `{"buzz":3.141592653589793,"fizz":"0001-01-01T00:00:00Z"}`,
		canonicalize: true,
	}, {
		name: "Structs/InlinedFallback/MapStringAny/NonEmptyWithOthers",
		in: structInlineMapStringAny{
			A: 1,
			X: jsonObject{"fizz": nil},
			B: 2,
		},
		// NOTE: Inlined fallback fields are always serialized last.
		want: `{"A":1,"B":2,"fizz":null}`,
	}, {
		name:    "Structs/InlinedFallback/MapStringAny/RejectInvalidUTF8",
		eopts:   EncodeOptions{AllowInvalidUTF8: false},
		in:      structInlineMapStringAny{X: jsonObject{"\xde\xad\xbe\xef": nil}},
		want:    `{`,
		wantErr: &SyntacticError{str: "invalid UTF-8 within string"},
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/AllowInvalidUTF8",
		eopts: EncodeOptions{AllowInvalidUTF8: true},
		in:    structInlineMapStringAny{X: jsonObject{"\xde\xad\xbe\xef": nil}},
		want:  `{"ޭ��":null}`,
	}, {
		name:    "Structs/InlinedFallback/MapStringAny/InvalidValue",
		eopts:   EncodeOptions{AllowInvalidUTF8: true},
		in:      structInlineMapStringAny{X: jsonObject{"name": make(chan string)}},
		want:    `{"name"`,
		wantErr: &SemanticError{action: "marshal", GoType: chanStringType},
	}, {
		name: "Structs/InlinedFallback/MapStringAny/Nested/Nil",
		in:   structInlinePointerInlineMapStringAny{},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/PointerMapStringAny/Nil",
		in:   structInlinePointerMapStringAny{X: nil},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/PointerMapStringAny/NonEmpty",
		in:   structInlinePointerMapStringAny{X: addr(jsonObject{"name": "value"}).(*jsonObject)},
		want: `{"name":"value"}`,
	}, {
		name: "Structs/InlinedFallback/PointerMapStringAny/Nested/Nil",
		in:   structInlineInlinePointerMapStringAny{},
		want: `{}`,
	}, {
		name: "Structs/InlinedFallback/MapStringInt",
		in: structInlineMapStringInt{
			X: map[string]int{"zero": 0, "one": 1, "two": 2},
		},
		want:         `{"one":1,"two":2,"zero":0}`,
		canonicalize: true,
	}, {
		name:  "Structs/InlinedFallback/MapStringInt/StringifiedNumbers",
		mopts: MarshalOptions{StringifyNumbers: true},
		in: structInlineMapStringInt{
			X: map[string]int{"zero": 0, "one": 1, "two": 2},
		},
		want:         `{"one":"1","two":"2","zero":"0"}`,
		canonicalize: true,
	}, {
		name:  "Structs/InlinedFallback/DiscardUnknownMembers",
		mopts: MarshalOptions{DiscardUnknownMembers: true},
		in: structInlineRawValue{
			A: 1,
			X: RawValue(` { "fizz" : "buzz" } `),
			B: 2,
		},
		// NOTE: DiscardUnknownMembers has no effect since this is "inline".
		want: `{"A":1,"B":2,"fizz":"buzz"}`,
	}, {
		name:  "Structs/UnknownFallback/DiscardUnknownMembers",
		mopts: MarshalOptions{DiscardUnknownMembers: true},
		in: structUnknownRawValue{
			A: 1,
			X: RawValue(` { "fizz" : "buzz" } `),
			B: 2,
		},
		want: `{"A":1,"B":2}`,
	}, {
		name: "Structs/UnknownFallback",
		in: structUnknownRawValue{
			A: 1,
			X: RawValue(` { "fizz" : "buzz" } `),
			B: 2,
		},
		want: `{"A":1,"B":2,"fizz":"buzz"}`,
	}, {
		name:    "Structs/Invalid/Conflicting",
		in:      structConflicting{},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: structConflictingType, Err: errors.New("Go struct fields A and B conflict over JSON object name \"conflict\"")},
	}, {
		name:    "Structs/Invalid/NoneExported",
		in:      structNoneExported{},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: structNoneExportedType, Err: errors.New("Go struct kind has no exported fields")},
	}, {
		name:    "Structs/Invalid/MalformedTag",
		in:      structMalformedTag{},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: structMalformedTagType, Err: errors.New("Go struct field Malformed has malformed `json` tag: invalid character '\"' at start of option (expecting Unicode letter or single quote)")},
	}, {
		name:    "Structs/Invalid/UnexportedTag",
		in:      structUnexportedTag{},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: structUnexportedTagType, Err: errors.New("unexported Go struct field unexported cannot have non-ignored `json:\"name\"` tag")},
	}, {
		name:    "Structs/Invalid/UnexportedEmbedded",
		in:      structUnexportedEmbedded{},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: structUnexportedEmbeddedType, Err: errors.New("embedded Go struct field namedString of an unexported type must be explicitly ignored with a `json:\"-\"` tag")},
	}, {
		name: "Slices/Interface",
		in: []interface{}{
			false, true,
			"hello", []byte("world"),
			int32(-32), namedInt64(-64),
			uint32(+32), namedUint64(+64),
			float32(32.32), namedFloat64(64.64),
		},
		want: `[false,true,"hello","d29ybGQ=",-32,-64,32,64,32.32,64.64]`,
	}, {
		name:    "Slices/Invalid/Channel",
		in:      [](chan string){nil},
		want:    `[`,
		wantErr: &SemanticError{action: "marshal", GoType: chanStringType},
	}, {
		name: "Slices/RecursiveSlice",
		in: recursiveSlice{
			nil,
			{},
			{nil},
			{nil, {}},
		},
		want: `[[],[],[[]],[[],[]]]`,
	}, {
		name: "Slices/CyclicSlice",
		in: func() recursiveSlice {
			s := recursiveSlice{{}}
			s[0] = s
			return s
		}(),
		want:    strings.Repeat(`[`, startDetectingCyclesAfter) + `[`,
		wantErr: &SemanticError{action: "marshal", GoType: reflect.TypeOf(recursiveSlice{}), Err: errors.New("encountered a cycle")},
	}, {
		name: "Arrays/Empty",
		in:   [0]struct{}{},
		want: `[]`,
	}, {
		name: "Arrays/Bool",
		in:   [2]bool{false, true},
		want: `[false,true]`,
	}, {
		name: "Arrays/String",
		in:   [2]string{"hello", "goodbye"},
		want: `["hello","goodbye"]`,
	}, {
		name: "Arrays/Bytes",
		in:   [2][]byte{[]byte("hello"), []byte("goodbye")},
		want: `["aGVsbG8=","Z29vZGJ5ZQ=="]`,
	}, {
		name: "Arrays/Int",
		in:   [2]int64{math.MinInt64, math.MaxInt64},
		want: `[-9223372036854775808,9223372036854775807]`,
	}, {
		name: "Arrays/Uint",
		in:   [2]uint64{0, math.MaxUint64},
		want: `[0,18446744073709551615]`,
	}, {
		name: "Arrays/Float",
		in:   [2]float64{-math.MaxFloat64, +math.MaxFloat64},
		want: `[-1.7976931348623157e+308,1.7976931348623157e+308]`,
	}, {
		name:    "Arrays/Invalid/Channel",
		in:      new([1]chan string),
		want:    `[`,
		wantErr: &SemanticError{action: "marshal", GoType: chanStringType},
	}, {
		name: "Pointers/NilL0",
		in:   (*int)(nil),
		want: `null`,
	}, {
		name: "Pointers/NilL1",
		in:   (**int)(new(*int)),
		want: `null`,
	}, {
		name: "Pointers/Bool",
		in:   addr(addr(bool(true))),
		want: `true`,
	}, {
		name: "Pointers/String",
		in:   addr(addr(string("string"))),
		want: `"string"`,
	}, {
		name: "Pointers/Bytes",
		in:   addr(addr([]byte("bytes"))),
		want: `"Ynl0ZXM="`,
	}, {
		name: "Pointers/Int",
		in:   addr(addr(int(-100))),
		want: `-100`,
	}, {
		name: "Pointers/Uint",
		in:   addr(addr(uint(100))),
		want: `100`,
	}, {
		name: "Pointers/Float",
		in:   addr(addr(float64(3.14159))),
		want: `3.14159`,
	}, {
		name: "Pointers/CyclicPointer",
		in: func() *recursivePtr {
			p := new(recursivePtr)
			p.P = p
			return p
		}(),
		want:    strings.Repeat(`{"P":`, startDetectingCyclesAfter) + `{"P"`,
		wantErr: &SemanticError{action: "marshal", GoType: reflect.TypeOf((*recursivePtr)(nil)), Err: errors.New("encountered a cycle")},
	}, {
		name: "Interfaces/Nil/Empty",
		in:   [1]interface{}{nil},
		want: `[null]`,
	}, {
		name: "Interfaces/Nil/NonEmpty",
		in:   [1]io.Reader{nil},
		want: `[null]`,
	}, {
		name: "Methods/NilPointer",
		in:   struct{ X *allMethods }{X: (*allMethods)(nil)}, // method should not be called
		want: `{"X":null}`,
	}, {
		// NOTE: Fixes https://github.com/dominikh/go-tools/issues/975.
		name: "Methods/NilInterface",
		in:   struct{ X MarshalerV2 }{X: (*allMethods)(nil)}, // method should not be called
		want: `{"X":null}`,
	}, {
		name: "Methods/AllMethods",
		in:   struct{ X *allMethods }{X: &allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/AllMethodsExceptJSONv2",
		in:   struct{ X *allMethodsExceptJSONv2 }{X: &allMethodsExceptJSONv2{allMethods: allMethods{method: "MarshalJSON", value: []byte(`"hello"`)}}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/AllMethodsExceptJSONv1",
		in:   struct{ X *allMethodsExceptJSONv1 }{X: &allMethodsExceptJSONv1{allMethods: allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)}}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/AllMethodsExceptText",
		in:   struct{ X *allMethodsExceptText }{X: &allMethodsExceptText{allMethods: allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)}}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/OnlyMethodJSONv2",
		in:   struct{ X *onlyMethodJSONv2 }{X: &onlyMethodJSONv2{allMethods: allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)}}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/OnlyMethodJSONv1",
		in:   struct{ X *onlyMethodJSONv1 }{X: &onlyMethodJSONv1{allMethods: allMethods{method: "MarshalJSON", value: []byte(`"hello"`)}}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/OnlyMethodText",
		in:   struct{ X *onlyMethodText }{X: &onlyMethodText{allMethods: allMethods{method: "MarshalText", value: []byte(`hello`)}}},
		want: `{"X":"hello"}`,
	}, {
		name: "Methods/IP",
		in:   net.IPv4(192, 168, 0, 100),
		want: `"192.168.0.100"`,
	}, {
		// NOTE: Fixes https://golang.org/issue/46516.
		name: "Methods/Anonymous",
		in:   struct{ X struct{ allMethods } }{X: struct{ allMethods }{allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)}}},
		want: `{"X":"hello"}`,
	}, {
		// NOTE: Fixes https://golang.org/issue/22967.
		name: "Methods/Addressable",
		in: struct {
			V allMethods
			M map[string]allMethods
			I interface{}
		}{
			V: allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)},
			M: map[string]allMethods{"K": {method: "MarshalNextJSON", value: []byte(`"hello"`)}},
			I: allMethods{method: "MarshalNextJSON", value: []byte(`"hello"`)},
		},
		want: `{"V":"hello","M":{"K":"hello"},"I":"hello"}`,
	}, {
		// NOTE: Fixes https://golang.org/issue/29732.
		name:         "Methods/MapKey/JSONv2",
		in:           map[structMethodJSONv2]string{{"k1"}: "v1", {"k2"}: "v2"},
		want:         `{"k1":"v1","k2":"v2"}`,
		canonicalize: true,
	}, {
		// NOTE: Fixes https://golang.org/issue/29732.
		name:         "Methods/MapKey/JSONv1",
		in:           map[structMethodJSONv1]string{{"k1"}: "v1", {"k2"}: "v2"},
		want:         `{"k1":"v1","k2":"v2"}`,
		canonicalize: true,
	}, {
		name:         "Methods/MapKey/Text",
		in:           map[structMethodText]string{{"k1"}: "v1", {"k2"}: "v2"},
		want:         `{"k1":"v1","k2":"v2"}`,
		canonicalize: true,
	}, {
		name: "Methods/Invalid/JSONv2/Error",
		in: marshalJSONv2Func(func(*Encoder, MarshalOptions) error {
			return errors.New("some error")
		}),
		wantErr: &SemanticError{action: "marshal", GoType: marshalJSONv2FuncType, Err: errors.New("some error")},
	}, {
		name: "Methods/Invalid/JSONv2/TooFew",
		in: marshalJSONv2Func(func(*Encoder, MarshalOptions) error {
			return nil // do nothing
		}),
		wantErr: &SemanticError{action: "marshal", GoType: marshalJSONv2FuncType, Err: errors.New("must write exactly one JSON value")},
	}, {
		name: "Methods/Invalid/JSONv2/TooMany",
		in: marshalJSONv2Func(func(enc *Encoder, mo MarshalOptions) error {
			enc.WriteToken(Null)
			enc.WriteToken(Null)
			return nil
		}),
		want:    `nullnull`,
		wantErr: &SemanticError{action: "marshal", GoType: marshalJSONv2FuncType, Err: errors.New("must write exactly one JSON value")},
	}, {
		name: "Methods/Invalid/JSONv1/Error",
		in: marshalJSONv1Func(func() ([]byte, error) {
			return nil, errors.New("some error")
		}),
		wantErr: &SemanticError{action: "marshal", GoType: marshalJSONv1FuncType, Err: errors.New("some error")},
	}, {
		name: "Methods/Invalid/JSONv1/Syntax",
		in: marshalJSONv1Func(func() ([]byte, error) {
			return []byte("invalid"), nil
		}),
		wantErr: &SemanticError{action: "marshal", JSONKind: 'i', GoType: marshalJSONv1FuncType, Err: newInvalidCharacterError('i', "at start of value")},
	}, {
		name: "Methods/Invalid/Text/Error",
		in: marshalTextFunc(func() ([]byte, error) {
			return nil, errors.New("some error")
		}),
		wantErr: &SemanticError{action: "marshal", JSONKind: '"', GoType: marshalTextFuncType, Err: errors.New("some error")},
	}, {
		name: "Methods/Invalid/Text/UTF8",
		in: marshalTextFunc(func() ([]byte, error) {
			return []byte("\xde\xad\xbe\xef"), nil
		}),
		wantErr: &SemanticError{action: "marshal", JSONKind: '"', GoType: marshalTextFuncType, Err: &SyntacticError{str: "invalid UTF-8 within string"}},
	}, {
		name: "Methods/Invalid/MapKey/JSONv2/Syntax",
		in: map[interface{}]string{
			addr(marshalJSONv2Func(func(enc *Encoder, mo MarshalOptions) error {
				return enc.WriteToken(Null)
			})): "invalid",
		},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", GoType: marshalJSONv2FuncType, Err: errMissingName},
	}, {
		name: "Methods/Invalid/MapKey/JSONv1/Syntax",
		in: map[interface{}]string{
			addr(marshalJSONv1Func(func() ([]byte, error) {
				return []byte(`null`), nil
			})): "invalid",
		},
		want:    `{`,
		wantErr: &SemanticError{action: "marshal", JSONKind: 'n', GoType: marshalJSONv1FuncType, Err: errMissingName},
	}, {
		name: "Duration/Zero",
		in: struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{0, 0},
		want: `{"D1":"0s","D2":0}`,
	}, {
		name: "Duration/Positive",
		in: struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{
			123456789123456789,
			123456789123456789,
		},
		want: `{"D1":"34293h33m9.123456789s","D2":123456789123456789}`,
	}, {
		name: "Duration/Negative",
		in: struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{
			-123456789123456789,
			-123456789123456789,
		},
		want: `{"D1":"-34293h33m9.123456789s","D2":-123456789123456789}`,
	}, {
		name: "Duration/Format/Invalid",
		in: struct {
			D time.Duration `json:",format:invalid"`
		}{},
		want:    `{"D"`,
		wantErr: &SemanticError{action: "marshal", GoType: timeDurationType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name: "Time/Zero",
		in: struct {
			T1 time.Time
			T2 time.Time `json:",format:RFC822"`
			T3 time.Time `json:",format:'2006-01-02'"`
			T4 time.Time `json:",omitzero"`
			T5 time.Time `json:",omitempty"`
		}{
			time.Time{},
			time.Time{},
			time.Time{},
			// This is zero according to time.Time.IsZero,
			// but non-zero according to reflect.Value.IsZero.
			time.Date(1, 1, 1, 0, 0, 0, 0, time.FixedZone("UTC", 0)),
			time.Time{},
		},
		want: `{"T1":"0001-01-01T00:00:00Z","T2":"01 Jan 01 00:00 UTC","T3":"0001-01-01","T5":"0001-01-01T00:00:00Z"}`,
	}, {
		name:  "Time/Format",
		eopts: EncodeOptions{Indent: "\t"},
		in: structTimeFormat{
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
			time.Date(1234, 1, 2, 3, 4, 5, 6, time.UTC),
		},
		want: `{
	"T1": "1234-01-02T03:04:05.000000006Z",
	"T2": "Mon Jan  2 03:04:05 1234",
	"T3": "Mon Jan  2 03:04:05 UTC 1234",
	"T4": "Mon Jan 02 03:04:05 +0000 1234",
	"T5": "02 Jan 34 03:04 UTC",
	"T6": "02 Jan 34 03:04 +0000",
	"T7": "Monday, 02-Jan-34 03:04:05 UTC",
	"T8": "Mon, 02 Jan 1234 03:04:05 UTC",
	"T9": "Mon, 02 Jan 1234 03:04:05 +0000",
	"T10": "1234-01-02T03:04:05Z",
	"T11": "1234-01-02T03:04:05.000000006Z",
	"T12": "3:04AM",
	"T13": "Jan  2 03:04:05",
	"T14": "Jan  2 03:04:05.000",
	"T15": "Jan  2 03:04:05.000000",
	"T16": "Jan  2 03:04:05.000000006",
	"T17": "1234-01-02",
	"T18": "\"weird\"1234"
}`,
	}, {
		name: "Time/Format/Invalid",
		in: struct {
			T time.Time `json:",format:UndefinedConstant"`
		}{},
		want:    `{"T"`,
		wantErr: &SemanticError{action: "marshal", GoType: timeTimeType, Err: errors.New(`undefined format layout: UndefinedConstant`)},
	}, {
		name: "Time/Format/Overflow",
		in: struct {
			T1 time.Time
			T2 time.Time
		}{
			time.Date(10000, 1, 1, 0, 0, 0, 0, time.UTC).Add(-time.Second),
			time.Date(10000, 1, 1, 0, 0, 0, 0, time.UTC),
		},
		want:    `{"T1":"9999-12-31T23:59:59Z","T2"`,
		wantErr: &SemanticError{action: "marshal", GoType: timeTimeType, Err: errors.New(`year 10000 outside of range [0,9999]`)},
	}, {
		name: "Time/Format/Underflow",
		in: struct {
			T1 time.Time
			T2 time.Time
		}{
			time.Date(0, 1, 1, 0, 0, 0, 0, time.UTC),
			time.Date(0, 1, 1, 0, 0, 0, 0, time.UTC).Add(-time.Second),
		},
		want:    `{"T1":"0000-01-01T00:00:00Z","T2"`,
		wantErr: &SemanticError{action: "marshal", GoType: timeTimeType, Err: errors.New(`year -1 outside of range [0,9999]`)},
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var got []byte
			var gotErr error
			if tt.useWriter {
				bb := new(struct{ bytes.Buffer }) // avoid optimizations with bytes.Buffer
				gotErr = tt.mopts.MarshalFull(tt.eopts, bb, tt.in)
				got = bb.Bytes()
			} else {
				got, gotErr = tt.mopts.Marshal(tt.eopts, tt.in)
			}
			if tt.canonicalize {
				(*RawValue)(&got).Canonicalize()
			}
			if string(got) != tt.want {
				t.Errorf("Marshal output mismatch:\ngot  %s\nwant %s", got, tt.want)
			}
			if !reflect.DeepEqual(gotErr, tt.wantErr) {
				t.Errorf("Marshal error mismatch:\ngot  %v\nwant %v", gotErr, tt.wantErr)
			}
		})
	}
}

func TestUnmarshal(t *testing.T) {
	tests := []struct {
		name    string
		dopts   DecodeOptions
		uopts   UnmarshalOptions
		inBuf   string
		inVal   interface{}
		want    interface{}
		wantErr error
	}{{
		name:    "Nil",
		inBuf:   `null`,
		wantErr: &SemanticError{action: "unmarshal", Err: errors.New("value must be passed as a non-nil pointer reference")},
	}, {
		name:    "NilPointer",
		inBuf:   `null`,
		inVal:   (*string)(nil),
		want:    (*string)(nil),
		wantErr: &SemanticError{action: "unmarshal", GoType: stringType, Err: errors.New("value must be passed as a non-nil pointer reference")},
	}, {
		name:    "NonPointer",
		inBuf:   `null`,
		inVal:   "unchanged",
		want:    "unchanged",
		wantErr: &SemanticError{action: "unmarshal", GoType: stringType, Err: errors.New("value must be passed as a non-nil pointer reference")},
	}, {
		name:    "Bools/TrailingJunk",
		inBuf:   `falsetrue`,
		inVal:   addr(true),
		want:    addr(false),
		wantErr: newInvalidCharacterError('t', "after top-level value"),
	}, {
		name:  "Bools/Null",
		inBuf: `null`,
		inVal: addr(true),
		want:  addr(false),
	}, {
		name:  "Bools",
		inBuf: `[null,false,true]`,
		inVal: new([]bool),
		want:  addr([]bool{false, false, true}),
	}, {
		name:  "Bools/Named",
		inBuf: `[null,false,true]`,
		inVal: new([]namedBool),
		want:  addr([]namedBool{false, false, true}),
	}, {
		name:    "Bools/Invalid/StringifiedFalse",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"false"`,
		inVal:   addr(true),
		want:    addr(true),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: boolType},
	}, {
		name:    "Bools/Invalid/StringifiedTrue",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"true"`,
		inVal:   addr(true),
		want:    addr(true),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: boolType},
	}, {
		name:    "Bools/Invalid/Number",
		inBuf:   `0`,
		inVal:   addr(true),
		want:    addr(true),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: boolType},
	}, {
		name:    "Bools/Invalid/String",
		inBuf:   `""`,
		inVal:   addr(true),
		want:    addr(true),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: boolType},
	}, {
		name:    "Bools/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr(true),
		want:    addr(true),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: boolType},
	}, {
		name:    "Bools/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr(true),
		want:    addr(true),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: boolType},
	}, {
		name:  "Strings/Null",
		inBuf: `null`,
		inVal: addr("something"),
		want:  addr(""),
	}, {
		name:  "Strings",
		inBuf: `[null,"","hello","世界"]`,
		inVal: new([]string),
		want:  addr([]string{"", "", "hello", "世界"}),
	}, {
		name:  "Strings/Escaped",
		inBuf: `[null,"","\u0068\u0065\u006c\u006c\u006f","\u4e16\u754c"]`,
		inVal: new([]string),
		want:  addr([]string{"", "", "hello", "世界"}),
	}, {
		name:  "Strings/Named",
		inBuf: `[null,"","hello","世界"]`,
		inVal: new([]namedString),
		want:  addr([]namedString{"", "", "hello", "世界"}),
	}, {
		name:    "Strings/Invalid/False",
		inBuf:   `false`,
		inVal:   addr("nochange"),
		want:    addr("nochange"),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 'f', GoType: stringType},
	}, {
		name:    "Strings/Invalid/True",
		inBuf:   `true`,
		inVal:   addr("nochange"),
		want:    addr("nochange"),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: stringType},
	}, {
		name:    "Strings/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr("nochange"),
		want:    addr("nochange"),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: stringType},
	}, {
		name:    "Strings/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr("nochange"),
		want:    addr("nochange"),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: stringType},
	}, {
		name:  "Bytes/Null",
		inBuf: `null`,
		inVal: addr([]byte("something")),
		want:  addr([]byte(nil)),
	}, {
		name:  "Bytes",
		inBuf: `[null,"","AQ==","AQI=","AQID"]`,
		inVal: new([][]byte),
		want:  addr([][]byte{nil, {}, {1}, {1, 2}, {1, 2, 3}}),
	}, {
		name:  "Bytes/Large",
		inBuf: `"dGhlIHF1aWNrIGJyb3duIGZveCBqdW1wZWQgb3ZlciB0aGUgbGF6eSBkb2cgYW5kIGF0ZSB0aGUgaG9tZXdvcmsgdGhhdCBJIHNwZW50IHNvIG11Y2ggdGltZSBvbi4="`,
		inVal: new([]byte),
		want:  addr([]byte("the quick brown fox jumped over the lazy dog and ate the homework that I spent so much time on.")),
	}, {
		name:  "Bytes/Reuse",
		inBuf: `"AQID"`,
		inVal: addr([]byte("changed")),
		want:  addr([]byte{1, 2, 3}),
	}, {
		name:  "Bytes/Escaped",
		inBuf: `[null,"","\u0041\u0051\u003d\u003d","\u0041\u0051\u0049\u003d","\u0041\u0051\u0049\u0044"]`,
		inVal: new([][]byte),
		want:  addr([][]byte{nil, {}, {1}, {1, 2}, {1, 2, 3}}),
	}, {
		name:  "Bytes/Named",
		inBuf: `[null,"","AQ==","AQI=","AQID"]`,
		inVal: new([]namedBytes),
		want:  addr([]namedBytes{nil, {}, {1}, {1, 2}, {1, 2, 3}}),
	}, {
		name:  "Bytes/NotStringified",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `[null,"","AQ==","AQI=","AQID"]`,
		inVal: new([][]byte),
		want:  addr([][]byte{nil, {}, {1}, {1, 2}, {1, 2, 3}}),
	}, {
		// NOTE: []namedByte is not assignable to []byte,
		// so the following should be treated as a slice of uints.
		name:  "Bytes/Invariant",
		inBuf: `[null,[],[1],[1,2],[1,2,3]]`,
		inVal: new([][]namedByte),
		want:  addr([][]namedByte{nil, {}, {1}, {1, 2}, {1, 2, 3}}),
	}, {
		// NOTE: This differs in behavior from v1,
		// but keeps the representation of slices and arrays more consistent.
		name:  "Bytes/ByteArray",
		inBuf: `"aGVsbG8="`,
		inVal: new([5]byte),
		want:  addr([5]byte{'h', 'e', 'l', 'l', 'o'}),
	}, {
		name:  "Bytes/ByteArray0/Valid",
		inBuf: `""`,
		inVal: new([0]byte),
		want:  addr([0]byte{}),
	}, {
		name:  "Bytes/ByteArray0/Invalid",
		inBuf: `"A"`,
		inVal: new([0]byte),
		want:  addr([0]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array0ByteType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 0), []byte("A"))
			return err
		}()},
	}, {
		name:    "Bytes/ByteArray0/Overflow",
		inBuf:   `"AA=="`,
		inVal:   new([0]byte),
		want:    addr([0]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array0ByteType, Err: errors.New("decoded base64 length of 1 mismatches array length of 0")},
	}, {
		name:  "Bytes/ByteArray1/Valid",
		inBuf: `"AQ=="`,
		inVal: new([1]byte),
		want:  addr([1]byte{1}),
	}, {
		name:  "Bytes/ByteArray1/Invalid",
		inBuf: `"$$=="`,
		inVal: new([1]byte),
		want:  addr([1]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array1ByteType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 1), []byte("$$=="))
			return err
		}()},
	}, {
		name:    "Bytes/ByteArray1/Underflow",
		inBuf:   `""`,
		inVal:   new([1]byte),
		want:    addr([1]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array1ByteType, Err: errors.New("decoded base64 length of 0 mismatches array length of 1")},
	}, {
		name:    "Bytes/ByteArray1/Overflow",
		inBuf:   `"AQI="`,
		inVal:   new([1]byte),
		want:    addr([1]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array1ByteType, Err: errors.New("decoded base64 length of 2 mismatches array length of 1")},
	}, {
		name:  "Bytes/ByteArray2/Valid",
		inBuf: `"AQI="`,
		inVal: new([2]byte),
		want:  addr([2]byte{1, 2}),
	}, {
		name:  "Bytes/ByteArray2/Invalid",
		inBuf: `"$$$="`,
		inVal: new([2]byte),
		want:  addr([2]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array2ByteType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 2), []byte("$$$="))
			return err
		}()},
	}, {
		name:    "Bytes/ByteArray2/Underflow",
		inBuf:   `"AQ=="`,
		inVal:   new([2]byte),
		want:    addr([2]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array2ByteType, Err: errors.New("decoded base64 length of 1 mismatches array length of 2")},
	}, {
		name:    "Bytes/ByteArray2/Overflow",
		inBuf:   `"AQID"`,
		inVal:   new([2]byte),
		want:    addr([2]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array2ByteType, Err: errors.New("decoded base64 length of 3 mismatches array length of 2")},
	}, {
		name:  "Bytes/ByteArray3/Valid",
		inBuf: `"AQID"`,
		inVal: new([3]byte),
		want:  addr([3]byte{1, 2, 3}),
	}, {
		name:  "Bytes/ByteArray3/Invalid",
		inBuf: `"$$$$"`,
		inVal: new([3]byte),
		want:  addr([3]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array3ByteType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 3), []byte("$$$$"))
			return err
		}()},
	}, {
		name:    "Bytes/ByteArray3/Underflow",
		inBuf:   `"AQI="`,
		inVal:   new([3]byte),
		want:    addr([3]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array3ByteType, Err: errors.New("decoded base64 length of 2 mismatches array length of 3")},
	}, {
		name:    "Bytes/ByteArray3/Overflow",
		inBuf:   `"AQIDAQ=="`,
		inVal:   new([3]byte),
		want:    addr([3]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array3ByteType, Err: errors.New("decoded base64 length of 4 mismatches array length of 3")},
	}, {
		name:  "Bytes/ByteArray4/Valid",
		inBuf: `"AQIDBA=="`,
		inVal: new([4]byte),
		want:  addr([4]byte{1, 2, 3, 4}),
	}, {
		name:  "Bytes/ByteArray4/Invalid",
		inBuf: `"$$$$$$=="`,
		inVal: new([4]byte),
		want:  addr([4]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array4ByteType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 4), []byte("$$$$$$=="))
			return err
		}()},
	}, {
		name:    "Bytes/ByteArray4/Underflow",
		inBuf:   `"AQID"`,
		inVal:   new([4]byte),
		want:    addr([4]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array4ByteType, Err: errors.New("decoded base64 length of 3 mismatches array length of 4")},
	}, {
		name:    "Bytes/ByteArray4/Overflow",
		inBuf:   `"AQIDBAU="`,
		inVal:   new([4]byte),
		want:    addr([4]byte{}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array4ByteType, Err: errors.New("decoded base64 length of 5 mismatches array length of 4")},
	}, {
		// NOTE: []namedByte is not assignable to []byte,
		// so the following should be treated as a array of uints.
		name:  "Bytes/NamedByteArray",
		inBuf: `[104,101,108,108,111]`,
		inVal: new([5]namedByte),
		want:  addr([5]namedByte{'h', 'e', 'l', 'l', 'o'}),
	}, {
		name:  "Bytes/Valid/Denormalized",
		inBuf: `"AR=="`,
		inVal: new([]byte),
		want:  addr([]byte{1}),
	}, {
		name:  "Bytes/Invalid/Unpadded1",
		inBuf: `"AQ="`,
		inVal: addr([]byte("nochange")),
		want:  addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 0), []byte("AQ="))
			return err
		}()},
	}, {
		name:  "Bytes/Invalid/Unpadded2",
		inBuf: `"AQ"`,
		inVal: addr([]byte("nochange")),
		want:  addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 0), []byte("AQ"))
			return err
		}()},
	}, {
		name:  "Bytes/Invalid/Character",
		inBuf: `"@@@@"`,
		inVal: addr([]byte("nochange")),
		want:  addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 3), []byte("@@@@"))
			return err
		}()},
	}, {
		name:    "Bytes/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr([]byte("nochange")),
		want:    addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: bytesType},
	}, {
		name:    "Bytes/Invalid/Number",
		inBuf:   `0`,
		inVal:   addr([]byte("nochange")),
		want:    addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: bytesType},
	}, {
		name:    "Bytes/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr([]byte("nochange")),
		want:    addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: bytesType},
	}, {
		name:    "Bytes/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr([]byte("nochange")),
		want:    addr([]byte("nochange")),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: bytesType},
	}, {
		name:  "Ints/Null",
		inBuf: `null`,
		inVal: addr(int(1)),
		want:  addr(int(0)),
	}, {
		name:  "Ints/Int",
		inBuf: `1`,
		inVal: addr(int(0)),
		want:  addr(int(1)),
	}, {
		name:    "Ints/Int8/MinOverflow",
		inBuf:   `-129`,
		inVal:   addr(int8(-1)),
		want:    addr(int8(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int8Type, Err: fmt.Errorf(`cannot parse "-129" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Ints/Int8/Min",
		inBuf: `-128`,
		inVal: addr(int8(0)),
		want:  addr(int8(-128)),
	}, {
		name:  "Ints/Int8/Max",
		inBuf: `127`,
		inVal: addr(int8(0)),
		want:  addr(int8(127)),
	}, {
		name:    "Ints/Int8/MaxOverflow",
		inBuf:   `128`,
		inVal:   addr(int8(-1)),
		want:    addr(int8(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int8Type, Err: fmt.Errorf(`cannot parse "128" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:    "Ints/Int16/MinOverflow",
		inBuf:   `-32769`,
		inVal:   addr(int16(-1)),
		want:    addr(int16(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int16Type, Err: fmt.Errorf(`cannot parse "-32769" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Ints/Int16/Min",
		inBuf: `-32768`,
		inVal: addr(int16(0)),
		want:  addr(int16(-32768)),
	}, {
		name:  "Ints/Int16/Max",
		inBuf: `32767`,
		inVal: addr(int16(0)),
		want:  addr(int16(32767)),
	}, {
		name:    "Ints/Int16/MaxOverflow",
		inBuf:   `32768`,
		inVal:   addr(int16(-1)),
		want:    addr(int16(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int16Type, Err: fmt.Errorf(`cannot parse "32768" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:    "Ints/Int32/MinOverflow",
		inBuf:   `-2147483649`,
		inVal:   addr(int32(-1)),
		want:    addr(int32(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int32Type, Err: fmt.Errorf(`cannot parse "-2147483649" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Ints/Int32/Min",
		inBuf: `-2147483648`,
		inVal: addr(int32(0)),
		want:  addr(int32(-2147483648)),
	}, {
		name:  "Ints/Int32/Max",
		inBuf: `2147483647`,
		inVal: addr(int32(0)),
		want:  addr(int32(2147483647)),
	}, {
		name:    "Ints/Int32/MaxOverflow",
		inBuf:   `2147483648`,
		inVal:   addr(int32(-1)),
		want:    addr(int32(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int32Type, Err: fmt.Errorf(`cannot parse "2147483648" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:    "Ints/Int64/MinOverflow",
		inBuf:   `-9223372036854775809`,
		inVal:   addr(int64(-1)),
		want:    addr(int64(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int64Type, Err: fmt.Errorf(`cannot parse "-9223372036854775809" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Ints/Int64/Min",
		inBuf: `-9223372036854775808`,
		inVal: addr(int64(0)),
		want:  addr(int64(-9223372036854775808)),
	}, {
		name:  "Ints/Int64/Max",
		inBuf: `9223372036854775807`,
		inVal: addr(int64(0)),
		want:  addr(int64(9223372036854775807)),
	}, {
		name:    "Ints/Int64/MaxOverflow",
		inBuf:   `9223372036854775808`,
		inVal:   addr(int64(-1)),
		want:    addr(int64(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: int64Type, Err: fmt.Errorf(`cannot parse "9223372036854775808" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Ints/Named",
		inBuf: `-6464`,
		inVal: addr(namedInt64(0)),
		want:  addr(namedInt64(-6464)),
	}, {
		name:  "Ints/Stringified",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `"-6464"`,
		inVal: new(int),
		want:  addr(int(-6464)),
	}, {
		name:  "Ints/Escaped",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `"\u002d\u0036\u0034\u0036\u0034"`,
		inVal: new(int),
		want:  addr(int(-6464)),
	}, {
		name:  "Ints/Valid/NegativeZero",
		inBuf: `-0`,
		inVal: addr(int(1)),
		want:  addr(int(0)),
	}, {
		name:    "Ints/Invalid/Fraction",
		inBuf:   `1.0`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: intType, Err: fmt.Errorf(`cannot parse "1.0" as signed integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Ints/Invalid/Exponent",
		inBuf:   `1e0`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: intType, Err: fmt.Errorf(`cannot parse "1e0" as signed integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Ints/Invalid/StringifiedFraction",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"1.0"`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: intType, Err: fmt.Errorf(`cannot parse "1.0" as signed integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Ints/Invalid/StringifiedExponent",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"1e0"`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: intType, Err: fmt.Errorf(`cannot parse "1e0" as signed integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Ints/Invalid/Overflow",
		inBuf:   `100000000000000000000000000000`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: intType, Err: fmt.Errorf(`cannot parse "100000000000000000000000000000" as signed integer: %w`, strconv.ErrRange)},
	}, {
		name:    "Ints/Invalid/OverflowSyntax",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"100000000000000000000000000000x"`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: intType, Err: fmt.Errorf(`cannot parse "100000000000000000000000000000x" as signed integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Ints/Invalid/Whitespace",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"0 "`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: intType, Err: fmt.Errorf(`cannot parse "0 " as signed integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Ints/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: intType},
	}, {
		name:    "Ints/Invalid/String",
		inBuf:   `"0"`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: intType},
	}, {
		name:    "Ints/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: intType},
	}, {
		name:    "Ints/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr(int(-1)),
		want:    addr(int(-1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: intType},
	}, {
		name:  "Uints/Null",
		inBuf: `null`,
		inVal: addr(uint(1)),
		want:  addr(uint(0)),
	}, {
		name:  "Uints/Uint",
		inBuf: `1`,
		inVal: addr(uint(0)),
		want:  addr(uint(1)),
	}, {
		name:  "Uints/Uint8/Min",
		inBuf: `0`,
		inVal: addr(uint8(1)),
		want:  addr(uint8(0)),
	}, {
		name:  "Uints/Uint8/Max",
		inBuf: `255`,
		inVal: addr(uint8(0)),
		want:  addr(uint8(255)),
	}, {
		name:    "Uints/Uint8/MaxOverflow",
		inBuf:   `256`,
		inVal:   addr(uint8(1)),
		want:    addr(uint8(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uint8Type, Err: fmt.Errorf(`cannot parse "256" as unsigned integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Uints/Uint16/Min",
		inBuf: `0`,
		inVal: addr(uint16(1)),
		want:  addr(uint16(0)),
	}, {
		name:  "Uints/Uint16/Max",
		inBuf: `65535`,
		inVal: addr(uint16(0)),
		want:  addr(uint16(65535)),
	}, {
		name:    "Uints/Uint16/MaxOverflow",
		inBuf:   `65536`,
		inVal:   addr(uint16(1)),
		want:    addr(uint16(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uint16Type, Err: fmt.Errorf(`cannot parse "65536" as unsigned integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Uints/Uint32/Min",
		inBuf: `0`,
		inVal: addr(uint32(1)),
		want:  addr(uint32(0)),
	}, {
		name:  "Uints/Uint32/Max",
		inBuf: `4294967295`,
		inVal: addr(uint32(0)),
		want:  addr(uint32(4294967295)),
	}, {
		name:    "Uints/Uint32/MaxOverflow",
		inBuf:   `4294967296`,
		inVal:   addr(uint32(1)),
		want:    addr(uint32(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uint32Type, Err: fmt.Errorf(`cannot parse "4294967296" as unsigned integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Uints/Uint64/Min",
		inBuf: `0`,
		inVal: addr(uint64(1)),
		want:  addr(uint64(0)),
	}, {
		name:  "Uints/Uint64/Max",
		inBuf: `18446744073709551615`,
		inVal: addr(uint64(0)),
		want:  addr(uint64(18446744073709551615)),
	}, {
		name:    "Uints/Uint64/MaxOverflow",
		inBuf:   `18446744073709551616`,
		inVal:   addr(uint64(1)),
		want:    addr(uint64(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uint64Type, Err: fmt.Errorf(`cannot parse "18446744073709551616" as unsigned integer: %w`, strconv.ErrRange)},
	}, {
		name:  "Uints/Named",
		inBuf: `6464`,
		inVal: addr(namedUint64(0)),
		want:  addr(namedUint64(6464)),
	}, {
		name:  "Uints/Stringified",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `"6464"`,
		inVal: new(uint),
		want:  addr(uint(6464)),
	}, {
		name:  "Uints/Escaped",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `"\u0036\u0034\u0036\u0034"`,
		inVal: new(uint),
		want:  addr(uint(6464)),
	}, {
		name:    "Uints/Invalid/NegativeOne",
		inBuf:   `-1`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uintType, Err: fmt.Errorf(`cannot parse "-1" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/NegativeZero",
		inBuf:   `-0`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uintType, Err: fmt.Errorf(`cannot parse "-0" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/Fraction",
		inBuf:   `1.0`,
		inVal:   addr(uint(10)),
		want:    addr(uint(10)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uintType, Err: fmt.Errorf(`cannot parse "1.0" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/Exponent",
		inBuf:   `1e0`,
		inVal:   addr(uint(10)),
		want:    addr(uint(10)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uintType, Err: fmt.Errorf(`cannot parse "1e0" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/StringifiedFraction",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"1.0"`,
		inVal:   addr(uint(10)),
		want:    addr(uint(10)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: uintType, Err: fmt.Errorf(`cannot parse "1.0" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/StringifiedExponent",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"1e0"`,
		inVal:   addr(uint(10)),
		want:    addr(uint(10)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: uintType, Err: fmt.Errorf(`cannot parse "1e0" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/Overflow",
		inBuf:   `100000000000000000000000000000`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: uintType, Err: fmt.Errorf(`cannot parse "100000000000000000000000000000" as unsigned integer: %w`, strconv.ErrRange)},
	}, {
		name:    "Uints/Invalid/OverflowSyntax",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"100000000000000000000000000000x"`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: uintType, Err: fmt.Errorf(`cannot parse "100000000000000000000000000000x" as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/Whitespace",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"0 "`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: uintType, Err: fmt.Errorf(`cannot parse "0 " as unsigned integer: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Uints/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: uintType},
	}, {
		name:    "Uints/Invalid/String",
		inBuf:   `"0"`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: uintType},
	}, {
		name:    "Uints/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: uintType},
	}, {
		name:    "Uints/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr(uint(1)),
		want:    addr(uint(1)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: uintType},
	}, {
		name:  "Floats/Null",
		inBuf: `null`,
		inVal: addr(float64(64.64)),
		want:  addr(float64(0)),
	}, {
		name:  "Floats/Float32/Pi",
		inBuf: `3.14159265358979323846264338327950288419716939937510582097494459`,
		inVal: addr(float32(32.32)),
		want:  addr(float32(math.Pi)),
	}, {
		name:  "Floats/Float32/Underflow",
		inBuf: `-1e1000`,
		inVal: addr(float32(32.32)),
		want:  addr(float32(-math.MaxFloat32)),
	}, {
		name:  "Floats/Float32/Overflow",
		inBuf: `-1e1000`,
		inVal: addr(float32(32.32)),
		want:  addr(float32(-math.MaxFloat32)),
	}, {
		name:  "Floats/Float64/Pi",
		inBuf: `3.14159265358979323846264338327950288419716939937510582097494459`,
		inVal: addr(float64(64.64)),
		want:  addr(float64(math.Pi)),
	}, {
		name:  "Floats/Float64/Underflow",
		inBuf: `-1e1000`,
		inVal: addr(float64(64.64)),
		want:  addr(float64(-math.MaxFloat64)),
	}, {
		name:  "Floats/Float64/Overflow",
		inBuf: `-1e1000`,
		inVal: addr(float64(64.64)),
		want:  addr(float64(-math.MaxFloat64)),
	}, {
		name:  "Floats/Named",
		inBuf: `64.64`,
		inVal: addr(namedFloat64(0)),
		want:  addr(namedFloat64(64.64)),
	}, {
		name:  "Floats/Stringified",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `"64.64"`,
		inVal: new(float64),
		want:  addr(float64(64.64)),
	}, {
		name:  "Floats/Escaped",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `"\u0036\u0034\u002e\u0036\u0034"`,
		inVal: new(float64),
		want:  addr(float64(64.64)),
	}, {
		name:    "Floats/Invalid/NaN",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"NaN"`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type, Err: fmt.Errorf(`cannot parse "NaN" as JSON number: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Floats/Invalid/Infinity",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"Infinity"`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type, Err: fmt.Errorf(`cannot parse "Infinity" as JSON number: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Floats/Invalid/Whitespace",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"1 "`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type, Err: fmt.Errorf(`cannot parse "1 " as JSON number: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Floats/Invalid/GoSyntax",
		uopts:   UnmarshalOptions{StringifyNumbers: true},
		inBuf:   `"1p-2"`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type, Err: fmt.Errorf(`cannot parse "1p-2" as JSON number: %w`, strconv.ErrSyntax)},
	}, {
		name:    "Floats/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: float64Type},
	}, {
		name:    "Floats/Invalid/String",
		inBuf:   `"0"`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type},
	}, {
		name:    "Floats/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: float64Type},
	}, {
		name:    "Floats/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr(float64(64.64)),
		want:    addr(float64(64.64)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: float64Type},
	}, {
		name:  "Maps/Null",
		inBuf: `null`,
		inVal: addr(map[string]string{"key": "value"}),
		want:  new(map[string]string),
	}, {
		name:    "Maps/InvalidKey/Bool",
		inBuf:   `{"true":"false"}`,
		inVal:   new(map[bool]bool),
		want:    addr(make(map[bool]bool)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: boolType},
	}, {
		name:    "Maps/InvalidKey/NamedBool",
		inBuf:   `{"true":"false"}`,
		inVal:   new(map[namedBool]bool),
		want:    addr(make(map[namedBool]bool)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: namedBoolType},
	}, {
		name:    "Maps/InvalidKey/Array",
		inBuf:   `{"key":"value"}`,
		inVal:   new(map[[1]string]string),
		want:    addr(make(map[[1]string]string)),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array1StringType},
	}, {
		name:    "Maps/InvalidKey/Channel",
		inBuf:   `{"key":"value"}`,
		inVal:   new(map[chan string]string),
		want:    addr(make(map[chan string]string)),
		wantErr: &SemanticError{action: "unmarshal", GoType: chanStringType},
	}, {
		name:  "Maps/ValidKey/Int",
		inBuf: `{"0":0,"-1":1,"2":2,"-3":3}`,
		inVal: new(map[int]int),
		want:  addr(map[int]int{0: 0, -1: 1, 2: 2, -3: 3}),
	}, {
		// NOTE: For signed integers, the only possible way for duplicate keys
		// with different representations is negative zero and zero.
		name:  "Maps/ValidKey/Int/Duplicates",
		inBuf: `{"0":1,"-0":-1}`,
		inVal: new(map[int]int),
		want:  addr(map[int]int{0: -1}), // latter takes precedence
	}, {
		name:  "Maps/ValidKey/NamedInt",
		inBuf: `{"0":0,"-1":1,"2":2,"-3":3}`,
		inVal: new(map[namedInt64]int),
		want:  addr(map[namedInt64]int{0: 0, -1: 1, 2: 2, -3: 3}),
	}, {
		name:  "Maps/ValidKey/Uint",
		inBuf: `{"0":0,"1":1,"2":2,"3":3}`,
		inVal: new(map[uint]uint),
		want:  addr(map[uint]uint{0: 0, 1: 1, 2: 2, 3: 3}),
	}, {
		name:  "Maps/ValidKey/NamedUint",
		inBuf: `{"0":0,"1":1,"2":2,"3":3}`,
		inVal: new(map[namedUint64]uint),
		want:  addr(map[namedUint64]uint{0: 0, 1: 1, 2: 2, 3: 3}),
	}, {
		name:  "Maps/ValidKey/Float",
		inBuf: `{"1.234":1.234,"12.34":12.34,"123.4":123.4}`,
		inVal: new(map[float64]float64),
		want:  addr(map[float64]float64{1.234: 1.234, 12.34: 12.34, 123.4: 123.4}),
	}, {
		name:  "Maps/ValidKey/Float/Duplicates",
		inBuf: `{"1.0":"1.0","1":"1","1e0":"1e0"}`,
		inVal: new(map[float64]string),
		want:  addr(map[float64]string{1: "1e0"}), // latter takes precedence
	}, {
		name:  "Maps/ValidKey/Interface",
		inBuf: `{"false":"false","true":"true","string":"string","0":"0","[]":"[]","{}":"{}"}`,
		inVal: new(map[interface{}]string),
		want: addr(map[interface{}]string{
			"false":  "false",
			"true":   "true",
			"string": "string",
			"0":      "0",
			"[]":     "[]",
			"{}":     "{}",
		}),
	}, {
		name:  "Maps/InvalidValue/Channel",
		inBuf: `{"key":"value"}`,
		inVal: new(map[string]chan string),
		want: addr(map[string]chan string{
			"key": nil,
		}),
		wantErr: &SemanticError{action: "unmarshal", GoType: chanStringType},
	}, {
		name:  "Maps/RecursiveMap",
		inBuf: `{"buzz":{},"fizz":{"bar":{},"foo":{}}}`,
		inVal: new(recursiveMap),
		want: addr(recursiveMap{
			"fizz": {
				"foo": {},
				"bar": {},
			},
			"buzz": {},
		}),
	}, {
		// NOTE: The semantics differs from v1,
		// where existing map entries were not merged into.
		// See https://golang.org/issue/31924.
		name:  "Maps/Merge",
		dopts: DecodeOptions{AllowDuplicateNames: true},
		inBuf: `{"k1":{"k2":"v2"},"k2":{"k1":"v1"},"k2":{"k2":"v2"}}`,
		inVal: addr(map[string]map[string]string{
			"k1": {"k1": "v1"},
		}),
		want: addr(map[string]map[string]string{
			"k1": {"k1": "v1", "k2": "v2"},
			"k2": {"k1": "v1", "k2": "v2"},
		}),
	}, {
		name:    "Maps/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr(map[string]string{"key": "value"}),
		want:    addr(map[string]string{"key": "value"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: mapStringStringType},
	}, {
		name:    "Maps/Invalid/String",
		inBuf:   `""`,
		inVal:   addr(map[string]string{"key": "value"}),
		want:    addr(map[string]string{"key": "value"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: mapStringStringType},
	}, {
		name:    "Maps/Invalid/Number",
		inBuf:   `0`,
		inVal:   addr(map[string]string{"key": "value"}),
		want:    addr(map[string]string{"key": "value"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: mapStringStringType},
	}, {
		name:    "Maps/Invalid/Array",
		inBuf:   `[]`,
		inVal:   addr(map[string]string{"key": "value"}),
		want:    addr(map[string]string{"key": "value"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: mapStringStringType},
	}, {
		name:  "Structs/Null",
		inBuf: `null`,
		inVal: addr(structAll{String: "something"}),
		want:  addr(structAll{}),
	}, {
		name:  "Structs/Empty",
		inBuf: `{}`,
		inVal: addr(structAll{
			String: "hello",
			Map:    map[string]string{},
			Slice:  []string{},
		}),
		want: addr(structAll{
			String: "hello",
			Map:    map[string]string{},
			Slice:  []string{},
		}),
	}, {
		name: "Structs/Normal",
		inBuf: `{
	"Bool": true,
	"String": "hello",
	"Bytes": "AQID",
	"Int": -64,
	"Uint": 64,
	"Float": 3.14159,
	"Map": {"key": "value"},
	"StructScalars": {
		"Bool": true,
		"String": "hello",
		"Bytes": "AQID",
		"Int": -64,
		"Uint": 64,
		"Float": 3.14159
	},
	"StructMaps": {
		"MapBool": {"": true},
		"MapString": {"": "hello"},
		"MapBytes": {"": "AQID"},
		"MapInt": {"": -64},
		"MapUint": {"": 64},
		"MapFloat": {"": 3.14159}
	},
	"StructSlices": {
		"SliceBool": [true],
		"SliceString": ["hello"],
		"SliceBytes": ["AQID"],
		"SliceInt": [-64],
		"SliceUint": [64],
		"SliceFloat": [3.14159]
	},
	"Slice": ["fizz","buzz"],
	"Array": ["goodbye"],
	"Ptr": {},
	"Interface": null
}`,
		inVal: new(structAll),
		want: addr(structAll{
			Bool:   true,
			String: "hello",
			Bytes:  []byte{1, 2, 3},
			Int:    -64,
			Uint:   +64,
			Float:  3.14159,
			Map:    map[string]string{"key": "value"},
			StructScalars: structScalars{
				Bool:   true,
				String: "hello",
				Bytes:  []byte{1, 2, 3},
				Int:    -64,
				Uint:   +64,
				Float:  3.14159,
			},
			StructMaps: structMaps{
				MapBool:   map[string]bool{"": true},
				MapString: map[string]string{"": "hello"},
				MapBytes:  map[string][]byte{"": []byte{1, 2, 3}},
				MapInt:    map[string]int64{"": -64},
				MapUint:   map[string]uint64{"": +64},
				MapFloat:  map[string]float64{"": 3.14159},
			},
			StructSlices: structSlices{
				SliceBool:   []bool{true},
				SliceString: []string{"hello"},
				SliceBytes:  [][]byte{[]byte{1, 2, 3}},
				SliceInt:    []int64{-64},
				SliceUint:   []uint64{+64},
				SliceFloat:  []float64{3.14159},
			},
			Slice: []string{"fizz", "buzz"},
			Array: [1]string{"goodbye"},
			Ptr:   new(structAll),
		}),
	}, {
		name: "Structs/Merge",
		inBuf: `{
	"Bool": false,
	"String": "goodbye",
	"Int": -64,
	"Float": 3.14159,
	"Map": {"k2": "v2"},
	"StructScalars": {
		"Bool": true,
		"String": "hello",
		"Bytes": "AQID",
		"Int": -64
	},
	"StructMaps": {
		"MapBool": {"": true},
		"MapString": {"": "hello"},
		"MapBytes": {"": "AQID"},
		"MapInt": {"": -64},
		"MapUint": {"": 64},
		"MapFloat": {"": 3.14159}
	},
	"StructSlices": {
		"SliceString": ["hello"],
		"SliceBytes": ["AQID"],
		"SliceInt": [-64],
		"SliceUint": [64]
	},
	"Slice": ["fizz","buzz"],
	"Array": ["goodbye"],
	"Ptr": {},
	"Interface": {"k2":"v2"}
}`,
		inVal: addr(structAll{
			Bool:   true,
			String: "hello",
			Bytes:  []byte{1, 2, 3},
			Uint:   +64,
			Float:  math.NaN(),
			Map:    map[string]string{"k1": "v1"},
			StructScalars: structScalars{
				String: "hello",
				Bytes:  make([]byte, 2, 4),
				Uint:   +64,
				Float:  3.14159,
			},
			StructMaps: structMaps{
				MapBool:  map[string]bool{"": false},
				MapBytes: map[string][]byte{"": []byte{}},
				MapInt:   map[string]int64{"": 123},
				MapFloat: map[string]float64{"": math.Inf(+1)},
			},
			StructSlices: structSlices{
				SliceBool:  []bool{true},
				SliceBytes: [][]byte{nil, nil},
				SliceInt:   []int64{-123},
				SliceUint:  []uint64{+123},
				SliceFloat: []float64{3.14159},
			},
			Slice:     []string{"buzz", "fizz", "gizz"},
			Array:     [1]string{"hello"},
			Ptr:       new(structAll),
			Interface: map[string]string{"k1": "v1"},
		}),
		want: addr(structAll{
			Bool:   false,
			String: "goodbye",
			Bytes:  []byte{1, 2, 3},
			Int:    -64,
			Uint:   +64,
			Float:  3.14159,
			Map:    map[string]string{"k1": "v1", "k2": "v2"},
			StructScalars: structScalars{
				Bool:   true,
				String: "hello",
				Bytes:  []byte{1, 2, 3},
				Int:    -64,
				Uint:   +64,
				Float:  3.14159,
			},
			StructMaps: structMaps{
				MapBool:   map[string]bool{"": true},
				MapString: map[string]string{"": "hello"},
				MapBytes:  map[string][]byte{"": []byte{1, 2, 3}},
				MapInt:    map[string]int64{"": -64},
				MapUint:   map[string]uint64{"": +64},
				MapFloat:  map[string]float64{"": 3.14159},
			},
			StructSlices: structSlices{
				SliceBool:   []bool{true},
				SliceString: []string{"hello"},
				SliceBytes:  [][]byte{[]byte{1, 2, 3}},
				SliceInt:    []int64{-64},
				SliceUint:   []uint64{+64},
				SliceFloat:  []float64{3.14159},
			},
			Slice:     []string{"fizz", "buzz"},
			Array:     [1]string{"goodbye"},
			Ptr:       new(structAll),
			Interface: map[string]string{"k1": "v1", "k2": "v2"},
		}),
	}, {
		name: "Structs/Stringified/Normal",
		inBuf: `{
	"Bool": true,
	"String": "hello",
	"Bytes": "AQID",
	"Int": -64,
	"Uint": 64,
	"Float": 3.14159,
	"Map": {"key": "value"},
	"StructScalars": {
		"Bool": true,
		"String": "hello",
		"Bytes": "AQID",
		"Int": -64,
		"Uint": 64,
		"Float": 3.14159
	},
	"StructMaps": {
		"MapBool": {"": true},
		"MapString": {"": "hello"},
		"MapBytes": {"": "AQID"},
		"MapInt": {"": -64},
		"MapUint": {"": 64},
		"MapFloat": {"": 3.14159}
	},
	"StructSlices": {
		"SliceBool": [true],
		"SliceString": ["hello"],
		"SliceBytes": ["AQID"],
		"SliceInt": [-64],
		"SliceUint": [64],
		"SliceFloat": [3.14159]
	},
	"Slice": ["fizz","buzz"],
	"Array": ["goodbye"],
	"Ptr": {},
	"Interface": null
}`,
		inVal: new(structStringifiedAll),
		want: addr(structStringifiedAll{
			Bool:   true,
			String: "hello",
			Bytes:  []byte{1, 2, 3},
			Int:    -64,     // may be stringified
			Uint:   +64,     // may be stringified
			Float:  3.14159, // may be stringified
			Map:    map[string]string{"key": "value"},
			StructScalars: structScalars{
				Bool:   true,
				String: "hello",
				Bytes:  []byte{1, 2, 3},
				Int:    -64,     // may be stringified
				Uint:   +64,     // may be stringified
				Float:  3.14159, // may be stringified
			},
			StructMaps: structMaps{
				MapBool:   map[string]bool{"": true},
				MapString: map[string]string{"": "hello"},
				MapBytes:  map[string][]byte{"": []byte{1, 2, 3}},
				MapInt:    map[string]int64{"": -64},       // may be stringified
				MapUint:   map[string]uint64{"": +64},      // may be stringified
				MapFloat:  map[string]float64{"": 3.14159}, // may be stringified
			},
			StructSlices: structSlices{
				SliceBool:   []bool{true},
				SliceString: []string{"hello"},
				SliceBytes:  [][]byte{[]byte{1, 2, 3}},
				SliceInt:    []int64{-64},       // may be stringified
				SliceUint:   []uint64{+64},      // may be stringified
				SliceFloat:  []float64{3.14159}, // may be stringified
			},
			Slice: []string{"fizz", "buzz"},
			Array: [1]string{"goodbye"},
			Ptr:   new(structStringifiedAll), // may be stringified
		}),
	}, {
		name: "Structs/Stringified/String",
		inBuf: `{
	"Bool": true,
	"String": "hello",
	"Bytes": "AQID",
	"Int": "-64",
	"Uint": "64",
	"Float": "3.14159",
	"Map": {"key": "value"},
	"StructScalars": {
		"Bool": true,
		"String": "hello",
		"Bytes": "AQID",
		"Int": "-64",
		"Uint": "64",
		"Float": "3.14159"
	},
	"StructMaps": {
		"MapBool": {"": true},
		"MapString": {"": "hello"},
		"MapBytes": {"": "AQID"},
		"MapInt": {"": "-64"},
		"MapUint": {"": "64"},
		"MapFloat": {"": "3.14159"}
	},
	"StructSlices": {
		"SliceBool": [true],
		"SliceString": ["hello"],
		"SliceBytes": ["AQID"],
		"SliceInt": ["-64"],
		"SliceUint": ["64"],
		"SliceFloat": ["3.14159"]
	},
	"Slice": ["fizz","buzz"],
	"Array": ["goodbye"],
	"Ptr": {},
	"Interface": null
}`,
		inVal: new(structStringifiedAll),
		want: addr(structStringifiedAll{
			Bool:   true,
			String: "hello",
			Bytes:  []byte{1, 2, 3},
			Int:    -64,     // may be stringified
			Uint:   +64,     // may be stringified
			Float:  3.14159, // may be stringified
			Map:    map[string]string{"key": "value"},
			StructScalars: structScalars{
				Bool:   true,
				String: "hello",
				Bytes:  []byte{1, 2, 3},
				Int:    -64,     // may be stringified
				Uint:   +64,     // may be stringified
				Float:  3.14159, // may be stringified
			},
			StructMaps: structMaps{
				MapBool:   map[string]bool{"": true},
				MapString: map[string]string{"": "hello"},
				MapBytes:  map[string][]byte{"": []byte{1, 2, 3}},
				MapInt:    map[string]int64{"": -64},       // may be stringified
				MapUint:   map[string]uint64{"": +64},      // may be stringified
				MapFloat:  map[string]float64{"": 3.14159}, // may be stringified
			},
			StructSlices: structSlices{
				SliceBool:   []bool{true},
				SliceString: []string{"hello"},
				SliceBytes:  [][]byte{[]byte{1, 2, 3}},
				SliceInt:    []int64{-64},       // may be stringified
				SliceUint:   []uint64{+64},      // may be stringified
				SliceFloat:  []float64{3.14159}, // may be stringified
			},
			Slice: []string{"fizz", "buzz"},
			Array: [1]string{"goodbye"},
			Ptr:   new(structStringifiedAll), // may be stringified
		}),
	}, {
		name: "Structs/Format/Bytes",
		inBuf: `{
	"Base16": "0123456789abcdef",
	"Base32": "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567",
	"Base32Hex": "0123456789ABCDEFGHIJKLMNOPQRSTUV",
	"Base64": "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
	"Base64URL": "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",
	"UintArray": [1, 2, 3, 4]
}`,
		inVal: new(structFormatBytes),
		want: addr(structFormatBytes{
			Base16:    []byte("\x01\x23\x45\x67\x89\xab\xcd\xef"),
			Base32:    []byte("\x00D2\x14\xc7BT\xb65τe:V\xd7\xc6u\xbew\xdf"),
			Base32Hex: []byte("\x00D2\x14\xc7BT\xb65τe:V\xd7\xc6u\xbew\xdf"),
			Base64:    []byte("\x00\x10\x83\x10Q\x87 \x92\x8b0ӏA\x14\x93QU\x97a\x96\x9bqן\x82\x18\xa3\x92Y\xa7\xa2\x9a\xab\xb2ۯ\xc3\x1c\xb3\xd3]\xb7㞻\xf3߿"),
			Base64URL: []byte("\x00\x10\x83\x10Q\x87 \x92\x8b0ӏA\x14\x93QU\x97a\x96\x9bqן\x82\x18\xa3\x92Y\xa7\xa2\x9a\xab\xb2ۯ\xc3\x1c\xb3\xd3]\xb7㞻\xf3߿"),
			UintArray: []byte{1, 2, 3, 4},
		}),
	}, {
		name:    "Structs/Format/Bytes/Invalid/Base16/WrongKind",
		inBuf:   `{"Base16": [1,2,3,4]}`,
		inVal:   new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '[', GoType: bytesType},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base16/AllPadding",
		inBuf: `{"Base16": "===="}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := hex.Decode(make([]byte, 2), []byte("====="))
			return err
		}()},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base16/EvenPadding",
		inBuf: `{"Base16": "0123456789abcdef="}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := hex.Decode(make([]byte, 8), []byte("0123456789abcdef="))
			return err
		}()},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base16/OddPadding",
		inBuf: `{"Base16": "0123456789abcdef0="}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := hex.Decode(make([]byte, 9), []byte("0123456789abcdef0="))
			return err
		}()},
	}, {
		name: "Structs/Format/Bytes/Invalid/Base32/Padding",
		inBuf: `[
			{"Base32": "NA======"},
			{"Base32": "NBSQ===="},
			{"Base32": "NBSWY==="},
			{"Base32": "NBSWY3A="},
			{"Base32": "NBSWY3DP"}
		]`,
		inVal: new([]structFormatBytes),
		want: addr([]structFormatBytes{
			{Base32: []byte("h")},
			{Base32: []byte("he")},
			{Base32: []byte("hel")},
			{Base32: []byte("hell")},
			{Base32: []byte("hello")},
		}),
	}, {
		name: "Structs/Format/Bytes/Invalid/Base32/Invalid/NoPadding",
		inBuf: `[
				{"Base32": "NA"},
				{"Base32": "NBSQ"},
				{"Base32": "NBSWY"},
				{"Base32": "NBSWY3A"},
				{"Base32": "NBSWY3DP"}
			]`,
		inVal: new([]structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base32.StdEncoding.Decode(make([]byte, 1), []byte("NA"))
			return err
		}()},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base32/WrongAlphabet",
		inBuf: `{"Base32": "0123456789ABCDEFGHIJKLMNOPQRSTUV"}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base32.StdEncoding.Decode(make([]byte, 20), []byte("0123456789ABCDEFGHIJKLMNOPQRSTUV"))
			return err
		}()},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base32Hex/WrongAlphabet",
		inBuf: `{"Base32Hex": "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base32.HexEncoding.Decode(make([]byte, 20), []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"))
			return err
		}()},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base64/WrongAlphabet",
		inBuf: `{"Base64": "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base64.StdEncoding.Decode(make([]byte, 48), []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"))
			return err
		}()},
	}, {
		name:  "Structs/Format/Bytes/Invalid/Base64URL/WrongAlphabet",
		inBuf: `{"Base64URL": "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"}`,
		inVal: new(structFormatBytes),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: bytesType, Err: func() error {
			_, err := base64.URLEncoding.Decode(make([]byte, 48), []byte("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
			return err
		}()},
	}, {
		name: "Structs/Format/Floats",
		inBuf: `[
	{"NonFinite": 3.141592653589793, "PtrNonFinite": 3.141592653589793},
	{"NonFinite": "-Infinity", "PtrNonFinite": "-Infinity"},
	{"NonFinite": "Infinity", "PtrNonFinite": "Infinity"}
]`,
		inVal: new([]structFormatFloats),
		want: addr([]structFormatFloats{
			{NonFinite: math.Pi, PtrNonFinite: addr(math.Pi).(*float64)},
			{NonFinite: math.Inf(-1), PtrNonFinite: addr(math.Inf(-1)).(*float64)},
			{NonFinite: math.Inf(+1), PtrNonFinite: addr(math.Inf(+1)).(*float64)},
		}),
	}, {
		name:  "Structs/Format/Floats/NaN",
		inBuf: `{"NonFinite": "NaN"}`,
		inVal: new(structFormatFloats),
		// Avoid checking want since reflect.DeepEqual fails for NaNs.
	}, {
		name:    "Structs/Format/Floats/Invalid/NaN",
		inBuf:   `{"NonFinite": "nan"}`,
		inVal:   new(structFormatFloats),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type},
	}, {
		name:    "Structs/Format/Floats/Invalid/PositiveInfinity",
		inBuf:   `{"NonFinite": "+Infinity"}`,
		inVal:   new(structFormatFloats),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type},
	}, {
		name:    "Structs/Format/Floats/Invalid/NegativeInfinitySpace",
		inBuf:   `{"NonFinite": "-Infinity "}`,
		inVal:   new(structFormatFloats),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: float64Type},
	}, {
		name: "Structs/Format/Maps",
		inBuf: `[
	{"EmitNull": null, "PtrEmitNull": null},
	{"EmitNull": {}, "PtrEmitNull": {}},
	{"EmitNull": {"k": "v"}, "PtrEmitNull": {"k": "v"}}
]`,
		inVal: new([]structFormatMaps),
		want: addr([]structFormatMaps{
			{EmitNull: nil, PtrEmitNull: nil},
			{EmitNull: map[string]string{}, PtrEmitNull: addr(map[string]string{}).(*map[string]string)},
			{EmitNull: map[string]string{"k": "v"}, PtrEmitNull: addr(map[string]string{"k": "v"}).(*map[string]string)},
		}),
	}, {
		name: "Structs/Format/Slices",
		inBuf: `[
	{"EmitNull": null, "PtrEmitNull": null},
	{"EmitNull": [], "PtrEmitNull": []},
	{"EmitNull": ["v"], "PtrEmitNull": ["v"]}
]`,
		inVal: new([]structFormatSlices),
		want: addr([]structFormatSlices{
			{EmitNull: nil, PtrEmitNull: nil},
			{EmitNull: []string{}, PtrEmitNull: addr([]string{}).(*[]string)},
			{EmitNull: []string{"v"}, PtrEmitNull: addr([]string{"v"}).(*[]string)},
		}),
	}, {
		name:    "Structs/Format/Invalid/Bool",
		inBuf:   `{"Bool":true}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: boolType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/String",
		inBuf:   `{"String": "string"}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: stringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Bytes",
		inBuf:   `{"Bytes": "bytes"}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: bytesType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Int",
		inBuf:   `{"Int": 1}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: int64Type, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Uint",
		inBuf:   `{"Uint": 1}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: uint64Type, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Float",
		inBuf:   `{"Float": 1}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: float64Type, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Map",
		inBuf:   `{"Map":{}}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: mapStringStringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Struct",
		inBuf:   `{"Struct": {}}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: structAllType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Slice",
		inBuf:   `{"Slice": {}}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: sliceStringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Array",
		inBuf:   `{"Array": []}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: array1StringType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:    "Structs/Format/Invalid/Interface",
		inBuf:   `{"Interface": "anything"}`,
		inVal:   new(structFormatInvalid),
		wantErr: &SemanticError{action: "unmarshal", GoType: emptyInterfaceType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:  "Structs/Inline/Zero",
		inBuf: `{"D":""}`,
		inVal: new(structInlined),
		want:  new(structInlined),
	}, {
		name:  "Structs/Inline/Alloc",
		inBuf: `{"E":"","F":"","G":"","A":"","B":"","D":""}`,
		inVal: new(structInlined),
		want: addr(structInlined{
			X: structInlinedL1{
				X:            &structInlinedL2{},
				StructEmbed1: StructEmbed1{},
			},
			StructEmbed2: &StructEmbed2{},
		}),
	}, {
		name:  "Structs/Inline/NonZero",
		inBuf: `{"E":"E3","F":"F3","G":"G3","A":"A1","B":"B1","D":"D2"}`,
		inVal: new(structInlined),
		want: addr(structInlined{
			X: structInlinedL1{
				X:            &structInlinedL2{A: "A1", B: "B1" /* C: "C1" */},
				StructEmbed1: StructEmbed1{ /* C: "C2" */ D: "D2" /* E: "E2" */},
			},
			StructEmbed2: &StructEmbed2{E: "E3", F: "F3", G: "G3"},
		}),
	}, {
		name:  "Structs/Inline/Merge",
		inBuf: `{"E":"E3","F":"F3","G":"G3","A":"A1","B":"B1","D":"D2"}`,
		inVal: addr(structInlined{
			X: structInlinedL1{
				X:            &structInlinedL2{B: "##", C: "C1"},
				StructEmbed1: StructEmbed1{C: "C2", E: "E2"},
			},
			StructEmbed2: &StructEmbed2{E: "##", G: "G3"},
		}),
		want: addr(structInlined{
			X: structInlinedL1{
				X:            &structInlinedL2{A: "A1", B: "B1", C: "C1"},
				StructEmbed1: StructEmbed1{C: "C2", D: "D2", E: "E2"},
			},
			StructEmbed2: &StructEmbed2{E: "E3", F: "F3", G: "G3"},
		}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/Noop",
		inBuf: `{"A":1,"B":2}`,
		inVal: new(structInlineRawValue),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(nil), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/MergeN1/Nil",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: new(structInlineRawValue),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(`{"fizz":"buzz"}`), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/MergeN1/Empty",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: addr(structInlineRawValue{X: RawValue{}}),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(`{"fizz":"buzz"}`), B: 2}),
	}, {
		name:    "Structs/InlinedFallback/RawValue/MergeN1/Whitespace",
		inBuf:   `{"A":1,"fizz":"buzz","B":2}`,
		inVal:   addr(structInlineRawValue{X: RawValue("\n\r\t ")}),
		want:    addr(structInlineRawValue{A: 1, X: RawValue("")}),
		wantErr: &SemanticError{action: "unmarshal", GoType: rawValueType, Err: errors.New("inlined raw value must be a JSON object")},
	}, {
		name:    "Structs/InlinedFallback/RawValue/MergeN1/Null",
		inBuf:   `{"A":1,"fizz":"buzz","B":2}`,
		inVal:   addr(structInlineRawValue{X: RawValue("null")}),
		want:    addr(structInlineRawValue{A: 1, X: RawValue("null")}),
		wantErr: &SemanticError{action: "unmarshal", GoType: rawValueType, Err: errors.New("inlined raw value must be a JSON object")},
	}, {
		name:  "Structs/InlinedFallback/RawValue/MergeN1/ObjectN0",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: addr(structInlineRawValue{X: RawValue(` { } `)}),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(` {"fizz":"buzz"}`), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/MergeN2/ObjectN1",
		inBuf: `{"A":1,"fizz":"buzz","B":2,"foo": [ 1 , 2 , 3 ]}`,
		inVal: addr(structInlineRawValue{X: RawValue(` { "fizz" : "buzz" } `)}),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(` { "fizz" : "buzz","fizz":"buzz","foo":[ 1 , 2 , 3 ]}`), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/Merge/ObjectEnd",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: addr(structInlineRawValue{X: RawValue(` } `)}),
		// NOTE: This produces invalid output,
		// but the value being merged into is already invalid.
		want: addr(structInlineRawValue{A: 1, X: RawValue(`,"fizz":"buzz"}`), B: 2}),
	}, {
		name:    "Structs/InlinedFallback/RawValue/MergeInvalidValue",
		inBuf:   `{"A":1,"fizz":nil,"B":2}`,
		inVal:   new(structInlineRawValue),
		want:    addr(structInlineRawValue{A: 1, X: RawValue(`{"fizz":`)}),
		wantErr: newInvalidCharacterError('i', "within literal null (expecting 'u')").withOffset(int64(len(`{"A":1,"fizz":n`))),
	}, {
		name:  "Structs/InlinedFallback/RawValue/CaseSensitive",
		inBuf: `{"A":1,"fizz":"buzz","B":2,"a":3}`,
		inVal: new(structInlineRawValue),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(`{"fizz":"buzz","a":3}`), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/CaseInsensitive",
		uopts: UnmarshalOptions{MatchCaseInsensitiveNames: true},
		inBuf: `{"A":1,"fizz":"buzz","B":2,"a":3}`,
		inVal: new(structInlineRawValue),
		want:  addr(structInlineRawValue{A: 3, X: RawValue(`{"fizz":"buzz"}`), B: 2}),
	}, {
		name:    "Structs/InlinedFallback/RawValue/RejectDuplicateNames",
		dopts:   DecodeOptions{AllowDuplicateNames: false},
		inBuf:   `{"A":1,"fizz":"buzz","B":2,"fizz":"buzz"}`,
		inVal:   new(structInlineRawValue),
		want:    addr(structInlineRawValue{A: 1, X: RawValue(`{"fizz":"buzz"}`), B: 2}),
		wantErr: (&SyntacticError{str: `duplicate name "fizz" in object`}).withOffset(int64(len(`{"A":1,"fizz":"buzz","B":2,`))),
	}, {
		name:  "Structs/InlinedFallback/RawValue/AllowDuplicateNames",
		dopts: DecodeOptions{AllowDuplicateNames: true},
		inBuf: `{"A":1,"fizz":"buzz","B":2,"fizz":"buzz"}`,
		inVal: new(structInlineRawValue),
		want:  addr(structInlineRawValue{A: 1, X: RawValue(`{"fizz":"buzz","fizz":"buzz"}`), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/Nested/Noop",
		inBuf: `{}`,
		inVal: new(structInlinePointerInlineRawValue),
		want:  new(structInlinePointerInlineRawValue),
	}, {
		name:  "Structs/InlinedFallback/RawValue/Nested/Alloc",
		inBuf: `{"A":1,"fizz":"buzz"}`,
		inVal: new(structInlinePointerInlineRawValue),
		want: addr(structInlinePointerInlineRawValue{
			X: &struct {
				A int
				X RawValue `json:",inline"`
			}{A: 1, X: RawValue(`{"fizz":"buzz"}`)},
		}),
	}, {
		name:  "Structs/InlinedFallback/RawValue/Nested/Merge",
		inBuf: `{"fizz":"buzz"}`,
		inVal: addr(structInlinePointerInlineRawValue{
			X: &struct {
				A int
				X RawValue `json:",inline"`
			}{A: 1},
		}),
		want: addr(structInlinePointerInlineRawValue{
			X: &struct {
				A int
				X RawValue `json:",inline"`
			}{A: 1, X: RawValue(`{"fizz":"buzz"}`)},
		}),
	}, {
		name:  "Structs/InlinedFallback/PointerRawValue/Noop",
		inBuf: `{"A":1,"B":2}`,
		inVal: new(structInlinePointerRawValue),
		want:  addr(structInlinePointerRawValue{A: 1, X: nil, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/PointerRawValue/Alloc",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: new(structInlinePointerRawValue),
		want:  addr(structInlinePointerRawValue{A: 1, X: addr(RawValue(`{"fizz":"buzz"}`)).(*RawValue), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/PointerRawValue/Merge",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: addr(structInlinePointerRawValue{X: addr(RawValue(`{"fizz":"buzz"}`)).(*RawValue)}),
		want:  addr(structInlinePointerRawValue{A: 1, X: addr(RawValue(`{"fizz":"buzz","fizz":"buzz"}`)).(*RawValue), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/PointerRawValue/Nested/Nil",
		inBuf: `{"fizz":"buzz"}`,
		inVal: new(structInlineInlinePointerRawValue),
		want: addr(structInlineInlinePointerRawValue{
			X: struct {
				X *RawValue `json:",inline"`
			}{X: addr(RawValue(`{"fizz":"buzz"}`)).(*RawValue)},
		}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/Noop",
		inBuf: `{"A":1,"B":2}`,
		inVal: new(structInlineMapStringAny),
		want:  addr(structInlineMapStringAny{A: 1, X: nil, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/MergeN1/Nil",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: new(structInlineMapStringAny),
		want:  addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": "buzz"}, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/MergeN1/Empty",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: addr(structInlineMapStringAny{X: jsonObject{}}),
		want:  addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": "buzz"}, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/MergeN1/ObjectN1",
		inBuf: `{"A":1,"fizz":{"charlie":"DELTA","echo":"foxtrot"},"B":2}`,
		inVal: addr(structInlineMapStringAny{X: jsonObject{"fizz": jsonObject{
			"alpha":   "bravo",
			"charlie": "delta",
		}}}),
		want: addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": jsonObject{
			"alpha":   "bravo",
			"charlie": "DELTA",
			"echo":    "foxtrot",
		}}, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/MergeN2/ObjectN1",
		inBuf: `{"A":1,"fizz":"buzz","B":2,"foo": [ 1 , 2 , 3 ]}`,
		inVal: addr(structInlineMapStringAny{X: jsonObject{"fizz": "wuzz"}}),
		want:  addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": "buzz", "foo": jsonArray{1.0, 2.0, 3.0}}, B: 2}),
	}, {
		name:    "Structs/InlinedFallback/MapStringAny/MergeInvalidValue",
		inBuf:   `{"A":1,"fizz":nil,"B":2}`,
		inVal:   new(structInlineMapStringAny),
		want:    addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": nil}}),
		wantErr: newInvalidCharacterError('i', "within literal null (expecting 'u')").withOffset(int64(len(`{"A":1,"fizz":n`))),
	}, {
		name:    "Structs/InlinedFallback/MapStringAny/MergeInvalidValue/Existing",
		inBuf:   `{"A":1,"fizz":nil,"B":2}`,
		inVal:   addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": true}}),
		want:    addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": true}}),
		wantErr: newInvalidCharacterError('i', "within literal null (expecting 'u')").withOffset(int64(len(`{"A":1,"fizz":n`))),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/CaseSensitive",
		inBuf: `{"A":1,"fizz":"buzz","B":2,"a":3}`,
		inVal: new(structInlineMapStringAny),
		want:  addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": "buzz", "a": 3.0}, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/CaseInsensitive",
		uopts: UnmarshalOptions{MatchCaseInsensitiveNames: true},
		inBuf: `{"A":1,"fizz":"buzz","B":2,"a":3}`,
		inVal: new(structInlineMapStringAny),
		want:  addr(structInlineMapStringAny{A: 3, X: jsonObject{"fizz": "buzz"}, B: 2}),
	}, {
		name:    "Structs/InlinedFallback/MapStringAny/RejectDuplicateNames",
		dopts:   DecodeOptions{AllowDuplicateNames: false},
		inBuf:   `{"A":1,"fizz":"buzz","B":2,"fizz":"buzz"}`,
		inVal:   new(structInlineMapStringAny),
		want:    addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": "buzz"}, B: 2}),
		wantErr: (&SyntacticError{str: `duplicate name "fizz" in object`}).withOffset(int64(len(`{"A":1,"fizz":"buzz","B":2,`))),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/AllowDuplicateNames",
		dopts: DecodeOptions{AllowDuplicateNames: true},
		inBuf: `{"A":1,"fizz":{"one":1,"two":-2},"B":2,"fizz":{"two":2,"three":3}}`,
		inVal: new(structInlineMapStringAny),
		want:  addr(structInlineMapStringAny{A: 1, X: jsonObject{"fizz": jsonObject{"one": 1.0, "two": 2.0, "three": 3.0}}, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/Nested/Noop",
		inBuf: `{}`,
		inVal: new(structInlinePointerInlineMapStringAny),
		want:  new(structInlinePointerInlineMapStringAny),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/Nested/Alloc",
		inBuf: `{"A":1,"fizz":"buzz"}`,
		inVal: new(structInlinePointerInlineMapStringAny),
		want: addr(structInlinePointerInlineMapStringAny{
			X: &struct {
				A int
				X jsonObject `json:",inline"`
			}{A: 1, X: jsonObject{"fizz": "buzz"}},
		}),
	}, {
		name:  "Structs/InlinedFallback/MapStringAny/Nested/Merge",
		inBuf: `{"fizz":"buzz"}`,
		inVal: addr(structInlinePointerInlineMapStringAny{
			X: &struct {
				A int
				X jsonObject `json:",inline"`
			}{A: 1},
		}),
		want: addr(structInlinePointerInlineMapStringAny{
			X: &struct {
				A int
				X jsonObject `json:",inline"`
			}{A: 1, X: jsonObject{"fizz": "buzz"}},
		}),
	}, {
		name:  "Structs/InlinedFallback/PointerMapStringAny/Noop",
		inBuf: `{"A":1,"B":2}`,
		inVal: new(structInlinePointerMapStringAny),
		want:  addr(structInlinePointerMapStringAny{A: 1, X: nil, B: 2}),
	}, {
		name:  "Structs/InlinedFallback/PointerMapStringAny/Alloc",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: new(structInlinePointerMapStringAny),
		want:  addr(structInlinePointerMapStringAny{A: 1, X: addr(jsonObject{"fizz": "buzz"}).(*jsonObject), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/PointerMapStringAny/Merge",
		inBuf: `{"A":1,"fizz":"wuzz","B":2}`,
		inVal: addr(structInlinePointerMapStringAny{X: addr(jsonObject{"fizz": "buzz"}).(*jsonObject)}),
		want:  addr(structInlinePointerMapStringAny{A: 1, X: addr(jsonObject{"fizz": "wuzz"}).(*jsonObject), B: 2}),
	}, {
		name:  "Structs/InlinedFallback/PointerMapStringAny/Nested/Nil",
		inBuf: `{"fizz":"buzz"}`,
		inVal: new(structInlineInlinePointerMapStringAny),
		want: addr(structInlineInlinePointerMapStringAny{
			X: struct {
				X *jsonObject `json:",inline"`
			}{X: addr(jsonObject{"fizz": "buzz"}).(*jsonObject)},
		}),
	}, {
		name:  "Structs/InlinedFallback/MapStringInt",
		inBuf: `{"zero": 0, "one": 1, "two": 2}`,
		inVal: new(structInlineMapStringInt),
		want: addr(structInlineMapStringInt{
			X: map[string]int{"zero": 0, "one": 1, "two": 2},
		}),
	}, {
		name:  "Structs/InlinedFallback/MapStringInt/Null",
		inBuf: `{"zero": 0, "one": null, "two": 2}`,
		inVal: new(structInlineMapStringInt),
		want: addr(structInlineMapStringInt{
			X: map[string]int{"zero": 0, "one": 0, "two": 2},
		}),
	}, {
		name:  "Structs/InlinedFallback/MapStringInt/Invalid",
		inBuf: `{"zero": 0, "one": {}, "two": 2}`,
		inVal: new(structInlineMapStringInt),
		want: addr(structInlineMapStringInt{
			X: map[string]int{"zero": 0, "one": 0},
		}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: intType},
	}, {
		name:  "Structs/InlinedFallback/MapStringInt/StringifiedNumbers",
		uopts: UnmarshalOptions{StringifyNumbers: true},
		inBuf: `{"zero": 0, "one": "1", "two": 2}`,
		inVal: new(structInlineMapStringInt),
		want: addr(structInlineMapStringInt{
			X: map[string]int{"zero": 0, "one": 1, "two": 2},
		}),
	}, {
		name:  "Structs/InlinedFallback/RejectUnknownNames",
		uopts: UnmarshalOptions{RejectUnknownNames: true},
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: new(structInlineRawValue),
		// NOTE: DiscardUnknownMembers has no effect since this is "inline".
		want: addr(structInlineRawValue{
			A: 1,
			X: RawValue(`{"fizz":"buzz"}`),
			B: 2,
		}),
	}, {
		name:    "Structs/UnknownFallback/RejectUnknownNames",
		uopts:   UnmarshalOptions{RejectUnknownNames: true},
		inBuf:   `{"A":1,"fizz":"buzz","B":2}`,
		inVal:   new(structUnknownRawValue),
		want:    addr(structUnknownRawValue{A: 1}),
		wantErr: &SemanticError{action: "unmarshal", GoType: structUnknownRawValueType, Err: ErrUnknownName},
	}, {
		name:  "Structs/UnknownFallback",
		inBuf: `{"A":1,"fizz":"buzz","B":2}`,
		inVal: new(structUnknownRawValue),
		want: addr(structUnknownRawValue{
			A: 1,
			X: RawValue(`{"fizz":"buzz"}`),
			B: 2,
		}),
	}, {
		name:  "Structs/UnknownIgnored",
		uopts: UnmarshalOptions{RejectUnknownNames: false},
		inBuf: `{"unknown":"fizzbuzz"}`,
		inVal: new(structAll),
		want:  new(structAll),
	}, {
		name:    "Structs/RejectUnknownNames",
		uopts:   UnmarshalOptions{RejectUnknownNames: true},
		inBuf:   `{"unknown":"fizzbuzz"}`,
		inVal:   new(structAll),
		want:    new(structAll),
		wantErr: &SemanticError{action: "unmarshal", GoType: structAllType, Err: ErrUnknownName},
	}, {
		name:  "Structs/UnexportedIgnored",
		inBuf: `{"ignored":"unused"}`,
		inVal: new(structUnexportedIgnored),
		want:  new(structUnexportedIgnored),
	}, {
		name:  "Structs/IgnoredUnexportedEmbedded",
		inBuf: `{"namedString":"unused"}`,
		inVal: new(structIgnoredUnexportedEmbedded),
		want:  new(structIgnoredUnexportedEmbedded),
	}, {
		name:  "Structs/WeirdNames",
		inBuf: `{"":"empty",",":"comma","\"":"quote"}`,
		inVal: new(structWeirdNames),
		want:  addr(structWeirdNames{Empty: "empty", Comma: "comma", Quote: "quote"}),
	}, {
		name:  "Structs/NoCase/Exact",
		inBuf: `{"AaA":"AaA","AAa":"AAa","AAA":"AAA"}`,
		inVal: new(structNoCase),
		want:  addr(structNoCase{AaA: "AaA", AAa: "AAa", AAA: "AAA"}),
	}, {
		name:  "Structs/NoCase/Merge",
		inBuf: `{"AaA":"AaA","aaa":"aaa","aAa":"aAa"}`,
		inVal: new(structNoCase),
		want:  addr(structNoCase{AaA: "aAa"}),
	}, {
		name:  "Structs/OptionCaseInsensitive",
		uopts: UnmarshalOptions{MatchCaseInsensitiveNames: true},
		inBuf: `{"BOOL": true, "STRING": "hello", "BYTES": "AQID", "INT": -64, "UINT": 64, "FLOAT": 3.14159}`,
		inVal: new(structScalars),
		want:  addr(structScalars{Bool: true, String: "hello", Bytes: []byte{1, 2, 3}, Int: -64, Uint: 64, Float: 3.14159}),
	}, {
		name:  "Structs/OptionCaseSensitive",
		uopts: UnmarshalOptions{MatchCaseInsensitiveNames: false},
		inBuf: `{"BOOL": true, "STRING": "hello", "BYTES": "AQID", "INT": -64, "UINT": 64, "FLOAT": 3.14159}`,
		inVal: new(structScalars),
		want:  addr(structScalars{}),
	}, {
		name:    "Structs/Invalid/ErrUnexpectedEOF",
		inBuf:   ``,
		inVal:   addr(structAll{}),
		want:    addr(structAll{}),
		wantErr: io.ErrUnexpectedEOF,
	}, {
		name:    "Structs/Invalid/NestedErrUnexpectedEOF",
		inBuf:   `{"Ptr":`,
		inVal:   addr(structAll{}),
		want:    addr(structAll{Ptr: new(structAll)}),
		wantErr: io.ErrUnexpectedEOF,
	}, {
		name:    "Structs/Invalid/Conflicting",
		inBuf:   `{}`,
		inVal:   addr(structConflicting{}),
		want:    addr(structConflicting{}),
		wantErr: &SemanticError{action: "unmarshal", GoType: structConflictingType, Err: errors.New("Go struct fields A and B conflict over JSON object name \"conflict\"")},
	}, {
		name:    "Structs/Invalid/NoneExported",
		inBuf:   `{}`,
		inVal:   addr(structNoneExported{}),
		want:    addr(structNoneExported{}),
		wantErr: &SemanticError{action: "unmarshal", GoType: structNoneExportedType, Err: errors.New("Go struct kind has no exported fields")},
	}, {
		name:    "Structs/Invalid/MalformedTag",
		inBuf:   `{}`,
		inVal:   addr(structMalformedTag{}),
		want:    addr(structMalformedTag{}),
		wantErr: &SemanticError{action: "unmarshal", GoType: structMalformedTagType, Err: errors.New("Go struct field Malformed has malformed `json` tag: invalid character '\"' at start of option (expecting Unicode letter or single quote)")},
	}, {
		name:    "Structs/Invalid/UnexportedTag",
		inBuf:   `{}`,
		inVal:   addr(structUnexportedTag{}),
		want:    addr(structUnexportedTag{}),
		wantErr: &SemanticError{action: "unmarshal", GoType: structUnexportedTagType, Err: errors.New("unexported Go struct field unexported cannot have non-ignored `json:\"name\"` tag")},
	}, {
		name:    "Structs/Invalid/UnexportedEmbedded",
		inBuf:   `{}`,
		inVal:   addr(structUnexportedEmbedded{}),
		want:    addr(structUnexportedEmbedded{}),
		wantErr: &SemanticError{action: "unmarshal", GoType: structUnexportedEmbeddedType, Err: errors.New("embedded Go struct field namedString of an unexported type must be explicitly ignored with a `json:\"-\"` tag")},
	}, {
		name: "Structs/Unknown",
		inBuf: `{
	"object0": {},
	"object1": {"key1": "value"},
	"object2": {"key1": "value", "key2": "value"},
	"objects": {"":{"":{"":{}}}},
	"array0": [],
	"array1": ["value1"],
	"array2": ["value1", "value2"],
	"array": [[[]]],
	"scalars": [null, false, true, "string", 12.345]
}`,
		inVal: addr(struct{}{}),
		want:  addr(struct{}{}),
	}, {
		name:  "Slices/Null",
		inBuf: `null`,
		inVal: addr([]string{"something"}),
		want:  addr([]string(nil)),
	}, {
		name:  "Slices/Bool",
		inBuf: `[true,false]`,
		inVal: new([]bool),
		want:  addr([]bool{true, false}),
	}, {
		name:  "Slices/String",
		inBuf: `["hello","goodbye"]`,
		inVal: new([]string),
		want:  addr([]string{"hello", "goodbye"}),
	}, {
		name:  "Slices/Bytes",
		inBuf: `["aGVsbG8=","Z29vZGJ5ZQ=="]`,
		inVal: new([][]byte),
		want:  addr([][]byte{[]byte("hello"), []byte("goodbye")}),
	}, {
		name:  "Slices/Int",
		inBuf: `[-2,-1,0,1,2]`,
		inVal: new([]int),
		want:  addr([]int{-2, -1, 0, 1, 2}),
	}, {
		name:  "Slices/Uint",
		inBuf: `[0,1,2,3,4]`,
		inVal: new([]uint),
		want:  addr([]uint{0, 1, 2, 3, 4}),
	}, {
		name:  "Slices/Float",
		inBuf: `[3.14159,12.34]`,
		inVal: new([]float64),
		want:  addr([]float64{3.14159, 12.34}),
	}, {
		// NOTE: The semantics differs from v1, where the slice length is reset
		// and new elements are appended to the end.
		// See https://golang.org/issue/21092.
		name:  "Slices/Merge",
		inBuf: `[{"k3":"v3"},{"k4":"v4"}]`,
		inVal: addr([]map[string]string{{"k1": "v1"}, {"k2": "v2"}}[:1]),
		want:  addr([]map[string]string{{"k3": "v3"}, {"k4": "v4"}}),
	}, {
		name:    "Slices/Invalid/Channel",
		inBuf:   `["hello"]`,
		inVal:   new([]chan string),
		want:    addr([]chan string{nil}),
		wantErr: &SemanticError{action: "unmarshal", GoType: chanStringType},
	}, {
		name:  "Slices/RecursiveSlice",
		inBuf: `[[],[],[[]],[[],[]]]`,
		inVal: new(recursiveSlice),
		want: addr(recursiveSlice{
			{},
			{},
			{{}},
			{{}, {}},
		}),
	}, {
		name:    "Slices/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr([]string{"nochange"}),
		want:    addr([]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: sliceStringType},
	}, {
		name:    "Slices/Invalid/String",
		inBuf:   `""`,
		inVal:   addr([]string{"nochange"}),
		want:    addr([]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: sliceStringType},
	}, {
		name:    "Slices/Invalid/Number",
		inBuf:   `0`,
		inVal:   addr([]string{"nochange"}),
		want:    addr([]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: sliceStringType},
	}, {
		name:    "Slices/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr([]string{"nochange"}),
		want:    addr([]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: sliceStringType},
	}, {
		name:  "Arrays/Null",
		inBuf: `null`,
		inVal: addr([1]string{"something"}),
		want:  addr([1]string{}),
	}, {
		name:  "Arrays/Bool",
		inBuf: `[true,false]`,
		inVal: new([2]bool),
		want:  addr([2]bool{true, false}),
	}, {
		name:  "Arrays/String",
		inBuf: `["hello","goodbye"]`,
		inVal: new([2]string),
		want:  addr([2]string{"hello", "goodbye"}),
	}, {
		name:  "Arrays/Bytes",
		inBuf: `["aGVsbG8=","Z29vZGJ5ZQ=="]`,
		inVal: new([2][]byte),
		want:  addr([2][]byte{[]byte("hello"), []byte("goodbye")}),
	}, {
		name:  "Arrays/Int",
		inBuf: `[-2,-1,0,1,2]`,
		inVal: new([5]int),
		want:  addr([5]int{-2, -1, 0, 1, 2}),
	}, {
		name:  "Arrays/Uint",
		inBuf: `[0,1,2,3,4]`,
		inVal: new([5]uint),
		want:  addr([5]uint{0, 1, 2, 3, 4}),
	}, {
		name:  "Arrays/Float",
		inBuf: `[3.14159,12.34]`,
		inVal: new([2]float64),
		want:  addr([2]float64{3.14159, 12.34}),
	}, {
		// NOTE: The semantics differs from v1, where elements are not merged.
		// This is to maintain consistent merge semantics with slices.
		name:  "Arrays/Merge",
		inBuf: `[{"k3":"v3"},{"k4":"v4"}]`,
		inVal: addr([2]map[string]string{{"k1": "v1"}, {"k2": "v2"}}),
		want:  addr([2]map[string]string{{"k3": "v3"}, {"k4": "v4"}}),
	}, {
		name:    "Arrays/Invalid/Channel",
		inBuf:   `["hello"]`,
		inVal:   new([1]chan string),
		want:    new([1]chan string),
		wantErr: &SemanticError{action: "unmarshal", GoType: chanStringType},
	}, {
		name:    "Arrays/Invalid/Underflow",
		inBuf:   `[]`,
		inVal:   new([1]string),
		want:    addr([1]string{}),
		wantErr: &SemanticError{action: "unmarshal", GoType: array1StringType, Err: errors.New("too few array elements")},
	}, {
		name:    "Arrays/Invalid/Overflow",
		inBuf:   `["1","2"]`,
		inVal:   new([1]string),
		want:    addr([1]string{"1"}),
		wantErr: &SemanticError{action: "unmarshal", GoType: array1StringType, Err: errors.New("too many array elements")},
	}, {
		name:    "Arrays/Invalid/Bool",
		inBuf:   `true`,
		inVal:   addr([1]string{"nochange"}),
		want:    addr([1]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: 't', GoType: array1StringType},
	}, {
		name:    "Arrays/Invalid/String",
		inBuf:   `""`,
		inVal:   addr([1]string{"nochange"}),
		want:    addr([1]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: array1StringType},
	}, {
		name:    "Arrays/Invalid/Number",
		inBuf:   `0`,
		inVal:   addr([1]string{"nochange"}),
		want:    addr([1]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: array1StringType},
	}, {
		name:    "Arrays/Invalid/Object",
		inBuf:   `{}`,
		inVal:   addr([1]string{"nochange"}),
		want:    addr([1]string{"nochange"}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: array1StringType},
	}, {
		name:  "Pointers/NullL0",
		inBuf: `null`,
		inVal: new(*string),
		want:  addr((*string)(nil)),
	}, {
		name:  "Pointers/NullL1",
		inBuf: `null`,
		inVal: addr((**string)(new(*string))),
		want:  addr((**string)(nil)),
	}, {
		name:  "Pointers/Bool",
		inBuf: `true`,
		inVal: addr(new(bool)),
		want:  addr(addr(true)),
	}, {
		name:  "Pointers/String",
		inBuf: `"hello"`,
		inVal: addr(new(string)),
		want:  addr(addr("hello")),
	}, {
		name:  "Pointers/Bytes",
		inBuf: `"aGVsbG8="`,
		inVal: addr(new([]byte)),
		want:  addr(addr([]byte("hello"))),
	}, {
		name:  "Pointers/Int",
		inBuf: `-123`,
		inVal: addr(new(int)),
		want:  addr(addr(int(-123))),
	}, {
		name:  "Pointers/Uint",
		inBuf: `123`,
		inVal: addr(new(int)),
		want:  addr(addr(int(123))),
	}, {
		name:  "Pointers/Float",
		inBuf: `123.456`,
		inVal: addr(new(float64)),
		want:  addr(addr(float64(123.456))),
	}, {
		name:  "Pointers/Allocate",
		inBuf: `"hello"`,
		inVal: addr((*string)(nil)),
		want:  addr(addr("hello")),
	}, {
		name:  "Interfaces/Empty/Null",
		inBuf: `null`,
		inVal: new(interface{}),
		want:  new(interface{}),
	}, {
		name:  "Interfaces/NonEmpty/Null",
		inBuf: `null`,
		inVal: new(io.Reader),
		want:  new(io.Reader),
	}, {
		name:    "Interfaces/NonEmpty/Invalid",
		inBuf:   `"hello"`,
		inVal:   new(io.Reader),
		want:    new(io.Reader),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: ioReaderType, Err: errors.New("cannot derive concrete type for non-empty interface")},
	}, {
		name:  "Interfaces/Empty/False",
		inBuf: `false`,
		inVal: new(interface{}),
		want: func() interface{} {
			var vi interface{} = false
			return &vi
		}(),
	}, {
		name:  "Interfaces/Empty/True",
		inBuf: `true`,
		inVal: new(interface{}),
		want: func() interface{} {
			var vi interface{} = true
			return &vi
		}(),
	}, {
		name:  "Interfaces/Empty/String",
		inBuf: `"string"`,
		inVal: new(interface{}),
		want: func() interface{} {
			var vi interface{} = "string"
			return &vi
		}(),
	}, {
		name:  "Interfaces/Empty/Number",
		inBuf: `3.14159`,
		inVal: new(interface{}),
		want: func() interface{} {
			var vi interface{} = 3.14159
			return &vi
		}(),
	}, {
		name:  "Interfaces/Empty/Object",
		inBuf: `{"k":"v"}`,
		inVal: new(interface{}),
		want: func() interface{} {
			var vi interface{} = map[string]interface{}{"k": "v"}
			return &vi
		}(),
	}, {
		name:  "Interfaces/Empty/Array",
		inBuf: `["v"]`,
		inVal: new(interface{}),
		want: func() interface{} {
			var vi interface{} = []interface{}{"v"}
			return &vi
		}(),
	}, {
		// NOTE: The semantics differs from v1,
		// where existing map entries were not merged into.
		// See https://golang.org/issue/26946.
		// See https://golang.org/issue/33993.
		name:  "Interfaces/Merge/Map",
		inBuf: `{"k2":"v2"}`,
		inVal: func() interface{} {
			var vi interface{} = map[string]string{"k1": "v1"}
			return &vi
		}(),
		want: func() interface{} {
			var vi interface{} = map[string]string{"k1": "v1", "k2": "v2"}
			return &vi
		}(),
	}, {
		name:  "Interfaces/Merge/Struct",
		inBuf: `{"Array":["goodbye"]}`,
		inVal: func() interface{} {
			var vi interface{} = structAll{String: "hello"}
			return &vi
		}(),
		want: func() interface{} {
			var vi interface{} = structAll{String: "hello", Array: [1]string{"goodbye"}}
			return &vi
		}(),
	}, {
		name:  "Interfaces/Merge/NamedInt",
		inBuf: `64`,
		inVal: func() interface{} {
			var vi interface{} = namedInt64(-64)
			return &vi
		}(),
		want: func() interface{} {
			var vi interface{} = namedInt64(+64)
			return &vi
		}(),
	}, {
		name:  "Methods/NilPointer/Null",
		inBuf: `{"X":null}`,
		inVal: addr(struct{ X *allMethods }{X: (*allMethods)(nil)}),
		want:  addr(struct{ X *allMethods }{X: (*allMethods)(nil)}), // method should not be called
	}, {
		name:  "Methods/NilPointer/Value",
		inBuf: `{"X":"value"}`,
		inVal: addr(struct{ X *allMethods }{X: (*allMethods)(nil)}),
		want:  addr(struct{ X *allMethods }{X: &allMethods{method: "UnmarshalNextJSON", value: []byte(`"value"`)}}),
	}, {
		name:  "Methods/NilInterface/Null",
		inBuf: `{"X":null}`,
		inVal: addr(struct{ X MarshalerV2 }{X: (*allMethods)(nil)}),
		want:  addr(struct{ X MarshalerV2 }{X: nil}), // interface value itself is nil'd out
	}, {
		name:  "Methods/NilInterface/Value",
		inBuf: `{"X":"value"}`,
		inVal: addr(struct{ X MarshalerV2 }{X: (*allMethods)(nil)}),
		want:  addr(struct{ X MarshalerV2 }{X: &allMethods{method: "UnmarshalNextJSON", value: []byte(`"value"`)}}),
	}, {
		name:  "Methods/AllMethods",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *allMethods }),
		want:  addr(struct{ X *allMethods }{X: &allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)}}),
	}, {
		name:  "Methods/AllMethodsExceptJSONv2",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *allMethodsExceptJSONv2 }),
		want:  addr(struct{ X *allMethodsExceptJSONv2 }{X: &allMethodsExceptJSONv2{allMethods: allMethods{method: "UnmarshalJSON", value: []byte(`"hello"`)}}}),
	}, {
		name:  "Methods/AllMethodsExceptJSONv1",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *allMethodsExceptJSONv1 }),
		want:  addr(struct{ X *allMethodsExceptJSONv1 }{X: &allMethodsExceptJSONv1{allMethods: allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)}}}),
	}, {
		name:  "Methods/AllMethodsExceptText",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *allMethodsExceptText }),
		want:  addr(struct{ X *allMethodsExceptText }{X: &allMethodsExceptText{allMethods: allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)}}}),
	}, {
		name:  "Methods/OnlyMethodJSONv2",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *onlyMethodJSONv2 }),
		want:  addr(struct{ X *onlyMethodJSONv2 }{X: &onlyMethodJSONv2{allMethods: allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)}}}),
	}, {
		name:  "Methods/OnlyMethodJSONv1",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *onlyMethodJSONv1 }),
		want:  addr(struct{ X *onlyMethodJSONv1 }{X: &onlyMethodJSONv1{allMethods: allMethods{method: "UnmarshalJSON", value: []byte(`"hello"`)}}}),
	}, {
		name:  "Methods/OnlyMethodText",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X *onlyMethodText }),
		want:  addr(struct{ X *onlyMethodText }{X: &onlyMethodText{allMethods: allMethods{method: "UnmarshalText", value: []byte(`hello`)}}}),
	}, {
		name:  "Methods/IP",
		inBuf: `"192.168.0.100"`,
		inVal: new(net.IP),
		want:  addr(net.IPv4(192, 168, 0, 100)),
	}, {
		// NOTE: Fixes https://golang.org/issue/46516.
		name:  "Methods/Anonymous",
		inBuf: `{"X":"hello"}`,
		inVal: new(struct{ X struct{ allMethods } }),
		want:  addr(struct{ X struct{ allMethods } }{X: struct{ allMethods }{allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)}}}),
	}, {
		// NOTE: Fixes https://golang.org/issue/22967.
		name:  "Methods/Addressable",
		inBuf: `{"V":"hello","M":{"K":"hello"},"I":"hello"}`,
		inVal: addr(struct {
			V allMethods
			M map[string]allMethods
			I interface{}
		}{
			I: allMethods{}, // need to initialize with concrete value
		}),
		want: addr(struct {
			V allMethods
			M map[string]allMethods
			I interface{}
		}{
			V: allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)},
			M: map[string]allMethods{"K": {method: "UnmarshalNextJSON", value: []byte(`"hello"`)}},
			I: allMethods{method: "UnmarshalNextJSON", value: []byte(`"hello"`)},
		}),
	}, {
		// NOTE: Fixes https://golang.org/issue/29732.
		name:  "Methods/MapKey/JSONv2",
		inBuf: `{"k1":"v1b","k2":"v2"}`,
		inVal: addr(map[structMethodJSONv2]string{{"k1"}: "v1a", {"k3"}: "v3"}),
		want:  addr(map[structMethodJSONv2]string{{"k1"}: "v1b", {"k2"}: "v2", {"k3"}: "v3"}),
	}, {
		// NOTE: Fixes https://golang.org/issue/29732.
		name:  "Methods/MapKey/JSONv1",
		inBuf: `{"k1":"v1b","k2":"v2"}`,
		inVal: addr(map[structMethodJSONv1]string{{"k1"}: "v1a", {"k3"}: "v3"}),
		want:  addr(map[structMethodJSONv1]string{{"k1"}: "v1b", {"k2"}: "v2", {"k3"}: "v3"}),
	}, {
		name:  "Methods/MapKey/Text",
		inBuf: `{"k1":"v1b","k2":"v2"}`,
		inVal: addr(map[structMethodText]string{{"k1"}: "v1a", {"k3"}: "v3"}),
		want:  addr(map[structMethodText]string{{"k1"}: "v1b", {"k2"}: "v2", {"k3"}: "v3"}),
	}, {
		name:  "Methods/Invalid/JSONv2/Error",
		inBuf: `{}`,
		inVal: addr(unmarshalJSONv2Func(func(*Decoder, UnmarshalOptions) error {
			return errors.New("some error")
		})),
		wantErr: &SemanticError{action: "unmarshal", GoType: unmarshalJSONv2FuncType, Err: errors.New("some error")},
	}, {
		name: "Methods/Invalid/JSONv2/TooFew",
		inVal: addr(unmarshalJSONv2Func(func(*Decoder, UnmarshalOptions) error {
			return nil // do nothing
		})),
		wantErr: &SemanticError{action: "unmarshal", GoType: unmarshalJSONv2FuncType, Err: errors.New("must read exactly one JSON value")},
	}, {
		name:  "Methods/Invalid/JSONv2/TooMany",
		inBuf: `{}{}`,
		inVal: addr(unmarshalJSONv2Func(func(dec *Decoder, uo UnmarshalOptions) error {
			dec.ReadValue()
			dec.ReadValue()
			return nil
		})),
		wantErr: &SemanticError{action: "unmarshal", GoType: unmarshalJSONv2FuncType, Err: errors.New("must read exactly one JSON value")},
	}, {
		name:  "Methods/Invalid/JSONv1/Error",
		inBuf: `{}`,
		inVal: addr(unmarshalJSONv1Func(func([]byte) error {
			return errors.New("some error")
		})),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: unmarshalJSONv1FuncType, Err: errors.New("some error")},
	}, {
		name:  "Methods/Invalid/Text/Error",
		inBuf: `"value"`,
		inVal: addr(unmarshalTextFunc(func([]byte) error {
			return errors.New("some error")
		})),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: unmarshalTextFuncType, Err: errors.New("some error")},
	}, {
		name:  "Methods/Invalid/Text/Syntax",
		inBuf: `{}`,
		inVal: addr(unmarshalTextFunc(func([]byte) error {
			panic("should not be called")
		})),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '{', GoType: unmarshalTextFuncType, Err: errors.New("JSON value must be string type")},
	}, {
		name:  "Duration/Null",
		inBuf: `{"D1":null,"D2":null}`,
		inVal: addr(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{1, 1}),
		want: addr(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{0, 0}),
	}, {
		name:  "Duration/Zero",
		inBuf: `{"D1":"0s","D2":0}`,
		inVal: addr(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{1, 1}),
		want: addr(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{0, 0}),
	}, {
		name:  "Duration/Positive",
		inBuf: `{"D1":"34293h33m9.123456789s","D2":123456789123456789}`,
		inVal: new(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}),
		want: addr(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{
			123456789123456789,
			123456789123456789,
		}),
	}, {
		name:  "Duration/Negative",
		inBuf: `{"D1":"-34293h33m9.123456789s","D2":-123456789123456789}`,
		inVal: new(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}),
		want: addr(struct {
			D1 time.Duration
			D2 time.Duration `json:",format:nanos"`
		}{
			-123456789123456789,
			-123456789123456789,
		}),
	}, {
		name:  "Duration/Nanos/Mismatch",
		inBuf: `{"D":"34293h33m9.123456789s"}`,
		inVal: addr(struct {
			D time.Duration `json:",format:nanos"`
		}{1}),
		want: addr(struct {
			D time.Duration `json:",format:nanos"`
		}{1}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: timeDurationType},
	}, {
		name:  "Duration/Nanos/Invalid",
		inBuf: `{"D":1.324}`,
		inVal: addr(struct {
			D time.Duration `json:",format:nanos"`
		}{1}),
		want: addr(struct {
			D time.Duration `json:",format:nanos"`
		}{1}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: timeDurationType, Err: func() error {
			_, err := strconv.ParseInt("1.324", 10, 64)
			return err
		}()},
	}, {
		name:  "Duration/String/Mismatch",
		inBuf: `{"D":-123456789123456789}`,
		inVal: addr(struct {
			D time.Duration
		}{1}),
		want: addr(struct {
			D time.Duration
		}{1}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: timeDurationType},
	}, {
		name:  "Duration/String/Invalid",
		inBuf: `{"D":"5minkutes"}`,
		inVal: addr(struct {
			D time.Duration
		}{1}),
		want: addr(struct {
			D time.Duration
		}{1}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: timeDurationType, Err: func() error {
			_, err := time.ParseDuration("5minkutes")
			return err
		}()},
	}, {
		name:  "Duration/Syntax/Invalid",
		inBuf: `{"D":x}`,
		inVal: addr(struct {
			D time.Duration
		}{1}),
		want: addr(struct {
			D time.Duration
		}{1}),
		wantErr: newInvalidCharacterError('x', "at start of value").withOffset(int64(len(`{"D":`))),
	}, {
		name:  "Duration/Format/Invalid",
		inBuf: `{"D":"0s"}`,
		inVal: addr(struct {
			D time.Duration `json:",format:invalid"`
		}{1}),
		want: addr(struct {
			D time.Duration `json:",format:invalid"`
		}{1}),
		wantErr: &SemanticError{action: "unmarshal", GoType: timeDurationType, Err: errors.New(`invalid format flag: "invalid"`)},
	}, {
		name:  "Time/Zero",
		inBuf: `{"T1":"0001-01-01T00:00:00Z","T2":"01 Jan 01 00:00 UTC","T3":"0001-01-01","T4":"0001-01-01T00:00:00Z","T5":"0001-01-01T00:00:00Z"}`,
		inVal: new(struct {
			T1 time.Time
			T2 time.Time `json:",format:RFC822"`
			T3 time.Time `json:",format:'2006-01-02'"`
			T4 time.Time `json:",omitzero"`
			T5 time.Time `json:",omitempty"`
		}),
		want: addr(struct {
			T1 time.Time
			T2 time.Time `json:",format:RFC822"`
			T3 time.Time `json:",format:'2006-01-02'"`
			T4 time.Time `json:",omitzero"`
			T5 time.Time `json:",omitempty"`
		}{
			mustParseTime(time.RFC3339Nano, "0001-01-01T00:00:00Z"),
			mustParseTime(time.RFC822, "01 Jan 01 00:00 UTC"),
			mustParseTime("2006-01-02", "0001-01-01"),
			mustParseTime(time.RFC3339Nano, "0001-01-01T00:00:00Z"),
			mustParseTime(time.RFC3339Nano, "0001-01-01T00:00:00Z"),
		}),
	}, {
		name: "Time/Format",
		inBuf: `{
			"T1": "1234-01-02T03:04:05.000000006Z",
			"T2": "Mon Jan  2 03:04:05 1234",
			"T3": "Mon Jan  2 03:04:05 UTC 1234",
			"T4": "Mon Jan 02 03:04:05 +0000 1234",
			"T5": "02 Jan 34 03:04 UTC",
			"T6": "02 Jan 34 03:04 +0000",
			"T7": "Monday, 02-Jan-34 03:04:05 UTC",
			"T8": "Mon, 02 Jan 1234 03:04:05 UTC",
			"T9": "Mon, 02 Jan 1234 03:04:05 +0000",
			"T10": "1234-01-02T03:04:05Z",
			"T11": "1234-01-02T03:04:05.000000006Z",
			"T12": "3:04AM",
			"T13": "Jan  2 03:04:05",
			"T14": "Jan  2 03:04:05.000",
			"T15": "Jan  2 03:04:05.000000",
			"T16": "Jan  2 03:04:05.000000006",
			"T17": "1234-01-02",
			"T18": "\"weird\"1234"
		}`,
		inVal: new(structTimeFormat),
		want: addr(structTimeFormat{
			mustParseTime(time.RFC3339Nano, "1234-01-02T03:04:05.000000006Z"),
			mustParseTime(time.ANSIC, "Mon Jan  2 03:04:05 1234"),
			mustParseTime(time.UnixDate, "Mon Jan  2 03:04:05 UTC 1234"),
			mustParseTime(time.RubyDate, "Mon Jan 02 03:04:05 +0000 1234"),
			mustParseTime(time.RFC822, "02 Jan 34 03:04 UTC"),
			mustParseTime(time.RFC822Z, "02 Jan 34 03:04 +0000"),
			mustParseTime(time.RFC850, "Monday, 02-Jan-34 03:04:05 UTC"),
			mustParseTime(time.RFC1123, "Mon, 02 Jan 1234 03:04:05 UTC"),
			mustParseTime(time.RFC1123Z, "Mon, 02 Jan 1234 03:04:05 +0000"),
			mustParseTime(time.RFC3339, "1234-01-02T03:04:05Z"),
			mustParseTime(time.RFC3339Nano, "1234-01-02T03:04:05.000000006Z"),
			mustParseTime(time.Kitchen, "3:04AM"),
			mustParseTime(time.Stamp, "Jan  2 03:04:05"),
			mustParseTime(time.StampMilli, "Jan  2 03:04:05.000"),
			mustParseTime(time.StampMicro, "Jan  2 03:04:05.000000"),
			mustParseTime(time.StampNano, "Jan  2 03:04:05.000000006"),
			mustParseTime("2006-01-02", "1234-01-02"),
			mustParseTime(`\"weird\"2006`, `\"weird\"1234`),
		}),
	}, {
		name:  "Time/Format/Null",
		inBuf: `{"T1": null,"T2": null,"T3": null,"T4": null,"T5": null,"T6": null,"T7": null,"T8": null,"T9": null,"T10": null,"T11": null,"T12": null,"T13": null,"T14": null,"T15": null,"T16": null,"T17": null,"T18": null}`,
		inVal: addr(structTimeFormat{
			mustParseTime(time.RFC3339Nano, "1234-01-02T03:04:05.000000006Z"),
			mustParseTime(time.ANSIC, "Mon Jan  2 03:04:05 1234"),
			mustParseTime(time.UnixDate, "Mon Jan  2 03:04:05 UTC 1234"),
			mustParseTime(time.RubyDate, "Mon Jan 02 03:04:05 +0000 1234"),
			mustParseTime(time.RFC822, "02 Jan 34 03:04 UTC"),
			mustParseTime(time.RFC822Z, "02 Jan 34 03:04 +0000"),
			mustParseTime(time.RFC850, "Monday, 02-Jan-34 03:04:05 UTC"),
			mustParseTime(time.RFC1123, "Mon, 02 Jan 1234 03:04:05 UTC"),
			mustParseTime(time.RFC1123Z, "Mon, 02 Jan 1234 03:04:05 +0000"),
			mustParseTime(time.RFC3339, "1234-01-02T03:04:05Z"),
			mustParseTime(time.RFC3339Nano, "1234-01-02T03:04:05.000000006Z"),
			mustParseTime(time.Kitchen, "3:04AM"),
			mustParseTime(time.Stamp, "Jan  2 03:04:05"),
			mustParseTime(time.StampMilli, "Jan  2 03:04:05.000"),
			mustParseTime(time.StampMicro, "Jan  2 03:04:05.000000"),
			mustParseTime(time.StampNano, "Jan  2 03:04:05.000000006"),
			mustParseTime("2006-01-02", "1234-01-02"),
			mustParseTime(`\"weird\"2006`, `\"weird\"1234`),
		}),
		want: new(structTimeFormat),
	}, {
		name:  "Time/RFC3339/Mismatch",
		inBuf: `{"T":1234}`,
		inVal: new(struct {
			T time.Time
		}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '0', GoType: timeTimeType},
	}, {
		name:  "Time/RFC3339/Mismatch",
		inBuf: `{"T":"2021-09-29T12:44:52"}`,
		inVal: new(struct {
			T time.Time
		}),
		wantErr: &SemanticError{action: "unmarshal", JSONKind: '"', GoType: timeTimeType, Err: func() error {
			_, err := time.Parse(time.RFC3339Nano, "2021-09-29T12:44:52")
			return err
		}()},
	}, {
		name:  "Time/Format/Invalid",
		inBuf: `{"T":""}`,
		inVal: new(struct {
			T time.Time `json:",format:UndefinedConstant"`
		}),
		wantErr: &SemanticError{action: "unmarshal", GoType: timeTimeType, Err: errors.New(`undefined format layout: UndefinedConstant`)},
	}, {
		name:  "Time/Syntax/Invalid",
		inBuf: `{"T":x}`,
		inVal: new(struct {
			T time.Time
		}),
		wantErr: newInvalidCharacterError('x', "at start of value").withOffset(int64(len(`{"D":`))),
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.inVal
			gotErr := tt.uopts.Unmarshal(tt.dopts, []byte(tt.inBuf), got)
			if !reflect.DeepEqual(got, tt.want) && tt.want != nil {
				t.Errorf("Unmarshal output mismatch:\ngot  %v\nwant %v", got, tt.want)
			}
			if !reflect.DeepEqual(gotErr, tt.wantErr) {
				t.Errorf("Unmarshal error mismatch:\ngot  %v\nwant %v", gotErr, tt.wantErr)
			}
		})
	}
}

func TestUnmarshalReuse(t *testing.T) {
	t.Run("Bytes", func(t *testing.T) {
		in := make([]byte, 3)
		want := &in[0]
		if err := Unmarshal([]byte(`"AQID"`), &in); err != nil {
			t.Fatalf("Unmarshal error: %v", err)
		}
		got := &in[0]
		if got != want {
			t.Errorf("input buffer was not reused")
		}
	})
	t.Run("Slices", func(t *testing.T) {
		in := make([]int, 3)
		want := &in[0]
		if err := Unmarshal([]byte(`[0,1,2]`), &in); err != nil {
			t.Fatalf("Unmarshal error: %v", err)
		}
		got := &in[0]
		if got != want {
			t.Errorf("input slice was not reused")
		}
	})
	t.Run("Maps", func(t *testing.T) {
		in := make(map[string]string)
		want := reflect.ValueOf(in).Pointer()
		if err := Unmarshal([]byte(`{"key":"value"}`), &in); err != nil {
			t.Fatalf("Unmarshal error: %v", err)
		}
		got := reflect.ValueOf(in).Pointer()
		if got != want {
			t.Errorf("input map was not reused")
		}
	})
	t.Run("Pointers", func(t *testing.T) {
		in := addr(addr(addr("hello"))).(***string)
		want := **in
		if err := Unmarshal([]byte(`"goodbye"`), &in); err != nil {
			t.Fatalf("Unmarshal error: %v", err)
		}
		got := **in
		if got != want {
			t.Errorf("input pointer was not reused")
		}
	})
}
