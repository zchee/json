// Copyright 2021 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package json

import (
	"encoding"
	"errors"
	"reflect"
	"testing"
)

type unexported struct{}

func TestMakeStructFields(t *testing.T) {
	type Embed struct {
		Foo string
	}
	type Recursive struct {
		A          string
		*Recursive `json:",inline"`
		B          string
	}
	type MapStringAny map[string]interface{}
	tests := []struct {
		name    string
		in      interface{}
		want    structFields
		wantErr error
	}{{
		name: "Names",
		in: struct {
			F1 string
			F2 string `json:"-"`
			F3 string `json:"json_name"`
			f3 string
			F5 string `json:"json_name_nocase,nocase"`
		}{},
		want: structFields{
			flattened: []structField{
				{id: 0, index: []int{0}, typ: stringType, fieldOptions: fieldOptions{name: "F1"}},
				{id: 1, index: []int{2}, typ: stringType, fieldOptions: fieldOptions{name: "json_name", hasName: true}},
				{id: 2, index: []int{4}, typ: stringType, fieldOptions: fieldOptions{name: "json_name_nocase", hasName: true, nocase: true}},
			},
		},
	}, {
		name: "BreadthFirstSearch",
		in: struct {
			L1A string
			L1B struct {
				L2A string
				L2B struct {
					L3A string
				} `json:",inline"`
				L2C string
			} `json:",inline"`
			L1C string
			L1D struct {
				L2D string
				L2E struct {
					L3B string
				} `json:",inline"`
				L2F string
			} `json:",inline"`
			L1E string
		}{},
		want: structFields{
			flattened: []structField{
				{id: 0, index: []int{0}, typ: stringType, fieldOptions: fieldOptions{name: "L1A"}},
				{id: 1, index: []int{2}, typ: stringType, fieldOptions: fieldOptions{name: "L1C"}},
				{id: 2, index: []int{4}, typ: stringType, fieldOptions: fieldOptions{name: "L1E"}},
				{id: 3, index: []int{1, 0}, typ: stringType, fieldOptions: fieldOptions{name: "L2A"}},
				{id: 4, index: []int{1, 2}, typ: stringType, fieldOptions: fieldOptions{name: "L2C"}},
				{id: 5, index: []int{3, 0}, typ: stringType, fieldOptions: fieldOptions{name: "L2D"}},
				{id: 6, index: []int{3, 2}, typ: stringType, fieldOptions: fieldOptions{name: "L2F"}},
				{id: 7, index: []int{1, 1, 0}, typ: stringType, fieldOptions: fieldOptions{name: "L3A"}},
				{id: 8, index: []int{3, 1, 0}, typ: stringType, fieldOptions: fieldOptions{name: "L3B"}},
			},
		},
	}, {
		name: "NameResolution",
		in: struct {
			X1 struct {
				X struct {
					A string // loses in precedence to A
					B string // cancels out with X2.X.B
					D string // loses in precedence to D
				} `json:",inline"`
			} `json:",inline"`
			X2 struct {
				X struct {
					B string // cancels out with X1.X.B
					C string
					D string // loses in precedence to D
				} `json:",inline"`
			} `json:",inline"`
			A string // takes precedence over X1.X.A
			D string // takes precedence over X1.X.D and X2.X.D
		}{},
		want: structFields{
			flattened: []structField{
				{id: 0, index: []int{2}, typ: stringType, fieldOptions: fieldOptions{name: "A"}},
				{id: 1, index: []int{3}, typ: stringType, fieldOptions: fieldOptions{name: "D"}},
				{id: 2, index: []int{1, 0, 1}, typ: stringType, fieldOptions: fieldOptions{name: "C"}},
			},
		},
	}, {
		name: "Embed/Implicit",
		in: struct {
			Embed
		}{},
		want: structFields{
			flattened: []structField{
				{id: 0, index: []int{0, 0}, typ: stringType, fieldOptions: fieldOptions{name: "Foo"}},
			},
		},
	}, {
		name: "Embed/Explicit",
		in: struct {
			Embed `json:",inline"`
		}{},
		want: structFields{
			flattened: []structField{
				{id: 0, index: []int{0, 0}, typ: stringType, fieldOptions: fieldOptions{name: "Foo"}},
			},
		},
	}, {
		name: "Recursive",
		in: struct {
			A         string
			Recursive `json:",inline"`
			C         string
		}{},
		want: structFields{
			flattened: []structField{
				{id: 0, index: []int{0}, typ: stringType, fieldOptions: fieldOptions{name: "A"}},
				{id: 1, index: []int{2}, typ: stringType, fieldOptions: fieldOptions{name: "C"}},
				{id: 2, index: []int{1, 2}, typ: stringType, fieldOptions: fieldOptions{name: "B"}},
			},
		},
	}, {
		name: "InlinedFallback/Cancelation",
		in: struct {
			X1 struct {
				X RawValue `json:",inline"`
			} `json:",inline"`
			X2 struct {
				X map[string]interface{} `json:",unknown"`
			} `json:",inline"`
		}{},
		want: structFields{},
	}, {
		name: "InlinedFallback/Precedence",
		in: struct {
			X1 struct {
				X RawValue `json:",inline"`
			} `json:",inline"`
			X2 struct {
				X map[string]interface{} `json:",unknown"`
			} `json:",inline"`
			X map[string]RawValue `json:",unknown"`
		}{},
		want: structFields{
			inlinedFallback: &structField{id: 0, index: []int{2}, typ: reflect.TypeOf(map[string]RawValue(nil)), fieldOptions: fieldOptions{name: "X", unknown: true}},
		},
	}, {
		name: "InvalidUTF8",
		in: struct {
			Name string `json:"'\\xde\\xad\\xbe\\xef'"`
		}{},
		wantErr: errors.New(`Go struct fields Name has JSON object name "ޭ\xbe\xef" with invalid UTF-8`),
	}, {
		name: "DuplicateName",
		in: struct {
			A string `json:"same"`
			B string `json:"same"`
		}{},
		wantErr: errors.New(`Go struct fields A and B conflict over JSON object name "same"`),
	}, {
		name: "BothInlineAndUnknown",
		in: struct {
			A struct{} `json:",inline,unknown"`
		}{},
		wantErr: errors.New("Go struct field A cannot have both `inline` and `unknown` specified"),
	}, {
		name: "InlineWithOptions",
		in: struct {
			A struct{} `json:",inline,omitempty"`
		}{},
		wantErr: errors.New("Go struct field A cannot have any options other than `inline` or `unknown` specified"),
	}, {
		name: "UnknownWithOptions",
		in: struct {
			A map[string]interface{} `json:",inline,omitempty"`
		}{},
		wantErr: errors.New("Go struct field A cannot have any options other than `inline` or `unknown` specified"),
	}, {
		name: "InlineTextMarshaler",
		in: struct {
			A struct{ encoding.TextMarshaler } `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type struct { encoding.TextMarshaler } must not implement JSON marshal or unmarshal methods`),
	}, {
		name: "UnknownJSONMarshalerV1",
		in: struct {
			A struct{ MarshalerV1 } `json:",unknown"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type struct { json.MarshalerV1 } must not implement JSON marshal or unmarshal methods`),
	}, {
		name: "InlineJSONMarshalerV2",
		in: struct {
			A struct{ MarshalerV2 } `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type struct { json.MarshalerV2 } must not implement JSON marshal or unmarshal methods`),
	}, {
		name: "UnknownTextUnmarshaler",
		in: struct {
			A *struct{ encoding.TextUnmarshaler } `json:",unknown"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type struct { encoding.TextUnmarshaler } must not implement JSON marshal or unmarshal methods`),
	}, {
		name: "InlineJSONUnmarshalerV1",
		in: struct {
			A *struct{ UnmarshalerV1 } `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type struct { json.UnmarshalerV1 } must not implement JSON marshal or unmarshal methods`),
	}, {
		name: "UnknownJSONUnmarshalerV2",
		in: struct {
			A struct{ UnmarshalerV2 } `json:",unknown"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type struct { json.UnmarshalerV2 } must not implement JSON marshal or unmarshal methods`),
	}, {
		name: "UnknownStruct",
		in: struct {
			A struct {
				X, Y, Z string
			} `json:",unknown"`
		}{},
		wantErr: errors.New("inlined Go struct field A of type struct { X string; Y string; Z string } with `unknown` tag must be a Go map of string key or a json.RawValue"),
	}, {
		name: "InlineUnsupported/MapIntKey",
		in: struct {
			A map[int]interface{} `json:",unknown"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type map[int]interface {} must be a Go struct, Go map of string key, or json.RawValue`),
	}, {
		name: "InlineUnsupported/MapNamedStringKey",
		in: struct {
			A map[namedString]interface{} `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type map[json.namedString]interface {} must be a Go struct, Go map of string key, or json.RawValue`),
	}, {
		name: "InlineUnsupported/DoublePointer",
		in: struct {
			A **struct{} `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct field A of type *struct {} must be a Go struct, Go map of string key, or json.RawValue`),
	}, {
		name: "DuplicateInline",
		in: struct {
			A map[string]interface{} `json:",inline"`
			B RawValue               `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct fields A and B cannot both be a Go map or json.RawValue`),
	}, {
		name: "DuplicateEmbedInline",
		in: struct {
			MapStringAny
			B RawValue `json:",inline"`
		}{},
		wantErr: errors.New(`inlined Go struct fields MapStringAny and B cannot both be a Go map or json.RawValue`),
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := makeStructFields(reflect.TypeOf(tt.in))

			// Sanity check that pointers are consistent.
			ptrs := make(map[*structField]bool)
			for i := range got.flattened {
				ptrs[&got.flattened[i]] = true
			}
			for _, f := range got.byActualName {
				if !ptrs[f] {
					t.Error("byActualName pointer not in flattened")
				}
			}
			for _, fs := range got.byFoldedName {
				for _, f := range fs {
					if !ptrs[f] {
						t.Error("byFoldedName pointer not in flattened")
					}
				}
			}

			// Zero out fields that are incomparable.
			for i := range got.flattened {
				got.flattened[i].fncs = nil
				got.flattened[i].isEmpty = nil
			}
			if got.inlinedFallback != nil {
				got.inlinedFallback.fncs = nil
				got.inlinedFallback.isEmpty = nil
			}

			// Reproduce maps in want.
			if tt.wantErr == nil {
				tt.want.byActualName = make(map[string]*structField)
				for i := range tt.want.flattened {
					f := &tt.want.flattened[i]
					tt.want.byActualName[f.name] = f
				}
				tt.want.byFoldedName = make(map[string][]*structField)
				for i, f := range tt.want.flattened {
					foldedName := string(foldName([]byte(f.name)))
					tt.want.byFoldedName[foldedName] = append(tt.want.byFoldedName[foldedName], &tt.want.flattened[i])
				}
			}

			// Only compare underlying error to simplify test logic.
			var gotErr error
			if err != nil {
				gotErr = err.Err
			}

			if !reflect.DeepEqual(got, tt.want) || !reflect.DeepEqual(gotErr, tt.wantErr) {
				t.Errorf("makeStructFields(%T):\n\tgot  (%v, %v)\n\twant (%v, %v)", tt.in, got, gotErr, tt.want, tt.wantErr)
			}
		})
	}
}

func TestParseTagOptions(t *testing.T) {
	tests := []struct {
		name     string
		in       interface{} // must be a struct with a single field
		wantOpts fieldOptions
		wantErr  error
	}{{
		name: "GoName",
		in: struct {
			FieldName int
		}{},
		wantOpts: fieldOptions{name: "FieldName"},
	}, {
		name: "GoNameWithOptions",
		in: struct {
			FieldName int `json:",inline"`
		}{},
		wantOpts: fieldOptions{name: "FieldName", inline: true},
	}, {
		name: "Empty",
		in: struct {
			V int `json:""`
		}{},
		wantOpts: fieldOptions{name: "V"},
	}, {
		name: "Unexported",
		in: struct {
			v int `json:"Hello"`
		}{},
		wantErr: errors.New("unexported Go struct field v cannot have non-ignored `json:\"Hello\"` tag"),
	}, {
		name: "UnexportedEmpty",
		in: struct {
			v int `json:""`
		}{},
		wantErr: errors.New("unexported Go struct field v cannot have non-ignored `json:\"\"` tag"),
	}, {
		name: "EmbedUnexported",
		in: struct {
			unexported
		}{},
		wantErr: errors.New("embedded Go struct field unexported of an unexported type must be explicitly ignored with a `json:\"-\"` tag"),
	}, {
		name: "Ignored",
		in: struct {
			V int `json:"-"`
		}{},
		wantErr: errIgnoredField,
	}, {
		name: "IgnoredEmbedUnexported",
		in: struct {
			unexported `json:"-"`
		}{},
		wantErr: errIgnoredField,
	}, {
		name: "DashComma",
		in: struct {
			V int `json:"-,"`
		}{},
		wantErr: errors.New("Go struct field V has malformed `json` tag: invalid trailing ',' character"),
	}, {
		name: "QuotedDashName",
		in: struct {
			V int `json:"'-'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "-"},
	}, {
		name: "LatinPunctuationName",
		in: struct {
			V int `json:"$%-/"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "$%-/"},
	}, {
		name: "QuotedLatinPunctuationName",
		in: struct {
			V int `json:"'$%-/'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "$%-/"},
	}, {
		name: "LatinDigitsName",
		in: struct {
			V int `json:"0123456789"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "0123456789"},
	}, {
		name: "QuotedLatinDigitsName",
		in: struct {
			V int `json:"'0123456789'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "0123456789"},
	}, {
		name: "LatinUppercaseName",
		in: struct {
			V int `json:"ABCDEFGHIJKLMOPQRSTUVWXYZ"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "ABCDEFGHIJKLMOPQRSTUVWXYZ"},
	}, {
		name: "LatinLowercaseName",
		in: struct {
			V int `json:"abcdefghijklmnopqrstuvwxyz_"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "abcdefghijklmnopqrstuvwxyz_"},
	}, {
		name: "GreekName",
		in: struct {
			V string `json:"Ελλάδα"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "Ελλάδα"},
	}, {
		name: "QuotedGreekName",
		in: struct {
			V string `json:"'Ελλάδα'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "Ελλάδα"},
	}, {
		name: "ChineseName",
		in: struct {
			V string `json:"世界"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "世界"},
	}, {
		name: "QuotedChineseName",
		in: struct {
			V string `json:"'世界'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "世界"},
	}, {
		name: "PercentSlashName",
		in: struct {
			V int `json:"text/html%"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "text/html%"},
	}, {
		name: "QuotedPercentSlashName",
		in: struct {
			V int `json:"'text/html%'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "text/html%"},
	}, {
		name: "PunctuationName",
		in: struct {
			V string `json:"!#$%&()*+-./:;<=>?@[]^_{|}~ "`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "!#$%&()*+-./:;<=>?@[]^_{|}~ "},
	}, {
		name: "QuotedPunctuationName",
		in: struct {
			V string `json:"'!#$%&()*+-./:;<=>?@[]^_{|}~ '"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: "!#$%&()*+-./:;<=>?@[]^_{|}~ "},
	}, {
		name: "EmptyName",
		in: struct {
			V int `json:"''"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: ""},
	}, {
		name: "SpaceName",
		in: struct {
			V int `json:"' '"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: " "},
	}, {
		name: "CommaQuotes",
		in: struct {
			V int `json:"',\\'\"\\\"'"`
		}{},
		wantOpts: fieldOptions{hasName: true, name: `,'""`},
	}, {
		name: "SingleComma",
		in: struct {
			V int `json:","`
		}{},
		wantErr: errors.New("Go struct field V has malformed `json` tag: invalid trailing ',' character"),
	}, {
		name: "SuperfluousCommas",
		in: struct {
			V int `json:",,,,\"\",,inline,unknown,,,,"`
		}{},
		wantErr: errors.New("Go struct field V has malformed `json` tag: invalid character ',' at start of option (expecting Unicode letter or single quote)"),
	}, {
		name: "NoCaseOption",
		in: struct {
			FieldName int `json:",nocase"`
		}{},
		wantOpts: fieldOptions{
			name:   "FieldName",
			nocase: true,
		},
	}, {
		name: "InlineOption",
		in: struct {
			FieldName int `json:",inline"`
		}{},
		wantOpts: fieldOptions{
			name:   "FieldName",
			inline: true,
		},
	}, {
		name: "UnknownOption",
		in: struct {
			FieldName int `json:",unknown"`
		}{},
		wantOpts: fieldOptions{name: "FieldName", unknown: true},
	}, {
		name: "OmitZeroOption",
		in: struct {
			FieldName int `json:",omitzero"`
		}{},
		wantOpts: fieldOptions{name: "FieldName", omitzero: true},
	}, {
		name: "OmitEmptyOption",
		in: struct {
			FieldName int `json:",omitempty"`
		}{},
		wantOpts: fieldOptions{name: "FieldName", omitempty: true},
	}, {
		name: "StringOption",
		in: struct {
			FieldName int `json:",string"`
		}{},
		wantOpts: fieldOptions{name: "FieldName", string: true},
	}, {
		name: "FormatOptionEqual",
		in: struct {
			FieldName int `json:",format=fizzbuzz"`
		}{},
		wantErr: errors.New("Go struct field FieldName is missing value for `json` tag option format"),
	}, {
		name: "FormatOptionColon",
		in: struct {
			FieldName int `json:",format:fizzbuzz"`
		}{},
		wantOpts: fieldOptions{
			name:   "FieldName",
			format: "fizzbuzz",
		},
	}, {
		name: "FormatOptionQuoted",
		in: struct {
			FieldName int `json:",format:'2006-01-02'"`
		}{},
		wantOpts: fieldOptions{
			name:   "FieldName",
			format: "2006-01-02",
		},
	}, {
		name: "FormatOptionInvalid",
		in: struct {
			FieldName int `json:",format:'2006-01-02"`
		}{},
		wantErr: errors.New("Go struct field FieldName has malformed value for `json` tag option format: single-quoted string not terminated: '2006-01-0..."),
	}, {
		name: "DuplicateFormatOptions",
		in: struct {
			FieldName int `json:",format:alpha,format:bravo"`
		}{},
		wantErr: errors.New("Go struct field FieldName has duplicate appearance of `json` tag option format"),
	}, {
		name: "AllOptions",
		in: struct {
			FieldName int `json:",nocase,inline,unknown,omitzero,omitempty,string,format:format"`
		}{},
		wantOpts: fieldOptions{
			name:      "FieldName",
			nocase:    true,
			inline:    true,
			unknown:   true,
			omitzero:  true,
			omitempty: true,
			string:    true,
			format:    "format",
		},
	}, {
		name: "AllOptionsQuoted",
		in: struct {
			FieldName int `json:",'nocase','inline','unknown','omitzero','omitempty','string','format':'format'"`
		}{},
		wantErr: errors.New("Go struct field FieldName has unnecessarily quoted appearance of `json` tag option 'nocase'; specify nocase instead"),
	}, {
		name: "AllOptionsCaseSensitive",
		in: struct {
			FieldName int `json:",NOCASE,INLINE,UNKNOWN,OMITZERO,OMITEMPTY,STRING,FORMAT:FORMAT"`
		}{},
		wantErr: errors.New("Go struct field FieldName has invalid appearance of `json` tag option NOCASE; specify nocase instead"),
	}, {
		name: "AllOptionsSpaceSensitive",
		in: struct {
			FieldName int `json:", nocase , inline , unknown , omitzero , omitempty , string , format:format "`
		}{},
		wantErr: errors.New("Go struct field FieldName has malformed `json` tag: invalid character ' ' at start of option (expecting Unicode letter or single quote)"),
	}, {
		name: "UnknownTagOption",
		in: struct {
			FieldName int `json:",inline,whoknows,string"`
		}{},
		wantOpts: fieldOptions{
			name:   "FieldName",
			inline: true,
			string: true,
		},
	}, {
		name: "MalformedQuotedString/MissingQuote",
		in: struct {
			FieldName int `json:"'hello,string"`
		}{},
		wantErr: errors.New("Go struct field FieldName has malformed `json` tag: single-quoted string not terminated: 'hello,str..."),
	}, {
		name: "MalformedQuotedString/MissingComma",
		in: struct {
			FieldName int `json:"'hello'inline,string"`
		}{},
		wantErr: errors.New("Go struct field FieldName has malformed `json` tag: invalid character 'i' before next option (expecting ',')"),
	}, {
		name: "MalformedQuotedString/InvalidEscape",
		in: struct {
			FieldName int `json:"'hello\\u####',inline,string"`
		}{},
		wantErr: errors.New("Go struct field FieldName has malformed `json` tag: invalid single-quoted string: 'hello\\u####'"),
	}, {
		name: "MisnamedTag",
		in: struct {
			V int `jsom:"Misnamed"`
		}{},
		wantOpts: fieldOptions{
			name: "V",
		},
	}}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fs := reflect.TypeOf(tt.in).Field(0)
			gotOpts, gotErr := parseFieldOptions(fs)
			if !reflect.DeepEqual(gotOpts, tt.wantOpts) || !reflect.DeepEqual(gotErr, tt.wantErr) {
				t.Errorf("parseFieldOptions(%T) = (%v, %v), want (%v, %v)", tt.in, gotOpts, gotErr, tt.wantOpts, tt.wantErr)
			}
		})
	}
}
