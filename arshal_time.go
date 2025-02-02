// Copyright 2020 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package json

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"
	"time"
)

var (
	timeDurationType = reflect.TypeOf((*time.Duration)(nil)).Elem()
	timeTimeType     = reflect.TypeOf((*time.Time)(nil)).Elem()
)

func makeTimeArshaler(fncs *arshaler, t reflect.Type) *arshaler {
	// Ideally, time types would implement MarshalerV2 and UnmarshalerV2,
	// but that would incur a dependency on package json from package time.
	// Given how widely used time is, it is more acceptable that we incur a
	// dependency on time from json.
	//
	// Injecting the arshaling functionality like this will not be identical
	// to actually declaring methods on these types since embedding of the
	// time types will not be able to forward this functionality.
	switch t {
	case timeDurationType:
		fncs.nonDefault = true
		fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
			var nanos bool
			switch mo.format {
			case "":
			case "nanos":
				nanos = true
			default:
				return newInvalidFormatError("marshal", t, mo.format)
			}

			td := va.Interface().(time.Duration)
			b := enc.UnusedBuffer()
			if !nanos {
				b = append(b, '"')
				b = append(b, td.String()...) // never contains special characters
				b = append(b, '"')
			} else {
				b = strconv.AppendInt(b, int64(td), 10)
			}
			return enc.WriteValue(b)
		}
		fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
			// TODO: Should there be a flag that specifies that we can unmarshal
			// from either form since there would be no ambiguity?
			var nanos bool
			switch uo.format {
			case "":
			case "nanos":
				nanos = true
			default:
				return newInvalidFormatError("unmarshal", t, uo.format)
			}

			td := va.Addr().Interface().(*time.Duration)
			val, err := dec.ReadValue()
			if err != nil {
				return err
			}
			k := val.Kind()
			switch {
			case k == 'n':
				*td = time.Duration(0)
				return nil
			case k == '"' && !nanos:
				val = unescapeSimpleString(val)
				td2, err := time.ParseDuration(string(val))
				if err != nil {
					return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
				}
				*td = td2
				return nil
			case k == '0' && nanos:
				n, err := strconv.ParseInt(string(val), 10, 64)
				if err != nil {
					return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
				}
				*td = time.Duration(n)
				return nil
			default:
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
			}
		}
	case timeTimeType:
		fncs.nonDefault = true
		fncs.marshal = func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
			format := time.RFC3339Nano
			if mo.format != "" {
				var err error
				format, err = checkTimeFormat(mo.format)
				if err != nil {
					return &SemanticError{action: "marshal", GoType: t, Err: err}
				}
			}

			tt := va.Interface().(time.Time)
			if y := tt.Year(); y < 0 || y >= 10000 {
				// RFC 3339 is clear that years are 4 digits exactly.
				// See https://golang.org/issue/4556#c15 for more discussion.
				err := fmt.Errorf("year %d outside of range [0,9999]", y)
				return &SemanticError{action: "marshal", GoType: t, Err: err}
			}
			b := enc.UnusedBuffer()
			b = append(b, '"')
			b = tt.AppendFormat(b, format)
			b = append(b, '"')
			// The format may contain special characters that need escaping.
			// Verify that the result is a valid JSON string (common case),
			// otherwise escape the string correctly (slower case).
			if consumeSimpleString(b) != len(b) {
				b, _ = appendString(nil, string(b[len(`"`):len(b)-len(`"`)]), true, nil)
			}
			return enc.WriteValue(b)
		}
		fncs.unmarshal = func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
			format := time.RFC3339Nano
			if uo.format != "" {
				var err error
				format, err = checkTimeFormat(uo.format)
				if err != nil {
					return &SemanticError{action: "unmarshal", GoType: t, Err: err}
				}
			}

			tt := va.Addr().Interface().(*time.Time)
			val, err := dec.ReadValue()
			if err != nil {
				return err
			}
			k := val.Kind()
			switch k {
			case 'n':
				*tt = time.Time{}
				return nil
			case '"':
				val = unescapeSimpleString(val)
				tt2, err := time.Parse(format, string(val))
				if err != nil {
					return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t, Err: err}
				}
				*tt = tt2
				return nil
			default:
				return &SemanticError{action: "unmarshal", JSONKind: k, GoType: t}
			}
		}
	}
	return fncs
}

func checkTimeFormat(format string) (string, error) {
	// Assume that an exported format constant in the time package
	// will always started with uppercase ASCII.
	if len(format) > 0 && 'A' <= format[0] && format[0] <= 'Z' {
		switch format {
		case "ANSIC":
			return time.ANSIC, nil
		case "UnixDate":
			return time.UnixDate, nil
		case "RubyDate":
			return time.RubyDate, nil
		case "RFC822":
			return time.RFC822, nil
		case "RFC822Z":
			return time.RFC822Z, nil
		case "RFC850":
			return time.RFC850, nil
		case "RFC1123":
			return time.RFC1123, nil
		case "RFC1123Z":
			return time.RFC1123Z, nil
		case "RFC3339":
			return time.RFC3339, nil
		case "RFC3339Nano":
			return time.RFC3339Nano, nil
		case "Kitchen":
			return time.Kitchen, nil
		case "Stamp":
			return time.Stamp, nil
		case "StampMilli":
			return time.StampMilli, nil
		case "StampMicro":
			return time.StampMicro, nil
		case "StampNano":
			return time.StampNano, nil
		default:
			// Reject any format that is an exported Go identifier in case
			// new format constants are added to the time package.
			if strings.TrimFunc(format, isLetterOrDigit) == "" {
				return "", fmt.Errorf("undefined format layout: %v", format)
			}
		}
	}
	return format, nil
}
