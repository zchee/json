package json

import (
	"errors"
	"reflect"
)

type typedMarshaler struct {
	typ     reflect.Type
	fnc     marshaler
}

func MarshalFuncV1[T any](fn func(T) ([]byte, error)) typedMarshaler {
	t := reflect.TypeOf((*T)(nil)).Elem()
	return typedMarshaler{
		typ: t,
		fnc: func(mo MarshalOptions, enc *Encoder, va addressableValue) error {
			val, err := fn(va.Convert(t).Interface().(T))
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
}

type typedUnmarshaler struct {
	typ     reflect.Type
	fnc     unmarshaler
	maySkip bool
}

func UnmarshalFuncV2[T any](fn func(UnmarshalOptions, *Decoder, T) error) *Unmarshalers {
	t := reflect.TypeOf((*T)(nil)).Elem()
	typFnc := typedUnmarshaler{
		typ: t,
		fnc: func(uo UnmarshalOptions, dec *Decoder, va addressableValue) error {
			prevDepth, prevLength := dec.tokens.depthLength()
			err := fn(uo, dec, va.Convert(t).Interface().(T))
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
	return &Unmarshalers{unmarshalers{fncVals: []typedUnmarshaler{typFnc}}}
}


func init() {
	_ = UnmarshalFuncV2(func(UnmarshalOptions, *Decoder, string) error {
		return nil
	})
}
